# Zelfi


## Functions


#' Format zelfi data: make better time variables, add variables, and order data according to "open_from".
#'
#' @param zelfi_data raw data frame containing the zelfi doe or zelfi denk data
#' @param type string equal to "doe" for doe data or "denk" for denk data
#' @return formatted zelfi data frame
#' @examples format_zelfi_data(data$answers$zelfi_doe, "doe");
#' format_zelfi_data(data$answers$zelfi_denk, "denk")
#' @importFrom stringr str_c
#' @export
format_zelfi_data <- function(zelfi_data, type) {
  time_variables <- c("open_from", "started_at", "completed_at")
  order_according_to <- "open_from"
  activity_keys <- str_c("values.v_27_a", 1:13)

  is.blank <- function(x) {
    length(x) == 0 || (length(x) == 1 && is.na(x))
  }

  get_experience_variables_denk <- function(zelfi_denk_flattened) {
    negative <- rowSums(zelfi_denk_flattened[str_c("values.v_23_a", 1:10)]) > 0
    positive <- rowSums(zelfi_denk_flattened[str_c("values.v_26_a", 1:10)]) > 0
    data.frame(
      negative = negative,
      positive = positive,
      negative_only = negative & !positive,
      positive_only = positive & !negative,
      both = negative & positive,
      neither = !positive & !negative
    )
  }

  format_time_variable <- function(time_variable) {
    as.POSIXct(as.character(time_variable), format = "%Y-%m-%dT%H:%M:%S", origin = "1970-01-01", tz = "GMT")
  }

  get_time_only <- function(time_variable) {
    as.POSIXct(format(time_variable, format = "%H:%M:%S"), format = "%H:%M:%S")
  }

  format_zelfi_data2 <- function() {
    zelfi_data_flattened <- jsonlite::flatten(zelfi_data)
    zelfi_data_flattened[time_variables] <- lapply(zelfi_data_flattened[time_variables], format_time_variable)
    zelfi_data_flattened[str_c("date_only_open_from")] <- as.Date(zelfi_data_flattened[,"open_from"])
    zelfi_data_flattened[str_c("time_only_open_from")] <- get_time_only(zelfi_data_flattened[,"open_from"])

    if (type == "doe")
      zelfi_data_flattened$done_nothing <- rowSums(zelfi_data_flattened[activity_keys]) == 0
    else
      zelfi_data_flattened$experiences <- get_experience_variables_denk(zelfi_data_flattened)

    zelfi_data_flattened[order(zelfi_data_flattened[order_according_to]), ]
  }

  result <- function() {
    if (is.blank(zelfi_data))
      return(NULL)
    format_zelfi_data2()
  }

  result()
}

select_relevant_columns2 <- function(data, net_cfg, failsafe = FALSE, number_of_columns = 6, log_level = 0, force_include = NULL) {
  mssds <- psych::mssd(data)
  rnames <- NULL
  all_columns <- colnames(data)
  if (!is.null(force_include))
    rnames <- c(rnames, force_include)
  remaining_columns <- all_columns
  remaining_columns <- remove_from_vector(remaining_columns, force_include)
  remaining_columns <- select_mssd_columns(remaining_columns, mssds)
  if (length(remaining_columns) > 0) {
    df <- data.frame(data[, remaining_columns])
    colnames(df) <- remaining_columns
    skews <- z_skewness_columns(df)
    remaining_order <- order_by_quantity_unbalanced(remaining_columns, skews)
    rnames <- c(rnames, remaining_columns[remaining_order])
  } else {
    return(NULL)
  }
  if (length(rnames) > number_of_columns)
    rnames <- rnames[1:number_of_columns]
  result <- data.frame(data[, rnames])
  colnames(result) <- rnames
  return(result)
}

dynamic_graph_count <- function(varest, from_node, to_node) {
  res <- varest$varresult
  var_names <- names(res)
  i <- 0
  r <- 0
  for (equation in res) {
    i <- i + 1
    eqsum <- summary(equation)
    eqname <- var_names[[i]]
    if (eqname != to_node) next
    for (fromnodename in var_names) {
      if (fromnodename != from_node) next
      p_val <- eqsum$coefficients[paste(fromnodename, '.l1', sep = ""), 4]
      if (p_val > 0.05) next
      r <- r + 1
    }
  }
  r
}
contemp_graph_count <- function(varest, from_node, to_node) {
  # This function is actually symmetric (from_node and to_node can be swapped
  # without affecting the outcome).
  res <- varest$varresult
  var_names <- names(res)
  i <- 0
  r <- 0
  signmat <- autovar::significance_matrix(summary(varest))
  n <- length(var_names)
  for (i in 1:(n - 1)) {
    eqname <- var_names[[i]]
    if (eqname != to_node && eqname != from_node) next
    for (j in (i + 1):n) {
      fromnodename <- var_names[[j]]
      if (fromnodename != from_node && fromnodename != to_node) next
      if (signmat[j * 2, i] > 0.05 || signmat[j * 2 - 1, i] == 0) next
      r <- r + 1
    }
  }
  r
}
my_number_of_edges <- function(varest, from_node, to_node) {
  PREFERRED_AFFECT_NODES <- c('negative_affect_deactive', 'positive_affect_active')
  r <- 0
  # tie breaker rule:
  if (from_node %in% PREFERRED_AFFECT_NODES || to_node %in% PREFERRED_AFFECT_NODES)
    r <- r + 0.5
  r <- r + contemp_graph_count(varest, from_node, to_node)
  r <- r + dynamic_graph_count(varest, to_node, from_node)
  r
}

generate_zelfi_networks <- function(data, timestamp, always_include = NULL, pairs = NULL, positive_variables = NULL,
                                    negative_variables = NULL, pick_best_of = NULL, incident_to_best_of = NULL,
                                    labels = list(), measurements_per_day = 3, max_network_size = 6,
                                    include_model = FALSE, second_significances = c(0.05, 0.01, 0.005)) {
  if (class(data) != "data.frame")
    return("Data argument is not a data.frame")
  if (class(timestamp) != "character")
    return("Timestamp argument is not a character string")
  if (nchar(timestamp) != 10)
    return("Wrong timestamp format, should be: yyyy-mm-dd")
  net_cfg <- new_net_cfg()
  net_cfg$vars <- unique(names(data))
  net_cfg$timestamp <- timestamp
  net_cfg$always_include <- always_include
  if (length(pairs) %% 2 != 0)
    return("Vector of pairs should have even length")
  net_cfg$pairs <- pairs
  net_cfg$positive_variables <- unique(positive_variables)
  net_cfg$negative_variables <- unique(negative_variables)
  net_cfg$pick_best_of <- unique(pick_best_of)
  net_cfg$incident_to_best_of <- unique(incident_to_best_of)
  net_cfg$labels <- labels
  net_cfg$include_model <- include_model
  if (!(measurements_per_day %in% 1:16))
    return("measurements_per_day needs to be in 1:16")
  net_cfg$measurements_per_day <- measurements_per_day
  if (!(max_network_size %in% 2:6))
    return("max_network_size needs to be in 2:6")
  net_cfg$max_network_size <- max_network_size
  check_res <- check_config_integrity(net_cfg)
  if (!is.null(check_res))
    return(check_res)
  #for (attempt in 2:(net_cfg$max_network_size)) {
  attempt <- 2
  fail_safe <- FALSE
  number_of_columns <- net_cfg$max_network_size
  if (attempt > 1) {
    fail_safe <- TRUE
    # attempt to generate networks for the initial network size (max_network_size) twice,
    # once with balancing and once without.
    number_of_columns <- net_cfg$max_network_size + 2 - attempt
  }
  list_of_column_configs <- list()
  if (is.null(net_cfg$pick_best_of) || is.null(net_cfg$incident_to_best_of)) {
    list_of_column_configs <-
      c(list_of_column_configs,list(
        select_relevant_columns2(data,net_cfg, fail_safe, number_of_columns, log_level = 3)
      ))
  } else {
    for (idx in 1:length(net_cfg$pick_best_of)) {
      if (psych::mssd(data[,net_cfg$pick_best_of[[idx]]]) <= mssd_threshold()) {
        list_of_column_configs <- c(list_of_column_configs, list(NULL))
        next
      }
      force_include_var <- net_cfg$pick_best_of[[idx]]
      force_exclude_vars <- net_cfg$pick_best_of[net_cfg$pick_best_of != force_include_var]
      # below statement goes wrong if there is not at least TWO columns that are not force excluded (does not happen in current use)
      filtered_data <- data[, !(names(data) %in% force_exclude_vars)]
      list_of_column_configs <- c(list_of_column_configs, list(
        select_relevant_columns2(
          filtered_data,
          net_cfg,
          fail_safe,
          number_of_columns,
          log_level = 3,
          force_include = force_include_var
        )
      ))
    }
  }

  # Imputation + cutting rows part
  new_list_of_column_configs <- list()
  for (i in 1:length(list_of_column_configs)) {
    odata <- list_of_column_configs[[i]]
    if (is.null(odata)) {
      new_list_of_column_configs <- c(new_list_of_column_configs, list(NULL))
      next
    }
    first_measurement_index <- 1
    res <- select_relevant_rows(odata, timestamp, net_cfg)
    odata <- res$data
    first_measurement_index <- res$first_measurement_index
    new_timestamp <- res$timestamp
    if (any(is.na(odata)))
      odata <- impute_dataframe(odata, net_cfg$measurements_per_day)
    if (any(is.na(odata))) {
      new_list_of_column_configs <- c(new_list_of_column_configs, list(NULL))
      next # sometimes it fails
    }
    new_list_of_column_configs <- c(new_list_of_column_configs, list(
      list(
        timestamp = new_timestamp,
        first_measurement_index = first_measurement_index,
        data = odata
      )
    ))
  }
  list_of_column_configs <- new_list_of_column_configs

  best_model <- NULL
  best_bucket <- -1
  most_incident_edges <- -1
  for (idx in 1:length(list_of_column_configs)) {
    column_config <- list_of_column_configs[[idx]]
    if (is.null(column_config)) next
    ndata <- column_config$data
    d <- autovarCore::autovar(raw_dataframe = ndata,
                              selected_column_names = names(ndata),
                              measurements_per_day = net_cfg$measurements_per_day,
                              significance_levels = c(0.05, 0.01))
    if (length(d) > 0) {
      current_model <- d[[1]]
      current_bucket <- current_model$bucket
      current_number_of_incident_edges <- my_number_of_edges(
        current_model$varest,
        from_node = net_cfg$incident_to_best_of,
        to_node = net_cfg$pick_best_of[[idx]]
      )
      if (current_bucket > best_bucket || (current_bucket == best_bucket && current_number_of_incident_edges > most_incident_edges)) {
        most_incident_edges <- current_number_of_incident_edges
        best_model <- current_model
        best_bucket <- current_bucket
      }
    }
  }
  best_model
}



## Script

zelfi_networks <- function(answers, type) {
  # Type has to be 'denk' or 'doe'

  ## Constants

  IMPUTATION_ITERATIONS <-  30    # value times five = the number of iterations of imputation. E.g., 30x5 = 150 iterations.
  MEASUREMENTS_PER_DAY <- 5

  POSITIVE_AFFECT_ACTIVE_VARS   <- str_c("values.v_", c(4, 10, 12))
  POSITIVE_AFFECT_DEACTIVE_VARS <- str_c("values.v_", c(7, 8, 13))
  NEGATIVE_AFFECT_ACTIVE_VARS   <- str_c("values.v_", c(5, 11, 15))
  NEGATIVE_AFFECT_DEACTIVE_VARS <- str_c("values.v_", c(6, 9, 14))
  DOE_COLUMN_LABELS <-  c('lichamelijk_actief', 'thuis_geweest', 'prettig_gezelschap', 'in_de_buitenlucht')
  DOE_COLUMN_VARS   <- str_c("values.v_", c(22, 24, 25, 26))
  DENK_COLUMN_LABELS <-  c('piekeren', 'negatieve_gedachten', 'positieve_gedachten')
  DENK_COLUMN_VARS   <- str_c("values.v_", c(22, 25, 28))


  result <- NULL
  subtype_data <- answers[[paste('zelfi_', type, sep = '')]]
  subtype_data <- format_zelfi_data(subtype_data, type)
  timestamp <- as.character(subtype_data$date_only_open_from[1])
  column_labels <- if (type == 'doe') DOE_COLUMN_LABELS else DENK_COLUMN_LABELS
  column_vars <- if (type == 'doe') DOE_COLUMN_VARS else DENK_COLUMN_VARS
  active_vars <- if (type == 'doe') POSITIVE_AFFECT_ACTIVE_VARS else NEGATIVE_AFFECT_ACTIVE_VARS
  deactive_vars <- if (type == 'doe') POSITIVE_AFFECT_DEACTIVE_VARS else NEGATIVE_AFFECT_DEACTIVE_VARS
  affect_types <- if (type == 'doe')
    c('positive_affect_active', 'positive_affect_deactive')
  else
    c('negative_affect_active', 'negative_affect_deactive')

  # Impute once, before calling autovar
  data_selection <- subtype_data[, c(column_vars, active_vars, deactive_vars)]
  if (any(is.na(data_selection)))
    data_selection <- impute_dataframe(data_selection, MEASUREMENTS_PER_DAY, IMPUTATION_ITERATIONS)

  # Add columns for active/deactive positive/negative vars
  data_selection[[affect_types[1]]] <- rowMeans(data_selection[active_vars], na.rm = TRUE)
  data_selection[[affect_types[1]]][which(is.nan(data_selection[[affect_types[1]]]))] <- NA

  data_selection[[affect_types[2]]] <- rowMeans(data_selection[deactive_vars], na.rm = TRUE)
  data_selection[[affect_types[2]]][which(is.nan(data_selection[[affect_types[2]]]))] <- NA

  for (i in 1:length(column_labels)) {
    column_var <- column_vars[i]
    data_formodel <- data_selection[, c(column_var, affect_types)]
    gn <- generate_zelfi_networks(
      data_formodel,
      timestamp = timestamp,
      pick_best_of = affect_types,
      incident_to_best_of = column_var,
      pairs = NULL,
      positive_variables = NULL,
      negative_variables = NULL,
      labels = list(),
      measurements_per_day = MEASUREMENTS_PER_DAY,
      max_network_size = 2,
      second_significances = c(0.05, 0.01)
    )
    if (!is.null(gn) && gn$bucket > 0) {
      best_model <- gn$varest
      vars <- names(best_model$varresult)
      cat("Found a model for: ", paste(vars, collapse=', '), ". bucket: ", gn$bucket, "\n", sep='')
      name_a <- vars[vars %in% affect_types]
      label_a <- if (type == 'doe') 'positive_affect' else 'negative_affect'
      name_b <- vars[!(vars %in% affect_types)]
      label_b <- column_labels[name_b == column_vars]
      # Add a line to the result with the coefs of the best model
      result <- c(result, list(
        list(
          name_a = name_a, label_a = label_a,
          name_b = name_b, label_b = label_b,
          model = best_model
        )
      ))
    } else {
      cat("Did not find a model for: ", paste(c(column_var, affect_types), collapse=', '), "\n", sep='')
      # Don't add a line, simply skip
    }
  }
  result
}
