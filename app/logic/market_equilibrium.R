# Market equilibrium and matching logic

# Check if all teams have submitted decisions for the current round
check_all_submitted <- function(game_id, round) {
  game_settings <- get_game_settings(game_id)

  if (is.null(game_settings)) {
    return(FALSE)
  }

  n_hospitals <- game_settings$n_hospitals
  n_insurers <- game_settings$n_insurers

  con <- get_db_connection()

  hospital_rates <- dbGetQuery(con,
    "SELECT hospital_id, COUNT(DISTINCT insurer_id) AS cnt
     FROM hospital_rates
     WHERE game_id = ? AND round = ?
     GROUP BY hospital_id",
    params = list(game_id, round)
  )

  insurer_count <- dbGetQuery(con,
    "SELECT COUNT(DISTINCT insurer_id) as count
     FROM insurer_premiums
     WHERE game_id = ? AND round = ?",
    params = list(game_id, round)
  )$count

  network_count <- dbGetQuery(con,
    "SELECT COUNT(DISTINCT insurer_id) as count
     FROM insurer_network
     WHERE game_id = ? AND round = ?",
    params = list(game_id, round)
  )$count

  close_db_connection(con)
  hospital_submitted <- hospital_rates$hospital_id[hospital_rates$cnt >= n_insurers]
  hospital_complete <- length(unique(hospital_submitted)) == n_hospitals

  hospital_complete && insurer_count == n_insurers && network_count == n_insurers
}

# Get market summary statistics
get_market_summary <- function(game_id, round) {
  results <- get_round_results(game_id, round)

  if (nrow(results) == 0) {
    return(NULL)
  }

  # Aggregate hospital results
  hospital_results <- results %>%
    filter(!is.na(hospital_id)) %>%
    group_by(hospital_id) %>%
    summarise(
      total_patients = sum(patients_insured + patients_uninsured_treated, na.rm = TRUE),
      medicare_patients = sum(patients_charity_care, na.rm = TRUE),
      revenue = sum(revenue, na.rm = TRUE),
      costs = sum(costs, na.rm = TRUE),
      profit = sum(profit, na.rm = TRUE)
    )

  # Aggregate insurer results
  insurer_results <- results %>%
    filter(!is.na(insurer_id)) %>%
    group_by(insurer_id) %>%
    summarise(
      enrollment = sum(patients_insured, na.rm = TRUE),
      claims_paid = sum(claims_paid, na.rm = TRUE),
      mlr = mean(mlr, na.rm = TRUE),
      profit = sum(profit, na.rm = TRUE)
    )

  # Overall market statistics
  total_population <- get_game_settings(game_id)$population
  total_insured <- sum(insurer_results$enrollment, na.rm = TRUE)
  uninsured_rate <- 1 - (total_insured / total_population)

  decisions <- get_round_decisions(game_id, round)
  avg_premium <- NA_real_
  if (!is.null(decisions$insurer_premiums) && nrow(decisions$insurer_premiums) > 0) {
    avg_premium <- decisions$insurer_premiums %>%
      dplyr::summarise(avg = mean(premium_young, na.rm = TRUE)) %>%
      dplyr::pull(avg)
  }

  avg_hospital_price <- NA_real_
  if (!is.null(decisions$hospital_rates) && nrow(decisions$hospital_rates) > 0) {
    avg_hospital_price <- mean(decisions$hospital_rates$rate, na.rm = TRUE)
  }

  total_medicare <- sum(hospital_results$medicare_patients, na.rm = TRUE)

  market_summary <- list(
    total_population = total_population,
    total_insured = total_insured,
    uninsured_rate = uninsured_rate,
    avg_premium = avg_premium,
    avg_hospital_price = avg_hospital_price,
    total_medicare = total_medicare,
    hospital_results = hospital_results,
    insurer_results = insurer_results
  )

  return(market_summary)
}

# Calculate welfare metrics
calculate_welfare_metrics <- function(game_id, round) {
  results <- get_round_results(game_id, round)

  if (nrow(results) == 0) {
    return(NULL)
  }

  settings <- get_game_settings(game_id)
  if (is.null(settings)) {
    return(NULL)
  }

  decisions <- get_round_decisions(game_id, round)
  decision_inputs <- prepare_decision_inputs(decisions, settings)
  policy <- build_policy_parameters(settings)

  simulation <- simulate_excel_round(decision_inputs, settings, game_id, round, policy)

  hospitals <- decision_inputs$hospitals
  insurers <- decision_inputs$insurers
  consumers <- LEGACY_CONSUMERS
  target_population <- suppressWarnings(as.numeric(settings$population))
  if (length(target_population) == 0 || is.na(target_population) || target_population <= 0) {
    target_population <- nrow(consumers)
  }
  target_population <- round(target_population)
  consumers <- resample_consumers(consumers, target_population, game_id * 1000 + round + 503)
  delta_values <- consumers$delta
  if (isTRUE(policy$emtala_enabled)) {
    idx <- consumers$age_group != "old"
    delta_values[idx] <- delta_values[idx] + GAME_CONSTANTS$EMTALA_CHARITY_HAZARD_BONUS
  }

  n_consumers <- nrow(consumers)
  n_insurers <- nrow(insurers)

  consumer_surplus <- NA_real_

  if (n_consumers > 0) {
    chosen_utilities <- delta_values
    outside_option <- delta_values

    if (n_insurers > 0 && nrow(hospitals) > 0) {
      rate_matrix <- decision_inputs$rate_matrix
      network_matrix <- rate_matrix > 0
      rownames(rate_matrix) <- rownames(network_matrix) <- as.character(hospitals$hospital_id)
      colnames(rate_matrix) <- colnames(network_matrix) <- as.character(insurers$insurer_id)

      distance_matrix <- outer(consumers$location, hospitals$location,
                               FUN = function(c_loc, h_loc) abs(c_loc - h_loc))
      colnames(distance_matrix) <- as.character(hospitals$hospital_id)

      min_distance_matrix <- matrix(Inf, nrow = n_consumers, ncol = n_insurers)

      for (k in seq_len(n_insurers)) {
        in_network <- which(network_matrix[, k])
        if (!length(in_network)) next

        dist_sub <- distance_matrix[, in_network, drop = FALSE]
        min_distance_matrix[, k] <- apply(dist_sub, 1, min)
        closest_idx <- apply(dist_sub, 1, which.min)
      }

      premium_matrix <- matrix(0, nrow = n_consumers, ncol = n_insurers)
      for (k in seq_len(n_insurers)) {
        premium_matrix[, k] <- ifelse(
          consumers$age_group == "young", insurers$premium_young[k],
          0
        )
      }

      alpha_matrix <- matrix(consumers$alpha, nrow = n_consumers, ncol = n_insurers)
      beta_matrix <- matrix(consumers$beta, nrow = n_consumers, ncol = n_insurers)
      gamma_matrix <- matrix(consumers$gamma, nrow = n_consumers, ncol = n_insurers)
      utilities <- alpha_matrix - beta_matrix * premium_matrix + gamma_matrix * min_distance_matrix
      utilities[consumers$age_group == "old", ] <- -Inf

      outcomes <- simulation$consumer_outcomes
      insurer_lookup <- setNames(seq_len(n_insurers), insurers$insurer_id)
      chosen_idx <- which(!is.na(outcomes$insurer_id))
      if (length(chosen_idx) > 0) {
        chosen_cols <- insurer_lookup[as.character(outcomes$insurer_id[chosen_idx])]
        valid <- which(!is.na(chosen_cols))
        if (length(valid) > 0) {
          idx_rows <- chosen_idx[valid]
          idx_cols <- chosen_cols[valid]
          chosen_utilities[idx_rows] <- utilities[cbind(idx_rows, idx_cols)]
        }
      }
    }

    consumer_surplus <- sum(pmax(chosen_utilities - outside_option, 0), na.rm = TRUE)
  }

  total_hospital_profit <- simulation$hospital_financials %>%
    dplyr::summarise(total = sum(profit, na.rm = TRUE)) %>%
    dplyr::pull(total)

  total_insurer_profit <- simulation$insurer_financials %>%
    dplyr::summarise(total = sum(profit, na.rm = TRUE)) %>%
    dplyr::pull(total)

  producer_surplus <- total_hospital_profit + total_insurer_profit
  social_welfare <- consumer_surplus + producer_surplus

  list(
    consumer_surplus = consumer_surplus,
    total_hospital_profit = total_hospital_profit,
    total_insurer_profit = total_insurer_profit,
    producer_surplus = producer_surplus,
    social_welfare = social_welfare
  )
}

# Save round results to database
save_round_results <- function(game_id, round, results) {
  con <- get_db_connection()

  # Clear existing results for this round (in case of recalculation)
  dbExecute(con,
    "DELETE FROM round_results WHERE game_id = ? AND round = ?",
    params = list(game_id, round)
  )

  # Prepare hospital results
  if (!is.null(results$hospital_financials) && nrow(results$hospital_financials) > 0) {
    hospital_data <- results$hospital_financials %>%
      left_join(results$hospital_volumes, by = "hospital_id") %>%
      left_join(results$charity_care, by = "hospital_id") %>%
      mutate(
        insured_patients = dplyr::coalesce(insured_patients, 0L),
        paying_uninsured = dplyr::coalesce(paying_uninsured, 0L),
        charity_patients = dplyr::coalesce(charity_patients, 0L),
        public_patients = dplyr::coalesce(public_patients, 0L),
        total_patients = dplyr::coalesce(total_patients, insured_patients + paying_uninsured + charity_patients + public_patients)
      ) %>%
      mutate(
        game_id = game_id,
        round = round,
        insurer_id = NA,
        claims_paid = NA,
        mlr = NA
      ) %>%
      select(game_id, round, hospital_id, insurer_id,
             patients_insured = insured_patients,
             patients_uninsured_treated = paying_uninsured,
             patients_charity_care = charity_patients,
             patients_public = public_patients,
             patients_total = total_patients,
             revenue, costs, profit, claims_paid, mlr)

    dbWriteTable(con, "round_results", hospital_data, append = TRUE)
  }

  # Prepare insurer results
  if (!is.null(results$insurer_financials) && nrow(results$insurer_financials) > 0) {
    insurer_data <- results$insurer_financials %>%
      left_join(results$enrollments %>% filter(!is.na(insurer_id)), by = "insurer_id") %>%
      mutate(
        game_id = game_id,
        round = round,
        hospital_id = NA,
        patients_insured = enrollment,
        patients_uninsured_treated = NA,
        patients_charity_care = NA,
        patients_public = NA,
        patients_total = enrollment,
        revenue = premium_revenue,
        costs = claims_paid + administrative_costs
      ) %>%
      select(game_id, round, hospital_id, insurer_id,
             patients_insured, patients_uninsured_treated, patients_charity_care,
             patients_public, patients_total,
             revenue, costs, profit, claims_paid, mlr)

    dbWriteTable(con, "round_results", insurer_data, append = TRUE)
  }

  if (!is.null(results$enrollments) && any(is.na(results$enrollments$insurer_id))) {
    uninsured_total <- results$enrollments %>%
      filter(is.na(insurer_id)) %>%
      summarise(total = sum(enrollment, na.rm = TRUE)) %>%
      pull(total)

    if (!is.null(uninsured_total) && !is.na(uninsured_total) && uninsured_total > 0) {
      uninsured_df <- tibble::tibble(
        game_id = game_id,
        round = round,
        hospital_id = NA_integer_,
        insurer_id = NA_integer_,
        patients_insured = uninsured_total,
        patients_uninsured_treated = NA_integer_,
        patients_charity_care = NA_integer_,
        patients_public = NA_integer_,
        patients_total = uninsured_total,
        revenue = 0,
        costs = 0,
        profit = 0,
        claims_paid = NA_real_,
        mlr = NA_real_
      )

      dbWriteTable(con, "round_results", uninsured_df, append = TRUE)
    }
  }

  close_db_connection(con)
  return(TRUE)
}

# Calculate market concentration (HHI - Herfindahl-Hirschman Index)
calculate_market_concentration <- function(game_id, round) {
  results <- get_round_results(game_id, round)

  if (nrow(results) == 0) {
    return(NULL)
  }

  # Hospital market concentration (by patient volume)
  hospital_hhi <- results %>%
    filter(!is.na(hospital_id)) %>%
    mutate(total_patients = dplyr::coalesce(patients_total,
                                           patients_insured + patients_uninsured_treated + dplyr::coalesce(patients_charity_care, 0))) %>%
    mutate(market_share = total_patients / sum(total_patients)) %>%
    summarise(hhi = sum(market_share^2) * 10000) %>%
    pull(hhi)

  # Insurer market concentration (by enrollment)
  insurer_hhi <- results %>%
    filter(!is.na(insurer_id)) %>%
    group_by(insurer_id) %>%
    summarise(enrollment = sum(patients_insured)) %>%
    mutate(market_share = enrollment / sum(enrollment)) %>%
    summarise(hhi = sum(market_share^2) * 10000) %>%
    pull(hhi)

  concentration <- list(
    hospital_hhi = hospital_hhi,
    insurer_hhi = insurer_hhi,
    hospital_concentration = ifelse(hospital_hhi > 2500, "Highly Concentrated",
                                   ifelse(hospital_hhi > 1500, "Moderately Concentrated", "Competitive")),
    insurer_concentration = ifelse(insurer_hhi > 2500, "Highly Concentrated",
                                  ifelse(insurer_hhi > 1500, "Moderately Concentrated", "Competitive"))
  )

  return(concentration)
}

# Detect potential gaming behaviors or unrealistic strategies
detect_anomalies <- function(game_id, round) {
  decisions <- get_round_decisions(game_id, round)
  
  anomalies <- list()

  # Check for unrealistic hospital rates
  if (!is.null(decisions$hospital_rates) && nrow(decisions$hospital_rates) > 0) {
    # Extremely high negotiated rates (> 2.0)
    high_rate_hospitals <- decisions$hospital_rates %>%
      filter(rate > 2.0) %>%
      pull(hospital_id) %>%
      unique()

    if (length(high_rate_hospitals) > 0) {
      anomalies$high_hospital_rates <- high_rate_hospitals
    }

    # Extremely low rates (< 0.7, excluding 0 which means no contract)
    low_rate_hospitals <- decisions$hospital_rates %>%
      filter(rate > 0 & rate < 0.7) %>%
      pull(hospital_id) %>%
      unique()

    if (length(low_rate_hospitals) > 0) {
      anomalies$low_hospital_rates <- low_rate_hospitals
    }
  }

  # Check insurer premiums
  if (!is.null(decisions$insurer_premiums) && nrow(decisions$insurer_premiums) > 0) {
    # Premiums that are very low (potential adverse selection trap)
    # Young: expected cost = 600 * 0.3 = 180
    # Mid: expected cost = 1000 * 0.5 = 500
    low_premium_insurers <- decisions$insurer_premiums %>%
      filter(premium_young < 100) %>%  # Below ~55% of expected cost
      pull(insurer_id) %>%
      unique()

    if (length(low_premium_insurers) > 0) {
      anomalies$low_premiums <- low_premium_insurers
    }

    # Very high premiums (likely to get no enrollment)
    high_premium_insurers <- decisions$insurer_premiums %>%
      filter(premium_young > 400) %>%  # More than 2x expected cost
      pull(insurer_id) %>%
      unique()

    if (length(high_premium_insurers) > 0) {
      anomalies$high_premiums <- high_premium_insurers
    }
  }

  return(anomalies)
}

# Generate performance feedback for teams
generate_team_feedback <- function(game_id, entity_type, entity_id, round) {
  history <- get_performance_history(game_id, entity_type, entity_id)

  if (is.null(history) || nrow(history) == 0) {
    return("No performance data available yet.")
  }

  current_round_data <- history %>% filter(round == !!round)

  if (nrow(current_round_data) == 0) {
    return("No data for current round.")
  }

  feedback <- list()

  if (entity_type == "hospital") {
    # Profit feedback
    if (current_round_data$profit > 0) {
      feedback$profit <- paste0("Profitable! Generated ", format_currency(current_round_data$profit))
    } else {
      feedback$profit <- paste0("Loss of ", format_currency(abs(current_round_data$profit)),
                               ". Consider reviewing your pricing strategy.")
    }

    # Public payer feedback
    charity_pct <- current_round_data$patients_charity_care /
      (current_round_data$total_patients + current_round_data$patients_charity_care)

    if (charity_pct > 0.2) {
      feedback$charity <- "High charity care burden may be impacting profitability."
    } else if (charity_pct < 0.05) {
      feedback$charity <- "Low charity care provision. Consider the social mission."
    }

  } else if (entity_type == "insurer") {
    # Enrollment feedback
    if (current_round_data$enrollment < 500) {
      feedback$enrollment <- "Low enrollment. Consider lowering premiums or improving benefits."
    }
  }

  # Trend feedback (if multiple rounds)
  if (nrow(history) > 1) {
    profit_trend <- current_round_data$profit - history$profit[nrow(history) - 1]
    if (profit_trend > 0) {
      feedback$trend <- paste0("Profit improved by ", format_currency(profit_trend))
    } else if (profit_trend < 0) {
      feedback$trend <- paste0("Profit decreased by ", format_currency(abs(profit_trend)))
    }
  }

  return(feedback)
}
