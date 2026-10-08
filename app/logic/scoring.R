# Scoring and performance evaluation system

# Calculate per-round scores (students earn points each round independently)
calculate_cumulative_scores <- function(game_id) {
  game_settings <- get_game_settings(game_id)

  if (is.null(game_settings)) {
    return(NULL)
  }

  current_round <- game_settings$current_round

  con <- get_db_connection()

  # Get results for current round only (per-round scoring, not cumulative)
  all_results <- dbGetQuery(con,
    "SELECT * FROM round_results WHERE game_id = ? AND round = ?",
    params = list(game_id, current_round)
  )

  close_db_connection(con)

  if (nrow(all_results) == 0) {
    return(NULL)
  }

  # Hospital scores for this round
  hospital_scores <- all_results %>%
    filter(!is.na(hospital_id)) %>%
    mutate(
      total_profit = profit,
      total_patients = dplyr::coalesce(patients_total, patients_insured + patients_uninsured_treated + dplyr::coalesce(patients_charity_care, 0) + dplyr::coalesce(patients_public, 0)),
      total_charity = dplyr::coalesce(patients_charity_care, 0),
      avg_margin = ifelse(revenue > 0, profit / revenue, 0),
      rounds_played = 1L
    ) %>%
    select(hospital_id, total_profit, total_patients, total_charity, avg_margin, rounds_played) %>%
    arrange(desc(total_patients), desc(total_profit))

  # Insurer scores for this round
  insurer_scores <- all_results %>%
    filter(!is.na(insurer_id)) %>%
    mutate(
      total_profit = profit,
      total_enrollment = patients_insured,
      avg_mlr = mlr,
      total_claims = claims_paid,
      rounds_played = 1L
    ) %>%
    select(insurer_id, total_profit, total_enrollment, avg_mlr, total_claims, rounds_played) %>%
    arrange(desc(total_profit))

  scores <- list(
    hospitals = hospital_scores,
    insurers = insurer_scores
  )

  return(scores)
}

# Assign rankings based on performance
assign_rankings <- function(scores) {
  if (is.null(scores)) {
    return(NULL)
  }

  # Rank hospitals by profit
  if (!is.null(scores$hospitals) && nrow(scores$hospitals) > 0) {
    scores$hospitals <- scores$hospitals %>%
      mutate(
        profit_rank = rank(-total_profit, ties.method = "min"),
        volume_rank = rank(-total_patients, ties.method = "min"),
        charity_rank = rank(-total_charity, ties.method = "min")
      )
  }

  # Rank insurers by profit and enrollment
  if (!is.null(scores$insurers) && nrow(scores$insurers) > 0) {
    scores$insurers <- scores$insurers %>%
      mutate(
        profit_rank = rank(-total_profit, ties.method = "min"),
        enrollment_rank = rank(-total_enrollment, ties.method = "min")
      )
  }

  return(scores)
}

# Get historical performance data for charting
get_performance_history <- function(game_id, entity_type, entity_id) {
  # entity_type: "hospital" or "insurer"
  # entity_id: the specific hospital_id or insurer_id

  con <- get_db_connection()

  if (entity_type == "hospital") {
    history <- dbGetQuery(con,
      "SELECT round, revenue, costs, profit,
              COALESCE(patients_total, patients_insured + patients_uninsured_treated + patients_charity_care) as total_patients,
              patients_charity_care
       FROM round_results
       WHERE game_id = ? AND hospital_id = ?
       ORDER BY round",
      params = list(game_id, entity_id)
    )
  } else if (entity_type == "insurer") {
    history <- dbGetQuery(con,
      "SELECT round, revenue, costs, profit,
              patients_insured as enrollment,
              claims_paid, mlr
       FROM round_results
       WHERE game_id = ? AND insurer_id = ?
       ORDER BY round",
      params = list(game_id, entity_id)
    )
  } else {
    close_db_connection(con)
    return(NULL)
  }

  close_db_connection(con)
  return(history)
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
    mutate(total_patients = dplyr::coalesce(patients_total, patients_insured + patients_uninsured_treated + patients_charity_care)) %>%
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
    # Young: expected cost = 1000 * 0.3 = 300
    low_premium_insurers <- decisions$insurer_premiums %>%
      filter(premium_young < 165) %>%  # Below ~55% of expected cost
      pull(insurer_id) %>%
      unique()

    if (length(low_premium_insurers) > 0) {
      anomalies$low_premiums <- low_premium_insurers
    }

    # Very high premiums (likely to get no enrollment)
    high_premium_insurers <- decisions$insurer_premiums %>%
      filter(premium_young > 600) %>%  # More than 2x expected cost
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
