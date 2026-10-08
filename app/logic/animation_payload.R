# Helpers to build a rich animation payload for the market storyboard

# Source animation utilities
source("logic/animation_utils.R", local = TRUE)

assign_default_colors <- function(items, palette) {
  n <- nrow(items)
  if (!n) {
    return(character(0))
  }
  palette_len <- length(palette)
  if (!palette_len) {
    return(rep("#94a3b8", n))
  }
  palette[((seq_len(n) - 1) %% palette_len) + 1]
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

round_if_numeric <- function(x, digits = 2) {
  if (is.numeric(x)) {
    return(round(x, digits))
  }
  x
}

sanitize_list_row <- function(row_list) {
  lapply(row_list, function(value) {
    if (is.list(value)) {
      return(sanitize_list_row(value))
    }
    if (is.null(value)) {
      return(NULL)
    }
    if (length(value) == 1) {
      if (is.na(value)) {
        return(NULL)
      }
      return(value)
    }
    value[is.na(value)] <- NULL
    value
  })
}

#' Optimized conversion of data frame to list of rows
#'
#' Uses vectorized operations where possible instead of row-by-row processing
#' @param df Data frame to convert
#' @return List of sanitized row lists
df_to_list_optimized <- function(df) {
  if (nrow(df) == 0) {
    return(list())
  }

  # Convert entire data frame to JSON and back for efficiency
  # This is faster than row-by-row lapply for large datasets
  json_str <- jsonlite::toJSON(
    df,
    dataframe = "rows",
    na = "null",
    auto_unbox = TRUE,
    digits = NA  # Preserve numeric precision
  )

  # Parse back to list
  result_list <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  result_list
}

build_animation_payload <- function(game_id = 1, round = 1, ns_prefix = "", settings = NULL) {
  settings <- settings %||% get_game_settings(game_id)
  if (is.null(settings)) {
    return(NULL)
  }

  stored_results <- try(get_round_results(game_id, round), silent = TRUE)
  if (inherits(stored_results, "try-error") || is.null(stored_results) || nrow(stored_results) == 0) {
    return(NULL)
  }

  decisions <- get_round_decisions(game_id, round)
  decision_inputs <- prepare_decision_inputs(decisions, settings)
  policy <- build_policy_parameters(settings)
  simulation <- simulate_excel_round(decision_inputs, settings, game_id, round, policy)

  hospitals_db <- get_hospitals(game_id)
  insurers_db <- get_insurers(game_id)

  hospital_default_map <- setNames(
    assign_default_colors(decision_inputs$hospitals, TEAM_COLORS$hospitals),
    as.character(decision_inputs$hospitals$hospital_id)
  )

  hospitals_lookup <- decision_inputs$hospitals %>%
    dplyr::mutate(
      hospital_id = as.integer(hospital_id),
      hospital_location = as.numeric(location)
    ) %>%
    dplyr::select(hospital_id, hospital_location) %>%
    dplyr::left_join(hospitals_db, by = "hospital_id") %>%
    dplyr::mutate(
      hospital_name = dplyr::coalesce(hospital_name, paste0("Hospital ", hospital_id)),
      hospital_color = dplyr::coalesce(dplyr::na_if(color, ""), hospital_default_map[as.character(hospital_id)])
    ) %>%
    dplyr::select(hospital_id, hospital_name, hospital_color, hospital_location)

  insurer_default_map <- setNames(
    assign_default_colors(decision_inputs$insurers, TEAM_COLORS$insurers),
    as.character(decision_inputs$insurers$insurer_id)
  )

  insurers_lookup <- decision_inputs$insurers %>%
    dplyr::mutate(
      insurer_id = as.integer(insurer_id),
      premium_young = as.numeric(premium_young)
    ) %>%
    dplyr::left_join(insurers_db, by = "insurer_id") %>%
    dplyr::mutate(
      insurer_name = dplyr::coalesce(insurer_name, paste0("Insurer ", insurer_id)),
      insurer_color = dplyr::coalesce(dplyr::na_if(color, ""), insurer_default_map[as.character(insurer_id)])
    ) %>%
    dplyr::select(insurer_id, insurer_name, insurer_color, premium_young)

  # Timeline definition (seconds) - now with configurable speed and easing
  speed_multiplier <- 1.0  # Can be made configurable via settings in future
  timeline_phases <- calculate_timeline_phases(speed_multiplier)

  timeline_events <- tibble::tibble(
    time = c(0, 20, 30, 70, 120),
    label = c(
      "Enrollment opens",
      "Health shock hits",
      "Patients travel",
      "Profits tally",
      "Round complete"
    )
  )

  consumer_df <- simulation$consumer_outcomes %>%
    dplyr::mutate(
      insurer_id = as.integer(insurer_id),
      hospital_id = as.integer(hospital_id),
      location = as.numeric(location),
      sick = as.logical(sick)
    ) %>%
    dplyr::left_join(hospitals_lookup, by = "hospital_id") %>%
    dplyr::left_join(insurers_lookup, by = "insurer_id")

  # Get routing phase timing
  routing_phase <- timeline_phases %>% dplyr::filter(id == "routing")
  routing_start <- routing_phase$start
  routing_duration <- routing_phase$duration

  # Compute nearest hospital for fallback visuals and calculate movement parameters
  if (nrow(hospitals_lookup) > 0 && nrow(consumer_df) > 0) {
    loc_values <- c(consumer_df$location, hospitals_lookup$hospital_location)
    if (all(is.finite(loc_values))) {
      loc_min <- min(loc_values, na.rm = TRUE)
      loc_max <- max(loc_values, na.rm = TRUE)
      ring_length <- max(0, loc_max - loc_min)
      dist_matrix <- abs(outer(consumer_df$location, hospitals_lookup$hospital_location, "-"))
      if (ring_length > 0) {
        dist_matrix <- pmin(dist_matrix, ring_length - dist_matrix)
      }
      nearest_idx <- apply(dist_matrix, 1, function(row) {
        if (all(is.infinite(row))) return(NA_integer_)
        which.min(row)
      })
      consumer_df$nearest_hospital_id <- hospitals_lookup$hospital_id[nearest_idx]

      # Calculate enhanced movement parameters for animation
      consumer_df$travel_distance <- NA_real_
      consumer_df$travel_time <- NA_real_
      consumer_df$movement_start <- NA_real_
      consumer_df$arc_height <- NA_real_

      for (i in seq_len(nrow(consumer_df))) {
        target_hospital_id <- if (!is.na(consumer_df$hospital_id[i])) {
          consumer_df$hospital_id[i]
        } else {
          consumer_df$nearest_hospital_id[i]
        }

        if (!is.na(target_hospital_id)) {
          hospital_row <- hospitals_lookup %>%
            dplyr::filter(hospital_id == target_hospital_id) %>%
            dplyr::slice(1)

          if (nrow(hospital_row) > 0) {
            movement_params <- calculate_movement_params(
              consumer_df$location[i],
              hospital_row$hospital_location,
              loc_min, loc_max,
              routing_start, routing_duration
            )

            consumer_df$travel_distance[i] <- movement_params$travel_distance
            consumer_df$travel_time[i] <- movement_params$travel_time
            consumer_df$movement_start[i] <- movement_params$movement_start
            consumer_df$arc_height[i] <- movement_params$arc_height
          }
        }
      }
    } else {
      consumer_df$nearest_hospital_id <- NA_integer_
      consumer_df$travel_distance <- NA_real_
      consumer_df$travel_time <- NA_real_
      consumer_df$movement_start <- NA_real_
      consumer_df$arc_height <- NA_real_
    }
  } else {
    consumer_df$nearest_hospital_id <- NA_integer_
    consumer_df$travel_distance <- NA_real_
    consumer_df$travel_time <- NA_real_
    consumer_df$movement_start <- NA_real_
    consumer_df$arc_height <- NA_real_
  }

  emtala_enabled <- isTRUE(as.numeric(settings$emtala_enabled) == 1)
  medicare_pct <- ifelse(is.null(policy$medicare_reimbursement_pct),
                         GAME_CONSTANTS$POLICY_DEFAULT_MEDICARE_REIMBURSEMENT,
                         policy$medicare_reimbursement_pct)

  base_cost_young <- policy$base_costs$young %||% GAME_CONSTANTS$BASE_COSTS$young
  base_cost_old <- policy$base_costs$old %||% GAME_CONSTANTS$BASE_COSTS$old

  rate_matrix <- decision_inputs$rate_matrix
  hospital_ids_chr <- rownames(rate_matrix)
  insurer_ids_chr <- colnames(rate_matrix)

  # Add consumer state transitions based on timeline
  insurance_phase_end <- timeline_phases$start[timeline_phases$id == "insurance"] +
                        timeline_phases$duration[timeline_phases$id == "insurance"]
  illness_phase_end <- timeline_phases$start[timeline_phases$id == "illness"] +
                      timeline_phases$duration[timeline_phases$id == "illness"]
  profit_phase_start <- timeline_phases$start[timeline_phases$id == "profit"]

  consumer_df <- consumer_df %>%
    dplyr::mutate(
      insurer_category = dplyr::case_when(
        !is.na(insurer_id) ~ "private",
        insurer_choice == "Medicare" ~ "medicare",
        insurer_choice == "Uninsured" ~ "uninsured",
        TRUE ~ "other"
      ),
      # Compute bounce before it is referenced by state_final
      bounce = sick & insurer_category == "uninsured" & !emtala_enabled,
      # Calculate consumer state at key timeline points
      state_at_enrollment = "enrolled",
      state_enrollment_time = insurance_phase_end - 5 + stats::runif(dplyr::n(), 0, 5),
      state_after_illness = dplyr::if_else(sick, "sick_untreated", "healthy"),
      state_illness_time = illness_phase_end,
      state_final = dplyr::case_when(
        !sick ~ "healthy",
        sick & !is.na(hospital_id) ~ "treated",
        sick & bounce ~ "denied",
        TRUE ~ "traveling"
      ),
      state_final_time = dplyr::case_when(
        !sick ~ illness_phase_end,
        sick & !is.na(hospital_id) ~ profit_phase_start,
        TRUE ~ profit_phase_start
      ),
      insurer_label = dplyr::case_when(
        !is.na(insurer_id) ~ insurer_name,
        insurer_choice == "Medicare" ~ "Medicare",
        insurer_choice == "Uninsured" ~ "Uninsured",
        TRUE ~ insurer_choice
      ),
      insurer_color = dplyr::case_when(
        !is.na(insurer_id) ~ insurer_color,
        insurer_choice == "Medicare" ~ "#38bdf8",
        insurer_choice == "Uninsured" ~ "#f87171",
        TRUE ~ "#cbd5f5"
      ),
      hospital_name = dplyr::coalesce(hospital_name, "No Admission"),
      hospital_color = dplyr::coalesce(hospital_color, "#94a3b8"),
      base_cost = dplyr::if_else(age_group == "young", base_cost_young, base_cost_old),
      contract_rate = dplyr::case_when(
        !is.na(hospital_id) & !is.na(insurer_id) &
          as.character(hospital_id) %in% hospital_ids_chr &
          as.character(insurer_id) %in% insurer_ids_chr ~
            rate_matrix[cbind(as.character(hospital_id), as.character(insurer_id))],
        TRUE ~ NA_real_
      ),
      hospital_revenue = dplyr::case_when(
        sick & insurer_category == "private" & !is.na(contract_rate) ~ base_cost * contract_rate,
        sick & insurer_category == "medicare" & !is.na(hospital_id) ~ base_cost * medicare_pct,
        sick & insurer_category == "uninsured" & emtala_enabled & !is.na(hospital_id) ~ 0,
        TRUE ~ 0
      ),
      hospital_cost = dplyr::case_when(
        sick & !is.na(hospital_id) ~ base_cost,
        TRUE ~ 0
      ),
      insurer_claim = dplyr::case_when(
        sick & insurer_category == "private" & !is.na(contract_rate) ~ base_cost * contract_rate,
        TRUE ~ 0
      ),
      insurer_premium = dplyr::case_when(
        insurer_category == "private" & age_group == "young" ~ dplyr::coalesce(premium_young, 0),
        TRUE ~ 0
      ),
      hospital_id = ifelse(is.na(hospital_id) & bounce, nearest_hospital_id, hospital_id)
    )

  people_payload <- consumer_df %>%
    dplyr::transmute(
      id = as.integer(consumer_id),
      ageGroup = age_group,
      location = round_if_numeric(location, 2),
      insurerId = ifelse(!is.na(insurer_id), as.integer(insurer_id), NA_integer_),
      insurerCategory = insurer_category,
      insurerLabel = insurer_label,
      insurerColor = insurer_color,
      hospitalId = ifelse(!is.na(hospital_id), as.integer(hospital_id), NA_integer_),
      hospitalLabel = hospital_name,
      hospitalColor = hospital_color,
      nearestHospitalId = ifelse(!is.na(nearest_hospital_id), as.integer(nearest_hospital_id), NA_integer_),
      sick = sick,
      bounce = bounce,
      baseCost = round_if_numeric(base_cost, 2),
      hospitalRevenue = round_if_numeric(hospital_revenue, 2),
      hospitalCost = round_if_numeric(hospital_cost, 2),
      insurerPremium = round_if_numeric(insurer_premium, 2),
      insurerClaim = round_if_numeric(insurer_claim, 2),
      # Enhanced animation fields
      travelDistance = round_if_numeric(travel_distance, 2),
      travelTime = round_if_numeric(travel_time, 2),
      movementStart = round_if_numeric(movement_start, 2),
      arcHeight = round_if_numeric(arc_height, 2),
      stateEnrollment = state_at_enrollment,
      stateEnrollmentTime = round_if_numeric(state_enrollment_time, 2),
      stateAfterIllness = state_after_illness,
      stateIllnessTime = round_if_numeric(state_illness_time, 2),
      stateFinal = state_final,
      stateFinalTime = round_if_numeric(state_final_time, 2)
    )

  # Use optimized conversion for large datasets
  people_list <- df_to_list_optimized(people_payload)

  # Create financial flow events for animation
  premium_flows <- consumer_df %>%
    dplyr::filter(insurer_category == "private", insurer_premium > 0) %>%
    dplyr::transmute(
      fromType = "consumer",
      fromId = as.integer(consumer_id),
      toType = "insurer",
      toId = as.integer(insurer_id),
      amount = round_if_numeric(insurer_premium, 2),
      # Premium payments happen during enrollment phase
      timing = round_if_numeric(stats::runif(dplyr::n(), 0, insurance_phase_end), 2),
      flowType = "premium"
    )

  claim_flows <- consumer_df %>%
    dplyr::filter(sick, insurer_category == "private", !is.na(hospital_id), insurer_claim > 0) %>%
    dplyr::transmute(
      fromType = "insurer",
      fromId = as.integer(insurer_id),
      toType = "hospital",
      toId = as.integer(hospital_id),
      amount = round_if_numeric(insurer_claim, 2),
      # Claim payments happen during profit phase
      timing = round_if_numeric(stats::runif(dplyr::n(), profit_phase_start, profit_phase_start + 30), 2),
      flowType = "claim"
    )

  # Combine all financial flows
  financial_flows <- dplyr::bind_rows(premium_flows, claim_flows) %>%
    dplyr::arrange(timing)

  financial_flows_list <- if (nrow(financial_flows) > 0) {
    df_to_list_optimized(financial_flows)
  } else {
    list()
  }

  # Calculate hospital occupancy with capacity metrics
  # Estimate reasonable capacity based on typical patient load (20% buffer above average)
  avg_patients_per_hospital <- if (nrow(consumer_df) > 0 && nrow(hospitals_lookup) > 0) {
    ceiling(nrow(consumer_df) * 0.35 / nrow(hospitals_lookup) * 1.2)
  } else {
    20
  }

  hospital_occupancy <- calculate_hospital_occupancy(consumer_df, avg_patients_per_hospital)

  # Aggregate occupancy metrics by hospital
  occupancy_summary <- if (nrow(hospital_occupancy) > 0) {
    hospital_occupancy %>%
      dplyr::group_by(hospital_id) %>%
      dplyr::summarise(
        queue_length = sum(overflow, na.rm = TRUE),
        max_wait_time = max(wait_time, na.rm = TRUE),
        utilization_rate = dplyr::n() / avg_patients_per_hospital,
        .groups = "drop"
      )
  } else {
    tibble::tibble(
      hospital_id = integer(),
      queue_length = integer(),
      max_wait_time = numeric(),
      utilization_rate = numeric()
    )
  }

  hospital_summary <- simulation$hospital_financials %>%
    dplyr::left_join(simulation$hospital_volumes %>%
      dplyr::select(hospital_id, insured_patients, uninsured_patients, public_patients, total_patients),
      by = "hospital_id") %>%
    dplyr::left_join(hospitals_lookup, by = "hospital_id") %>%
    dplyr::left_join(occupancy_summary, by = "hospital_id") %>%
    dplyr::mutate(
      insured_patients = dplyr::coalesce(insured_patients, 0L),
      uninsured_patients = dplyr::coalesce(uninsured_patients, 0L),
      public_patients = dplyr::coalesce(public_patients, 0L),
      total_patients = dplyr::coalesce(total_patients, insured_patients + uninsured_patients + public_patients),
      hospital_name = dplyr::coalesce(hospital_name, paste0("Hospital ", hospital_id)),
      hospital_color = dplyr::coalesce(hospital_color, "#94a3b8"),
      private_revenue = round_if_numeric(private_revenue, 2),
      public_revenue = round_if_numeric(public_revenue, 2),
      uninsured_revenue = round_if_numeric(uninsured_revenue, 2),
      revenue = round_if_numeric(revenue, 2),
      private_cost = round_if_numeric(private_cost, 2),
      public_cost = round_if_numeric(public_cost, 2),
      uninsured_cost = round_if_numeric(uninsured_cost, 2),
      costs = round_if_numeric(costs, 2),
      profit = round_if_numeric(profit, 2),
      network_contract_cost = round_if_numeric(network_contract_cost, 2),
      capacity = avg_patients_per_hospital,
      queue_length = dplyr::coalesce(queue_length, 0L),
      max_wait_time = dplyr::coalesce(max_wait_time, 0),
      utilization_rate = round_if_numeric(dplyr::coalesce(utilization_rate, 0), 3)
    ) %>%
    dplyr::mutate(
      hospitalShort = substr(hospital_name, 1, 24)
    ) %>%
    dplyr::select(hospital_id, hospital_name, hospitalShort, hospital_color, total_patients,
                  insured_patients, uninsured_patients, public_patients,
                  private_revenue, public_revenue, uninsured_revenue, revenue,
                  private_cost, public_cost, uninsured_cost, costs,
                  profit, network_contract_cost,
                  capacity, queue_length, max_wait_time, utilization_rate)

  hospital_list <- df_to_list_optimized(hospital_summary)

  insurer_summary <- simulation$insurer_financials %>%
    dplyr::left_join(insurers_lookup, by = "insurer_id") %>%
    dplyr::mutate(
      insurer_name = dplyr::coalesce(insurer_name, paste0("Insurer ", insurer_id)),
      insurer_color = dplyr::coalesce(insurer_color, "#38bdf8"),
      insurerShort = substr(insurer_name, 1, 24),
      premium_revenue = round_if_numeric(premium_revenue, 2),
      claims_paid = round_if_numeric(claims_paid, 2),
      administrative_costs = round_if_numeric(administrative_costs, 2),
      profit = round_if_numeric(profit, 2)
    ) %>%
    dplyr::select(insurer_id, insurer_name, insurerShort, insurer_color, total_enrollees,
                  premium_revenue, claims_paid, administrative_costs, profit)

  insurer_list <- df_to_list_optimized(insurer_summary)

  summary_counts <- list(
    totalPeople = nrow(consumer_df),
    totalSick = sum(consumer_df$sick, na.rm = TRUE),
    totalBounce = sum(consumer_df$bounce, na.rm = TRUE),
    treatedPatients = sum(consumer_df$sick & !is.na(consumer_df$hospital_id), na.rm = TRUE),
    uninsuredTreated = sum(consumer_df$sick & consumer_df$insurer_category == "uninsured" & !consumer_df$bounce, na.rm = TRUE)
  )

  financial_totals <- list(
    hospitalProfit = round_if_numeric(sum(simulation$hospital_financials$profit, na.rm = TRUE), 2),
    insurerProfit = round_if_numeric(sum(simulation$insurer_financials$profit, na.rm = TRUE), 2)
  )

  # Generate educational annotations for key learning moments
  learning_annotations <- generate_learning_annotations(emtala_enabled, medicare_pct)
  annotations_list <- df_to_list_optimized(learning_annotations)

  # Calculate market dynamics over time
  market_dynamics <- calculate_market_dynamics(consumer_df, timeline_phases)

  # Convert market dynamics to serializable format
  market_dynamics_list <- lapply(names(market_dynamics), function(metric_name) {
    metric_data <- market_dynamics[[metric_name]]
    list(
      metric = metric_name,
      values = as.list(metric_data$values),
      times = as.list(metric_data$times)
    )
  })
  names(market_dynamics_list) <- names(market_dynamics)

  # Prepare the complete animation payload with validation
  payload <- list(
    nsPrefix = ns_prefix,
    round = as.integer(round),
    totalRounds = as.integer(settings$n_rounds),
    status = settings$game_status,
    policy = list(
      emtala = emtala_enabled,
      medicarePct = round_if_numeric(medicare_pct, 2),
      baseCostYoung = round_if_numeric(base_cost_young, 2),
      baseCostOld = round_if_numeric(base_cost_old, 2)
    ),
    timeline = list(
      totalSeconds = 120 * speed_multiplier,
      phases = df_to_list_optimized(timeline_phases),
      events = df_to_list_optimized(timeline_events),
      annotations = annotations_list
    ),
    summary = summary_counts,
    people = people_list,
    hospitals = hospital_list,
    insurers = insurer_list,
    financials = financial_totals,
    financialFlows = financial_flows_list,
    marketDynamics = market_dynamics_list
  )

  # Validate the payload before returning
  payload <- validate_animation_data(payload)

  payload
}
