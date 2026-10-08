# Animation utilities for smooth transitions and timing
# Provides easing functions, interpolation helpers, and timeline calculations

#' Apply easing function to a progress value (0-1)
#'
#' @param progress Numeric value between 0 and 1
#' @param easing Type of easing: "linear", "ease-in", "ease-out", "ease-in-out"
#' @return Eased progress value between 0 and 1
apply_easing <- function(progress, easing = "linear") {
  progress <- pmin(pmax(progress, 0), 1)  # Clamp to [0, 1]

  switch(easing,
    "ease-in" = progress^2,
    "ease-out" = 1 - (1 - progress)^2,
    "ease-in-out" = progress^2 * (3 - 2 * progress),  # Smoothstep function
    "linear" = progress,
    progress  # Default to linear if unknown
  )
}

#' Interpolate between two values with optional easing
#'
#' @param start_val Starting value
#' @param end_val Ending value
#' @param current_time Current time point
#' @param start_time Start time of transition
#' @param duration Duration of transition
#' @param easing Easing function to apply
#' @return Interpolated value
interpolate_value <- function(start_val, end_val, current_time, start_time,
                             duration, easing = "linear") {
  if (duration <= 0) {
    return(end_val)
  }

  raw_progress <- (current_time - start_time) / duration
  progress <- apply_easing(pmin(pmax(raw_progress, 0), 1), easing)

  start_val + (end_val - start_val) * progress
}

#' Calculate timeline phases with optional speed multiplier
#'
#' @param speed_multiplier Multiplier for animation speed (default 1.0)
#' @return Tibble with phase definitions
calculate_timeline_phases <- function(speed_multiplier = 1.0) {
  base_phases <- tibble::tibble(
    id = c("insurance", "illness", "routing", "profit"),
    label = c("Insurance Choice", "Illness Reveal", "Hospital Routing", "Profit & Loss"),
    start_base = c(0, 20, 30, 70),
    duration_base = c(20, 10, 40, 50),
    easing = c("ease-in-out", "ease-out", "linear", "ease-in")
  )

  base_phases %>%
    dplyr::mutate(
      start = start_base * speed_multiplier,
      duration = duration_base * speed_multiplier,
      end = start + duration
    ) %>%
    dplyr::select(id, label, start, duration, end, easing)
}

#' Calculate consumer movement parameters
#'
#' @param consumer_location Consumer's location on the ring
#' @param hospital_location Hospital's location on the ring
#' @param ring_min Minimum location value
#' @param ring_max Maximum location value
#' @param phase_start Start time of routing phase
#' @param phase_duration Duration of routing phase
#' @return List with movement parameters
calculate_movement_params <- function(consumer_location, hospital_location,
                                     ring_min, ring_max,
                                     phase_start = 30, phase_duration = 40) {
  ring_length <- max(0, ring_max - ring_min)

  # Calculate shortest distance on circular ring
  raw_distance <- abs(consumer_location - hospital_location)
  if (ring_length > 0) {
    circular_distance <- min(raw_distance, ring_length - raw_distance)
  } else {
    circular_distance <- raw_distance
  }

  # Normalize to [0, 1] for max_distance
  max_distance <- ring_length
  normalized_distance <- if (max_distance > 0) circular_distance / max_distance else 0

  # Calculate travel time proportional to distance
  travel_time <- normalized_distance * phase_duration

  # Stagger start times to avoid simultaneous movement (randomize within first 5 seconds)
  movement_start <- phase_start + stats::runif(1, 0, min(5, phase_duration * 0.125))

  # Arc height for curved path visualization
  arc_height <- circular_distance * 0.3

  list(
    travel_distance = circular_distance,
    travel_time = travel_time,
    movement_start = movement_start,
    arc_height = arc_height
  )
}

#' Define consumer state transitions
#'
#' @return Tibble with state definitions and timings
define_consumer_states <- function() {
  tibble::tibble(
    state = c("shopping", "enrolled", "healthy", "sick_untreated",
             "traveling", "in_treatment", "treated", "denied"),
    label = c("Shopping", "Enrolled", "Healthy", "Sick (Untreated)",
             "Traveling", "In Treatment", "Treated", "Denied Care"),
    typical_start = c(0, 15, 20, 25, 30, 40, 70, 30),
    typical_duration = c(15, 5, 5, 5, 10, 30, 50, 0),
    color = c("#94a3b8", "#38bdf8", "#22c55e", "#f59e0b",
             "#8b5cf6", "#ec4899", "#10b981", "#ef4444")
  )
}

#' Generate educational annotations for key learning moments
#'
#' @param emtala_enabled Whether EMTALA is enabled
#' @param medicare_pct Medicare reimbursement percentage
#' @return Tibble with annotation events
generate_learning_annotations <- function(emtala_enabled = FALSE,
                                         medicare_pct = 1.0) {
  annotations <- tibble::tibble(
    time = numeric(),
    message = character(),
    highlight = character()
  )

  # Base annotations (always shown)
  base_annotations <- tibble::tibble(
    time = c(15, 25, 45),
    message = c(
      "Young consumers weigh premiums against health risk",
      "Health shock: 30% of young and 70% of old become ill",
      "Network contracts determine where patients can receive care"
    ),
    highlight = c("premium_comparison", "illness_shock", "network_routing")
  )

  annotations <- dplyr::bind_rows(annotations, base_annotations)

  # Conditional annotations based on policy settings
  if (emtala_enabled) {
    emtala_annotation <- tibble::tibble(
      time = 35,
      message = "EMTALA requires hospitals to treat uninsured emergency patients",
      highlight = "emtala_effect"
    )
    annotations <- dplyr::bind_rows(annotations, emtala_annotation)
  }

  if (medicare_pct < 1.0) {
    medicare_annotation <- tibble::tibble(
      time = 75,
      message = sprintf("Medicare reimburses at %d%% of cost (encouraging cost-shifting)",
                       round(medicare_pct * 100)),
      highlight = "medicare_reimbursement"
    )
    annotations <- dplyr::bind_rows(annotations, medicare_annotation)
  }

  # Final annotation about profitability
  profit_annotation <- tibble::tibble(
    time = 85,
    message = "Hospitals with better networks and pricing strategies profit more",
    highlight = "network_value"
  )

  annotations <- dplyr::bind_rows(annotations, profit_annotation) %>%
    dplyr::arrange(time)

  annotations
}

#' Calculate hospital occupancy and capacity metrics over time
#'
#' @param consumer_df Data frame of consumers with hospital assignments
#' @param hospital_capacity Maximum capacity per hospital (optional)
#' @return Data frame with occupancy metrics
calculate_hospital_occupancy <- function(consumer_df, hospital_capacity = NULL) {
  # Group by hospital and calculate arrival order
  occupancy <- consumer_df %>%
    dplyr::filter(sick, !is.na(hospital_id)) %>%
    dplyr::group_by(hospital_id) %>%
    dplyr::mutate(
      arrival_order = dplyr::row_number(),
      # If capacity specified, determine overflow
      overflow = if (!is.null(hospital_capacity)) {
        arrival_order > hospital_capacity
      } else {
        FALSE
      },
      # Wait time increases for overflow patients (5 seconds per overflow position)
      wait_time = if (!is.null(hospital_capacity)) {
        pmax(0, (arrival_order - hospital_capacity) * 5)
      } else {
        0
      }
    ) %>%
    dplyr::ungroup()

  occupancy
}

#' Validate animation data structure
#'
#' @param payload Animation payload list
#' @return Validated payload (with corrections if needed)
validate_animation_data <- function(payload) {
  if (is.null(payload)) {
    stop("Animation payload cannot be NULL")
  }

  # Check for people data
  if (is.null(payload$people) || length(payload$people) == 0) {
    warning("No consumer data in animation payload")
    return(payload)
  }

  # Validate required fields in people
  required_fields <- c("id", "location", "sick", "insurerCategory")

  # Check first person for required fields
  if (length(payload$people) > 0) {
    first_person <- payload$people[[1]]
    missing <- setdiff(required_fields, names(first_person))
    if (length(missing) > 0) {
      stop(paste("Missing animation fields:", paste(missing, collapse = ", ")))
    }
  }

  # Validate numeric ranges in people (check for invalid values)
  for (i in seq_along(payload$people)) {
    person <- payload$people[[i]]

    # Fix infinite or NA locations
    if (!is.null(person$location) && (is.infinite(person$location) || is.na(person$location))) {
      warning(sprintf("Invalid location for consumer %d, setting to 0", person$id))
      payload$people[[i]]$location <- 0
    }

    # Ensure sick is logical
    if (!is.null(person$sick) && !is.logical(person$sick)) {
      payload$people[[i]]$sick <- as.logical(person$sick)
    }
  }

  payload
}

#' Create market dynamics timeline showing aggregate metrics over time
#'
#' @param consumer_df Consumer outcomes data
#' @param timeline_phases Timeline phase definitions
#' @return List with time-series data for key metrics
calculate_market_dynamics <- function(consumer_df, timeline_phases) {
  # Harmonize column names (accept snake_case input from R pipeline)
  if (!"insurerCategory" %in% names(consumer_df) && "insurer_category" %in% names(consumer_df)) {
    consumer_df$insurerCategory <- consumer_df$insurer_category
  }
  if (!"insurerId" %in% names(consumer_df) && "insurer_id" %in% names(consumer_df)) {
    consumer_df$insurerId <- consumer_df$insurer_id
  }
  if (!"insurerPremium" %in% names(consumer_df) && "insurer_premium" %in% names(consumer_df)) {
    consumer_df$insurerPremium <- consumer_df$insurer_premium
  }

  total_consumers <- nrow(consumer_df)
  if (total_consumers == 0) {
    return(list())
  }

  # Calculate metrics at key time points
  insurance_phase_end <- timeline_phases$start[timeline_phases$id == "insurance"] +
                        timeline_phases$duration[timeline_phases$id == "insurance"]
  illness_phase_end <- timeline_phases$start[timeline_phases$id == "illness"] +
                      timeline_phases$duration[timeline_phases$id == "illness"]

  # Uninsured rate over time (drops after enrollment, before illness reveal)
  uninsured_count <- sum(consumer_df$insurerCategory == "uninsured", na.rm = TRUE)
  uninsured_rate_initial <- uninsured_count / total_consumers

  uninsured_rate <- list(
    values = c(uninsured_rate_initial, uninsured_rate_initial, uninsured_rate_initial, uninsured_rate_initial),
    times = c(0, insurance_phase_end - 1, insurance_phase_end, 120)
  )

  # Average premium over time (weighted by enrollees)
  idx <- which(!is.na(consumer_df$insurerCategory) & consumer_df$insurerCategory == "private" &
                 !is.na(consumer_df$insurerId))
  if (length(idx) > 0) {
    counts <- as.integer(table(consumer_df$insurerId[idx]))
    premiums_by_insurer <- tapply(consumer_df$insurerPremium[idx], consumer_df$insurerId[idx], function(x) mean(x, na.rm = TRUE))
    total_count <- sum(counts, na.rm = TRUE)
    if (total_count > 0) {
      common_ids <- intersect(names(counts), names(premiums_by_insurer))
      weighted_avg_premium <- sum(premiums_by_insurer[common_ids] * counts[common_ids], na.rm = TRUE) / total_count
    } else {
      weighted_avg_premium <- 0
    }
  } else {
    weighted_avg_premium <- 0
  }

  average_premium <- list(
    values = c(0, weighted_avg_premium, weighted_avg_premium, weighted_avg_premium),
    times = c(0, insurance_phase_end, illness_phase_end, 120)
  )

  # Hospital utilization (jumps when illness revealed)
  total_sick <- sum(consumer_df$sick, na.rm = TRUE)
  utilization_rate <- if (total_consumers > 0) total_sick / total_consumers else 0

  hospital_utilization <- list(
    values = c(0, 0, utilization_rate, utilization_rate),
    times = c(0, illness_phase_end - 1, illness_phase_end, 120)
  )

  # Coverage rate (proportion with insurance)
  coverage_rate_value <- 1 - uninsured_rate_initial
  coverage_rate <- list(
    values = c(0, coverage_rate_value, coverage_rate_value, coverage_rate_value),
    times = c(0, insurance_phase_end, illness_phase_end, 120)
  )

  list(
    uninsured_rate = uninsured_rate,
    average_premium = average_premium,
    hospital_utilization = hospital_utilization,
    coverage_rate = coverage_rate
  )
}
