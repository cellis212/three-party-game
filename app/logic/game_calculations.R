# Core game calculation engine aligned with the original Excel workbook

# Build the static consumer roster exactly as defined in the legacy spreadsheet
build_consumer_baseline <- function() {
  consumers_list <- list()
  next_id <- 1

  for (age_key in names(LEGACY_MODEL$consumers)) {
    group <- LEGACY_MODEL$consumers[[age_key]]
    n <- length(group$locations)

    consumers_list[[age_key]] <- tibble::tibble(
      consumer_id = seq.int(next_id, next_id + n - 1),
      age_group = age_key,
      location = as.numeric(group$locations),
      alpha = as.numeric(group$alpha),
      beta = as.numeric(group$beta),
      gamma = as.numeric(group$gamma),
      delta = as.numeric(group$delta)
    )

    next_id <- next_id + n
  }

  dplyr::bind_rows(consumers_list)
}

LEGACY_CONSUMERS <- build_consumer_baseline()

generate_random_consumers <- function(seed = NULL,
                                      location_sd = 4,
                                      alpha_sd = 0.08,
                                      beta_sd = 0.25,  # Increased from 0.1 for more heterogeneity
                                      gamma_sd = 0.12,
                                      delta_sd = 0.1) {
  consumers <- LEGACY_CONSUMERS

  if (!is.null(seed) && !is.na(seed)) {
    set.seed(seed)
  }

  n <- nrow(consumers)
  if (!n) {
    return(consumers)
  }

  location_bounds <- range(consumers$location, na.rm = TRUE)

  consumers <- consumers %>%
    dplyr::mutate(
      location = pmin(pmax(location + stats::rnorm(n, mean = 0, sd = location_sd), location_bounds[1]), location_bounds[2]),
      alpha = pmax(0, alpha * (1 + stats::rnorm(n, mean = 0, sd = alpha_sd))),
      # Beta varies around baseline (0.5) with heterogeneity, clamped to [0.25, 1.0]
      beta = pmin(pmax(beta * (1 + stats::rnorm(n, mean = 0, sd = beta_sd)), 0.25), 1.0),
      gamma = gamma * (1 + stats::rnorm(n, mean = 0, sd = gamma_sd)),
      delta = pmax(0, delta * (1 + stats::rnorm(n, mean = 0, sd = delta_sd)))
    )

  consumers$location <- round(consumers$location, 2)
  consumers$consumer_id <- seq_len(nrow(consumers))

  consumers
}

get_base_cost <- function(age_key) {
  GAME_CONSTANTS$BASE_COSTS[[age_key]]
}

get_health_probability <- function(age_key) {
  GAME_CONSTANTS$HEALTH_PROBABILITIES[[age_key]]
}

resample_consumers <- function(consumers, target_n, seed) {
  if (is.null(target_n) || length(target_n) == 0 || is.na(target_n) || target_n <= 0) {
    return(consumers)
  }

  current_n <- nrow(consumers)
  if (!current_n) {
    return(consumers)
  }

  set.seed(seed)

  target_props <- c(young = 0.7, old = 0.3)
  available_groups <- intersect(names(target_props), unique(consumers$age_group))

  if (length(available_groups) == length(target_props)) {
    desired_counts_raw <- target_props * target_n
    base_counts <- floor(desired_counts_raw)
    remainder <- target_n - sum(base_counts)

    if (remainder > 0) {
      fractional <- desired_counts_raw - base_counts
      order_groups <- names(sort(fractional, decreasing = TRUE))
      idx <- 1
      while (remainder > 0) {
        group <- order_groups[idx]
        base_counts[group] <- base_counts[group] + 1
        remainder <- remainder - 1
        idx <- if (idx %% length(order_groups) == 0) 1 else idx + 1
      }
    }

    sampled_list <- vector("list", length = length(base_counts))
    names(sampled_list) <- names(base_counts)
    split_consumers <- split(consumers, consumers$age_group)

    for (age in names(base_counts)) {
      count <- base_counts[[age]]
      if (!count) {
        sampled_list[[age]] <- consumers[FALSE, , drop = FALSE]
        next
      }
      cohort <- split_consumers[[age]]
      if (is.null(cohort) || !nrow(cohort)) {
        next
      }
      replace_flag <- nrow(cohort) < count
      indices <- sample.int(nrow(cohort), size = count, replace = replace_flag)
      sampled_list[[age]] <- cohort[indices, , drop = FALSE]
    }

    adjusted <- do.call(rbind, sampled_list)
  } else {
    replace_flag <- target_n > current_n
    indices <- sample.int(current_n, size = target_n, replace = replace_flag)
    adjusted <- consumers[indices, , drop = FALSE]
  }

  adjusted$consumer_id <- seq_len(nrow(adjusted))
  rownames(adjusted) <- NULL
  adjusted
}

build_policy_parameters <- function(settings) {
  to_numeric <- function(value, default) {
    if (is.null(value)) {
      return(default)
    }
    value_num <- as.numeric(value)
    if (length(value_num) == 0 || is.na(value_num)) {
      return(default)
    }
    value_num
  }

  clamp01 <- function(x) {
    pmin(pmax(x, 0), 1)
  }

  base_costs <- list(
    young = to_numeric(settings$cost_young, GAME_CONSTANTS$BASE_COSTS$young),
    old = to_numeric(settings$cost_old, GAME_CONSTANTS$BASE_COSTS$old)
  )

  health_probabilities <- list(
    young = clamp01(to_numeric(settings$prob_young, GAME_CONSTANTS$HEALTH_PROBABILITIES$young)),
    old = clamp01(to_numeric(settings$prob_old, GAME_CONSTANTS$HEALTH_PROBABILITIES$old))
  )

  list(
    emtala_enabled = isTRUE(as.numeric(settings$emtala_enabled) == 1),
    medicare_reimbursement_pct = to_numeric(settings$medicare_reimbursement_pct,
                                            GAME_CONSTANTS$POLICY_DEFAULT_MEDICARE_REIMBURSEMENT),
    dsh_reimbursement_pct = to_numeric(settings$dsh_reimbursement_pct,
                                       GAME_CONSTANTS$POLICY_DEFAULT_DSH_REIMBURSEMENT),
    base_costs = base_costs,
    health_probabilities = health_probabilities
  )
}

prepare_decision_inputs <- function(decision_bundle, settings) {
  n_hospitals <- settings$n_hospitals
  n_insurers <- settings$n_insurers

  if (is.na(n_hospitals) || n_hospitals <= 0) {
    stop("Number of hospitals must be a positive integer.")
  }
  if (is.na(n_insurers) || n_insurers <= 0) {
    stop("Number of insurers must be a positive integer.")
  }

  if (n_hospitals > GAME_CONSTANTS$MAX_HOSPITALS) {
    stop(sprintf("Excel model supports at most %d hospitals", GAME_CONSTANTS$MAX_HOSPITALS))
  }
  if (n_insurers > GAME_CONSTANTS$MAX_INSURERS) {
    stop(sprintf("Excel model supports at most %d insurers", GAME_CONSTANTS$MAX_INSURERS))
  }

  # Place hospitals evenly around a circular market so ids 1..n are neighbors on a ring
  legacy_range <- range(LEGACY_MODEL$hospital_locations, na.rm = TRUE)
  min_loc <- legacy_range[1]
  max_loc <- legacy_range[2]
  circumference <- max(0, max_loc - min_loc)
  even_positions <- if (n_hospitals <= 0 || circumference <= 0) {
    rep(min_loc, length.out = n_hospitals)
  } else {
    min_loc + (circumference * (seq_len(n_hospitals) - 1) / n_hospitals)
  }

  hospitals <- tibble::tibble(
    hospital_id = seq_len(n_hospitals),
    location = even_positions
  )

  insurer_ids <- seq_len(n_insurers)
  insurers <- tibble::tibble(
    insurer_id = insurer_ids,
    premium_young = rep(GAME_CONSTANTS$DEFAULT_PREMIUM_YOUNG, n_insurers)
  )

  if (!is.null(decision_bundle$insurer_premiums) && nrow(decision_bundle$insurer_premiums) > 0) {
    premiums <- decision_bundle$insurer_premiums
    if (!"round" %in% names(premiums)) premiums$round <- 0
    if (!"submitted_at" %in% names(premiums)) premiums$submitted_at <- Sys.time()
    # Ensure submitted_at is POSIXct for proper ordering and coalescing
    premiums$submitted_at <- suppressWarnings(as.POSIXct(premiums$submitted_at))
    premiums$submitted_at <- dplyr::coalesce(premiums$submitted_at, Sys.time())
    premiums <- premiums %>%
      dplyr::arrange(round, submitted_at, insurer_id) %>%
      dplyr::group_by(insurer_id) %>%
      dplyr::summarise(
        premium_young = dplyr::last(premium_young, default = GAME_CONSTANTS$DEFAULT_PREMIUM_YOUNG),
        .groups = "drop"
      )
    insurers <- insurers %>%
      dplyr::left_join(premiums, by = "insurer_id", suffix = c("", "_new")) %>%
      dplyr::mutate(
        premium_young = dplyr::coalesce(premium_young_new, premium_young)
      ) %>%
      dplyr::select(-dplyr::ends_with("_new"))
  }

  rate_matrix <- matrix(
    GAME_CONSTANTS$DEFAULT_NEGOTIATED_RATE,
    nrow = n_hospitals,
    ncol = n_insurers,
    dimnames = list(as.character(hospitals$hospital_id), as.character(insurer_ids))
  )
  rate_matrix[,] <- 0  # Default to no contract unless specified

  if (!is.null(decision_bundle$hospital_rates) && nrow(decision_bundle$hospital_rates) > 0) {
    rates <- decision_bundle$hospital_rates %>%
      dplyr::filter(hospital_id <= n_hospitals, insurer_id <= n_insurers)
    if (!"submitted_at" %in% names(rates)) rates$submitted_at <- Sys.time()
    # Ensure submitted_at is POSIXct for proper ordering
    rates$submitted_at <- suppressWarnings(as.POSIXct(rates$submitted_at))
    rates$submitted_at <- dplyr::coalesce(rates$submitted_at, Sys.time())
    rates <- rates %>%
      dplyr::arrange(hospital_id, insurer_id, submitted_at) %>%
      dplyr::group_by(hospital_id, insurer_id) %>%
      dplyr::summarise(rate = dplyr::last(rate, default = 0), .groups = "drop")

    for (i in seq_len(nrow(rates))) {
      row <- rates[i, ]
      rate_matrix[as.character(row$hospital_id), as.character(row$insurer_id)] <- as.numeric(row$rate)
    }
  }

  # Apply insurer network choices: zero out rejected hospitals
  if (!is.null(decision_bundle$insurer_network) && nrow(decision_bundle$insurer_network) > 0) {
    for (row in seq_len(nrow(decision_bundle$insurer_network))) {
      entry <- decision_bundle$insurer_network[row, ]
      if (entry$accepted == 0) {
        h_key <- as.character(entry$hospital_id)
        i_key <- as.character(entry$insurer_id)
        if (h_key %in% rownames(rate_matrix) && i_key %in% colnames(rate_matrix)) {
          rate_matrix[h_key, i_key] <- 0
        }
      }
    }
  }

  list(
    hospitals = hospitals,
    insurers = insurers,
    rate_matrix = rate_matrix
  )
}

sample_health_status <- function(consumers, round_seed, health_probs) {
  sick <- rep(FALSE, nrow(consumers))
  age_order <- c("young", "old")

  for (i in seq_along(age_order)) {
    age <- age_order[i]
    idx <- which(consumers$age_group == age)
    if (!length(idx)) next

    prob <- health_probs[[age]]
    if (is.null(prob) || is.na(prob)) {
      prob <- get_health_probability(age)
    }
    if (is.null(prob) || is.na(prob)) {
      next
    }

    set.seed(round_seed + i)
    draws <- sample.int(10, length(idx), replace = TRUE)
    threshold <- round(prob * 10)
    sick[idx] <- draws <= threshold
  }

  sick
}

simulate_excel_round <- function(decision_inputs, settings, game_id, round, policy, override_sick = NULL) {
  hospitals <- decision_inputs$hospitals
  insurers <- decision_inputs$insurers
  rate_matrix <- decision_inputs$rate_matrix

  n_hospitals <- nrow(hospitals)
  n_insurers <- nrow(insurers)

  consumers <- get_game_consumers(game_id)
  if (is.null(consumers) || nrow(consumers) == 0) {
    consumers <- LEGACY_CONSUMERS
  } else {
    consumers <- consumers %>%
      dplyr::arrange(consumer_id) %>%
      dplyr::mutate(consumer_id = seq_len(dplyr::n()))
  }
  consumers <- as.data.frame(consumers)
  consumers$age_group <- as.character(consumers$age_group)
  population_target <- suppressWarnings(as.numeric(settings$population))
  if (length(population_target) == 0 || is.na(population_target) || population_target <= 0) {
    population_target <- nrow(consumers)
  }
  population_target <- round(population_target)
  consumers <- resample_consumers(consumers, population_target, game_id * 1000 + round + 503)

  total_consumers <- nrow(consumers)

  policy_cost_lookup <- function(age_key) {
    cost <- policy$base_costs[[age_key]]
    if (is.null(cost) || is.na(cost)) {
      return(GAME_CONSTANTS$BASE_COSTS[[age_key]])
    }
    cost
  }

  policy_cost_vector <- function(age_vec) {
    vapply(age_vec, policy_cost_lookup, numeric(1))
  }

  medicare_pct <- policy$medicare_reimbursement_pct
  if (is.null(medicare_pct) || is.na(medicare_pct)) {
    medicare_pct <- GAME_CONSTANTS$POLICY_DEFAULT_MEDICARE_REIMBURSEMENT
  }

  policy_health_lookup <- function(age_key) {
    prob <- NULL
    if (!is.null(policy$health_probabilities)) {
      prob <- policy$health_probabilities[[age_key]]
    }
    if (is.null(prob) || is.na(prob)) {
      prob <- GAME_CONSTANTS$HEALTH_PROBABILITIES[[age_key]]
    }
    prob
  }

  policy_health_probabilities <- list(
    young = policy_health_lookup("young"),
    old = policy_health_lookup("old")
  )

  delta_values <- consumers$delta
  if (isTRUE(policy$emtala_enabled)) {
    # EMTALA charity hazard: free ER care makes being uninsured more attractive
    # Healthy people rationally skip insurance when hospitals must treat them regardless
    uninsured_idx <- consumers$age_group != "old"
    delta_values[uninsured_idx] <- delta_values[uninsured_idx] + GAME_CONSTANTS$EMTALA_CHARITY_HAZARD_BONUS
  }

  # Circular distance with wrap-around on the location axis
  loc_min <- min(c(consumers$location, hospitals$location), na.rm = TRUE)
  loc_max <- max(c(consumers$location, hospitals$location), na.rm = TRUE)
  ring_length <- max(0, loc_max - loc_min)
  distance_matrix <- outer(
    consumers$location, hospitals$location,
    FUN = function(c_loc, h_loc) {
      d <- abs(c_loc - h_loc)
      if (ring_length > 0) pmin(d, ring_length - d) else d
    }
  )
  colnames(distance_matrix) <- as.character(hospitals$hospital_id)

  network_matrix <- rate_matrix > 0
  rownames(rate_matrix) <- rownames(network_matrix) <- as.character(hospitals$hospital_id)
  colnames(rate_matrix) <- colnames(network_matrix) <- as.character(insurers$insurer_id)

  if (n_insurers > 0) {
    hospital_network_counts <- rowSums(network_matrix)
  } else {
    hospital_network_counts <- rep(0, n_hospitals)
  }

  if (n_hospitals > 0) {
    insurer_network_counts <- colSums(network_matrix)
  } else {
    insurer_network_counts <- rep(0, n_insurers)
  }

  # Linear cost: first deal free, each additional deal costs step_cost
  # 1 deal: 0, 2 deals: step, 3 deals: 2*step, 4 deals: 3*step, ...
  compute_network_cost <- function(counts, step_cost) {
    n <- pmax(counts, 0)
    ifelse(n <= 1, 0, step_cost * (n - 1))
  }

  hospital_network_stats <- tibble::tibble(
    hospital_id = hospitals$hospital_id,
    network_contracts = as.integer(hospital_network_counts),
    network_contract_cost = compute_network_cost(hospital_network_counts, GAME_CONSTANTS$HOSPITAL_NETWORK_CONTRACT_STEP)
  )

  insurer_network_stats <- tibble::tibble(
    insurer_id = insurers$insurer_id,
    network_contracts = as.integer(insurer_network_counts),
    network_contract_cost = compute_network_cost(insurer_network_counts, GAME_CONSTANTS$INSURER_NETWORK_CONTRACT_STEP)
  )

  # Precompute minimum distances and closest hospitals for each insurer
  min_distance_matrix <- matrix(Inf, nrow = total_consumers, ncol = n_insurers)
  closest_hospital_matrix <- matrix(NA_integer_, nrow = total_consumers, ncol = n_insurers)

  if (n_insurers > 0) {
    for (k in seq_len(n_insurers)) {
      in_network <- which(network_matrix[, k])
      if (!length(in_network)) next

      dist_sub <- distance_matrix[, in_network, drop = FALSE]
      min_distance_matrix[, k] <- apply(dist_sub, 1, min)
      closest_idx <- apply(dist_sub, 1, which.min)
      closest_hospital_matrix[, k] <- in_network[closest_idx]
    }
  }

  closest_uninsured <- apply(distance_matrix, 1, which.min)
  closest_medicare <- closest_uninsured

  premium_matrix <- matrix(0, nrow = total_consumers, ncol = n_insurers)
  if (n_insurers > 0) {
    for (k in seq_len(n_insurers)) {
      premium_matrix[, k] <- ifelse(
        consumers$age_group == "young", insurers$premium_young[k],
        0
      )
    }
  }

  # Consumer-specific insurer taste shocks to avoid all-or-nothing switching on small premium changes
  preference_matrix <- matrix(0, nrow = total_consumers, ncol = n_insurers)
  if (n_insurers > 0) {
    game_seed <- suppressWarnings(as.numeric(game_id))
    round_seed <- suppressWarnings(as.numeric(round))
    if (is.na(game_seed)) game_seed <- 0
    if (is.na(round_seed)) round_seed <- 0
    preference_seed <- as.integer((game_seed * 1000 + round_seed + 811) %% .Machine$integer.max)
    if (is.na(preference_seed) || preference_seed < 0L) {
      preference_seed <- 811L
    }
    set.seed(preference_seed)
    preference_matrix <- matrix(
      stats::runif(total_consumers * n_insurers, min = 0, max = 80),
      nrow = total_consumers,
      ncol = n_insurers
    )
  }

  utilities <- matrix(-Inf, nrow = total_consumers, ncol = n_insurers)
  if (n_insurers > 0) {
    alpha_matrix <- matrix(consumers$alpha, nrow = total_consumers, ncol = n_insurers)
    beta_matrix <- matrix(consumers$beta, nrow = total_consumers, ncol = n_insurers)
    gamma_matrix <- matrix(consumers$gamma, nrow = total_consumers, ncol = n_insurers)
    utilities <- alpha_matrix - beta_matrix * premium_matrix + gamma_matrix * min_distance_matrix + preference_matrix
    utilities[consumers$age_group == "old", ] <- -Inf
  }

  insurer_choice <- rep("Medicare", total_consumers)
  insurer_id <- rep(NA_integer_, total_consumers)

  if (n_insurers > 0) {
    non_old_idx <- which(consumers$age_group != "old")
    if (length(non_old_idx) > 0) {
      option_matrix <- cbind(utilities[non_old_idx, , drop = FALSE], delta_values[non_old_idx])
      choice_idx <- apply(option_matrix, 1, which.max)

      for (i in seq_along(non_old_idx)) {
        idx <- non_old_idx[i]
        choice <- choice_idx[i]
        if (choice <= n_insurers) {
          chosen_insurer <- insurers$insurer_id[choice]
          insurer_id[idx] <- chosen_insurer
          insurer_choice[idx] <- as.character(chosen_insurer)
        } else {
          insurer_choice[idx] <- "Uninsured"
        }
      }
    }
  } else {
    # No insurers available => everyone either Medicare (old) or uninsured (others)
    non_old_idx <- which(consumers$age_group != "old")
    insurer_choice[non_old_idx] <- "Uninsured"
  }

  sick <- if (is.null(override_sick)) {
    sample_health_status(consumers, game_id * 1000 + round, policy_health_probabilities)
  } else {
    if (length(override_sick) != nrow(consumers)) {
      stop("override_sick must have length equal to the number of consumers")
    }
    as.logical(override_sick)
  }

  hospital_choice <- rep(NA_integer_, total_consumers)

  if (n_insurers > 0) {
    insurer_index_lookup <- setNames(seq_len(n_insurers), insurers$insurer_id)
    private_idx <- which(sick & !is.na(insurer_id))

    if (length(private_idx) > 0) {
      for (idx in private_idx) {
        col_idx <- insurer_index_lookup[as.character(insurer_id[idx])]
        chosen_hospital <- closest_hospital_matrix[idx, col_idx]
        hospital_choice[idx] <- chosen_hospital
      }
    }
  }

  uninsured_idx <- which(sick & insurer_choice == "Uninsured")
  if (length(uninsured_idx) > 0 && isTRUE(policy$emtala_enabled)) {
    # With EMTALA: hospitals must treat uninsured, reimbursed at DSH rate
    distance_sub <- distance_matrix[uninsured_idx, , drop = FALSE]
    best_cols <- apply(distance_sub, 1, which.min)
    hospital_choice[uninsured_idx] <- hospitals$hospital_id[best_cols]
  }
  # Without EMTALA: uninsured get no care (hospital_choice stays NA)

  medicare_idx <- which(sick & insurer_choice == "Medicare")
  if (length(medicare_idx) > 0) {
    # Distribute Medicare patients as evenly as possible across hospitals
    total_medicare <- length(medicare_idx)
    if (n_hospitals > 0) {
      base_per_hospital <- total_medicare %/% n_hospitals
      extra <- total_medicare %% n_hospitals
      start_pos <- ((round - 1) %% n_hospitals) + 1  # rotate remainder start by round
      counts <- rep(base_per_hospital, n_hospitals)
      if (extra > 0) {
        order_idx <- c(seq.int(start_pos, n_hospitals), if (start_pos > 1) seq.int(1, start_pos - 1) else integer(0))
        counts[order_idx[seq_len(extra)]] <- counts[order_idx[seq_len(extra)]] + 1
      }
      assignment <- rep(hospitals$hospital_id, counts)
      hospital_choice[medicare_idx] <- assignment
    }
  }

  consumer_outcomes <- tibble::tibble(
    consumer_id = consumers$consumer_id,
    age_group = consumers$age_group,
    location = consumers$location,
    insurer_choice = insurer_choice,
    insurer_id = insurer_id,
    sick = sick,
    hospital_id = hospital_choice
  )

  # Enrollment including uninsured
  enrollments_private <- consumer_outcomes %>%
    dplyr::filter(!is.na(insurer_id)) %>%
    dplyr::group_by(insurer_id) %>%
    dplyr::summarise(enrollment = dplyr::n(), .groups = "drop")

  uninsured_total <- sum(consumer_outcomes$insurer_choice == "Uninsured")
  enrollments <- enrollments_private
  if (uninsured_total > 0) {
    enrollments <- dplyr::bind_rows(enrollments,
                                    tibble::tibble(insurer_id = NA_integer_, enrollment = uninsured_total))
  }

  private_visits <- consumer_outcomes %>%
    dplyr::filter(sick, !is.na(insurer_id), !is.na(hospital_id)) %>%
    dplyr::group_by(hospital_id, insurer_id, age_group) %>%
    dplyr::summarise(patients = dplyr::n(), .groups = "drop")

  uninsured_visits <- consumer_outcomes %>%
    dplyr::filter(sick, insurer_choice == "Uninsured", !is.na(hospital_id)) %>%
    dplyr::group_by(hospital_id, age_group) %>%
    dplyr::summarise(patients = dplyr::n(), .groups = "drop")

  medicare_visits <- consumer_outcomes %>%
    dplyr::filter(sick, insurer_choice == "Medicare", !is.na(hospital_id)) %>%
    dplyr::group_by(hospital_id, age_group) %>%
    dplyr::summarise(patients = dplyr::n(), .groups = "drop")

  # Hospital volumes
  hospital_private_counts <- private_visits %>%
    dplyr::group_by(hospital_id) %>%
    dplyr::summarise(insured_patients = sum(patients), .groups = "drop")

  hospital_uninsured_counts <- uninsured_visits %>%
    dplyr::group_by(hospital_id) %>%
    dplyr::summarise(uninsured_patients = sum(patients), .groups = "drop")

  hospital_public_counts <- medicare_visits %>%
    dplyr::group_by(hospital_id) %>%
    dplyr::summarise(public_patients = sum(patients), .groups = "drop")

  hospital_volumes <- hospitals %>%
    dplyr::select(hospital_id) %>%
    dplyr::left_join(hospital_private_counts, by = "hospital_id") %>%
    dplyr::left_join(hospital_uninsured_counts, by = "hospital_id") %>%
    dplyr::left_join(hospital_public_counts, by = "hospital_id") %>%
    dplyr::mutate(
      insured_patients = dplyr::coalesce(insured_patients, 0L),
      uninsured_patients = dplyr::coalesce(uninsured_patients, 0L),
      public_patients = dplyr::coalesce(public_patients, 0L),
      total_patients = insured_patients + uninsured_patients + public_patients
    )

  charity_care <- hospital_volumes %>%
    dplyr::transmute(
      hospital_id,
      charity_patients = if (isTRUE(policy$emtala_enabled)) uninsured_patients else 0L,
      paying_uninsured = if (isTRUE(policy$emtala_enabled)) 0L else uninsured_patients
    )

  insurer_index_lookup <- setNames(seq_len(n_insurers), insurers$insurer_id)

  private_financials <- private_visits %>%
    dplyr::mutate(
      rate = rate_matrix[cbind(as.character(hospital_id), as.character(insurer_id))],
      base_cost = policy_cost_vector(age_group)
    ) %>%
    dplyr::mutate(
      revenue = base_cost * rate * patients,
      cost = base_cost * patients
    ) %>%
    dplyr::group_by(hospital_id) %>%
    dplyr::summarise(
      private_revenue = sum(revenue),
      private_cost = sum(cost),
      .groups = "drop"
    )

  dsh_pct <- policy$dsh_reimbursement_pct
  if (is.null(dsh_pct) || is.na(dsh_pct)) {
    dsh_pct <- GAME_CONSTANTS$POLICY_DEFAULT_DSH_REIMBURSEMENT
  }

  # Without EMTALA: uninsured get no care, zero cost/revenue
  # With EMTALA: charity care reimbursed at DSH rate
  uninsured_reimb <- if (isTRUE(policy$emtala_enabled)) dsh_pct else 0
  uninsured_financials <- uninsured_visits %>%
    dplyr::mutate(
      base_cost = policy_cost_vector(age_group),
      reimbursement = uninsured_reimb,
      revenue = base_cost * reimbursement * patients,
      cost = if (isTRUE(policy$emtala_enabled)) base_cost * patients else 0
    ) %>%
    dplyr::group_by(hospital_id) %>%
    dplyr::summarise(
      uninsured_revenue = sum(revenue),
      uninsured_cost = sum(cost),
      .groups = "drop"
    )

  medicare_effective_pct <- medicare_pct + dsh_pct * max(0, 1 - medicare_pct)

  medicare_financials <- medicare_visits %>%
    dplyr::mutate(
      base_cost = policy_cost_vector(age_group),
      revenue = base_cost * medicare_effective_pct * patients,
      cost = base_cost * patients
    ) %>%
    dplyr::group_by(hospital_id) %>%
    dplyr::summarise(
      public_revenue = sum(revenue),
      public_cost = sum(cost),
      .groups = "drop"
    )

  hospital_financials <- hospitals %>%
    dplyr::select(hospital_id) %>%
    dplyr::left_join(hospital_network_stats, by = "hospital_id") %>%
    dplyr::left_join(private_financials, by = "hospital_id") %>%
    dplyr::left_join(uninsured_financials, by = "hospital_id") %>%
    dplyr::left_join(medicare_financials, by = "hospital_id") %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~dplyr::coalesce(., 0))) %>%
    dplyr::mutate(
      revenue = private_revenue + uninsured_revenue + public_revenue,
      costs = private_cost + uninsured_cost + public_cost + network_contract_cost,
      profit = revenue - costs
    )

  enrollment_detail <- consumer_outcomes %>%
    dplyr::filter(!is.na(insurer_id)) %>%
    dplyr::group_by(insurer_id, age_group) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  premium_revenue <- enrollment_detail %>%
    dplyr::mutate(
      premium = dplyr::case_when(
        age_group == "young" ~ insurers$premium_young[match(insurer_id, insurers$insurer_id)],
        TRUE ~ 0
      ),
      revenue = premium * count
    ) %>%
    dplyr::group_by(insurer_id) %>%
    dplyr::summarise(premium_revenue = sum(revenue), .groups = "drop")

  claims_paid <- private_visits %>%
    dplyr::mutate(
      rate = rate_matrix[cbind(as.character(hospital_id), as.character(insurer_id))],
      base_cost = policy_cost_vector(age_group),
      claim = base_cost * rate * patients
    ) %>%
    dplyr::group_by(insurer_id) %>%
    dplyr::summarise(claims_paid = sum(claim), .groups = "drop")

  # Get enrollment counts for per-enrollee admin costs
  enrollment_counts <- consumer_outcomes %>%
    dplyr::filter(!is.na(insurer_id)) %>%
    dplyr::group_by(insurer_id) %>%
    dplyr::summarise(total_enrollees = dplyr::n(), .groups = "drop")

  insurer_financials <- insurers %>%
    dplyr::select(insurer_id) %>%
    dplyr::left_join(insurer_network_stats, by = "insurer_id") %>%
    dplyr::left_join(premium_revenue, by = "insurer_id") %>%
    dplyr::left_join(claims_paid, by = "insurer_id") %>%
    dplyr::left_join(enrollment_counts, by = "insurer_id") %>%
    dplyr::mutate(
      premium_revenue = dplyr::coalesce(premium_revenue, 0),
      claims_paid = dplyr::coalesce(claims_paid, 0),
      network_contract_cost = dplyr::coalesce(network_contract_cost, 0),
      network_contracts = dplyr::coalesce(network_contracts, 0L),
      total_enrollees = dplyr::coalesce(total_enrollees, 0L),
      per_enrollee_admin_cost = total_enrollees * GAME_CONSTANTS$INSURER_ADMIN_COST_PER_ENROLLEE,
      administrative_costs = network_contract_cost + per_enrollee_admin_cost,
      profit = premium_revenue - claims_paid - administrative_costs,
      mlr = dplyr::if_else(premium_revenue > 0, claims_paid / premium_revenue, NA_real_)
    )

  list(
    enrollments = enrollments,
    hospital_volumes = hospital_volumes,
    charity_care = charity_care,
    hospital_financials = hospital_financials,
    insurer_financials = insurer_financials,
    consumer_outcomes = consumer_outcomes,
    private_visits = private_visits,
    uninsured_visits = uninsured_visits,
    medicare_visits = medicare_visits
  )
}

# Main entry point called by the app
calculate_round_results <- function(game_id, round) {
  decisions <- get_round_decisions(game_id, round)
  settings <- get_game_settings(game_id)

  if (is.null(settings)) {
    stop("Game settings not found; create a game before calculating results.")
  }

  # Default to Excel counts if instructor configured larger numbers
  if (settings$n_hospitals > GAME_CONSTANTS$MAX_HOSPITALS) {
    warning(sprintf(
      "Reducing hospitals to %d to match Excel template.", GAME_CONSTANTS$MAX_HOSPITALS
    ))
    settings$n_hospitals <- GAME_CONSTANTS$MAX_HOSPITALS
  }
  if (settings$n_insurers > GAME_CONSTANTS$MAX_INSURERS) {
    warning(sprintf(
      "Reducing insurers to %d to match Excel template.", GAME_CONSTANTS$MAX_INSURERS
    ))
    settings$n_insurers <- GAME_CONSTANTS$MAX_INSURERS
  }

  decision_inputs <- prepare_decision_inputs(decisions, settings)
  policy <- build_policy_parameters(settings)
  simulate_excel_round(decision_inputs, settings, game_id, round, policy)
}
