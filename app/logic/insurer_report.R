# Helper functions to build insurer-level reports that detail enrollment,
# sick members, hospital destinations, and payments made to providers.

empty_insurer_report <- function() {
  tibble::tibble(
    round = integer(),
    insurer_id = integer(),
    insurer_name = character(),
    total_enrollees = integer(),
    total_sick = integer(),
    premium = numeric(),
    hospital_id = integer(),
    hospital_name = character(),
    sick_patients = integer(),
    total_payment = numeric()
  )
}

determine_completed_rounds <- function(game_id, settings, rounds = NULL) {
  if (!is.null(rounds)) {
    rounds_vec <- unique(as.integer(rounds))
    rounds_vec <- rounds_vec[!is.na(rounds_vec)]
    return(sort(rounds_vec))
  }

  con <- get_db_connection()
  on.exit(close_db_connection(con), add = TRUE)

  rounds_df <- dbGetQuery(
    con,
    "SELECT DISTINCT round FROM round_results WHERE game_id = ? ORDER BY round",
    params = list(game_id)
  )

  rounds_vec <- sort(unique(as.integer(rounds_df$round)))

  if (length(rounds_vec) == 0 && !is.null(settings)) {
    current_round <- suppressWarnings(as.integer(settings$current_round))
    if (!is.na(current_round) && current_round > 1) {
      rounds_vec <- seq_len(current_round - 1L)
    } else if (!is.na(current_round) && current_round >= 1) {
      rounds_vec <- current_round
    }
  }

  rounds_vec
}

lookup_rates <- function(hospital_ids, insurer_ids, rate_matrix) {
  rates <- numeric(length(hospital_ids))
  valid <- !is.na(hospital_ids) & !is.na(insurer_ids)
  if (any(valid)) {
    row_keys <- as.character(hospital_ids[valid])
    col_keys <- as.character(insurer_ids[valid])
    rates[valid] <- rate_matrix[cbind(row_keys, col_keys)]
    rates[is.na(rates)] <- 0
  }
  rates
}

sanitize_filename_token <- function(x) {
  token <- gsub("[^A-Za-z0-9]+", "_", tolower(x))
  token <- gsub("_+", "_", token)
  token <- gsub("^_+|_+$", "", token)
  if (!nzchar(token)) {
    token <- "insurer"
  }
  token
}

build_insurer_report <- function(game_id = 1,
                                 rounds = NULL,
                                 settings = NULL,
                                 insurer_id = NULL) {
  empty <- empty_insurer_report()

  if (is.null(settings)) {
    settings <- get_game_settings(game_id)
  }

  if (is.null(settings)) {
    return(empty)
  }

  rounds_vec <- determine_completed_rounds(game_id, settings, rounds)
  if (length(rounds_vec) == 0) {
    return(empty)
  }

  insurers <- get_insurers(game_id)
  if (is.null(insurers) || nrow(insurers) == 0) {
    return(empty)
  }

  hospitals <- get_hospitals(game_id)
  if (is.null(hospitals)) {
    hospitals <- tibble::tibble(hospital_id = integer(), hospital_name = character())
  }

  if (!is.null(insurer_id)) {
    insurers <- insurers[insurers$insurer_id == insurer_id, , drop = FALSE]
    if (nrow(insurers) == 0) {
      return(empty)
    }
  }

  settings$n_hospitals <- suppressWarnings(as.integer(settings$n_hospitals))
  settings$n_insurers <- suppressWarnings(as.integer(settings$n_insurers))

  policy <- build_policy_parameters(settings)

  report_list <- lapply(rounds_vec, function(rnd) {
    decisions <- get_round_decisions(game_id, rnd)
    has_data <- (!is.null(decisions$hospital_rates) && nrow(decisions$hospital_rates) > 0) ||
      (!is.null(decisions$insurer_premiums) && nrow(decisions$insurer_premiums) > 0)
    if (!has_data) {
      return(tibble::tibble())
    }

    decision_inputs <- tryCatch(
      prepare_decision_inputs(decisions, settings),
      error = function(e) NULL
    )

    if (is.null(decision_inputs)) {
      return(tibble::tibble())
    }

    simulation <- simulate_excel_round(decision_inputs, settings, game_id, rnd, policy)
    consumer_outcomes <- simulation$consumer_outcomes

    if (is.null(consumer_outcomes) || nrow(consumer_outcomes) == 0) {
      return(tibble::tibble())
    }

    consumer_outcomes <- consumer_outcomes %>%
      dplyr::transmute(
        consumer_id,
        age_group,
        insurer_id = as.integer(insurer_id),
        sick = sick,
        hospital_id = as.integer(hospital_id)
      )

    if (!is.null(insurer_id)) {
      consumer_outcomes <- consumer_outcomes[consumer_outcomes$insurer_id == insurer_id, , drop = FALSE]
      if (nrow(consumer_outcomes) == 0) {
        return(tibble::tibble())
      }
    }

    insurer_enrollment <- consumer_outcomes %>%
      dplyr::filter(!is.na(insurer_id)) %>%
      dplyr::count(insurer_id, name = "total_enrollees")

    sick_rows <- consumer_outcomes %>%
      dplyr::filter(!is.na(insurer_id), sick)

    sick_totals <- sick_rows %>%
      dplyr::count(insurer_id, name = "total_sick")

    premium_info <- decision_inputs$insurers %>%
      dplyr::transmute(
        insurer_id = as.integer(insurer_id),
        premium = as.numeric(premium_young)
      )

    if (!is.null(insurer_id)) {
      premium_info <- premium_info[premium_info$insurer_id == insurer_id, , drop = FALSE]
    }

    summary_rows <- insurers %>%
      dplyr::transmute(insurer_id = as.integer(insurer_id)) %>%
      dplyr::left_join(insurer_enrollment, by = "insurer_id") %>%
      dplyr::left_join(sick_totals, by = "insurer_id") %>%
      dplyr::left_join(premium_info, by = "insurer_id") %>%
      dplyr::mutate(
        total_enrollees = dplyr::coalesce(total_enrollees, 0L),
        total_sick = dplyr::coalesce(total_sick, 0L),
        premium = dplyr::coalesce(premium, NA_real_)
      )

    if (nrow(summary_rows) == 0) {
      return(tibble::tibble())
    }

    if (nrow(sick_rows) == 0) {
      return(summary_rows %>%
        dplyr::transmute(
          round = rnd,
          insurer_id,
          total_enrollees,
          total_sick,
          premium,
          hospital_id = NA_integer_,
          sick_patients = 0L,
          total_payment = 0
        ))
    }

    rate_matrix <- decision_inputs$rate_matrix
    rates <- lookup_rates(sick_rows$hospital_id, sick_rows$insurer_id, rate_matrix)
    base_costs <- ifelse(sick_rows$age_group == "young",
                         policy$base_costs$young,
                         policy$base_costs$old)
    payments <- base_costs * rates

    per_hospital <- sick_rows %>%
      dplyr::mutate(
        payment = payments,
        hospital_id = as.integer(hospital_id)
      ) %>%
      dplyr::group_by(insurer_id, hospital_id) %>%
      dplyr::summarise(
        sick_patients = dplyr::n(),
        total_payment = sum(payment, na.rm = TRUE),
        .groups = "drop"
      )

    per_hospital %>%
      dplyr::right_join(summary_rows, by = "insurer_id") %>%
      dplyr::mutate(
        sick_patients = dplyr::coalesce(sick_patients, 0L),
        total_payment = dplyr::coalesce(total_payment, 0),
        round = rnd
      )
  })

  report_df <- dplyr::bind_rows(report_list)
  if (nrow(report_df) == 0) {
    return(empty)
  }

  insurer_names <- insurers %>%
    dplyr::transmute(insurer_id = as.integer(insurer_id), insurer_name)

  hospital_names <- hospitals %>%
    dplyr::transmute(hospital_id = as.integer(hospital_id), hospital_name)

  report_df %>%
    dplyr::left_join(insurer_names, by = "insurer_id") %>%
    dplyr::left_join(hospital_names, by = "hospital_id") %>%
    dplyr::select(
      round,
      insurer_id,
      insurer_name,
      total_enrollees,
      total_sick,
      premium,
      hospital_id,
      hospital_name,
      sick_patients,
      total_payment
    ) %>%
    dplyr::arrange(round, insurer_id, dplyr::desc(sick_patients), hospital_id)
}
