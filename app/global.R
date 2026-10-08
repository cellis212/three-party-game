# Global configuration and shared functions for Three-Party System Game
# This file is loaded once when the app starts

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(RSQLite)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinyjs)

# Load legacy Excel model parameters so the simulation matches the original workbook
LEGACY_MODEL <- fromJSON("data/legacy_model.json")

# Collapse the legacy mid-age cohort into the young cohort so we only track two groups
if (!is.null(LEGACY_MODEL$consumers$mid)) {
  cohort_fields <- c("locations", "alpha", "beta", "gamma", "delta")
  for (field in cohort_fields) {
    mid_values <- LEGACY_MODEL$consumers$mid[[field]]
    if (!is.null(mid_values)) {
      LEGACY_MODEL$consumers$young[[field]] <- c(
        LEGACY_MODEL$consumers$young[[field]],
        mid_values
      )
    }
  }
  LEGACY_MODEL$consumers$mid <- NULL
}

# Set outside option (delta) for young consumers so demand responds to premium changes
LEGACY_MODEL$consumers$young$delta <- rep(150, length(LEGACY_MODEL$consumers$young$delta)) # config

# Reduce price sensitivity so consumers are less responsive to premium differences
LEGACY_MODEL$consumers$young$beta <- rep(0.5, length(LEGACY_MODEL$consumers$young$beta)) # config

legacy_old_cost <- LEGACY_MODEL$base_costs$old
if (is.null(legacy_old_cost) || is.na(legacy_old_cost)) {
  legacy_old_cost <- 1600
}
legacy_old_prob <- LEGACY_MODEL$health_probabilities$old
if (is.null(legacy_old_prob) || is.na(legacy_old_prob)) {
  legacy_old_prob <- 0.7
}

LEGACY_MODEL$base_costs <- list(young = 1000, old = legacy_old_cost)
LEGACY_MODEL$health_probabilities <- list(young = 0.3, old = legacy_old_prob)

if (is.null(LEGACY_MODEL$health_probabilities$young) ||
    is.null(LEGACY_MODEL$health_probabilities$old)) {
  LEGACY_MODEL$health_probabilities <- list(young = 0.3, old = 0.7)
}

# Game Constants (aligned with Excel workbook assumptions)
GAME_CONSTANTS <- list(
  MAX_HOSPITALS = max(10, length(LEGACY_MODEL$hospital_locations)),
  MAX_INSURERS = 10,
  DEFAULT_POPULATION = 1000,  # Large enough to smooth insurer profit variance across 6 teams
  DEFAULT_ROUNDS = 6,

  # Patient parameters derived from Excel
  BASE_COSTS = LEGACY_MODEL$base_costs,
  HEALTH_PROBABILITIES = LEGACY_MODEL$health_probabilities,

  # Default decision values — equilibrium with 2-3 contracts per entity at MC 120%
  DEFAULT_NEGOTIATED_RATE = 1.1,  # 10% markup; both sides profit with partial networks
  DEFAULT_PREMIUM_YOUNG = 400,    # Above break-even ($330 at rate 1.1); ~25% uninsured

  # Policy defaults
  POLICY_DEFAULT_EMTALA = FALSE,
  POLICY_DEFAULT_MEDICARE_REIMBURSEMENT = 1.2,  # Start at 120%, can be lowered to demonstrate cost-shifting
  POLICY_DEFAULT_DSH_REIMBURSEMENT = 0.50,  # DSH payments for uninsured patients (% of cost). Auto-set to 50% when EMTALA enabled # config

  # EMTALA behavioral tuning
  EMTALA_CHARITY_HAZARD_BONUS = 25,  # EMTALA makes being uninsured more attractive (free ER care)

  # Network contracting costs: first contract free, each additional costs step
  # 1 deal: $0, 2 deals: $2000, 3 deals: $4000, etc.
  HOSPITAL_NETWORK_CONTRACT_STEP = 2000,
  INSURER_NETWORK_CONTRACT_STEP = 2000,
  
  # Insurer administrative costs
  INSURER_ADMIN_COST_PER_ENROLLEE = 0 # Per-enrollee administrative cost
)

# Team Colors (consistent across all visualizations)
TEAM_COLORS <- list(
  hospitals = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#6A994E",
                "#BC4B51", "#8CB369", "#5B8E7D", "#F4A259", "#BC9CB0"),
  insurers = c("#06A77D", "#D62246", "#F77F00", "#4A5899", "#8D5B4C",
               "#7209B7", "#3A86FF", "#FB5607", "#FFBE0B", "#8338EC")
)

# TRUE when the app runs in the browser through shinylive (webR), as on the
# GitHub Pages site. There is no server then: each browser tab runs its own
# copy of R with its own in-memory database.
IS_WEBR <- identical(R.version$os, "emscripten")

# Styles for the pop-out negotiation board (server page and browser pop-out)
NEGOTIATION_POPUP_CSS <- "
  body {
    background: #1a1a2e;
    margin: 0;
    padding: 20px 30px;
    min-height: 100vh;
  }
  .neg-popup-title {
    text-align: center;
    color: #ecf0f1;
    font-size: 36px;
    font-weight: 700;
    margin-bottom: 5px;
  }
  .neg-popup-round {
    text-align: center;
    color: #95a5a6;
    font-size: 24px;
    margin-bottom: 20px;
  }
  .neg-counter {
    color: #ecf0f1;
  }
  .neg-table {
    max-width: 1100px;
    margin: 0 auto;
  }
  .neg-table th, .neg-table td {
    border-color: #34495e;
  }
  .neg-header {
    background: #2c3e50;
    font-size: 22px;
    word-break: break-word;
    overflow-wrap: anywhere;
  }
  .neg-row-label {
    background: #2c3e50;
    font-size: 22px;
    word-break: break-word;
    overflow-wrap: anywhere;
  }
  .neg-cell {
    font-size: 56px;
    height: 80px;
  }
  .neg-pending {
    background-color: #2c3e50;
    color: #4a5568;
  }
"

# plotly attaches plotly.js and crosstalk only when the first chart renders.
# In the browser build those scripts can arrive after the chart tries to draw,
# so the page loads them up front instead.
plotly_page_dependencies <- function() {
  plotly_build(plot_ly(x = numeric(0), y = numeric(0),
                       type = "scatter", mode = "markers"))$dependencies
}

# Resolve a writable database path (shinyapps.io uses a read-only app dir)
get_database_path <- function() {
  shiny_port <- Sys.getenv("SHINY_PORT", unset = "")
  if (nzchar(shiny_port)) {
    file.path(tempdir(), "game_state.db")
  } else {
    "data/game_state.db"
  }
}

remove_legacy_password_column <- function(con, table_name, schema_definition, columns_keep) {
  info <- dbGetQuery(con, paste0("PRAGMA table_info(", table_name, ")"))
  if (!("team_password" %in% info$name)) {
    return(invisible(FALSE))
  }

  dropped <- tryCatch({
    dbExecute(con, sprintf("ALTER TABLE %s DROP COLUMN team_password", table_name))
    TRUE
  }, error = function(e) {
    FALSE
  })

  if (dropped) {
    return(invisible(TRUE))
  }

  temp_table <- paste0(table_name, "_tmp_no_password")
  column_list <- paste(columns_keep, collapse = ", ")

  dbExecute(con, "BEGIN TRANSACTION")
  success <- FALSE
  tryCatch({
    dbExecute(con, sprintf("CREATE TABLE %s %s", temp_table, schema_definition))
    dbExecute(con, sprintf(
      "INSERT INTO %s (%s) SELECT %s FROM %s",
      temp_table, column_list, column_list, table_name
    ))
    dbExecute(con, sprintf("DROP TABLE %s", table_name))
    dbExecute(con, sprintf("ALTER TABLE %s RENAME TO %s", temp_table, table_name))
    dbExecute(con, "COMMIT")
    success <- TRUE
  }, error = function(e) {
    dbExecute(con, "ROLLBACK")
    warning(sprintf("Failed to migrate table %s to remove team_password: %s", table_name, e$message))
  })

  invisible(success)
}

ensure_column_exists <- function(con, table_name, column_name, column_definition) {
  info <- dbGetQuery(con, paste0("PRAGMA table_info(", table_name, ")"))
  if (column_name %in% info$name) {
    return(invisible(FALSE))
  }

  dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s", table_name, column_definition))
  invisible(TRUE)
}

# Initialize database connection
init_database <- function(db_path = get_database_path()) {
  con <- dbConnect(SQLite(), db_path)

  # Create tables if they don't exist

  # Game settings table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS game_settings (
      game_id INTEGER PRIMARY KEY,
      n_hospitals INTEGER,
      n_insurers INTEGER,
      n_rounds INTEGER,
      population INTEGER,
      current_round INTEGER DEFAULT 1,
      game_status TEXT DEFAULT 'setup',
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  ensure_column_exists(con, "game_settings", "emtala_enabled", "emtala_enabled INTEGER DEFAULT 0")
  ensure_column_exists(con, "game_settings", "medicare_reimbursement_pct", "medicare_reimbursement_pct REAL DEFAULT 1.2")
  ensure_column_exists(con, "game_settings", "cost_young", sprintf("cost_young REAL DEFAULT %s", GAME_CONSTANTS$BASE_COSTS$young))
  ensure_column_exists(con, "game_settings", "cost_old", sprintf("cost_old REAL DEFAULT %s", GAME_CONSTANTS$BASE_COSTS$old))
  ensure_column_exists(con, "game_settings", "prob_young", sprintf("prob_young REAL DEFAULT %s", GAME_CONSTANTS$HEALTH_PROBABILITIES$young))
  ensure_column_exists(con, "game_settings", "prob_old", sprintf("prob_old REAL DEFAULT %s", GAME_CONSTANTS$HEALTH_PROBABILITIES$old))
  ensure_column_exists(con, "game_settings", "random_seed", "random_seed INTEGER")
  ensure_column_exists(con, "game_settings", "dsh_reimbursement_pct", "dsh_reimbursement_pct REAL DEFAULT 0")

  # Hospitals table
  hospitals_schema <- "(
      hospital_id INTEGER PRIMARY KEY,
      game_id INTEGER,
      hospital_name TEXT,
      color TEXT,
      FOREIGN KEY (game_id) REFERENCES game_settings(game_id)
    )"
  dbExecute(con, paste0("CREATE TABLE IF NOT EXISTS hospitals ", hospitals_schema))
  remove_legacy_password_column(con, "hospitals", hospitals_schema,
                                c("hospital_id", "game_id", "hospital_name", "color"))
  ensure_column_exists(con, "hospitals", "team_pin", "team_pin TEXT")

  # Insurers table
  insurers_schema <- "(
      insurer_id INTEGER PRIMARY KEY,
      game_id INTEGER,
      insurer_name TEXT,
      color TEXT,
      FOREIGN KEY (game_id) REFERENCES game_settings(game_id)
    )"
  dbExecute(con, paste0("CREATE TABLE IF NOT EXISTS insurers ", insurers_schema))
  remove_legacy_password_column(con, "insurers", insurers_schema,
                                c("insurer_id", "game_id", "insurer_name", "color"))
  ensure_column_exists(con, "insurers", "team_pin", "team_pin TEXT")

  # Negotiated rates between hospitals and insurers (mirrors Excel negotiated rate grid)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS hospital_rates (
      rate_id INTEGER PRIMARY KEY AUTOINCREMENT,
      game_id INTEGER,
      round INTEGER,
      hospital_id INTEGER,
      insurer_id INTEGER,
      rate REAL,
      submitted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (hospital_id) REFERENCES hospitals(hospital_id),
      FOREIGN KEY (insurer_id) REFERENCES insurers(insurer_id)
    )
  ")

  # Premium decisions by insurers (single young cohort tracked)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS insurer_premiums (
      premium_id INTEGER PRIMARY KEY AUTOINCREMENT,
      game_id INTEGER,
      round INTEGER,
      insurer_id INTEGER,
      premium_young REAL,
      submitted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (insurer_id) REFERENCES insurers(insurer_id)
    )
  ")

  # Insurer network choices (accept/reject each hospital)
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS insurer_network (
      network_id INTEGER PRIMARY KEY AUTOINCREMENT,
      game_id INTEGER,
      round INTEGER,
      insurer_id INTEGER,
      hospital_id INTEGER,
      accepted INTEGER DEFAULT 1,
      submitted_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  # Per-game randomized consumer preferences
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS game_consumers (
      game_id INTEGER,
      consumer_id INTEGER,
      age_group TEXT,
      location REAL,
      alpha REAL,
      beta REAL,
      gamma REAL,
      delta REAL,
      PRIMARY KEY (game_id, consumer_id),
      FOREIGN KEY (game_id) REFERENCES game_settings(game_id)
    )
  ")

  legacy_tables <- intersect(dbListTables(con),
                             c("hospital_decisions", "insurer_decisions", "network_contracts"))
  for (tbl in legacy_tables) {
    dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", tbl))
  }

  # Round results table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS round_results (
      result_id INTEGER PRIMARY KEY AUTOINCREMENT,
      game_id INTEGER,
      round INTEGER,
      hospital_id INTEGER,
      insurer_id INTEGER,
      patients_insured INTEGER,
      patients_uninsured_treated INTEGER,
      patients_charity_care INTEGER,
      patients_total INTEGER,
      revenue REAL,
      costs REAL,
      profit REAL,
      claims_paid REAL,
      mlr REAL,
      calculated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  ensure_column_exists(con, "round_results", "patients_total", "patients_total INTEGER DEFAULT 0")
  ensure_column_exists(con, "round_results", "patients_public", "patients_public INTEGER DEFAULT 0")

  dbDisconnect(con)
  return(TRUE)
}

# Database helper functions
get_db_connection <- function() {
  dbConnect(SQLite(), get_database_path())
}

close_db_connection <- function(con) {
  dbDisconnect(con)
}

# Get current game settings
get_game_settings <- function(game_id = 1) {
  con <- get_db_connection()
  settings <- dbGetQuery(con, "SELECT * FROM game_settings WHERE game_id = ?", params = list(game_id))
  close_db_connection(con)

  if (nrow(settings) == 0) {
    return(NULL)
  }
  return(settings)
}

# Retrieve per-game consumer preferences (if any)
get_game_consumers <- function(game_id = 1) {
  con <- get_db_connection()
  consumers <- dbGetQuery(con,
    "SELECT consumer_id, age_group, location, alpha, beta, gamma, delta
     FROM game_consumers WHERE game_id = ? ORDER BY consumer_id",
    params = list(game_id)
  )
  close_db_connection(con)

  if (nrow(consumers) == 0) {
    return(NULL)
  }

  consumers
}

# Overwrite stored consumer preferences for a specific game
save_game_consumers <- function(game_id = 1, consumers_df) {
  if (is.null(consumers_df) || nrow(consumers_df) == 0) {
    warning("No consumer data supplied; skipping save_game_consumers().")
    return(invisible(FALSE))
  }

  consumers_df <- as.data.frame(consumers_df)
  required_cols <- c("consumer_id", "age_group", "location", "alpha", "beta", "gamma", "delta")
  missing_cols <- setdiff(required_cols, names(consumers_df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Consumer data missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  consumers_df$game_id <- as.integer(game_id)
  consumers_df$consumer_id <- as.integer(consumers_df$consumer_id)
  numeric_cols <- c("location", "alpha", "beta", "gamma", "delta")
  consumers_df[numeric_cols] <- lapply(consumers_df[numeric_cols], as.numeric)
  consumers_df <- consumers_df[, c("game_id", required_cols), drop = FALSE]

  con <- get_db_connection()
  on.exit(close_db_connection(con), add = TRUE)

  dbExecute(con, "DELETE FROM game_consumers WHERE game_id = ?", params = list(game_id))
  dbWriteTable(con, "game_consumers", consumers_df, append = TRUE, row.names = FALSE)

  invisible(TRUE)
}

# Get all hospitals for current game
get_hospitals <- function(game_id = 1) {
  con <- get_db_connection()
  hospitals <- dbGetQuery(con, "SELECT * FROM hospitals WHERE game_id = ?", params = list(game_id))
  close_db_connection(con)
  return(hospitals)
}

# Get all insurers for current game
get_insurers <- function(game_id = 1) {
  con <- get_db_connection()
  insurers <- dbGetQuery(con, "SELECT * FROM insurers WHERE game_id = ?", params = list(game_id))
  close_db_connection(con)
  return(insurers)
}

# Get insurer network choices for a specific round
get_insurer_network <- function(game_id = 1, round = 1) {
  con <- get_db_connection()
  network <- dbGetQuery(con,
    "SELECT * FROM insurer_network WHERE game_id = ? AND round = ?",
    params = list(game_id, round))
  close_db_connection(con)
  return(network)
}

# Get negotiation status derived from hospital_rates (rate exists = deal reached)
get_negotiation_status <- function(game_id = 1, round = 1) {
  con <- get_db_connection()
  status <- dbGetQuery(con,
    "SELECT hospital_id, insurer_id, 1 AS negotiated FROM hospital_rates WHERE game_id = ? AND round = ?",
    params = list(game_id, round))
  close_db_connection(con)
  return(status)
}

# Save or delete a single hospital-insurer rate (used by auto-save)
save_single_rate <- function(game_id = 1, round, hospital_id, insurer_id, rate_value) {
  con <- get_db_connection()
  on.exit(close_db_connection(con), add = TRUE)

  dbExecute(con,
    "DELETE FROM hospital_rates WHERE game_id = ? AND round = ? AND hospital_id = ? AND insurer_id = ?",
    params = list(game_id, round, hospital_id, insurer_id))

  if (!is.null(rate_value) && !is.na(rate_value)) {
    dbExecute(con,
      "INSERT INTO hospital_rates (game_id, round, hospital_id, insurer_id, rate) VALUES (?, ?, ?, ?, ?)",
      params = list(game_id, round, hospital_id, insurer_id, rate_value))
  }

  # Auto-populate insurer_network: rate present = accepted
  dbExecute(con,
    "DELETE FROM insurer_network WHERE game_id = ? AND round = ? AND insurer_id = ? AND hospital_id = ?",
    params = list(game_id, round, insurer_id, hospital_id))
  accepted <- if (!is.null(rate_value) && !is.na(rate_value)) 1L else 0L
  dbExecute(con,
    "INSERT INTO insurer_network (game_id, round, insurer_id, hospital_id, accepted) VALUES (?, ?, ?, ?, ?)",
    params = list(game_id, round, insurer_id, hospital_id, accepted))
}

# Build negotiation grid HTML for display (shared between display_ui and pop-out)
render_negotiation_grid_html <- function(game_id = 1, round = 1) {
  hospitals <- get_hospitals(game_id)
  insurers <- get_insurers(game_id)
  if (nrow(hospitals) == 0 || nrow(insurers) == 0) {
    return(div("Waiting for teams...", class = "negotiation-waiting"))
  }

  status <- get_negotiation_status(game_id, round)

  header_cells <- c(
    list(tags$th("", style = "width:200px;")),
    lapply(seq_len(nrow(insurers)), function(j) {
      iname <- insurers$insurer_name[j]
      if (is.null(iname) || is.na(iname) || !nzchar(iname)) iname <- paste("Insurer", j)
      icolor <- insurers$color[j]
      tags$th(iname, class = "neg-header",
              style = sprintf("text-align:center; color:%s;", icolor))
    })
  )

  body_rows <- lapply(seq_len(nrow(hospitals)), function(i) {
    hid <- hospitals$hospital_id[i]
    hname <- hospitals$hospital_name[i]
    if (is.null(hname) || is.na(hname) || !nzchar(hname)) hname <- paste("Hospital", hid)
    hcolor <- hospitals$color[i]

    cells <- c(
      list(tags$td(tags$strong(hname), class = "neg-row-label",
                   style = sprintf("color:%s;", hcolor))),
      lapply(seq_len(nrow(insurers)), function(j) {
        iid <- insurers$insurer_id[j]
        is_negotiated <- any(status$hospital_id == hid & status$insurer_id == iid & status$negotiated == 1)

        cell_class <- if (is_negotiated) "neg-cell neg-done" else "neg-cell neg-pending"
        cell_content <- if (is_negotiated) "\u2713" else ""

        tags$td(cell_content, class = cell_class)
      })
    )
    do.call(tags$tr, cells)
  })

  total_pairs <- nrow(hospitals) * nrow(insurers)
  done_pairs <- if (nrow(status) > 0) sum(status$negotiated == 1) else 0

  tagList(
    div(
      style = "text-align:center; margin-bottom:15px;",
      tags$span(
        sprintf("Deals Reached: %d / %d", done_pairs, total_pairs),
        class = "neg-counter"
      )
    ),
    div(style = "overflow-x:auto;",
      tags$table(
        class = "neg-table",
        tags$thead(do.call(tags$tr, header_cells)),
        do.call(tags$tbody, body_rows)
      )
    )
  )
}

# Get decisions for a specific round
get_round_decisions <- function(game_id = 1, round = 1) {
  con <- get_db_connection()

  hospital_rates <- dbGetQuery(con,
    "SELECT * FROM hospital_rates WHERE game_id = ? AND round = ?",
    params = list(game_id, round))

  insurer_premiums <- dbGetQuery(con,
    "SELECT * FROM insurer_premiums WHERE game_id = ? AND round = ?",
    params = list(game_id, round))

  insurer_network <- dbGetQuery(con,
    "SELECT * FROM insurer_network WHERE game_id = ? AND round = ?",
    params = list(game_id, round))

  close_db_connection(con)

  return(list(
    hospital_rates = hospital_rates,
    insurer_premiums = insurer_premiums,
    insurer_network = insurer_network
  ))
}

# Get results for a specific round
get_round_results <- function(game_id = 1, round = 1) {
  con <- get_db_connection()
  results <- dbGetQuery(con,
    "SELECT * FROM round_results WHERE game_id = ? AND round = ?",
    params = list(game_id, round))
  close_db_connection(con)
  return(results)
}

# Utility functions
format_currency <- function(x) {
  paste0("$", formatC(x, format = "f", digits = 0, big.mark = ","))
}

format_percentage <- function(x) {
  paste0(round(x * 100, 1), "%")
}

# Generate team names if not provided
generate_team_names <- function(n, type = "Hospital") {
  if (type == "Hospital") {
    base_names <- c("General Hospital", "Medical Center", "Community Hospital",
                    "Regional Medical", "Memorial Hospital", "Health System",
                    "University Hospital", "City Hospital", "County Medical",
                    "Veterans Hospital")
  } else {
    base_names <- c("BlueCross", "HealthPlus", "MediCare Choice", "WellCare",
                    "National Health", "Premier Insurance", "United Health",
                    "Community Care", "Family Health", "SafeGuard Insurance")
  }
  return(base_names[1:n])
}

# Initialize database on app start
init_database()
