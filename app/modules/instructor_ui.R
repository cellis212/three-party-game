# Instructor Control Module

instructorUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Include animation libraries
    singleton(tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/pixi.js/7.3.2/pixi.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/gsap/3.12.5/gsap.min.js"),
      tags$script(src = "js/market-animation.js?v=20260324")
    )),
    fluidRow(
      box(
        title = "Game Setup", width = 6, status = "primary", solidHeader = TRUE,

        h4("Initialize New Game"),

        sliderInput(ns("n_hospitals"), "Number of Hospitals:",
                    min = 2, max = 10, value = 6, step = 1),

        sliderInput(ns("n_insurers"), "Number of Insurers:",
                    min = 2, max = 10, value = 6, step = 1),

        sliderInput(ns("n_rounds"), "Number of Rounds:",
                    min = 1, max = 10, value = 5, step = 1),

        textInput(ns("random_seed"), "Random Seed (optional):",
                  value = "", placeholder = "Leave blank for random seed"),

        actionButton(ns("create_game"), "Create New Game",
                    class = "btn-success btn-lg", icon = icon("play")),

        hr(),

        actionButton(ns("reset_game"), "Reset Current Game",
                    class = "btn-danger", icon = icon("redo"))
      ),

      box(
        title = "Round Control", width = 6, status = "info", solidHeader = TRUE,

        uiOutput(ns("round_status")),

        hr(),

        actionButton(ns("advance_round"), "Advance to Next Round",
                    class = "btn-primary btn-lg", icon = icon("forward")),

        br(), br(),

        actionButton(ns("calculate_results"), "Calculate Round Results",
                    class = "btn-warning", icon = icon("calculator")),

        br(), br(),

        checkboxInput(ns("show_animation"), "Show animation", value = TRUE)

      )
    ),

    fluidRow(
      box(
        title = "Policy Controls", width = 12, status = "info", solidHeader = TRUE,
        checkboxInput(
          ns("policy_emtala"),
          "EMTALA protections for uninsured patients",
          value = GAME_CONSTANTS$POLICY_DEFAULT_EMTALA
        ),
        helpText("When enabled, uninsured patients can seek care at any hospital without paying, reducing their incentive to remain uninsured."),
        sliderInput(
          ns("policy_dsh_reimbursement"),
          "DSH reimbursement for uninsured patients (% of cost):",
          min = 0,
          max = 100,
          step = 5,
          value = GAME_CONSTANTS$POLICY_DEFAULT_DSH_REIMBURSEMENT * 100
        ),
        helpText("Disproportionate Share Hospital payments partially reimburse hospitals for charity care. Only applies when EMTALA is enabled."),
        sliderInput(
          ns("policy_medicare_reimbursement"),
          "Medicare reimbursement (% of provider cost):",
          min = 0,
          max = 200,
          step = 1,
          value = GAME_CONSTANTS$POLICY_DEFAULT_MEDICARE_REIMBURSEMENT * 100
        ),
        numericInput(
          ns("policy_market_size"),
          "Market size (total population):",
          value = GAME_CONSTANTS$DEFAULT_POPULATION,
          min = 1,
          max = NA,
          step = 100
        ),
        fluidRow(
          column(6,
            numericInput(
              ns("policy_cost_young"),
              "Base treatment cost – Young ($):",
              value = GAME_CONSTANTS$BASE_COSTS$young,
              min = 0,
              step = 50
            )
          ),
          column(6,
            numericInput(
              ns("policy_cost_old"),
              "Base treatment cost – Older ($):",
              value = GAME_CONSTANTS$BASE_COSTS$old,
              min = 0,
              step = 50
            )
          )
        ),
        hr(),
        fluidRow(
          column(6,
            sliderInput(
              ns("policy_prob_young"),
              "Chance of illness – Young (%)",
              min = 0,
              max = 100,
              step = 1,
              value = GAME_CONSTANTS$HEALTH_PROBABILITIES$young * 100
            )
          ),
          column(6,
            sliderInput(
              ns("policy_prob_old"),
              "Chance of illness – Older (%)",
              min = 0,
              max = 100,
              step = 1,
              value = GAME_CONSTANTS$HEALTH_PROBABILITIES$old * 100
            )
          )
        )
      )
    ),

    fluidRow(
      box(
        title = "Enter Decisions", width = 12, status = "danger", solidHeader = TRUE,

        div(style = "margin-bottom: 12px;",
          actionButton(ns("popup_negotiation"), "Pop Out Negotiation Board",
                       icon = icon("external-link-alt"), class = "btn-info btn-sm"),
          helpText("Enter rates for each hospital-insurer pair. Entering a rate marks the deal as reached and puts the pair in-network. Leave empty for no deal.",
                   style = "margin-top: 8px;")
        ),

        # JavaScript for auto-saving rates on input change
        tags$script(HTML(sprintf("
          $(document).on('change', 'input[type=\"number\"][id*=\"_rate_\"]', function() {
            var id = $(this).attr('id');
            if (!id) return;
            var match = id.match(/h_(\\d+)_rate_(\\d+)/);
            if (match) {
              var rawVal = $(this).val();
              Shiny.setInputValue('%s', {
                hospital_id: parseInt(match[1]),
                insurer_id: parseInt(match[2]),
                value: rawVal === '' ? null : parseFloat(rawVal),
                ts: Date.now()
              }, {priority: 'event'});
              var cell = $(this).closest('td');
              cell.css('background-color', '#d4edda');
              setTimeout(function() { cell.css('background-color', ''); }, 600);
            }
          });
        ", ns("rate_autosave")))
        ),

        # JavaScript for pop-out window. On a server the pop-out loads its own
        # page (?view=negotiation). In the browser build (webR) a second window
        # cannot connect to R, so this session writes the board into it.
        tags$script(HTML(sprintf("
          var negotiationPopup = null;
          Shiny.addCustomMessageHandler('%s', function(msg) {
            if (msg.url) {
              window.open(msg.url, 'negotiation_board', 'width=1200,height=800,menubar=no,toolbar=no');
              return;
            }
            negotiationPopup = window.open('', 'negotiation_board', 'width=1200,height=800,menubar=no,toolbar=no');
            if (!negotiationPopup) return;
            negotiationPopup.document.open();
            negotiationPopup.document.write(msg.shell);
            negotiationPopup.document.close();
          });
          Shiny.addCustomMessageHandler('%s', function(msg) {
            if (!negotiationPopup || negotiationPopup.closed) return;
            var target = negotiationPopup.document.getElementById('neg-popup-body');
            if (target) target.innerHTML = msg.html;
          });
        ", ns("open_popup"), ns("update_popup")))),

        tabsetPanel(
          tabPanel("Hospital Rates",
            br(),
            uiOutput(ns("manual_hospital_inputs")),
            br(),
            actionButton(ns("save_hospital_decisions"), "Save All Hospital Rates",
                        class = "btn-success btn-sm")
          ),

          tabPanel("Insurer Premiums",
            br(),
            uiOutput(ns("manual_insurer_inputs")),
            br(),
            actionButton(ns("save_insurer_decisions"), "Save Insurer Premiums",
                        class = "btn-success")
          )
        )
      )
    ),

    fluidRow(
      box(
        title = "Team Management", width = 12, status = "warning", solidHeader = TRUE,

        tabsetPanel(
          tabPanel("Hospitals",
            br(),
            uiOutput(ns("hospital_name_editor")),
            br(),
            actionButton(ns("save_hospital_names"), "Save Hospital Names",
                        class = "btn-success")
          ),

          tabPanel("Insurers",
            br(),
            uiOutput(ns("insurer_name_editor")),
            br(),
            actionButton(ns("save_insurer_names"), "Save Insurer Names",
                        class = "btn-success")
          )
        )
      )
    ),

    fluidRow(
      box(
        title = "Submission Status", width = 12, status = "success", solidHeader = TRUE,

        uiOutput(ns("submission_status"))
      )
    ),

    fluidRow(
      box(
        title = "Import / Export", width = 12, status = "primary", solidHeader = TRUE,

        h4("Reload Saved Game"),
        helpText("Select the CSV files that were previously downloaded using the Export buttons."),
        fileInput(ns("upload_results"), "Results CSV", accept = c(".csv"), multiple = FALSE),
        fileInput(ns("upload_decisions"), "Decisions CSV", accept = c(".csv"), multiple = FALSE),
        actionButton(ns("reload_saved_game"), "Load Saved Game",
                    class = "btn-warning", icon = icon("upload")),

        hr(),

        h4("Export Current Game"),
        downloadButton(ns("download_results"), "Download All Results (CSV)",
                      class = "btn-success"),

        downloadButton(ns("download_decisions"), "Download All Decisions (CSV)",
                      class = "btn-info"),

        downloadButton(ns("download_insurer_report"), "Download Insurer Report (CSV)",
                      class = "btn-secondary")
      )
    )
  )
}

instructorServer <- function(id, game_state) {
  moduleServer(id, function(input, output, session) {

    update_game_setting <- function(column, value) {
      con <- get_db_connection()
      dbExecute(con,
        sprintf("UPDATE game_settings SET %s = ? WHERE game_id = 1", column),
        params = list(value)
      )
      close_db_connection(con)
      game_state(get_game_settings(1))
    }

    coerce_numeric_or_default <- function(value, default) {
      value_num <- as.numeric(value)
      if (length(value_num) == 0 || is.na(value_num)) {
        return(default)
      }
      value_num
    }

    # Keep policy inputs synchronized with stored settings
    observeEvent(game_state(), {
      settings <- game_state()
      if (is.null(settings)) {
        return()
      }

      emtala_value <- isTRUE(as.numeric(settings$emtala_enabled) == 1)
      medicare_pct_value <- coerce_numeric_or_default(
        settings$medicare_reimbursement_pct,
        GAME_CONSTANTS$POLICY_DEFAULT_MEDICARE_REIMBURSEMENT
      )

      updateSliderInput(
        session,
        "n_hospitals",
        value = max(2, min(GAME_CONSTANTS$MAX_HOSPITALS,
                           coerce_numeric_or_default(settings$n_hospitals, 6)))
      )
      updateSliderInput(
        session,
        "n_insurers",
        value = max(2, min(GAME_CONSTANTS$MAX_INSURERS,
                           coerce_numeric_or_default(settings$n_insurers, 6)))
      )

      dsh_pct_value <- coerce_numeric_or_default(
        settings$dsh_reimbursement_pct,
        GAME_CONSTANTS$POLICY_DEFAULT_DSH_REIMBURSEMENT
      )

      updateCheckboxInput(session, "policy_emtala", value = emtala_value)
      updateSliderInput(session, "policy_dsh_reimbursement",
                        value = round(dsh_pct_value * 100, 0))
      updateSliderInput(session, "policy_medicare_reimbursement",
                        value = round(medicare_pct_value * 100, 0))
      updateNumericInput(
        session,
        "policy_market_size",
        value = coerce_numeric_or_default(settings$population, GAME_CONSTANTS$DEFAULT_POPULATION)
      )
      seed_value <- settings$random_seed
      if (length(seed_value) == 0 || is.na(seed_value)) {
        seed_text <- ""
      } else {
        seed_text <- as.character(seed_value)
      }
      updateTextInput(session, "random_seed", value = seed_text)

      updateNumericInput(
        session,
        "policy_cost_young",
        value = coerce_numeric_or_default(settings$cost_young, GAME_CONSTANTS$BASE_COSTS$young)
      )
      updateNumericInput(
        session,
        "policy_cost_old",
        value = coerce_numeric_or_default(settings$cost_old, GAME_CONSTANTS$BASE_COSTS$old)
      )
      updateSliderInput(
        session,
        "policy_prob_young",
        value = round(coerce_numeric_or_default(settings$prob_young, GAME_CONSTANTS$HEALTH_PROBABILITIES$young) * 100, 0)
      )
      updateSliderInput(
        session,
        "policy_prob_old",
        value = round(coerce_numeric_or_default(settings$prob_old, GAME_CONSTANTS$HEALTH_PROBABILITIES$old) * 100, 0)
      )
    }, ignoreNULL = TRUE)

    observeEvent(input$policy_emtala, {
      settings <- game_state()
      if (is.null(settings)) {
        showNotification("Create a game before adjusting policies.", type = "error")
        return()
      }

      new_value <- ifelse(isTRUE(input$policy_emtala), 1L, 0L)
      current_value <- as.integer(settings$emtala_enabled)
      if (is.na(current_value)) current_value <- 0L

      if (identical(current_value, new_value)) {
        return()
      }

      update_game_setting("emtala_enabled", new_value)

      # Auto-link DSH to EMTALA: set to 50% when enabled, 0% when disabled
      if (new_value == 1L) {
        update_game_setting("dsh_reimbursement_pct", 0.50)
        updateSliderInput(session, "policy_dsh_reimbursement", value = 50)
        showNotification("EMTALA enabled. DSH auto-set to 50%.", type = "message")
      } else {
        update_game_setting("dsh_reimbursement_pct", 0)
        updateSliderInput(session, "policy_dsh_reimbursement", value = 0)
        showNotification("EMTALA disabled. DSH reset to 0%.", type = "message")
      }
    }, ignoreInit = TRUE)

    observeEvent(input$policy_dsh_reimbursement, {
      settings <- game_state()
      if (is.null(settings)) {
        showNotification("Create a game before adjusting policies.", type = "error")
        return()
      }

      new_pct <- as.numeric(input$policy_dsh_reimbursement) / 100
      if (is.na(new_pct) || new_pct < 0) {
        showNotification("Enter a valid DSH reimbursement percentage.", type = "error")
        return()
      }

      current_pct <- coerce_numeric_or_default(
        settings$dsh_reimbursement_pct,
        GAME_CONSTANTS$POLICY_DEFAULT_DSH_REIMBURSEMENT
      )

      if (abs(current_pct - new_pct) <= 1e-6) {
        return()
      }

      update_game_setting("dsh_reimbursement_pct", new_pct)
      showNotification("DSH reimbursement updated.", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$policy_medicare_reimbursement, {
      settings <- game_state()
      if (is.null(settings)) {
        showNotification("Create a game before adjusting policies.", type = "error")
        return()
      }

      new_pct <- as.numeric(input$policy_medicare_reimbursement) / 100
      if (is.na(new_pct) || new_pct < 0) {
        showNotification("Enter a valid Medicare reimbursement percentage.", type = "error")
        return()
      }

      current_pct <- coerce_numeric_or_default(
        settings$medicare_reimbursement_pct,
        GAME_CONSTANTS$POLICY_DEFAULT_MEDICARE_REIMBURSEMENT
      )

      if (abs(current_pct - new_pct) <= 1e-6) {
        return()
      }

      update_game_setting("medicare_reimbursement_pct", new_pct)
      showNotification("Medicare reimbursement updated.", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$policy_market_size, {
      settings <- game_state()
      if (is.null(settings)) {
        showNotification("Create a game before adjusting policies.", type = "error")
        return()
      }

      new_value <- as.numeric(input$policy_market_size)
      if (is.na(new_value) || new_value <= 0) {
        showNotification("Enter a positive population size.", type = "error")
        return()
      }

      new_value <- round(new_value)
      current_value <- coerce_numeric_or_default(settings$population, GAME_CONSTANTS$DEFAULT_POPULATION)

      if (abs(current_value - new_value) <= 1e-6) {
        return()
      }

      update_game_setting("population", new_value)
      showNotification("Market size updated.", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$policy_cost_young, {
      settings <- game_state()
      if (is.null(settings)) {
        showNotification("Create a game before adjusting policies.", type = "error")
        return()
      }

      new_value <- as.numeric(input$policy_cost_young)
      if (is.na(new_value) || new_value < 0) {
        showNotification("Enter a non-negative cost for young patients.", type = "error")
        return()
      }

      current_value <- coerce_numeric_or_default(settings$cost_young, GAME_CONSTANTS$BASE_COSTS$young)

      if (abs(current_value - new_value) <= 1e-6) {
        return()
      }

      update_game_setting("cost_young", new_value)
      showNotification("Young patient cost updated.", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$policy_cost_old, {
      settings <- game_state()
      if (is.null(settings)) {
        showNotification("Create a game before adjusting policies.", type = "error")
        return()
      }

      new_value <- as.numeric(input$policy_cost_old)
      if (is.na(new_value) || new_value < 0) {
        showNotification("Enter a non-negative cost for older patients.", type = "error")
        return()
      }

      current_value <- coerce_numeric_or_default(settings$cost_old, GAME_CONSTANTS$BASE_COSTS$old)

      if (abs(current_value - new_value) <= 1e-6) {
        return()
      }

      update_game_setting("cost_old", new_value)
      showNotification("Older patient cost updated.", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$policy_prob_young, {
      settings <- game_state()
      if (is.null(settings)) {
        showNotification("Create a game before adjusting policies.", type = "error")
        return()
      }

      new_value <- as.numeric(input$policy_prob_young) / 100
      if (is.na(new_value) || new_value < 0 || new_value > 1) {
        showNotification("Enter a probability between 0% and 100% for young patients.", type = "error")
        return()
      }

      current_value <- coerce_numeric_or_default(settings$prob_young, GAME_CONSTANTS$HEALTH_PROBABILITIES$young)

      if (abs(current_value - new_value) <= 1e-6) {
        return()
      }

      update_game_setting("prob_young", new_value)
      showNotification("Young illness probability updated.", type = "message")
    }, ignoreInit = TRUE)

    observeEvent(input$policy_prob_old, {
      settings <- game_state()
      if (is.null(settings)) {
        showNotification("Create a game before adjusting policies.", type = "error")
        return()
      }

      new_value <- as.numeric(input$policy_prob_old) / 100
      if (is.na(new_value) || new_value < 0 || new_value > 1) {
        showNotification("Enter a probability between 0% and 100% for older patients.", type = "error")
        return()
      }

      current_value <- coerce_numeric_or_default(settings$prob_old, GAME_CONSTANTS$HEALTH_PROBABILITIES$old)

      if (abs(current_value - new_value) <= 1e-6) {
        return()
      }

      update_game_setting("prob_old", new_value)
      showNotification("Older illness probability updated.", type = "message")
    }, ignoreInit = TRUE)

    # Create new game
    observeEvent(input$create_game, {
      seed_input <- suppressWarnings(as.integer(input$random_seed))
      if (length(seed_input) == 0 || is.na(seed_input) || seed_input < 0) {
        seed_input <- sample.int(1e9, 1)
      }
      consumer_profiles <- generate_random_consumers(seed_input)

      con <- get_db_connection()

      # Create game settings
      population_value <- coerce_numeric_or_default(input$policy_market_size, GAME_CONSTANTS$DEFAULT_POPULATION)
      population_value <- max(1, round(population_value))
      n_hosp <- min(input$n_hospitals, GAME_CONSTANTS$MAX_HOSPITALS)
      n_ins <- min(input$n_insurers, GAME_CONSTANTS$MAX_INSURERS)

      dbExecute(con,
        "INSERT INTO game_settings (game_id, n_hospitals, n_insurers, n_rounds, population, current_round, game_status,
                                    emtala_enabled, medicare_reimbursement_pct, dsh_reimbursement_pct, cost_young, cost_old,
                                    prob_young, prob_old, random_seed)
         VALUES (1, ?, ?, ?, ?, 1, 'active', ?, ?, ?, ?, ?, ?, ?, ?)
         ON CONFLICT(game_id) DO UPDATE SET
         n_hospitals = ?, n_insurers = ?, n_rounds = ?, population = ?, current_round = 1, game_status = 'active',
         emtala_enabled = ?, medicare_reimbursement_pct = ?, dsh_reimbursement_pct = ?, cost_young = ?, cost_old = ?,
         prob_young = ?, prob_old = ?, random_seed = ?",
        params = list(
          n_hosp, n_ins, input$n_rounds, population_value,
          as.integer(GAME_CONSTANTS$POLICY_DEFAULT_EMTALA),
          GAME_CONSTANTS$POLICY_DEFAULT_MEDICARE_REIMBURSEMENT,
          GAME_CONSTANTS$POLICY_DEFAULT_DSH_REIMBURSEMENT,
          GAME_CONSTANTS$BASE_COSTS$young,
          GAME_CONSTANTS$BASE_COSTS$old,
          GAME_CONSTANTS$HEALTH_PROBABILITIES$young,
          GAME_CONSTANTS$HEALTH_PROBABILITIES$old,
          seed_input,
          n_hosp, n_ins, input$n_rounds, population_value,
          as.integer(GAME_CONSTANTS$POLICY_DEFAULT_EMTALA),
          GAME_CONSTANTS$POLICY_DEFAULT_MEDICARE_REIMBURSEMENT,
          GAME_CONSTANTS$POLICY_DEFAULT_DSH_REIMBURSEMENT,
          GAME_CONSTANTS$BASE_COSTS$young,
          GAME_CONSTANTS$BASE_COSTS$old,
          GAME_CONSTANTS$HEALTH_PROBABILITIES$young,
          GAME_CONSTANTS$HEALTH_PROBABILITIES$old,
          seed_input
        )
      )

      # Clear existing game data for a clean start
      dbExecute(con, "DELETE FROM round_results WHERE game_id = 1")
      dbExecute(con, "DELETE FROM hospital_rates WHERE game_id = 1")
      dbExecute(con, "DELETE FROM insurer_premiums WHERE game_id = 1")
      # Clear existing teams
      dbExecute(con, "DELETE FROM hospitals WHERE game_id = 1")
      dbExecute(con, "DELETE FROM insurers WHERE game_id = 1")

      # Create hospitals with unique 4-digit PINs
      hospital_names <- generate_team_names(n_hosp, "Hospital")
      all_pins <- sample(1000:9999, n_hosp + n_ins)
      for (i in 1:n_hosp) {
        dbExecute(con,
          "INSERT INTO hospitals (hospital_id, game_id, hospital_name, color, team_pin)
           VALUES (?, 1, ?, ?, ?)",
          params = list(i, hospital_names[i], TEAM_COLORS$hospitals[i], as.character(all_pins[i]))
        )
      }

      # Create insurers with unique 4-digit PINs
      insurer_names <- generate_team_names(n_ins, "Insurer")
      for (i in 1:n_ins) {
        dbExecute(con,
          "INSERT INTO insurers (insurer_id, game_id, insurer_name, color, team_pin)
           VALUES (?, 1, ?, ?, ?)",
          params = list(i, insurer_names[i], TEAM_COLORS$insurers[i], as.character(all_pins[n_hosp + i]))
        )
      }

      close_db_connection(con)

      save_game_consumers(1, consumer_profiles)

      # Update game state
      game_state(get_game_settings(1))
      updateTextInput(session, "random_seed", value = as.character(seed_input))

      showNotification("New game created successfully!", type = "message")
    })

    # Advance round
    observeEvent(input$advance_round, {
      settings <- game_state()

      if (is.null(settings)) {
        showNotification("No active game. Create a game first.", type = "error")
        return()
      }

      if (settings$current_round >= settings$n_rounds) {
        showNotification("Game already at final round!", type = "warning")
        return()
      }

      con <- get_db_connection()
      new_round <- settings$current_round + 1

      dbExecute(con,
        "UPDATE game_settings SET current_round = ? WHERE game_id = 1",
        params = list(new_round)
      )

      close_db_connection(con)

      game_state(get_game_settings(1))

      showNotification(paste("Advanced to Round", new_round), type = "message")
    })

    # Calculate results
    observeEvent(input$calculate_results, {
      settings <- game_state()

      if (is.null(settings)) {
        showNotification("No active game!", type = "error")
        return()
      }

      withProgress(message = 'Calculating results...', value = 0, {
        tryCatch({
          results <- calculate_round_results(1, settings$current_round)
          incProgress(0.5)

          save_round_results(1, settings$current_round, results)
          incProgress(0.5)

          showNotification("Results calculated successfully!", type = "message")

          # Show animation modal if checkbox is checked
          if (isTRUE(input$show_animation)) {
            showModal(modalDialog(
              title = paste("Market Simulation - Round", settings$current_round),
              size = "l",
              easyClose = TRUE,
              footer = modalButton("Close"),
              div(
                id = session$ns("market_sim_root"),
                class = "market-sim-root",
                `data-ns-prefix` = session$ns(""),
                div(
                  class = "market-sim-stage",
                  div(
                    id = session$ns("market_sim_mount"),
                    class = "market-sim-canvas-host",
                    `data-role` = "canvas-host"
                  ),
                  div(
                    class = "market-sim-panel-labels",
                    div(class = "market-sim-panel-label market-sim-panel-label--insurance", span("Insurance Choice")),
                    div(class = "market-sim-panel-label market-sim-panel-label--care", span("Care Delivered")),
                    div(class = "market-sim-panel-label market-sim-panel-label--claims", span("Claims Paid")),
                    div(class = "market-sim-panel-label market-sim-panel-label--public", span("Public Payments"))
                  )
                ),
                div(
                  class = "market-sim-timeline",
                  div(class = "market-sim-timeline-track"),
                  div(class = "market-sim-timeline-progress"),
                  div(class = "market-sim-timeline-marker")
                ),
                div(
                  id = session$ns("market_sim_event_caption"),
                  class = "market-sim-event-caption",
                  "Timeline ready"
                ),
                div(
                  id = session$ns("market_sim_annotation"),
                  class = "market-sim-annotation",
                  ""
                ),
                div(
                  class = "market-sim-controls",
                  div(
                    class = "market-sim-controls-left",
                    tags$button(
                      type = "button",
                      id = session$ns("market_sim_play"),
                      class = "market-sim-btn",
                      `data-action` = "toggle-play",
                      icon("play"),
                      span(class = "label", "Play")
                    ),
                    tags$button(
                      type = "button",
                      id = session$ns("market_sim_restart"),
                      class = "market-sim-btn",
                      `data-action` = "restart",
                      icon("redo"),
                      span(class = "label", "Restart")
                    ),
                    tags$button(
                      type = "button",
                      id = session$ns("market_sim_skip"),
                      class = "market-sim-btn",
                      `data-action` = "skip",
                      icon("forward"),
                      span(class = "label", "Skip Phase")
                    ),
                    div(
                      class = "market-sim-speed-toggle",
                      span(class = "speed-label", "Speed"),
                      tags$button(
                        type = "button",
                        id = session$ns("market_sim_speed_1x"),
                        class = "market-sim-btn market-sim-btn--speed active",
                        `data-action` = "speed",
                        `data-speed` = "1",
                        span("1x")
                      ),
                      tags$button(
                        type = "button",
                        id = session$ns("market_sim_speed_2x"),
                        class = "market-sim-btn market-sim-btn--speed",
                        `data-action` = "speed",
                        `data-speed` = "2",
                        span("2x")
                    ),
                    )
                  ),
                  div(
                    class = "market-sim-controls-right",
                    span(id = session$ns("market_sim_clock"), class = "market-sim-clock", "00:00 / 00:00"),
                    tags$button(
                      type = "button",
                      id = session$ns("market_sim_mute"),
                      class = "market-sim-btn market-sim-btn--icon",
                      `data-action` = "toggle-audio",
                      icon("volume-up")
                    )
                  )
                )
              )
            ))

            # Send animation data to modal
            payload <- build_animation_payload(
              game_id = 1,
              round = settings$current_round,
              ns_prefix = session$ns(""),
              settings = settings
            )

            if (!is.null(payload)) {
              session$sendCustomMessage("market-sim-update", payload)
            }
          }
        }, error = function(e) {
          showNotification(paste("Error calculating results:", e$message), type = "error")
        })
      })
    })

    # Manual entry: Hospitals UI (rate matrix)
    output$manual_hospital_inputs <- renderUI({
      settings <- game_state()

      if (is.null(settings)) {
        return(p("No active game"))
      }

      hospitals <- get_hospitals(1)
      insurers <- get_insurers(1)
      if (nrow(hospitals) == 0 || nrow(insurers) == 0) {
        return(p("Ensure both hospital and insurer teams are created."))
      }

      con <- get_db_connection()
      existing <- dbGetQuery(con,
        "SELECT hospital_id, insurer_id, rate
         FROM hospital_rates WHERE game_id = 1 AND round = ?",
        params = list(settings$current_round)
      )
      close_db_connection(con)

      insurer_ths <- lapply(seq_len(nrow(insurers)), function(j) {
        tags$th(insurers$insurer_name[j], style = "text-align:center;min-width:110px;")
      })
      header_cells <- c(list(tags$th("", style = "min-width:130px;")), insurer_ths)

      body_rows <- lapply(seq_len(nrow(hospitals)), function(i) {
        hid <- hospitals$hospital_id[i]
        hname <- hospitals$hospital_name[i]
        rates <- existing[existing$hospital_id == hid, ]

        cells <- c(
          list(tags$td(tags$strong(hname), style = "word-break:break-word; overflow-wrap:anywhere; max-width:150px;")),
          lapply(seq_len(nrow(insurers)), function(j) {
            iid <- insurers$insurer_id[j]
            value <- rates$rate[rates$insurer_id == iid]
            value <- if (length(value) == 1) value else if (hid == iid) GAME_CONSTANTS$DEFAULT_NEGOTIATED_RATE else NA
            tags$td(
              numericInput(
                session$ns(sprintf("h_%s_rate_%s", hid, iid)),
                label = NULL,
                value = value, min = 0, max = 5, step = 0.05, width = "90px"
              ),
              style = "padding:2px 4px; text-align:center;"
            )
          })
        )
        do.call(tags$tr, cells)
      })

      tagList(
        helpText("Rate multiplier applied to base cost (e.g. 1.2 = 120% of cost)"),
        div(style = "overflow-x:auto;",
          tags$table(
            class = "table table-bordered table-condensed",
            style = "margin-bottom:0;",
            tags$thead(do.call(tags$tr, header_cells)),
            do.call(tags$tbody, body_rows)
          )
        ),
        div(style = "margin-top: 8px;",
          actionButton(session$ns("clear_rates"), "Clear All Rates",
                       class = "btn-warning btn-sm", icon = icon("eraser"))
        )
      )
    })

    # Clear all rates for current round
    observeEvent(input$clear_rates, {
      settings <- game_state()
      if (is.null(settings)) return()
      con <- get_db_connection()
      dbExecute(con, "DELETE FROM hospital_rates WHERE game_id = 1 AND round = ?",
                params = list(settings$current_round))
      dbExecute(con, "DELETE FROM insurer_network WHERE game_id = 1 AND round = ?",
                params = list(settings$current_round))
      close_db_connection(con)
      showNotification("All rates cleared for this round.", type = "warning")
    }, ignoreInit = TRUE)

    # Auto-save individual rate on input change (JS sends rate_autosave event)
    observeEvent(input$rate_autosave, {
      data <- input$rate_autosave
      settings <- game_state()
      if (is.null(settings)) return()
      save_single_rate(1, settings$current_round, data$hospital_id, data$insurer_id, data$value)
    }, ignoreInit = TRUE)

    # Pop-out negotiation board button
    popup_open <- reactiveVal(FALSE)
    popup_refresh <- reactiveTimer(3000)

    observeEvent(input$popup_negotiation, {
      if (!IS_WEBR) {
        session$sendCustomMessage(
          session$ns("open_popup"),
          list(url = "?view=negotiation")
        )
        return()
      }

      # Plain string: as.character() on tags$head() would drop the styles
      shell <- paste0(
        "<!DOCTYPE html><html><head><title>Negotiation Board</title><style>",
        paste(readLines("www/custom.css", warn = FALSE), collapse = "\n"),
        NEGOTIATION_POPUP_CSS,
        "body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }",
        "</style></head><body>",
        as.character(div(class = "neg-popup-title", "Negotiation Board")),
        as.character(div(id = "neg-popup-body",
                         div("Waiting for game...", class = "negotiation-waiting"))),
        "</body></html>"
      )
      session$sendCustomMessage(session$ns("open_popup"), list(shell = shell))
      popup_open(TRUE)
    })

    # Browser build: push the current board into the pop-out every 3 seconds
    observe({
      req(IS_WEBR, popup_open())
      popup_refresh()
      settings <- game_state()
      board <- if (is.null(settings)) {
        div("Waiting for game to start...", class = "negotiation-waiting")
      } else {
        tagList(
          div(class = "neg-popup-round",
              paste("Round", settings$current_round, "of", settings$n_rounds)),
          render_negotiation_grid_html(1, settings$current_round)
        )
      }
      session$sendCustomMessage(session$ns("update_popup"), list(html = as.character(board)))
    })

    # Batch save all hospital rates (fallback)
    observeEvent(input$save_hospital_decisions, {
      settings <- game_state()
      if (is.null(settings)) {
        showNotification("No active game", type = "error")
        return()
      }

      hospitals <- get_hospitals(1)
      insurers <- get_insurers(1)
      if (nrow(hospitals) == 0 || nrow(insurers) == 0) return()

      for (hid in hospitals$hospital_id) {
        for (iid in insurers$insurer_id) {
          rate_value <- as.numeric(input[[sprintf("h_%s_rate_%s", hid, iid)]])
          save_single_rate(1, settings$current_round, hid, iid, rate_value)
        }
      }
      showNotification("Hospital rates saved", type = "message")
    })

    # Manual entry: Insurers UI (premium table)
    output$manual_insurer_inputs <- renderUI({
      settings <- game_state()
      if (is.null(settings)) return(p("No active game"))

      insurers <- get_insurers(1)
      if (nrow(insurers) == 0) return(p("No insurers created yet"))

      con <- get_db_connection()
      existing <- dbGetQuery(con,
        "SELECT insurer_id, premium_young
         FROM insurer_premiums WHERE game_id = 1 AND round = ?",
        params = list(settings$current_round)
      )
      close_db_connection(con)

      body_rows <- lapply(seq_len(nrow(insurers)), function(i) {
        iid <- insurers$insurer_id[i]
        iname <- insurers$insurer_name[i]
        row <- existing[existing$insurer_id == iid, ]
        premium_young <- if (nrow(row) == 1) row$premium_young else GAME_CONSTANTS$DEFAULT_PREMIUM_YOUNG

        tags$tr(
          tags$td(tags$strong(iname), style = "word-break:break-word; overflow-wrap:anywhere; vertical-align:middle; max-width:150px;"),
          tags$td(
            numericInput(
              session$ns(paste0("i_", iid, "_premium_young")), label = NULL,
              value = premium_young, min = 0, max = 10000, step = 10, width = "120px"
            ),
            style = "padding:2px 4px;"
          )
        )
      })

      tagList(
        helpText("Annual premium charged to young enrollees ($)"),
        tags$table(
          class = "table table-bordered table-condensed",
          style = "margin-bottom:0;max-width:350px;",
          tags$thead(tags$tr(
            tags$th("Insurer"),
            tags$th("Premium ($)", style = "text-align:center;")
          )),
          do.call(tags$tbody, body_rows)
        )
      )
    })

    # Manual entry: Save insurer premiums
    observeEvent(input$save_insurer_decisions, {
      settings <- game_state()
      if (is.null(settings)) {
        showNotification("No active game", type = "error")
        return()
      }

      insurers <- get_insurers(1)
      if (nrow(insurers) == 0) return()

      con <- get_db_connection()
      for (iid in insurers$insurer_id) {
        premium_young <- as.numeric(input[[paste0("i_", iid, "_premium_young")]])

        if (is.na(premium_young)) next

        dbExecute(con,
          "DELETE FROM insurer_premiums WHERE game_id = 1 AND round = ? AND insurer_id = ?",
          params = list(settings$current_round, iid)
        )

        dbExecute(con,
          "INSERT INTO insurer_premiums (game_id, round, insurer_id, premium_young)
           VALUES (1, ?, ?, ?)",
          params = list(settings$current_round, iid, premium_young)
        )
      }
      close_db_connection(con)
      showNotification("Insurer premiums saved", type = "message")
    })
    # Reset game
    observeEvent(input$reset_game, {
      showModal(modalDialog(
        title = "Confirm Reset",
        "Are you sure you want to reset the game? All data will be lost!",
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_reset"), "Yes, Reset", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_reset, {
      con <- get_db_connection()

      dbExecute(con, "DELETE FROM round_results WHERE game_id = 1")
      dbExecute(con, "DELETE FROM hospital_rates WHERE game_id = 1")
      dbExecute(con, "DELETE FROM insurer_premiums WHERE game_id = 1")
      dbExecute(con, "DELETE FROM insurer_network WHERE game_id = 1")
      dbExecute(con, "UPDATE game_settings SET current_round = 1 WHERE game_id = 1")

      close_db_connection(con)

      game_state(get_game_settings(1))

      removeModal()
      showNotification("Game reset to Round 1", type = "warning")
    })


    # Round status display
    output$round_status <- renderUI({
      settings <- game_state()

      if (is.null(settings)) {
        return(div(
          h4("No active game", style = "color: red;"),
          p("Create a new game to begin")
        ))
      }

      all_submitted <- check_all_submitted(1, settings$current_round)

      div(
        h3(paste("Current Round:", settings$current_round, "of", settings$n_rounds)),
        if (all_submitted) {
          div(
            icon("check-circle", class = "fa-2x"),
            span(" All teams have submitted!", style = "color: green; font-size: 18px; margin-left: 10px;")
          )
        } else {
          div(
            icon("clock", class = "fa-2x"),
            span(" Waiting for team submissions...", style = "color: orange; font-size: 18px; margin-left: 10px;")
          )
        }
      )
    })

    # Hospital name editor
    output$hospital_name_editor <- renderUI({
      settings <- game_state()
      input$save_hospital_names

      if (is.null(settings)) {
        return(p("No hospitals. Create a game first."))
      }

      hospitals <- get_hospitals(1)

      if (nrow(hospitals) == 0) {
        return(p("No hospitals. Create a game first."))
      }

      hospital_inputs <- lapply(seq_len(nrow(hospitals)), function(i) {
        hid <- hospitals$hospital_id[i]
        box(
          title = paste0("Hospital ", hid), width = 6, status = "primary", solidHeader = TRUE,
          textInput(session$ns(paste0("hospital_name_", hid)), "Team name:",
                    value = hospitals$hospital_name[i], placeholder = "Enter hospital team name"),
          helpText("Suggested length: under 60 characters")
        )
      })

      do.call(tagList, hospital_inputs)
    })

    observeEvent(input$save_hospital_names, {
      hospitals <- get_hospitals(1)

      if (nrow(hospitals) == 0) {
        showNotification("No hospitals available to rename.", type = "warning")
        return()
      }

      new_names <- sapply(hospitals$hospital_id, function(hid) {
        input_name <- input[[paste0("hospital_name_", hid)]]
        trimws(ifelse(is.null(input_name), "", input_name))
      })

      if (any(new_names == "")) {
        showNotification("Hospital names cannot be blank.", type = "error")
        return()
      }

      if (any(nchar(new_names) > 60)) {
        showNotification("Hospital names must be 60 characters or fewer.", type = "error")
        return()
      }

      con <- get_db_connection()
      on.exit(close_db_connection(con), add = TRUE)

      for (i in seq_along(new_names)) {
        dbExecute(con,
          "UPDATE hospitals SET hospital_name = ? WHERE game_id = 1 AND hospital_id = ?",
          params = list(new_names[i], hospitals$hospital_id[i])
        )
      }

      showNotification("Hospital names updated.", type = "message")
    })

    # Insurer name editor
    output$insurer_name_editor <- renderUI({
      settings <- game_state()
      input$save_insurer_names

      if (is.null(settings)) {
        return(p("No insurers. Create a game first."))
      }

      insurers <- get_insurers(1)

      if (nrow(insurers) == 0) {
        return(p("No insurers. Create a game first."))
      }

      insurer_inputs <- lapply(seq_len(nrow(insurers)), function(i) {
        iid <- insurers$insurer_id[i]
        box(
          title = paste0("Insurer ", iid), width = 6, status = "success", solidHeader = TRUE,
          textInput(session$ns(paste0("insurer_name_", iid)), "Team name:",
                    value = insurers$insurer_name[i], placeholder = "Enter insurer team name"),
          helpText("Suggested length: under 60 characters")
        )
      })

      do.call(tagList, insurer_inputs)
    })

    observeEvent(input$save_insurer_names, {
      insurers <- get_insurers(1)

      if (nrow(insurers) == 0) {
        showNotification("No insurers available to rename.", type = "warning")
        return()
      }

      new_names <- sapply(insurers$insurer_id, function(iid) {
        input_name <- input[[paste0("insurer_name_", iid)]]
        trimws(ifelse(is.null(input_name), "", input_name))
      })

      if (any(new_names == "")) {
        showNotification("Insurer names cannot be blank.", type = "error")
        return()
      }

      if (any(nchar(new_names) > 60)) {
        showNotification("Insurer names must be 60 characters or fewer.", type = "error")
        return()
      }

      con <- get_db_connection()
      on.exit(close_db_connection(con), add = TRUE)

      for (i in seq_along(new_names)) {
        dbExecute(con,
          "UPDATE insurers SET insurer_name = ? WHERE game_id = 1 AND insurer_id = ?",
          params = list(new_names[i], insurers$insurer_id[i])
        )
      }

      showNotification("Insurer names updated.", type = "message")
    })

    # Submission status
    output$submission_status <- renderUI({
      settings <- game_state()
      if (is.null(settings)) return(p("No active game"))

      hospitals <- get_hospitals(1)
      insurers <- get_insurers(1)
      n_hospitals <- nrow(hospitals)
      n_insurers <- nrow(insurers)
      total_rate_cells <- n_hospitals * n_insurers

      con <- get_db_connection()
      rate_count <- dbGetQuery(con,
        "SELECT COUNT(*) AS cnt FROM hospital_rates WHERE game_id = 1 AND round = ?",
        params = list(settings$current_round)
      )$cnt

      insurer_premiums <- dbGetQuery(con,
        "SELECT DISTINCT insurer_id FROM insurer_premiums WHERE game_id = 1 AND round = ?",
        params = list(settings$current_round)
      )$insurer_id
      close_db_connection(con)

      premiums_missing <- setdiff(insurers$insurer_id, insurer_premiums)
      premiums_pending_names <- insurers$insurer_name[match(premiums_missing, insurers$insurer_id)]
      premiums_pending_names <- premiums_pending_names[!is.na(premiums_pending_names)]

      div(
        h4("Rates Entered:"),
        p(paste(rate_count, "of", total_rate_cells, "rate cells filled")),

        h4("Insurer Premiums:"),
        p(paste(length(insurer_premiums), "of", n_insurers, "submitted")),
        if (length(premiums_pending_names) > 0) {
          p(paste("Waiting for:", paste(premiums_pending_names, collapse = ", ")),
            style = "color: orange;")
        }
      )
    })

    # Download handlers
    output$download_results <- downloadHandler(
      filename = function() {
        paste0("game_results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        con <- get_db_connection()
        results <- dbGetQuery(con, "SELECT * FROM round_results WHERE game_id = 1")
        close_db_connection(con)
        write.csv(results, file, row.names = FALSE)
      }
    )

    output$download_decisions <- downloadHandler(
      filename = function() {
        paste0("game_decisions_", Sys.Date(), ".csv")
      },
      content = function(file) {
        con <- get_db_connection()
        hosp_rates <- dbGetQuery(con, "SELECT * FROM hospital_rates WHERE game_id = 1")
        ins_premiums <- dbGetQuery(con, "SELECT * FROM insurer_premiums WHERE game_id = 1")
        close_db_connection(con)

        combined <- dplyr::bind_rows(
          hosp_rates %>% dplyr::mutate(type = "hospital_rate"),
          ins_premiums %>% dplyr::mutate(type = "insurer_premium")
        )

        write.csv(combined, file, row.names = FALSE)
      }
    )

    output$download_insurer_report <- downloadHandler(
      filename = function() {
        paste0("insurer_report_", Sys.Date(), ".csv")
      },
      content = function(file) {
        settings <- get_game_settings(1)
        report_df <- build_insurer_report(game_id = 1, settings = settings)
        write.csv(report_df, file, row.names = FALSE)
      }
    )

    observeEvent(input$reload_saved_game, {
      req(input$upload_results, input$upload_decisions)

      parse_integer <- function(x) {
        if (is.null(x)) return(NA_integer_)
        vals <- trimws(as.character(x))
        vals[vals %in% c("", "NA", "NaN")] <- NA
        as.integer(vals)
      }

      parse_numeric <- function(x) {
        if (is.null(x)) return(NA_real_)
        vals <- trimws(as.character(x))
        vals[vals %in% c("", "NA", "NaN")] <- NA
        as.numeric(vals)
      }

      withProgress(message = "Loading saved game...", value = 0, {
        tryCatch({
          results_df <- read.csv(input$upload_results$datapath, stringsAsFactors = FALSE)
          decisions_df <- read.csv(input$upload_decisions$datapath, stringsAsFactors = FALSE)

          if (!"type" %in% names(decisions_df)) {
            stop("Decisions file is missing the 'type' column that distinguishes hospital and insurer data.")
          }

          if (nrow(decisions_df) == 0) {
            stop("Decisions file does not contain any rows.")
          }

          decisions_df$type <- trimws(tolower(decisions_df$type))

          if (any(decisions_df$type == "hospital_rate")) {
            missing_hr <- setdiff(c("round", "hospital_id", "insurer_id", "rate"), names(decisions_df))
            if (length(missing_hr) > 0) {
              stop(sprintf("Decisions file is missing required hospital rate columns: %s", paste(missing_hr, collapse = ", ")))
            }
          }

          if (any(decisions_df$type == "insurer_premium")) {
            missing_ip <- setdiff(c("round", "insurer_id", "premium_young"), names(decisions_df))
            if (length(missing_ip) > 0) {
              stop(sprintf("Decisions file is missing required insurer premium columns: %s", paste(missing_ip, collapse = ", ")))
            }
          }

          if (nrow(results_df) == 0) {
            stop("Results file does not contain any rows.")
          }

          missing_results <- setdiff(
            c(
              "round", "hospital_id", "patients_insured",
              "patients_uninsured_treated", "patients_charity_care",
              "revenue", "costs", "profit"
            ),
            names(results_df)
          )
          if (length(missing_results) > 0) {
            stop(sprintf("Results file is missing required columns: %s", paste(missing_results, collapse = ", ")))
          }

          if (!"insurer_id" %in% names(results_df)) results_df$insurer_id <- NA
          if (!"patients_total" %in% names(results_df)) results_df$patients_total <- NA
          if (!"claims_paid" %in% names(results_df)) results_df$claims_paid <- NA
          if (!"mlr" %in% names(results_df)) results_df$mlr <- NA
          if (!"calculated_at" %in% names(results_df)) results_df$calculated_at <- NA

          hospital_rates_df <- decisions_df %>%
            dplyr::filter(type == "hospital_rate") %>%
            dplyr::select(-dplyr::any_of(c("rate_id", "premium_id"))) %>%
            dplyr::mutate(
              game_id = 1L,
              round = parse_integer(round),
              hospital_id = parse_integer(hospital_id),
              insurer_id = parse_integer(insurer_id),
              rate = parse_numeric(rate),
              submitted_at = if ("submitted_at" %in% names(.)) as.character(submitted_at) else NA_character_
            ) %>%
            dplyr::select(dplyr::any_of(c("game_id", "round", "hospital_id", "insurer_id", "rate", "submitted_at")))

          insurer_premiums_df <- decisions_df %>%
            dplyr::filter(type == "insurer_premium") %>%
            dplyr::select(-dplyr::any_of(c("rate_id", "premium_id"))) %>%
            dplyr::mutate(
              game_id = 1L,
              round = parse_integer(round),
              insurer_id = parse_integer(insurer_id),
              premium_young = parse_numeric(premium_young),
              submitted_at = if ("submitted_at" %in% names(.)) as.character(submitted_at) else NA_character_
            ) %>%
            dplyr::select(dplyr::any_of(c("game_id", "round", "insurer_id", "premium_young", "submitted_at")))

          if (nrow(results_df) > 0 && !"round" %in% names(results_df)) {
            stop("Results file is missing the 'round' column.")
          }

          round_results_df <- results_df %>%
            dplyr::select(-dplyr::any_of(c("result_id"))) %>%
            dplyr::mutate(
              game_id = 1L,
              round = parse_integer(round),
              hospital_id = parse_integer(hospital_id),
              insurer_id = parse_integer(insurer_id),
              patients_insured = parse_integer(patients_insured),
              patients_uninsured_treated = parse_integer(patients_uninsured_treated),
              patients_charity_care = parse_integer(patients_charity_care),
              patients_total = if ("patients_total" %in% names(.)) parse_integer(patients_total) else NA_integer_,
              revenue = parse_numeric(revenue),
              costs = parse_numeric(costs),
              profit = parse_numeric(profit),
              claims_paid = parse_numeric(claims_paid),
              mlr = parse_numeric(mlr),
              calculated_at = if ("calculated_at" %in% names(.)) as.character(calculated_at) else NA_character_
            ) %>%
            dplyr::select(dplyr::any_of(c(
              "game_id", "round", "hospital_id", "insurer_id", "patients_insured",
              "patients_uninsured_treated", "patients_charity_care", "patients_total",
              "revenue", "costs", "profit", "claims_paid", "mlr", "calculated_at"
            )))

          all_rounds <- c(
            round_results_df$round,
            hospital_rates_df$round,
            insurer_premiums_df$round
          )
          max_round <- max(all_rounds, na.rm = TRUE)
          if (!is.finite(max_round)) {
            max_round <- 1L
          }
          max_round <- as.integer(max_round)

          unique_hospitals <- unique(c(
            round_results_df$hospital_id,
            hospital_rates_df$hospital_id
          ))
          unique_hospitals <- unique_hospitals[!is.na(unique_hospitals)]
          n_hosp <- length(unique_hospitals)

          unique_insurers <- unique(c(
            round_results_df$insurer_id,
            hospital_rates_df$insurer_id,
            insurer_premiums_df$insurer_id
          ))
          unique_insurers <- unique_insurers[!is.na(unique_insurers)]
          n_ins <- length(unique_insurers)

          con <- get_db_connection()
          on.exit(close_db_connection(con), add = TRUE)

          ensure_teams <- function(target_n, table_name, id_col, name_col, palette, label) {
            if (target_n <= 0) return(invisible(NULL))

            existing <- dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE game_id = 1", id_col, table_name))
            existing_ids <- if (nrow(existing) > 0) existing[[1]] else integer(0)
            needed_ids <- setdiff(seq_len(target_n), existing_ids)
            if (length(needed_ids) == 0) {
              return(invisible(NULL))
            }

            default_names <- generate_team_names(target_n, label)

            for (team_id in needed_ids) {
              team_name <- if (length(default_names) >= team_id) default_names[team_id] else paste(label, team_id)
              team_color <- palette[((team_id - 1) %% length(palette)) + 1]
              dbExecute(con,
                sprintf(
                  "INSERT INTO %s (%s, game_id, %s, color) VALUES (?, 1, ?, ?)",
                  table_name, id_col, name_col
                ),
                params = list(team_id, team_name, team_color)
              )
            }
          }

          population_default <- GAME_CONSTANTS$DEFAULT_POPULATION
          settings_existing <- dbGetQuery(con, "SELECT * FROM game_settings WHERE game_id = 1")
          if (nrow(settings_existing) > 0) {
            if (!is.na(settings_existing$population[1])) {
              population_default <- settings_existing$population[1]
            }
          }

          dbWithTransaction(con, {
            dbExecute(con, "DELETE FROM round_results WHERE game_id = 1")
            dbExecute(con, "DELETE FROM hospital_rates WHERE game_id = 1")
            dbExecute(con, "DELETE FROM insurer_premiums WHERE game_id = 1")

            if (nrow(round_results_df) > 0) {
              dbWriteTable(con, "round_results", round_results_df, append = TRUE, row.names = FALSE)
            }

            if (nrow(hospital_rates_df) > 0) {
              dbWriteTable(con, "hospital_rates", hospital_rates_df, append = TRUE, row.names = FALSE)
            }

            if (nrow(insurer_premiums_df) > 0) {
              dbWriteTable(con, "insurer_premiums", insurer_premiums_df, append = TRUE, row.names = FALSE)
            }

            if (nrow(settings_existing) == 0) {
              dbExecute(con,
                "INSERT INTO game_settings (
                   game_id, n_hospitals, n_insurers, n_rounds, population, current_round,
                   game_status, emtala_enabled, medicare_reimbursement_pct, dsh_reimbursement_pct,
                   cost_young, cost_old, prob_young, prob_old, random_seed
                 ) VALUES (1, ?, ?, ?, ?, ?, 'active', ?, ?, ?, ?, ?, ?, ?, ?)",
                params = list(
                  max(n_hosp, 1L),
                  max(n_ins, 1L),
                  max_round,
                  population_default,
                  max_round,
                  as.integer(GAME_CONSTANTS$POLICY_DEFAULT_EMTALA),
                  GAME_CONSTANTS$POLICY_DEFAULT_MEDICARE_REIMBURSEMENT,
                  GAME_CONSTANTS$POLICY_DEFAULT_DSH_REIMBURSEMENT,
                  GAME_CONSTANTS$BASE_COSTS$young,
                  GAME_CONSTANTS$BASE_COSTS$old,
                  GAME_CONSTANTS$HEALTH_PROBABILITIES$young,
                  GAME_CONSTANTS$HEALTH_PROBABILITIES$old,
                  NA
                )
              )
            } else {
              dbExecute(con,
                "UPDATE game_settings
                   SET n_hospitals = ?,
                       n_insurers = ?,
                       n_rounds = ?,
                       current_round = ?,
                       game_status = 'active'
                 WHERE game_id = 1",
                params = list(max(n_hosp, 1L), max(n_ins, 1L), max_round, max_round)
              )
            }
          })

          ensure_teams(max(n_hosp, 0L), "hospitals", "hospital_id", "hospital_name", TEAM_COLORS$hospitals, "Hospital")
          ensure_teams(max(n_ins, 0L), "insurers", "insurer_id", "insurer_name", TEAM_COLORS$insurers, "Insurer")

          incProgress(1)

          game_state(get_game_settings(1))
          showNotification("Saved game loaded successfully.", type = "message")
        }, error = function(err) {
          showNotification(paste("Failed to load saved game:", err$message), type = "error")
        })
      })
    })
  })
}
