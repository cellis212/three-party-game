# Public Display Dashboard Module (for classroom projection)

displayUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Game header with round info
    fluidRow(
      column(12,
        div(
          class = "display-header",
          style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 30px; border-radius: 10px; margin-bottom: 20px;",
          h1("Three-Party System Healthcare Market", style = "text-align: center; margin: 0; font-size: 48px;"),
          h2(uiOutput(ns("round_display"), inline = TRUE), style = "text-align: center; margin-top: 10px; font-size: 36px;")
        )
      )
    ),

    # Market Network Diagram (top position for classroom visibility)
    fluidRow(
      box(
        title = "Market Network Diagram", width = 12, status = "info", solidHeader = TRUE,
        plotlyOutput(ns("network_diagram"), height = "500px")
      )
    ),

    # Market summary cards
    fluidRow(
      valueBoxOutput(ns("total_insured_box"), width = 3),
      valueBoxOutput(ns("uninsured_rate_box"), width = 3),
      valueBoxOutput(ns("avg_premium_box"), width = 3),
      valueBoxOutput(ns("charity_care_box"), width = 3)
    ),

    fluidRow(
      valueBoxOutput(ns("avg_rate_box"), width = 3)
    ),

    fluidRow(
      box(
        title = "Welfare Summary", width = 12, status = "info", solidHeader = TRUE,
        uiOutput(ns("welfare_summary"))
      )
    ),

    # Profit rankings this round
    fluidRow(
      box(
        title = "Hospital Profit \u2014 This Round", width = 6, status = "primary", solidHeader = TRUE,
        tableOutput(ns("hospital_profit_ranking"))
      ),
      box(
        title = "Insurer Profit \u2014 This Round", width = 6, status = "success", solidHeader = TRUE,
        tableOutput(ns("insurer_profit_ranking"))
      )
    ),

    # Volume rankings this round
    fluidRow(
      box(
        title = "Hospital Volume \u2014 This Round", width = 6, status = "primary", solidHeader = TRUE,
        tableOutput(ns("hospital_volume_ranking"))
      ),
      box(
        title = "Insurer Enrollment \u2014 This Round", width = 6, status = "success", solidHeader = TRUE,
        tableOutput(ns("insurer_volume_ranking"))
      )
    ),

    # Profit over time
    fluidRow(
      box(
        title = "Hospital Profit Over Time", width = 6, status = "primary", solidHeader = TRUE,
        plotlyOutput(ns("hospital_profit_trend"), height = "380px")
      ),
      box(
        title = "Insurer Profit Over Time", width = 6, status = "success", solidHeader = TRUE,
        plotlyOutput(ns("insurer_profit_trend"), height = "380px")
      )
    ),

    fluidRow(
      box(
        title = "Patient Coverage", width = 6, status = "primary", solidHeader = TRUE,
        plotlyOutput(ns("coverage_pie"), height = "350px")
      ),
      box(
        title = "Market Trends", width = 6, status = "success", solidHeader = TRUE,
        plotlyOutput(ns("market_trends"), height = "350px")
      )
    ),

    fluidRow(
      box(
        title = "Average Negotiated Rate Over Time", width = 12, status = "info", solidHeader = TRUE,
        plotlyOutput(ns("avg_rate_trend"), height = "380px")
      )
    )
  )
}

displayServer <- function(id, game_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Shared helper: wrap long text for plotly labels
    wrap_text <- function(text, width = 35) {
      sapply(text, function(txt) {
        words <- strsplit(txt, " ")[[1]]
        lines <- character()
        current_line <- character()
        for (word in words) {
          test_line <- paste(c(current_line, word), collapse = " ")
          if (nchar(test_line) <= width) {
            current_line <- c(current_line, word)
          } else {
            if (length(current_line) > 0) {
              lines <- c(lines, paste(current_line, collapse = " "))
            }
            current_line <- word
          }
        }
        if (length(current_line) > 0) {
          lines <- c(lines, paste(current_line, collapse = " "))
        }
        paste(lines, collapse = "<br>")
      })
    }

    # Auto-refresh for live updates
    autorefresh <- reactiveTimer(3000)

    # Round display
    output$round_display <- renderUI({
      autorefresh()
      settings <- game_state()

      if (is.null(settings)) {
        return(span("Waiting for game to start...", style = "color: #ecf0f1;"))
      }

      span(
        paste("Round", settings$current_round, "of", settings$n_rounds),
        style = "color: #ecf0f1; font-weight: bold;"
      )
    })

    # Value boxes
    output$total_insured_box <- renderValueBox({
      autorefresh()
      settings <- game_state()

      if (is.null(settings)) {
        return(valueBox("--", "Total Insured", icon = icon("users"), color = "blue"))
      }

      results <- get_round_results(1, settings$current_round)

      if (nrow(results) == 0) {
        return(valueBox("--", "Total Insured", icon = icon("users"), color = "blue"))
      }

      total_insured <- sum(results$patients_insured[!is.na(results$insurer_id)], na.rm = TRUE)

      valueBox(
        formatC(total_insured, format = "f", digits = 0, big.mark = ","),
        "Total Insured",
        icon = icon("users"),
        color = "blue"
      )
    })

    output$uninsured_rate_box <- renderValueBox({
      autorefresh()
      settings <- game_state()

      if (is.null(settings)) {
        return(valueBox("--", "Uninsured Rate", icon = icon("heartbeat"), color = "red"))
      }

      results <- get_round_results(1, settings$current_round)

      if (nrow(results) == 0) {
        return(valueBox("--", "Uninsured Rate", icon = icon("heartbeat"), color = "red"))
      }

      total_insured <- sum(results$patients_insured[!is.na(results$insurer_id)], na.rm = TRUE)
      uninsured_total <- results %>%
        dplyr::filter(is.na(insurer_id) & is.na(hospital_id)) %>%
        dplyr::summarise(total = sum(patients_insured, na.rm = TRUE)) %>%
        dplyr::pull(total)
      uninsured_total <- ifelse(length(uninsured_total) == 0, 0, uninsured_total)
      medicare_total <- sum(results$patients_public[!is.na(results$hospital_id)], na.rm = TRUE)
      total_population <- total_insured + uninsured_total + medicare_total
      uninsured_rate <- ifelse(total_population > 0, uninsured_total / total_population, NA_real_)

      color <- if (is.na(uninsured_rate) || uninsured_rate <= 0.1) "green" else if (uninsured_rate <= 0.2) "yellow" else "red"

      valueBox(
        if (is.na(uninsured_rate)) "--" else format_percentage(uninsured_rate),
        "Uninsured Rate",
        icon = icon("heartbeat"),
        color = color
      )
    })

    output$avg_premium_box <- renderValueBox({
      autorefresh()
      settings <- game_state()

      if (is.null(settings)) {
        return(valueBox("--", "Average Premium", icon = icon("dollar-sign"), color = "purple"))
      }

      decisions <- get_round_decisions(1, settings$current_round)

      avg_premium <- NA
      if (!is.null(decisions$insurer_premiums) && nrow(decisions$insurer_premiums) > 0) {
        avg_premium <- decisions$insurer_premiums %>%
          dplyr::summarise(avg = mean(premium_young, na.rm = TRUE)) %>%
          dplyr::pull(avg)
      }

      if (is.na(avg_premium)) {
        return(valueBox("--", "Average Premium", icon = icon("dollar-sign"), color = "purple"))
      }

      valueBox(
        format_currency(avg_premium),
        "Average Premium",
        icon = icon("dollar-sign"),
        color = "purple"
      )
    })

    output$charity_care_box <- renderValueBox({
      autorefresh()
      settings <- game_state()

      if (is.null(settings)) {
        return(valueBox("--", "Medicare Patients", icon = icon("hand-holding-heart"), color = "green"))
      }

      results <- get_round_results(1, settings$current_round)

      if (nrow(results) == 0) {
        return(valueBox("--", "Medicare Patients", icon = icon("hand-holding-heart"), color = "green"))
      }

      total_medicare <- sum(results$patients_public[!is.na(results$hospital_id)], na.rm = TRUE)

      valueBox(
        formatC(total_medicare, format = "f", digits = 0, big.mark = ","),
        "Medicare Patients",
        icon = icon("hand-holding-heart"),
        color = "green"
      )
    })

    output$avg_rate_box <- renderValueBox({
      autorefresh()
      settings <- game_state()

      if (is.null(settings)) {
        return(valueBox("--", "Avg Hospital Rate", icon = icon("handshake"), color = "teal"))
      }

      summary <- get_market_summary(1, settings$current_round)

      if (is.null(summary) || is.na(summary$avg_hospital_price)) {
        return(valueBox("--", "Avg Hospital Rate", icon = icon("handshake"), color = "teal"))
      }

      valueBox(
        round(summary$avg_hospital_price, 2),
        "Avg Hospital Rate",
        icon = icon("handshake"),
        color = "teal"
      )
    })

    output$welfare_summary <- renderUI({
      autorefresh()
      settings <- game_state()

      if (is.null(settings)) {
        return(div(
          style = "text-align: center; color: #7f8c8d; font-style: italic;",
          "No active game yet."
        ))
      }

      metrics <- calculate_welfare_metrics(1, settings$current_round)

      if (is.null(metrics)) {
        return(div(
          style = "text-align: center; color: #7f8c8d; font-style: italic;",
          "Results not available for this round yet."
        ))
      }

      format_metric_value <- function(value) {
        if (is.null(value) || is.na(value)) {
          return(tags$span("--", style = "font-size: 26px; font-weight: bold; color: #bdc3c7;"))
        }

        color <- if (value < 0) "#e74c3c" else "#1abc9c"
        tags$span(
          format_currency(value),
          style = paste0("font-size: 26px; font-weight: bold; color: ", color, ";")
        )
      }

      metric_tile <- function(label, value) {
        tags$div(
          style = "background: #fdfefe; border-radius: 8px; padding: 16px; text-align: center; box-shadow: 0 1px 3px rgba(0,0,0,0.08);",
          tags$div(label,
                   style = "font-size: 14px; text-transform: uppercase; letter-spacing: 0.5px; color: #7f8c8d;"),
          tags$div(format_metric_value(value), style = "margin-top: 6px;")
        )
      }

      tags$div(
        style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 18px;",
        metric_tile("Consumer Surplus", metrics$consumer_surplus),
        metric_tile("Hospital Profit (Loss)", metrics$total_hospital_profit),
        metric_tile("Insurer Profit (Loss)", metrics$total_insurer_profit),
        metric_tile("Total Welfare", metrics$social_welfare)
      )
    })

    # ---- Ranking tables (current round) ----

    output$hospital_profit_ranking <- renderTable({
      autorefresh()
      settings <- game_state()
      if (is.null(settings)) return(tibble::tibble(Status = "Waiting for game to start"))

      results <- get_round_results(1, settings$current_round)
      if (nrow(results) == 0) return(tibble::tibble(Status = "No data yet"))

      hospitals <- get_hospitals(1)

      results %>%
        dplyr::filter(!is.na(hospital_id)) %>%
        dplyr::group_by(hospital_id) %>%
        dplyr::summarise(profit = sum(dplyr::coalesce(profit, 0), na.rm = TRUE), .groups = "drop") %>%
        dplyr::left_join(hospitals, by = "hospital_id") %>%
        dplyr::mutate(
          hospital_name = dplyr::if_else(is.na(hospital_name) | hospital_name == "",
                                         paste("Hospital", hospital_id), hospital_name)
        ) %>%
        dplyr::arrange(desc(profit)) %>%
        dplyr::mutate(
          Rank = dplyr::row_number(),
          Profit = format_currency(profit)
        ) %>%
        dplyr::select(Rank, Hospital = hospital_name, Profit) %>%
        dplyr::slice_head(n = 10)
    }, striped = TRUE, bordered = FALSE, hover = TRUE, digits = 0, rownames = FALSE)

    output$insurer_profit_ranking <- renderTable({
      autorefresh()
      settings <- game_state()
      if (is.null(settings)) return(tibble::tibble(Status = "Waiting for game to start"))

      results <- get_round_results(1, settings$current_round)
      if (nrow(results) == 0) return(tibble::tibble(Status = "No data yet"))

      insurers <- get_insurers(1)

      results %>%
        dplyr::filter(!is.na(insurer_id)) %>%
        dplyr::group_by(insurer_id) %>%
        dplyr::summarise(profit = sum(dplyr::coalesce(profit, 0), na.rm = TRUE), .groups = "drop") %>%
        dplyr::left_join(insurers, by = "insurer_id") %>%
        dplyr::mutate(
          insurer_name = dplyr::if_else(is.na(insurer_name) | insurer_name == "",
                                        paste("Insurer", insurer_id), insurer_name)
        ) %>%
        dplyr::arrange(desc(profit)) %>%
        dplyr::mutate(
          Rank = dplyr::row_number(),
          Profit = format_currency(profit)
        ) %>%
        dplyr::select(Rank, Insurer = insurer_name, Profit) %>%
        dplyr::slice_head(n = 10)
    }, striped = TRUE, bordered = FALSE, hover = TRUE, digits = 0, rownames = FALSE)

    output$hospital_volume_ranking <- renderTable({
      autorefresh()
      settings <- game_state()
      if (is.null(settings)) return(tibble::tibble(Status = "Waiting for game to start"))

      results <- get_round_results(1, settings$current_round)
      if (nrow(results) == 0) return(tibble::tibble(Status = "No data yet"))

      hospitals <- get_hospitals(1)

      results %>%
        dplyr::filter(!is.na(hospital_id)) %>%
        dplyr::group_by(hospital_id) %>%
        dplyr::summarise(
          patients = sum(dplyr::coalesce(
            patients_total,
            patients_insured + patients_uninsured_treated +
              dplyr::coalesce(patients_charity_care, 0) +
              dplyr::coalesce(patients_public, 0)
          ), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::left_join(hospitals, by = "hospital_id") %>%
        dplyr::mutate(
          hospital_name = dplyr::if_else(is.na(hospital_name) | hospital_name == "",
                                         paste("Hospital", hospital_id), hospital_name)
        ) %>%
        dplyr::arrange(desc(patients)) %>%
        dplyr::mutate(
          Rank = dplyr::row_number(),
          Patients = formatC(patients, format = "f", digits = 0, big.mark = ",")
        ) %>%
        dplyr::select(Rank, Hospital = hospital_name, Patients) %>%
        dplyr::slice_head(n = 10)
    }, striped = TRUE, bordered = FALSE, hover = TRUE, digits = 0, rownames = FALSE)

    output$insurer_volume_ranking <- renderTable({
      autorefresh()
      settings <- game_state()
      if (is.null(settings)) return(tibble::tibble(Status = "Waiting for game to start"))

      results <- get_round_results(1, settings$current_round)
      if (nrow(results) == 0) return(tibble::tibble(Status = "No data yet"))

      insurers <- get_insurers(1)

      results %>%
        dplyr::filter(!is.na(insurer_id)) %>%
        dplyr::group_by(insurer_id) %>%
        dplyr::summarise(
          enrollment = sum(dplyr::coalesce(patients_insured, 0), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::left_join(insurers, by = "insurer_id") %>%
        dplyr::mutate(
          insurer_name = dplyr::if_else(is.na(insurer_name) | insurer_name == "",
                                        paste("Insurer", insurer_id), insurer_name)
        ) %>%
        dplyr::arrange(desc(enrollment)) %>%
        dplyr::mutate(
          Rank = dplyr::row_number(),
          Enrolled = formatC(enrollment, format = "f", digits = 0, big.mark = ",")
        ) %>%
        dplyr::select(Rank, Insurer = insurer_name, Enrolled) %>%
        dplyr::slice_head(n = 10)
    }, striped = TRUE, bordered = FALSE, hover = TRUE, digits = 0, rownames = FALSE)

    # ---- Profit over time (firm level) ----

    output$hospital_profit_trend <- renderPlotly({
      autorefresh()
      settings <- game_state()
      if (is.null(settings)) return(plotly_empty())

      con <- get_db_connection()
      on.exit(close_db_connection(con), add = TRUE)

      history <- dbGetQuery(con,
        "SELECT round, hospital_id, COALESCE(profit, 0) AS profit
         FROM round_results
         WHERE game_id = 1 AND hospital_id IS NOT NULL
         ORDER BY hospital_id, round"
      )

      if (nrow(history) == 0) {
        return(plotly_empty() %>% layout(title = "No data yet"))
      }

      hospitals <- get_hospitals(1)
      if (nrow(hospitals) == 0) return(plotly_empty())

      history <- history %>%
        dplyr::left_join(hospitals, by = "hospital_id") %>%
        dplyr::mutate(hospital_factor = factor(hospital_name, levels = hospitals$hospital_name))

      max_round <- max(history$round, 1)

      plot_ly(history,
              x = ~round, y = ~profit,
              color = ~hospital_factor, colors = hospitals$color,
              type = "scatter", mode = "lines+markers",
              hoverinfo = "text",
              hovertext = ~paste0(hospital_name, "<br>Round ", round, ": ", format_currency(profit)),
              line = list(width = 3), marker = list(size = 9)) %>%
        layout(
          xaxis = list(title = "Round", titlefont = list(size = 14), dtick = 1),
          yaxis = list(title = "Profit ($)", titlefont = list(size = 14)),
          legend = list(title = list(text = "Hospitals"), font = list(size = 12)),
          shapes = list(
            list(type = "line", x0 = 0, x1 = max_round,
                 y0 = 0, y1 = 0,
                 line = list(color = "rgba(0,0,0,0.3)", width = 1, dash = "dash"))
          )
        )
    })

    output$insurer_profit_trend <- renderPlotly({
      autorefresh()
      settings <- game_state()
      if (is.null(settings)) return(plotly_empty())

      con <- get_db_connection()
      on.exit(close_db_connection(con), add = TRUE)

      history <- dbGetQuery(con,
        "SELECT round, insurer_id, COALESCE(profit, 0) AS profit
         FROM round_results
         WHERE game_id = 1 AND insurer_id IS NOT NULL
         ORDER BY insurer_id, round"
      )

      if (nrow(history) == 0) {
        return(plotly_empty() %>% layout(title = "No data yet"))
      }

      insurers <- get_insurers(1)
      if (nrow(insurers) == 0) return(plotly_empty())

      history <- history %>%
        dplyr::left_join(insurers, by = "insurer_id") %>%
        dplyr::mutate(insurer_factor = factor(insurer_name, levels = insurers$insurer_name))

      max_round <- max(history$round, 1)

      plot_ly(history,
              x = ~round, y = ~profit,
              color = ~insurer_factor, colors = insurers$color,
              type = "scatter", mode = "lines+markers",
              hoverinfo = "text",
              hovertext = ~paste0(insurer_name, "<br>Round ", round, ": ", format_currency(profit)),
              line = list(width = 3), marker = list(size = 9)) %>%
        layout(
          xaxis = list(title = "Round", titlefont = list(size = 14), dtick = 1),
          yaxis = list(title = "Profit ($)", titlefont = list(size = 14)),
          legend = list(title = list(text = "Insurers"), font = list(size = 12)),
          shapes = list(
            list(type = "line", x0 = 0, x1 = max_round,
                 y0 = 0, y1 = 0,
                 line = list(color = "rgba(0,0,0,0.3)", width = 1, dash = "dash"))
          )
        )
    })

    # ---- Network diagram ----

    output$network_diagram <- renderPlotly({
      autorefresh()
      settings <- game_state()

      if (is.null(settings)) return(plotly_empty())

      decisions <- get_round_decisions(1, settings$current_round)
      hospitals <- get_hospitals(1)
      insurers <- get_insurers(1)

      if (is.null(decisions$hospital_rates) || nrow(decisions$hospital_rates) == 0 ||
          nrow(hospitals) == 0 || nrow(insurers) == 0) {
        return(plotly_empty() %>% layout(title = "No network data yet"))
      }

      # Layout: hospitals on top (ordered 1..N), insurers below
      hospitals <- hospitals %>% dplyr::arrange(hospital_id)
      n_hosp <- nrow(hospitals)
      n_ins <- nrow(insurers)

      hosp_x <- if (n_hosp > 1) seq(0.05, 0.95, length.out = n_hosp) else 0.5
      hosp_y <- rep(0.8, n_hosp)

      # Edges (connections)
      edges <- decisions$hospital_rates %>%
        dplyr::filter(rate > 0)

      insurer_x <- if (n_ins > 1) seq(0.05, 0.95, length.out = n_ins) else 0.5
      ins_y <- rep(0.2, n_ins)

      edge_shapes <- list()
      if (nrow(edges) > 0) {
        for (i in seq_len(nrow(edges))) {
          h_idx <- which(hospitals$hospital_id == edges$hospital_id[i])
          i_idx <- which(insurers$insurer_id == edges$insurer_id[i])
          if (length(h_idx) == 1 && length(i_idx) == 1) {
            edge_shapes[[length(edge_shapes) + 1]] <- list(
              type = "line",
              x0 = hosp_x[h_idx], x1 = insurer_x[i_idx],
              y0 = hosp_y[h_idx], y1 = ins_y[i_idx],
              line = list(color = "rgba(150, 150, 150, 0.3)", width = 2)
            )
          }
        }
      }

      plot_ly() %>%
        add_trace(
          x = hosp_x, y = hosp_y,
          type = "scatter", mode = "markers+text",
          text = wrap_text(hospitals$hospital_name, width = 25),
          textposition = "top center",
          textfont = list(size = 11),
          marker = list(size = 18, color = hospitals$color, line = list(color = "#2c3e50", width = 1)),
          cliponaxis = FALSE, name = "Hospitals",
          hoverinfo = "text", hovertext = hospitals$hospital_name
        ) %>%
        add_trace(
          x = insurer_x, y = ins_y,
          type = "scatter", mode = "markers+text",
          text = wrap_text(insurers$insurer_name, width = 25),
          textposition = "bottom center",
          textfont = list(size = 11),
          marker = list(size = 20, color = insurers$color),
          cliponaxis = FALSE, name = "Insurers",
          hoverinfo = "text", hovertext = insurers$insurer_name
        ) %>%
        layout(
          shapes = edge_shapes,
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, range = c(-0.15, 1.15)),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, range = c(-0.1, 1.1)),
          showlegend = FALSE,
          title = "Hospital-Insurer Networks",
          margin = list(l = 150, r = 150, t = 120, b = 120)
        )
    })

    # ---- Coverage pie chart ----

    output$coverage_pie <- renderPlotly({
      autorefresh()
      settings <- game_state()

      if (is.null(settings)) return(plotly_empty())

      results <- get_round_results(1, settings$current_round)

      if (nrow(results) == 0) {
        return(plotly_empty() %>% layout(title = "No data yet"))
      }

      total_insured <- sum(results$patients_insured[!is.na(results$insurer_id)], na.rm = TRUE)
      total_medicare <- sum(results$patients_public[!is.na(results$hospital_id)], na.rm = TRUE)
      uninsured_total <- results %>%
        dplyr::filter(is.na(insurer_id) & is.na(hospital_id)) %>%
        dplyr::summarise(total = sum(patients_insured, na.rm = TRUE)) %>%
        dplyr::pull(total)
      uninsured_total <- ifelse(length(uninsured_total) == 0, 0, uninsured_total)

      coverage_data <- data.frame(
        Category = c("Private Insurance", "Medicare", "Uninsured"),
        Count = c(total_insured, total_medicare, uninsured_total),
        Color = c("#3498db", "#2ecc71", "#e74c3c")
      )

      plot_ly(coverage_data,
              labels = ~Category, values = ~Count,
              type = "pie",
              marker = list(colors = ~Color),
              textinfo = "label+percent",
              textfont = list(size = 16)) %>%
        layout(
          title = "Patient Coverage Distribution",
          showlegend = TRUE,
          legend = list(font = list(size = 14))
        )
    })

    # ---- Market trends ----

    output$market_trends <- renderPlotly({
      autorefresh()
      settings <- game_state()

      if (is.null(settings)) return(plotly_empty())

      con <- get_db_connection()

      trends <- dbGetQuery(con,
        "SELECT round,
                SUM(CASE WHEN insurer_id IS NOT NULL THEN patients_insured ELSE 0 END) AS total_insured,
                SUM(CASE WHEN hospital_id IS NOT NULL THEN patients_public ELSE 0 END) AS total_medicare,
                SUM(CASE WHEN insurer_id IS NULL AND hospital_id IS NULL THEN patients_insured ELSE 0 END) AS total_uninsured
         FROM round_results
         WHERE game_id = 1
         GROUP BY round
         ORDER BY round"
      )

      close_db_connection(con)

      if (nrow(trends) == 0) {
        return(plotly_empty() %>% layout(title = "No historical data yet"))
      }

      plot_ly(trends) %>%
        add_trace(x = ~round, y = ~total_insured, type = "scatter", mode = "lines+markers",
                 name = "Private Insurance", line = list(color = "#3498db", width = 3),
                 marker = list(size = 10)) %>%
        add_trace(x = ~round, y = ~total_medicare, type = "scatter", mode = "lines+markers",
                 name = "Medicare", line = list(color = "#2ecc71", width = 3),
                 marker = list(size = 10)) %>%
        add_trace(x = ~round, y = ~total_uninsured, type = "scatter", mode = "lines+markers",
                 name = "Uninsured", line = list(color = "#e74c3c", width = 3),
                 marker = list(size = 10)) %>%
        layout(
          title = list(text = "Coverage Trends", font = list(size = 18)),
          xaxis = list(title = "Round", titlefont = list(size = 14)),
          yaxis = list(title = "Number of Patients", titlefont = list(size = 14)),
          hovermode = "x unified",
          legend = list(font = list(size = 14))
        )
    })

    # ---- Average Negotiated Rate Over Time ----

    output$avg_rate_trend <- renderPlotly({
      autorefresh()
      settings <- game_state()
      if (is.null(settings)) return(plotly_empty())

      con <- get_db_connection()
      on.exit(close_db_connection(con), add = TRUE)

      rates <- dbGetQuery(con,
        "SELECT round, hospital_id, AVG(rate) AS avg_rate
         FROM hospital_rates
         WHERE game_id = 1 AND rate > 0
         GROUP BY round, hospital_id
         ORDER BY hospital_id, round"
      )

      if (nrow(rates) == 0) {
        return(plotly_empty() %>% layout(title = "No rate data yet"))
      }

      hospitals <- get_hospitals(1)
      if (nrow(hospitals) == 0) return(plotly_empty())

      rates <- rates %>%
        dplyr::left_join(hospitals, by = "hospital_id")

      # Market average per round
      market_avg <- rates %>%
        dplyr::group_by(round) %>%
        dplyr::summarise(avg_rate = mean(avg_rate, na.rm = TRUE), .groups = "drop")

      max_round <- max(rates$round, 1)

      p <- plot_ly()

      # Per-hospital lines
      for (i in seq_len(nrow(hospitals))) {
        h_data <- rates %>% dplyr::filter(hospital_id == hospitals$hospital_id[i])
        if (nrow(h_data) == 0) next
        p <- p %>% add_trace(
          data = h_data,
          x = ~round, y = ~avg_rate,
          type = "scatter", mode = "lines+markers",
          name = hospitals$hospital_name[i],
          line = list(color = hospitals$color[i], width = 2),
          marker = list(size = 7, color = hospitals$color[i]),
          hoverinfo = "text",
          hovertext = ~paste0(hospital_name, "<br>Round ", round, ": ", round(avg_rate, 2))
        )
      }

      # Market average (bold dotted line)
      p <- p %>% add_trace(
        data = market_avg,
        x = ~round, y = ~avg_rate,
        type = "scatter", mode = "lines+markers",
        name = "Market Average",
        line = list(color = "black", width = 4, dash = "dot"),
        marker = list(size = 10, color = "black"),
        hoverinfo = "text",
        hovertext = ~paste0("Market Avg<br>Round ", round, ": ", round(avg_rate, 2))
      )

      p %>% layout(
        xaxis = list(title = "Round", titlefont = list(size = 14), dtick = 1),
        yaxis = list(title = "Avg Negotiated Rate (x Cost)", titlefont = list(size = 14)),
        legend = list(font = list(size = 12)),
        shapes = list(
          list(type = "line", x0 = 0, x1 = max_round,
               y0 = 1, y1 = 1,
               line = list(color = "rgba(0,0,0,0.2)", width = 1, dash = "dash"))
        )
      )
    })
  })
}
