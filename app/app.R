# Three-Party System Game - Main Shiny Application
# Educational simulation for health insurance markets

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

# Source all required files
source("global.R")

source("logic/game_calculations.R")
source("logic/market_equilibrium.R")
source("logic/insurer_report.R")
source("logic/scoring.R")
source("logic/animation_payload.R")
source("modules/instructor_ui.R")
source("modules/display_ui.R")

instructor_dashboard_ui <- dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(
    title = "Three-Party System Game",
    titleWidth = 300
  ),

  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Public Display", tabName = "display", icon = icon("tv")),
      menuItem("Instructor Control", tabName = "instructor", icon = icon("chalkboard-teacher"))
    ),

    # Game status in sidebar
    hr(),
    div(
      style = "padding: 15px;",
      h4("Game Status", style = "color: white;"),
      uiOutput("sidebar_game_status")
    )
  ),

  # Body
  dashboardBody(
    useShinyjs(),

    # Custom CSS
    tags$head(
      tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      plotly_page_dependencies()
    ),

    tabItems(
      # Public Display Tab
      tabItem(
        tabName = "display",
        displayUI("display")
      ),

      # Instructor Control Tab
      tabItem(
        tabName = "instructor",
        instructorUI("instructor")
      )
    )
  )
)

# Pop-out negotiation board page (minimal, full-screen for projection)
negotiation_popup_page <- function() {
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$style(HTML(NEGOTIATION_POPUP_CSS))
    ),
    div(class = "neg-popup-title", "Negotiation Board"),
    uiOutput("popup_round_display"),
    uiOutput("negotiation_grid_popup")
  )
}

# Dynamic UI: serve pop-out page or main dashboard based on URL query
ui <- function(req) {
  query <- parseQueryString(req$QUERY_STRING)
  if (identical(query$view, "negotiation")) {
    negotiation_popup_page()
  } else {
    instructor_dashboard_ui
  }
}

# Define Server
server <- function(input, output, session) {
  query <- parseQueryString(isolate(session$clientData$url_search))

  # Pop-out negotiation board server (separate session, reads from DB)
  if (identical(query$view, "negotiation")) {
    game_state <- reactiveVal(NULL)
    autorefresh <- reactiveTimer(3000)

    observe({
      autorefresh()
      settings <- get_game_settings(1)
      if (!is.null(settings)) game_state(settings)
    })

    output$popup_round_display <- renderUI({
      settings <- game_state()
      if (is.null(settings)) return(div(class = "neg-popup-round", "Waiting for game..."))
      div(class = "neg-popup-round", paste("Round", settings$current_round, "of", settings$n_rounds))
    })

    output$negotiation_grid_popup <- renderUI({
      autorefresh()
      settings <- game_state()
      if (is.null(settings)) return(div("Waiting for game to start...", class = "negotiation-waiting"))
      render_negotiation_grid_html(1, settings$current_round)
    })

    return(invisible(NULL))
  }

  # Normal dashboard server
  game_state <- reactiveVal(NULL)
  auto_refresh <- reactiveTimer(5000)

  observe({
    auto_refresh()
    settings <- get_game_settings(game_id = 1)
    if (!is.null(settings)) {
      game_state(settings)
    }
  })

  output$sidebar_game_status <- renderUI({
    settings <- game_state()

    if (is.null(settings)) {
      return(div(
        p("No active game", style = "color: #e74c3c;"),
        p("Go to Instructor Control to start a new game", style = "color: white; font-size: 12px;")
      ))
    }

    div(
      p(paste("Round:", settings$current_round, "of", settings$n_rounds),
        style = "color: #3498db; font-weight: bold; font-size: 16px;"),
      p(paste("Hospitals:", settings$n_hospitals), style = "color: white;"),
      p(paste("Insurers:", settings$n_insurers), style = "color: white;"),
      p(paste("Status:", tools::toTitleCase(settings$game_status)),
        style = "color: #2ecc71; font-weight: bold;")
    )
  })

  instructorServer("instructor", game_state)
  displayServer("display", game_state)
}

# Run the application
shinyApp(ui = ui, server = server)
