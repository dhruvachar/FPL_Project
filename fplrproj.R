library(shiny)
library(shinydashboard)
library(bslib)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(reactable)
library(plotly)
library(ggplot2)
library(dplyr)
library(httr)
library(jsonlite)
library(tidyr)
library(shinyjs)
library(shinyanimate)

# Fetch FPL data with error handling
fetch_fpl_data <- function() {
  response <- tryCatch(
    GET("https://fantasy.premierleague.com/api/bootstrap-static/"),
    error = function(e) {
      stop("Failed to fetch data from FPL API: ", e$message)
    }
  )
  if (http_error(response)) {
    stop("HTTP error fetching FPL API data: ", status_code(response))
  }
  content(response, as = "parsed", simplifyDataFrame = TRUE)
}

data <- fetch_fpl_data()
players <- data$elements
teams <- data$teams
positions <- data$element_types

# Data processing
players <- players %>%
  mutate(
    TEAMS = teams$name[team],
    position = positions$singular_name[element_type],
    cost_million = now_cost / 10,
    cost_pretty = paste0("£", format(round(cost_million, 1), nsmall = 1)),
    value = ifelse(cost_million > 0, round(total_points / cost_million, 2), NA),
    full_name = paste(first_name, second_name)
  )
players_filtered <- players %>% filter(minutes > 500)

# Debug data
print("Number of rows in players_filtered:")
print(nrow(players_filtered))
print("Unique positions:")
print(unique(players_filtered$position))

# Updated professional color scheme
get_plot_colors <- function(dark_mode) {
  if (isTRUE(dark_mode)) {
    list(
      text = "#E0E6F0",
      axis = "#A0B1C6",
      bg = "#1A2533",
      font = "Orbitron",
      accent = "#00D4FF",
      secondary = "#FF6B6B"
    )
  } else {
    list(
      text = "#1A2533",
      axis = "#4A5568",
      bg = "#F7FAFC",
      font = "Orbitron",
      accent = "#0077B6",
      secondary = "#F4A261"
    )
  }
}

# UI Definition
fpl_ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = NULL
  ),
  dashboardSidebar(
    div(
      style = "
        padding: 20px 12px;
        text-align: center;
        background: linear-gradient(180deg, #1A2533, #2D3A4F);
        border-bottom: 2px solid #00D4FF;
        margin-bottom: 20px;
        box-shadow: 0 0 10px rgba(0, 212, 255, 0.3);",
      div(
        style = "margin: 20px 0;",
        tags$label(
          class = "switch-pro",
          tags$input(id = "dark_mode_toggle", type = "checkbox"),
          tags$span(class = "slider-pro"),
          tags$span(class = "toggle-label")
        )
      )
    ),
    sidebarMenu(
      menuItem("Player Matrix", tabName = "playerExplorer", icon = icon("grid", lib = "font-awesome")),
      menuItem("Elite Squad", tabName = "bestXI", icon = icon("users-gear", lib = "font-awesome")),
      menuItem("Versus Scanner", tabName = "playerComparison", icon = icon("scale-balanced")),
      menuItem("Position Nexus", tabName = "positionDistribution", icon = icon("chart-simple")),
      menuItem("Team Dominance", tabName = "topTeams", icon = icon("trophy")),
      menuItem("Squad Forge", tabName = "teamSelector", icon = icon("gears")),
      menuItem("Stats Viewer", tabName = "statsExplorer", icon = icon("chart-radar"))
    ),
    pickerInput("position", "Position:", choices = c("All", unique(players_filtered$position)), selected = "All", multiple = FALSE,
                options = list(`style` = "btn-pro")),
    pickerInput("team", "Team:", choices = c("All", sort(unique(players_filtered$TEAMS))), selected = "All", multiple = FALSE,
                options = list(`style` = "btn-pro")),
    sliderInput("cost", "Max Cost (£M):", min = 4.0, max = 13.0, value = 13.0, step = 0.1,
                animate = TRUE, pre = "£"),
    sliderInput("minutes", "Min Minutes Played:", min = 0, max = max(players_filtered$minutes, na.rm = TRUE), value = 500, step = 100,
                animate = TRUE)
  ),
  dashboardBody(
    useShinyjs(),
    withAnim(),
    tags$head(
      tags$link(href = "https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700&display=swap", rel = "stylesheet"),
      tags$style(HTML('
        /* --- Futuristic Header --- */
        .main-header, .main-header .navbar {
          background: linear-gradient(90deg, #1A2533 0%, #00D4FF 100%) !important;
          border-bottom: 3px solid #00D4FF !important;
          box-shadow: 0 0 12px rgba(0, 212, 255, 0.5) !important;
          min-height: 70px !important;
          height: 70px !important;
          width: 100vw !important;
          margin-left: 0 !important;
          padding: 0 !important;
          position: fixed !important;
          top: 0;
          left: 0;
          z-index: 1002;
          display: flex !important;
          align-items: center !important;
          justify-content: center !important;
          animation: neonGlow 3s ease-in-out infinite alternate;
        }
        .fpl-header-bar {
          width: 100vw;
          display: flex;
          align-items: center;
          justify-content: center;
          padding: 0 20px;
          height: 70px;
          position: relative;
          z-index: 1003;
        }
        .fpl-header-title {
          display: flex;
          align-items: center;
          justify-content: center;
          flex: 1;
        }
        .fpl-glow-text {
          font-family: "Orbitron", sans-serif;
          font-size: 2rem;
          font-weight: 700;
          letter-spacing: 2px;
          color: #E0E6F0;
          text-shadow: 0 0 8px rgba(0, 212, 255, 0.7), 0 0 12px rgba(0, 212, 255, 0.5);
          animation: flicker 2s infinite;
        }
        .fpl-glow-main { color: #E0E6F0; }
        .fpl-header-link-inner {
          font-family: "Orbitron", sans-serif;
          font-size: 1rem;
          font-weight: 600;
          color: #E0E6F0 !important;
          background: rgba(26, 37, 51, 0.9);
          border: 2px solid #00D4FF;
          border-radius: 10px;
          padding: 8px 14px;
          box-shadow: 0 0 8px rgba(0, 212, 255, 0.4);
          transition: all 0.3s ease;
          text-decoration: none !important;
        }
        .fpl-header-link-inner:hover {
          background: #00D4FF;
          color: #1A2533 !important;
          box-shadow: 0 0 12px rgba(0, 212, 255, 0.6);
          transform: scale(1.05);
        }
        .fpl-logo {
          height: 80px;
          margin-right: 15px;
          transition: all 0.3s ease;
          animation: pulseLogo 2s ease-in-out infinite;
        }
        .fpl-logo:hover {
          transform: scale(1.1);
          filter: drop-shadow(0 0 8px rgba(0, 212, 255, 0.5));
        }

        /* --- Professional Slider --- */
        .irs--shiny .irs-bar {
          background: #00D4FF !important;
          border: none !important;
        }
        .irs--shiny .irs-line {
          background: #A0B1C6 !important;
          border-radius: 4px !important;
          height: 6px !important;
        }
        .irs--shiny .irs-handle {
          background: #00D4FF !important;
          border: 2px solid #E0E6F0 !important;
          box-shadow: 0 0 8px rgba(0, 212, 255, 0.4) !important;
          width: 16px !important;
          height: 16px !important;
          top: 21px !important;
          cursor: pointer !important;
        }
        .irs--shiny .irs-handle:hover {
          background: #0077B6 !important;
          box-shadow: 0 0 10px rgba(0, 212, 255, 0.6) !important;
        }
        .irs--shiny .irs-single {
          background: #00D4FF !important;
          color: #E0E6F0 !important;
          font-family: "Orbitron", sans-serif !important;
          font-size: 12px !important;
          border-radius: 4px !important;
          padding: 2px 6px !important;
        }
        .irs--shiny .irs-grid-text {
          color: #A0B1C6 !important;
          font-family: "Orbitron", sans-serif !important;
          font-size: 10px !important;
        }

        /* --- Professional Toggle Switch --- */
        .switch-pro {
          position: relative;
          display: inline-block;
          width: 50px;
          height: 24px;
        }
        .switch-pro input {
          opacity: 0;
          width: 0;
          height: 0;
        }
        .slider-pro {
          position: absolute;
          cursor: pointer;
          top: 0; left: 0; right: 0; bottom: 0;
          background: #A0B1C6;
          border-radius: 12px;
          transition: .3s;
          border: 2px solid #00D4FF;
        }
        .slider-pro:before {
          position: absolute;
          content: "";
          height: 18px;
          width: 18px;
          left: 3px;
          bottom: 3px;
          background: #E0E6F0;
          border-radius: 50%;
          transition: .3s;
          box-shadow: 0 0 6px rgba(0, 212, 255, 0.4);
        }
        input:checked + .slider-pro {
          background: #00D4FF;
        }
        input:checked + .slider-pro:before {
          transform: translateX(26px);
          background: #1A2533;
        }
        .toggle-label {
          margin-left: 8px;
          font-size: 1em;
          font-weight: 600;
          color: #E0E6F0;
          font-family: "Orbitron", sans-serif;
        }

        /* --- Theme Styles --- */
        :root {
          --pro-primary: #00D4FF;
          --pro-secondary: #FF6B6B;
          --pro-bg-dark: #1A2533;
          --pro-bg-light: #F7FAFC;
        }
        [data-bs-theme="dark"] {
          body, .content-wrapper {
            background: #1A2533;
            color: #E0E6F0;
          }
          .box {
            background: rgba(26, 37, 51, 0.95) !important;
            border: 2px solid #00D4FF !important;
            box-shadow: 0 0 10px rgba(0, 212, 255, 0.3);
            color: #E0E6F0 !important;
            transition: box-shadow 0.3s;
          }
          .box:hover {
            box-shadow: 0 0 14px rgba(0, 212, 255, 0.5);
          }
          .box-title { color: #E0E6F0 !important; }
          .sidebar {
            background: linear-gradient(180deg, #1A2533, #2D3A4F) !important;
            border-right: 2px solid #00D4FF !important;
            box-shadow: 0 0 10px rgba(0, 212, 255, 0.3);
          }
          .btn-pro, .bootstrap-select .dropdown-toggle, .selectize-input, .form-control, .picker {
            color: #E0E6F0 !important;
            background: rgba(26, 37, 51, 0.95) !important;
            border: 2px solid #00D4FF !important;
            border-radius: 8px !important;
            transition: all 0.3s;
            font-family: "Orbitron", sans-serif !important;
          }
          .btn-pro:hover, .bootstrap-select .dropdown-toggle:hover, .selectize-input:hover {
            background: #00D4FF !important;
            color: #1A2533 !important;
            box-shadow: 0 0 10px rgba(0, 212, 255, 0.5);
            transform: scale(1.02);
          }
          .reactable, .dataTable, table, th, td {
            background: rgba(26, 37, 51, 0.95) !important;
            color: #E0E6F0 !important;
            border-color: #00D4FF !important;
          }
          .plot-container {
            background: rgba(26, 37, 51, 0.95) !important;
            border: 2px solid #00D4FF !important;
            box-shadow: 0 0 10px rgba(0, 212, 255, 0.3);
            position: relative;
            overflow: hidden;
          }
          .plot-container::before {
            content: "";
            position: absolute;
            top: 50%;
            left: 50%;
            width: 350px;
            height: 350px;
            background-image: url("https://upload.wikimedia.org/wikipedia/en/f/f2/Premier_League_Logo.svg");
            background-size: contain;
            background-repeat: no-repeat;
            opacity: 0.15;
            pointer-events: none;
          }
          .plotly, .plot-container * {
            z-index: 1;
            position: relative;
          }
        }
        [data-bs-theme="light"] {
          body, .content-wrapper {
            background: #F7FAFC;
            color: #1A2533;
          }
          .box {
            background: #FFFFFF !important;
            border: 2px solid #0077B6 !important;
            box-shadow: 0 0 10px rgba(0, 119, 182, 0.2);
            color: #1A2533 !important;
            transition: box-shadow 0.3s;
          }
          .box:hover {
            box-shadow: 0 0 14px rgba(0, 119, 182, 0.3);
          }
          .box-title { color: #1A2533 !important; }
          .sidebar {
            background: linear-gradient(180deg, #F7FAFC, #EDF2F7) !important;
            border-right: 2px solid #0077B6 !important;
            box-shadow: 0 0 10px rgba(0, 119, 182, 0.2);
          }
          .btn-pro, .bootstrap-select .dropdown-toggle, .selectize-input, .form-control, .picker {
            color: #1A2533 !important;
            background: #FFFFFF !important;
            border: 2px solid #0077B6 !important;
            border-radius: 8px !important;
            transition: all 0.3s;
            font-family: "Orbitron", sans-serif !important;
          }
          .btn-pro:hover, .bootstrap-select .dropdown-toggle:hover, .selectize-input:hover {
            background: #0077B6 !important;
            color: #F7FAFC !important;
            box-shadow: 0 0 10px rgba(0, 119, 182, 0.4);
            transform: scale(1.02);
          }
          .reactable, .dataTable, table, th, td {
            background: #FFFFFF !important;
            color: #1A2533 !important;
            border-color: #0077B6 !important;
          }
          .plot-container {
            background: #FFFFFF !important;
            border: 2px solid #0077B6 !important;
            box-shadow: 0 0 10px rgba(0, 119, 182, 0.2);
            position: relative;
            overflow: hidden;
          }
          .plot-container::before {
            content: "";
            position: absolute;
            top: 50%;
            left: 50%;
            width: 400px;
            height: 400px;
            background-image: url("https://upload.wikimedia.org/wikipedia/en/f/f2/Premier_League_Logo.svg");
            background-size: contain;
            background-repeat: no-repeat;
            opacity: 0.2;
            pointer-events: none;
          }
          .plotly, .plot-container * {
            z-index: 1;
            position: relative;
          }
        }
        .box { border-radius: 10px; margin-bottom: 25px; position: relative; z-index: 1; }
        .plot-container {
          border-radius: 10px;
          padding: 10px;
          animation: fadeIn 0.05s ease-in;
        }
        .box-title {
          font-weight: 700 !important;
          font-size: 22px !important;
          font-family: "Orbitron", sans-serif !important;
          letter-spacing: 1px;
        }
        .sidebar a, .skin-blue .sidebar-menu>li.header {
          font-family: "Orbitron", sans-serif !important;
          font-size: 15px;
          transition: all 0.3s;
        }
        .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
          background: #00D4FF !important;
          color: #1A2533 !important;
          box-shadow: 0 0 8px rgba(0, 212, 255, 0.5);
          transform: translateX(4px);
        }
        .bootstrap-select .dropdown-menu, .selectize-dropdown-content {
          font-family: "Orbitron", sans-serif !important;
          font-size: 14px !important;
          font-weight: 500 !important;
          background: rgba(26, 37, 51, 0.95) !important;
          border: 2px solid #00D4FF !important;
        }

        /* Animations */
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(10px); }
          to { opacity: 1; transform: translateY(0); }
        }
        @keyframes neonGlow {
          from { box-shadow: 0 0 12px rgba(0, 212, 255, 0.5); }
          to { box-shadow: 0 0 20px rgba(0, 212, 255, 0.7); }
        }
        @keyframes flicker {
          0%, 100% { text-shadow: 0 0 8px rgba(0, 212, 255, 0.7), 0 0 12px rgba(0, 212, 255, 0.5); }
          50% { text-shadow: 0 0 12px rgba(0, 212, 255, 0.9), 0 0 16px rgba(0, 212, 255, 0.7); }
        }
        @keyframes pulseLogo {
          0%, 100% { transform: scale(1); }
          50% { transform: scale(1.05); }
        }
      ')),
      tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function() {
          $(document).on('shiny:connected', function() {
            document.documentElement.setAttribute('data-bs-theme', 'light');
            if ($('#custom-fpl-header').length === 0) {
              $('.main-header .navbar').html(
                `<div id='custom-fpl-header' class='fpl-header-bar'>
                  <div class='fpl-header-title'>
                    <img src='https://upload.wikimedia.org/wikipedia/en/f/f2/Premier_League_Logo.svg' 
                         onerror='this.src=\"https://upload.wikimedia.org/wikipedia/en/7/7a/Premier_League_logo_%282015%29.svg\"' 
                         class='fpl-logo' alt='Premier League Logo'>
                    <span class='fpl-glow-text'>FPL Analytics</span>
                  </div>
                  <div class='fpl-header-link'>
                    <a href='https://www.premierleague.com/' target='_blank' class='fpl-header-link-inner'>
                      Premier League
                    </a>
                  </div>
                </div>`
              );
            }
          });
          $(document).on('change', '#dark_mode_toggle', function() {
            if (this.checked) {
              document.documentElement.setAttribute('data-bs-theme', 'dark');
              Shiny.setInputValue('dark_mode_toggle', true, {priority: 'event'});
            } else {
              document.documentElement.setAttribute('data-bs-theme', 'light');
              Shiny.setInputValue('dark_mode_toggle', false, {priority: 'event'});
            }
          });
        });
      "))
    ),
    tabItems(
      tabItem(
        "playerExplorer",
        fluidRow(
          div(id = "playerExplorerPlotContainer", class = "plot-container",
              withSpinner(plotlyOutput("interactivePlot", height = "550px"), type = 8, color = "#00D4FF")
          ),
          box(
            title = span("Top 10 Value Players", class = "box-title"),
            div(id = "playerExplorerTableContainer",
                withSpinner(reactableOutput("topPlayersReactable"), type = 8, color = "#00D4FF")
            ),
            width = 12
          )
        )
      ),
      tabItem(
        "bestXI",
        fluidRow(box(
          title = span("Elite Squad (2 GK, 5 DEF, 5 MID, 3 FWD)", class = "box-title"),
          withSpinner(DTOutput("bestXI"), type = 8, color = "#00D4FF"),
          width = 12
        ))
      ),
      tabItem(
        "playerComparison",
        fluidRow(box(
          title = span("Player Comparison", class = "box-title"),
          pickerInput("comparePlayer1", "Player 1", choices = players_filtered$full_name, options = list(`live-search` = TRUE, `style` = "btn-pro")),
          pickerInput("comparePlayer2", "Player 2", choices = players_filtered$full_name, options = list(`live-search` = TRUE, `style` = "btn-pro")),
          div(class = "plot-container",
              withSpinner(plotlyOutput("comparisonPlot", height = "500px"), type = 8, color = "#00D4FF")
          ),
          width = 12
        ))
      ),
      tabItem(
        "positionDistribution",
        fluidRow(box(
          title = span("Position Distribution", class = "box-title"),
          div(class = "plot-container",
              withSpinner(plotlyOutput("positionBarChart", height = "500px"), type = 8, color = "#00D4FF")
          ),
          width = 12
        ))
      ),
      tabItem(
        "topTeams",
        fluidRow(box(
          title = span("Top Teams", class = "box-title"),
          div(class = "plot-container",
              withSpinner(plotlyOutput("topTeamsPlot", height = "500px"), type = 8, color = "#00D4FF")
          ),
          width = 12
        ))
      ),
      tabItem(
        "teamSelector",
        fluidRow(box(
          title = span("Team Selector", class = "box-title"),
          width = 12,
          pickerInput("selectTeamForSquad", "Select Team:", choices = sort(unique(players_filtered$TEAMS)), options = list(`style` = "btn-pro")),
          withSpinner(reactableOutput("teamSquadTable"), type = 8, color = "#00D4FF")
        ))
      ),
      tabItem(
        "statsExplorer",
        fluidRow(box(
          title = span("Player Stats", class = "box-title"),
          width = 12,
          pickerInput("radarPlayer", "Select Player:", choices = players_filtered$full_name, options = list(`live-search` = TRUE, `style` = "btn-pro")),
          div(class = "plot-container",
              withSpinner(plotlyOutput("radarPlot", height = "500px"), type = 8, color = "#00D4FF")
          )
        ))
      )
    )
  )
)

# Server Logic
fpl_server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  filtered <- reactive({
    data <- players_filtered
    if (!is.null(input$position) && input$position != "All" && input$position %in% unique(players_filtered$position)) {
      data <- data %>% filter(position == input$position)
    }
    data <- data %>%
      filter(
        cost_million <= input$cost,
        minutes >= input$minutes
      )
    if (!is.null(input$team) && input$team != "All" && input$team %in% unique(players_filtered$TEAMS)) {
      data <- data %>% filter(TEAMS == input$team)
    }
    data
  })
  
  best_15 <- reactive({
    gk <- players_filtered %>% filter(position == "Goalkeeper") %>% arrange(desc(total_points)) %>% head(2)
    def <- players_filtered %>% filter(position == "Defender") %>% arrange(desc(total_points)) %>% head(5)
    mid <- players_filtered %>% filter(position == "Midfielder") %>% arrange(desc(total_points)) %>% head(5)
    fwd <- players_filtered %>% filter(position == "Forward") %>% arrange(desc(total_points)) %>% head(3)
    bind_rows(gk, def, mid, fwd) %>%
      arrange(factor(position, levels = c("Goalkeeper", "Defender", "Midfielder", "Forward")), desc(total_points))
  })
  
  output$bestXI <- renderDT({
    dat <- best_15() %>%
      select(
        Player = full_name,
        Team = TEAMS,
        Position = position,
        `Total Points` = total_points,
        `Goals` = goals_scored,
        `Assists` = assists,
        `Clean Sheets` = clean_sheets,
        `Cost (£M)` = cost_million,
        Value = value
      )
    datatable(dat, rownames = FALSE, options = list(
      pageLength = 15,
      dom = 'tip',
      columnDefs = list(list(className = 'dt-center', targets = '_all'))
    )) %>% formatStyle(
      columns = colnames(dat),
      backgroundColor = styleInterval(c(0), c('rgba(26, 37, 51, 0.95)', 'rgba(26, 37, 51, 0.95)')),
      color = '#E0E6F0',
      fontFamily = '"Orbitron", sans-serif'
    )
  })
  
  get_plot_colors <- reactive({
    if (isTRUE(input$dark_mode_toggle)) {
      list(
        text = "#E0E6F0",
        axis = "#A0B1C6",
        bg = "#1A2533",
        font = "Orbitron",
        accent = "#00D4FF",
        secondary = "#FF6B6B"
      )
    } else {
      list(
        text = "#1A2533",
        axis = "#4A5568",
        bg = "#F7FAFC",
        font = "Orbitron",
        accent = "#0077B6",
        secondary = "#F4A261"
      )
    }
  })
  
  output$interactivePlot <- renderPlotly({
    colors <- get_plot_colors()
    p <- ggplot(
      filtered(),
      aes(
        x = cost_million,
        y = total_points,
        color = TEAMS,
        text = paste(full_name, "<br>Team:", TEAMS, "<br>Cost: £", cost_million, "<br>Points:", total_points)
      )
    ) +
      geom_point(size = 4, alpha = 0.9, aes(shape = position)) +
      labs(x = "Cost (£M)", y = "Total Points", title = "Points vs Cost") +
      theme_minimal(base_size = 16, base_family = colors$font) +
      theme(
        plot.title = element_text(color = colors$text, face = "bold", size = 20, hjust = 0.5),
        axis.title = element_text(color = colors$text, size = 14),
        axis.text = element_text(color = colors$axis, size = 12),
        legend.title = element_text(color = colors$text, size = 12),
        legend.text = element_text(color = colors$axis, size = 10),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_line(color = paste0(colors$axis, "33"), size = 0.3),
        panel.grid.minor = element_blank()
      )
    ggplotly(p, tooltip = "text") %>%
      layout(
        dragmode = "pan",
        font = list(family = colors$font, color = colors$text),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent",
        hoverlabel = list(
          bgcolor = colors$bg,
          font = list(color = colors$text, family = colors$font),
          bordercolor = colors$accent
        )
      ) %>%
      animation_opts(500, easing = "cubic-in-out")
  })
  
  output$topPlayersReactable <- renderReactable({
    colors <- get_plot_colors()
    data <- filtered() %>%
      arrange(desc(value)) %>%
      select(full_name, TEAMS, Position = position, total_points, `Cost(millions)` = cost_pretty, value) %>%
      head(10)
    
    if (nrow(data) == 0) {
      return(reactable(
        data.frame(Message = "No players match the selected criteria"),
        theme = reactableTheme(
          color = colors$text,
          backgroundColor = "transparent",
          borderColor = colors$accent,
          headerStyle = list(
            backgroundColor = colors$bg,
            color = colors$text,
            fontFamily = colors$font,
            borderBottom = paste0("1px solid ", colors$accent)
          ),
          cellStyle = list(
            fontFamily = colors$font,
            transition = "all 0.3s"
          )
        )
      ))
    }
    
    reactable(
      data,
      columns = list(
        full_name = colDef(name = "Player", minWidth = 150),
        TEAMS = colDef(name = "Team", minWidth = 120),
        Position = colDef(name = "Position", minWidth = 100),
        total_points = colDef(name = "Total Points", minWidth = 100),
        `Cost(millions)` = colDef(name = "Cost (£M)", minWidth = 100),
        value = colDef(name = "Value", minWidth = 100, format = colFormat(digits = 2))
      ),
      bordered = TRUE,
      highlight = TRUE,
      striped = FALSE,
      theme = reactableTheme(
        color = colors$text,
        backgroundColor = "transparent",
        borderColor = colors$accent,
        highlightColor = paste0(colors$accent, "33"),
        headerStyle = list(
          backgroundColor = colors$bg,
          color = colors$text,
          fontFamily = colors$font,
          borderBottom = paste0("1px solid ", colors$accent)
        ),
        cellStyle = list(
          fontFamily = colors$font,
          transition = "all 0.3s"
        )
      )
    )
  })
  
  output$radarPlot <- renderPlotly({
    colors <- get_plot_colors()
    req(input$radarPlayer)
    player_stats <- players_filtered %>% filter(full_name == input$radarPlayer)
    df <- data.frame(
      metric = c("Goals", "Assists", "Clean Sheets", "Minutes", "Total Points"),
      value = c(
        player_stats$goals_scored,
        player_stats$assists,
        player_stats$clean_sheets,
        player_stats$minutes / max(players_filtered$minutes, na.rm = TRUE) * 100,
        player_stats$total_points
      )
    )
    plot_ly(
      type = 'scatterpolar',
      r = df$value,
      theta = df$metric,
      fill = 'toself',
      marker = list(color = colors$secondary),
      fillcolor = paste0(colors$secondary, "33"),
      line = list(color = colors$accent, width = 2)
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            color = colors$text,
            gridcolor = paste0(colors$axis, "66"),
            range = c(0, max(df$value, na.rm = TRUE) * 1.1)
          ),
          angularaxis = list(color = colors$text)
        ),
        showlegend = FALSE,
        font = list(family = colors$font, color = colors$text),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent"
      ) %>%
      animation_opts(500, easing = "cubic-in-out")
  })
  
  output$comparisonPlot <- renderPlotly({
    req(input$comparePlayer1, input$comparePlayer2)
    colors <- get_plot_colors()
    stats <- c("goals_scored", "assists", "clean_sheets", "total_points", "minutes")
    player1 <- players_filtered %>% filter(full_name == input$comparePlayer1)
    player2 <- players_filtered %>% filter(full_name == input$comparePlayer2)
    df <- data.frame(
      Stat = c("Goals", "Assists", "Clean Sheets", "Total Points", "Minutes"),
      Player1 = as.numeric(player1[stats]),
      Player2 = as.numeric(player2[stats])
    )
    plot_ly(df, x = ~Stat) %>%
      add_bars(y = ~Player1, name = input$comparePlayer1, marker = list(color = colors$accent)) %>%
      add_bars(y = ~Player2, name = input$comparePlayer2, marker = list(color = colors$secondary)) %>%
      layout(
        barmode = 'group',
        yaxis = list(title = '', color = colors$text),
        xaxis = list(title = '', color = colors$text),
        font = list(family = colors$font, color = colors$text),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent",
        hoverlabel = list(
          bgcolor = colors$bg,
          font = list(color = colors$text, family = colors$font),
          bordercolor = colors$accent
        )
      ) %>%
      animation_opts(500, easing = "cubic-in-out")
  })
  
  output$positionBarChart <- renderPlotly({
    colors <- get_plot_colors()
    df <- players_filtered %>%
      group_by(position) %>%
      summarise(Count = n())
    plot_ly(df, x = ~position, y = ~Count, type = 'bar', marker = list(color = colors$accent)) %>%
      layout(
        xaxis = list(title = "Position", color = colors$text),
        yaxis = list(title = "Number of Players", color = colors$text),
        title = list(text = "Position Distribution", font = list(size = 20, color = colors$text, family = colors$font)),
        font = list(family = colors$font, color = colors$text),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent"
      ) %>%
      animation_opts(500, easing = "cubic-in-out")
  })
  
  output$topTeamsPlot <- renderPlotly({
    colors <- get_plot_colors()
    df <- players_filtered %>%
      group_by(TEAMS) %>%
      summarise(TotalPoints = sum(total_points)) %>%
      arrange(desc(TotalPoints)) %>%
      head(10)
    plot_ly(df, x = ~reorder(TEAMS, TotalPoints), y = ~TotalPoints, type = 'bar', marker = Ulist(color = colors$secondary)) %>%
      layout(
        xaxis = list(title = "Team", color = colors$text),
        yaxis = list(title = "Total Points", color = colors$text),
        title = list(text = "Top Teams", font = list(size = 20, color = colors$text, family = colors$font)),
        font = list(family = colors$font, color = colors$text),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent"
      ) %>%
      animation_opts(500, easing = "cubic-in-out")
  })
  
  output$teamSquadTable <- renderReactable({
    colors <- get_plot_colors()
    req(input$selectTeamForSquad)
    data <- players_filtered %>%
      filter(TEAMS == input$selectTeamForSquad) %>%
      select(Player = full_name, Position = position, `Total Points` = total_points, Cost = cost_million) %>%
      arrange(Position, desc(`Total Points`))
    
    if (nrow(data) == 0) {
      return(reactable(
        data.frame(Message = "No players match the selected team"),
        theme = reactableTheme(
          color = colors$text,
          backgroundColor = "transparent",
          borderColor = colors$accent,
          headerStyle = list(
            backgroundColor = colors$bg,
            color = colors$text,
            fontFamily = colors$font,
            borderBottom = paste0("1px solid ", colors$accent)
          ),
          cellStyle = list(
            fontFamily = colors$font,
            transition = "all 0.3s"
          )
        )
      ))
    }
    
    reactable(
      data,
      columns = list(
        Player = colDef(minWidth = 150),
        Position = colDef(minWidth = 100),
        `Total Points` = colDef(minWidth = 100),
        Cost = colDef(minWidth = 100, format = colFormat(prefix = "£", digits = 1))
      ),
      bordered = TRUE,
      highlight = TRUE,
      striped = FALSE,
      theme = reactableTheme(
        color = colors$text,
        backgroundColor = "transparent",
        borderColor = colors$accent,
        highlightColor = paste0(colors$accent, "33"),
        headerStyle = list(
          backgroundColor = colors$bg,
          color = colors$text,
          fontFamily = colors$font,
          borderBottom = paste0("1px solid ", colors$accent)
        ),
        cellStyle = list(
          fontFamily = colors$font,
          transition = "all 0.3s"
        )
      )
    )
  })
}

shinyApp(ui = fpl_ui, server = fpl_server)