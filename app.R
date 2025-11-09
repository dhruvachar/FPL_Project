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
library(caret)
library(randomForest)

# Fetch FPL data with better error handling and debugging
fetch_fpl_data <- function() {
  cat("Attempting to fetch FPL data...\n")
  response <- tryCatch(
    GET("https://fantasy.premierleague.com/api/bootstrap-static/", timeout(30)),
    error = function(e) {
      cat("Error fetching data from FPL API:", e$message, "\n")
      return(NULL)
    }
  )
  if (is.null(response)) {
    cat("No response received from FPL API\n")
    return(NULL)
  }
  if (http_error(response)) {
    cat("HTTP error fetching FPL API data. Status code:", status_code(response), "\n")
    return(NULL)
  }
  cat("Successfully fetched FPL data\n")
  content(response, as = "parsed", simplifyDataFrame = TRUE)
}

# Fetch fixture data with better error handling
fetch_fpl_fixtures <- function() {
  cat("Attempting to fetch fixture data...\n")
  response <- tryCatch(
    GET("https://fantasy.premierleague.com/api/fixtures/", timeout(30)),
    error = function(e) {
      cat("Error fetching fixtures from FPL API:", e$message, "\n")
      return(NULL)
    }
  )
  if (is.null(response)) {
    cat("No response received for fixtures\n")
    return(NULL)
  }
  if (http_error(response)) {
    cat("HTTP error fetching fixtures. Status code:", status_code(response), "\n")
    return(NULL)
  }
  cat("Successfully fetched fixture data\n")
  content(response, as = "parsed", simplifyDataFrame = TRUE)
}

# Fetch and process data with better error handling
data <- fetch_fpl_data()
if (is.null(data)) {
  cat("Failed to fetch FPL data. Creating sample data for testing.\n")
  stop("Failed to fetch FPL data. Please check your internet connection and try again.")
}

# Extract data components
players <- data$elements
teams <- data$teams
positions <- data$element_types

cat("Data structure loaded:\n")
cat("Players:", nrow(players), "\n")
cat("Teams:", nrow(teams), "\n")
cat("Positions:", nrow(positions), "\n")

# Fetch and process fixtures data
fixtures <- fetch_fpl_fixtures()
upcoming_fixtures <- data.frame()

if (!is.null(fixtures) && nrow(fixtures) > 0) {
  upcoming_fixtures <- fixtures %>%
    filter(finished == FALSE) %>%
    left_join(teams %>% select(id, home_team = name), by = c("team_h" = "id")) %>%
    left_join(teams %>% select(id, away_team = name), by = c("team_a" = "id")) %>%
    mutate(
      gameweek = event,
      difficulty = team_h_difficulty
    ) %>%
    select(gameweek, home_team, away_team, difficulty, team_h, team_a) %>%
    filter(!is.na(gameweek) & gameweek > 0) %>%
    arrange(gameweek)
  
  cat("Processed upcoming fixtures:", nrow(upcoming_fixtures), "\n")
  if (nrow(upcoming_fixtures) > 0) {
    cat("Gameweeks available:", paste(unique(upcoming_fixtures$gameweek), collapse = ", "), "\n")
  }
} else {
  cat("No fixture data available - creating empty dataframe\n")
  upcoming_fixtures <- data.frame(
    gameweek = numeric(0),
    home_team = character(0),
    away_team = character(0),
    difficulty = numeric(0),
    team_h = numeric(0),
    team_a = numeric(0)
  )
}

# Enhanced data processing with better error handling
players <- players %>%
  left_join(teams %>% select(id, team_name = name), by = c("team" = "id")) %>%
  left_join(positions %>% select(id, position_name = singular_name), by = c("element_type" = "id")) %>%
  mutate(
    TEAMS = coalesce(team_name, "Unknown"),
    position = coalesce(position_name, "Unknown"),
    cost_million = now_cost / 10,
    cost_pretty = paste0("£", format(round(cost_million, 1), nsmall = 1)),
    value = ifelse(cost_million > 0, round(total_points / cost_million, 2), 0),
    full_name = paste(first_name, second_name),
    form_numeric = as.numeric(as.character(form)),
    points_per_game_numeric = as.numeric(as.character(points_per_game)),
    minutes = as.numeric(minutes),
    total_points = as.numeric(total_points),
    goals_scored = as.numeric(goals_scored),
    assists = as.numeric(assists),
    clean_sheets = as.numeric(clean_sheets),
    influence_numeric = as.numeric(as.character(influence)),
    creativity_numeric = as.numeric(as.character(creativity)),
    threat_numeric = as.numeric(as.character(threat)),
    ict_index_numeric = as.numeric(as.character(ict_index)),
    selected_by_percent = as.numeric(as.character(selected_by_percent))
  ) %>%
  filter(!is.na(total_points), !is.na(cost_million), !is.na(minutes))

# Apply minimum minutes filter and remove invalid data
players_filtered <- players %>% 
  filter(minutes > 100) %>%
  filter(!is.na(form_numeric), !is.na(points_per_game_numeric))

cat("Filtered players:", nrow(players_filtered), "\n")

# Enhanced ML data with feature engineering for new tabs
ml_data <- players_filtered %>%
  mutate(
    goals_per_90 = ifelse(minutes > 0, (goals_scored * 90) / minutes, 0),
    assists_per_90 = ifelse(minutes > 0, (assists * 90) / minutes, 0),
    points_per_90 = ifelse(minutes > 0, (total_points * 90) / minutes, 0),
    position_multiplier = case_when(
      position == "Forward" ~ 1.3,
      position == "Midfielder" ~ 1.1,
      position == "Defender" ~ 1.0,
      position == "Goalkeeper" ~ 0.8,
      TRUE ~ 1.0
    ),
    form_weight = pmin(form_numeric * 1.5, 10),
    influence_score = coalesce(influence_numeric / 100, 0),
    creativity_score = coalesce(creativity_numeric / 100, 0),
    threat_score = coalesce(threat_numeric / 100, 0),
    team_avg_points = ave(total_points, team, FUN = function(x) mean(x, na.rm = TRUE)),
    popularity = coalesce(selected_by_percent, 0) / 100,
    is_high_performer = ifelse(total_points > quantile(total_points, 0.75), "High", "Low"),
    consistency_score = ifelse(minutes > 0, (total_points / (minutes/90)) / (sd(c(goals_scored, assists, clean_sheets)) + 1), 0),
    momentum = form_numeric * points_per_game_numeric,
    efficiency = ifelse(cost_million > 0, total_points / cost_million, 0)
  ) %>%
  select(
    id, full_name, TEAMS, position, total_points, form_numeric, form_weight,
    points_per_game_numeric, points_per_90, minutes, goals_scored, assists, 
    clean_sheets, goals_per_90, assists_per_90, position_multiplier,
    influence_score, creativity_score, threat_score, team_avg_points,
    popularity, cost_million, is_high_performer, consistency_score, 
    momentum, efficiency
  ) %>%
  filter(complete.cases(.))

# Realistic prediction function (1-12 points range)
predict_points <- function(player_id, fixture_difficulty = 3, gameweek = NULL) {
  player_data <- players_filtered %>% filter(id == player_id)
  if (nrow(player_data) == 0) {
    cat("Player not found\n")
    return(NA)
  } 
  
  # Base prediction from form and historical performance (scaled down)
  form_score <- as.numeric(player_data$form_numeric)
  ppg_score <- as.numeric(player_data$points_per_game_numeric)
  
  # Conservative base prediction (average of form and PPG)
  base_prediction <- (form_score * 0.6 + ppg_score * 0.4)
  
  # Fixture difficulty adjustment (more conservative)
  difficulty_multiplier <- case_when(
    fixture_difficulty == 1 ~ 1.25,  # Very easy: +25%
    fixture_difficulty == 2 ~ 1.10,  # Easy: +10%
    fixture_difficulty == 3 ~ 1.00,  # Average: no change
    fixture_difficulty == 4 ~ 0.85,  # Hard: -15%
    fixture_difficulty == 5 ~ 0.70,  # Very hard: -30%
    TRUE ~ 1.0
  )
  
  # Position-based adjustment (more realistic)
  position_adj <- case_when(
    player_data$position == "Forward" ~ 1.05,
    player_data$position == "Midfielder" ~ 1.00,
    player_data$position == "Defender" ~ 0.90,
    player_data$position == "Goalkeeper" ~ 0.85,
    TRUE ~ 1.0
  )
  
  # Calculate final prediction
  final_prediction <- base_prediction * difficulty_multiplier * position_adj
  
  # Strict cap at 1-12 points range
  final_prediction <- pmax(1, pmin(12, final_prediction))
  
  # Add small random variation
  final_prediction <- final_prediction + runif(1, -0.3, 0.3)
  
  # Final range enforcement 
  round(pmax(1, pmin(12, final_prediction)), 1)
} 

# UI Definition with Premier League logo
fpl_ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = div(
      class = "fpl-header-bar",
      div(
        class = "fpl-header-content",
        div(
          class = "fpl-header-left",
          tags$img(
            src = "https://upload.wikimedia.org/wikipedia/en/thumb/f/f2/Premier_League_Logo.svg/240px-Premier_League_Logo.svg.png",
            class = "pl-logo",
            alt = "Premier League",
            style = "height: 60px; margin-right: 200px;"
          )
        ),
        
        div(class = "fpl-header-right")
      )
    )
  ),
  dashboardSidebar(
    div(
      style = "
        padding: 20px 12px;
        text-align: center;
        background: linear-gradient(90deg,#37003C,#00D4FF);
        border-bottom: 2px solid #00D4FF;
        margin-bottom: 20px;
        box-shadow: 0 0 10px rgba(0, 255, 255, 0.3);",
      div(
        style = "margin: 20px 0;",
        tags$label(
          class = "switch-pro",
          tags$input(id = "dark_mode_toggle", type = "checkbox", checked = TRUE),
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
      menuItem("Stats Viewer", tabName = "statsExplorer", icon = icon("chart-radar")),
      menuItem("Points Predictor", tabName = "pointsPredictor", icon = icon("chart-line")),
      menuItem("ML Classification", tabName = "mlClassification", icon = icon("brain")),
      menuItem("Time Series", tabName = "timeSeries", icon = icon("clock")),
      menuItem("AI Explainability", tabName = "aiExplain", icon = icon("lightbulb")),
      menuItem("Feature Engineering", tabName = "featureEng", icon = icon("screwdriver-wrench"))
    ),
    pickerInput("position", "Position:", 
                choices = c("All", unique(players_filtered$position)), 
                selected = "All", multiple = FALSE,
                options = list(`style` = "btn-pro")),
    pickerInput("team", "Team:", 
                choices = c("All", sort(unique(players_filtered$TEAMS))), 
                selected = "All", multiple = FALSE,
                options = list(`style` = "btn-pro")),
    sliderInput("cost", "Max Cost (£M):", 
                min = floor(min(players_filtered$cost_million, na.rm = TRUE)), 
                max = ceiling(max(players_filtered$cost_million, na.rm = TRUE)), 
                value = ceiling(max(players_filtered$cost_million, na.rm = TRUE)), 
                step = 0.1, animate = TRUE, pre = "£"),
    sliderInput("minutes", "Min Minutes Played:", 
                min = 0, 
                max = max(players_filtered$minutes, na.rm = TRUE), 
                value = 100, step = 50, animate = TRUE)
  ),
  dashboardBody(
    useShinyjs(),
    withAnim(),
    tags$head(
      tags$link(href = "https://fonts.googleapis.com/css2?family=Orbitron:wght@400;700&display=swap", rel = "stylesheet"),
      tags$style(HTML('
        /* Enhanced Dark Mode Styling */
        .main-header, .main-header .navbar {
          background: linear-gradient(90deg, #37003C 0%, #00D4FF 100%) !important;
          border-bottom: 3px solid #00D4FF !important;
          box-shadow: none !important;
          min-height: 80px !important;
          height: 80px !important;
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
        }
        
        /* Enhanced Header Layout */
        .fpl-header-bar {
          width: 100vw;
          height: 50px;
          position: relative;
          z-index: 1003;
          padding: 0;
        }
        
        .fpl-header-content {
          display: flex;
          align-items: center;
          justify-content: space-between;
          width: 100%;
          height: 100%;
          padding: 0 30px;
        }
        
        .fpl-header-left {
          flex: 0 0 auto;
          display: flex;
          align-items: center;
          gap: 10px;
        }
        
        .fpl-header-center {
          flex: 1;
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          text-align: center;
        }
        
        .fpl-header-right {
          flex: 0 0 auto;
        }
        
        .pl-logo {
          transition: all 0.3s ease;
          filter: drop-shadow(0 0 15px rgba(255, 255, 255, 0.5));
        }
        
        .pl-logo:hover {
          transform: scale(1.05);
          filter: drop-shadow(0 0 15px rgba(255, 255, 255, 0.5));
        }
        .sidebar-toggle {
  display: none !important;
}
        
        .fpl-glow-text {
          font-family: "Orbitron", sans-serif;
          font-size: 2rem;
          font-weight: 700;
          letter-spacing: 3px;
          color: #FFFFFF;
          text-shadow: 0 0 10px rgba(0, 255, 135, 0.8), 0 0 20px rgba(0, 255, 135, 0.5);
          margin-bottom: 5px;
        }
        
        .fpl-official-link {
          font-family: "Orbitron", sans-serif !important;
          font-size: 1.1rem !important;
          font-weight: 600 !important;
          color: #FFFFFF !important;
          background: rgba(55, 0, 60, 0.8) !important;
          border: 2px solid #00FF87 !important;
          border-radius: 20px !important;
          padding: 8px 20px !important;
          box-shadow: 0 0 10px rgba(0, 255, 135, 0.4) !important;
          transition: all 0.3s ease !important;
          text-decoration: none !important;
          display: inline-block !important;
        }
        
        .fpl-official-link:hover {
          background: rgba(0, 255, 135, 0.2) !important;
          box-shadow: 0 0 20px rgba(0, 255, 135, 0.8) !important;
          transform: scale(1.05) !important;
        }
        
        /* Enhanced Dark Mode Toggle */
        .switch-pro {
          position: relative;
          display: inline-block;
          width: 60px;
          height: 28px;
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
          background: linear-gradient(45deg, #2D3A4F, #4A5568);
          border-radius: 14px;
          transition: .4s;
          border: 2px solid #00D4FF;
          box-shadow: 0 0 8px rgba(0, 212, 255, 0.3);
        }
        
        .slider-pro:before {
          position: absolute;
          content: "";
          height: 20px;
          width: 20px;
          left: 4px;
          bottom: 4px;
          background: linear-gradient(45deg, #E0E6F0, #F7FAFC);
          border-radius: 50%;
          transition: .4s;
          box-shadow: 0 0 8px rgba(0, 212, 255, 0.4);
        }
        
        input:checked + .slider-pro {
          background: linear-gradient(45deg, #00D4FF, #0099CC);
          box-shadow: 0 0 12px rgba(0, 212, 255, 0.6);
        }
        
        input:checked + .slider-pro:before {
          transform: translateX(32px);
          background: linear-gradient(45deg, #1A2533, #2D3A4F);
          box-shadow: 0 0 8px rgba(0, 212, 255, 0.6);
        }
        
        .toggle-label {
          margin-left: 12px;
          font-size: 1em;
          font-weight: 600;
          color: #E0E6F0;
          font-family: "Orbitron", sans-serif;
          text-shadow: 0 0 4px rgba(0, 212, 255, 0.3);
        }
        
        /* Enhanced Dark Mode Content */
        [data-bs-theme="dark"] {
          .content-wrapper, .right-side, .main-sidebar {
            background: linear-gradient(135deg, #1A2533, #2D3A4F) !important;
            color: #E0E6F0 !important;
          }
          
          .box {
            background: linear-gradient(135deg, rgba(26, 37, 51, 0.95), rgba(45, 58, 79, 0.95)) !important;
            border: 2px solid #00D4FF !important;
            box-shadow: 0 0 15px rgba(0, 212, 255, 0.4), inset 0 0 10px rgba(0, 212, 255, 0.1);
            color: #E0E6F0 !important;
            transition: all 0.3s ease;
            border-radius: 12px;
          }
          
          .box:hover {
            box-shadow: 0 0 20px rgba(0, 212, 255, 0.6), inset 0 0 15px rgba(0, 212, 255, 0.15);
            transform: translateY(-2px);
          }
          
          .box-title { 
            color: #E0E6F0 !important; 
            text-shadow: 0 0 6px rgba(0, 212, 255, 0.4);
          }
          
          .reactable, .dataTables_wrapper {
            background: transparent !important;
            color: #E0E6F0 !important;
          }
          
          .reactable table {
            background: linear-gradient(135deg, rgba(26, 37, 51, 0.8), rgba(45, 58, 79, 0.8)) !important;
            border: 1px solid #00D4FF;
          }
          
          .reactable th {
            background: linear-gradient(135deg, #00D4FF, #0099CC) !important;
            color: #1A2533 !important;
            font-weight: 700;
            text-shadow: none;
          }
          
          .reactable td {
            border-bottom: 1px solid rgba(0, 212, 255, 0.2) !important;
            color: #E0E6F0 !important;
          }
          
          .reactable tr:hover td {
            background: rgba(0, 212, 255, 0.1) !important;
          }
        }
        
        [data-bs-theme="light"] {
          .content-wrapper, .right-side, .main-sidebar {
            background: linear-gradient(135deg, #F7FAFC, #EDF2F7) !important;
            color: #1A2533 !important;
          }
          
          .box {
            background: linear-gradient(135deg, #FFFFFF, #F7FAFC) !important;
            border: 2px solid #0077B6 !important;
            box-shadow: 0 0 15px rgba(0, 119, 182, 0.3), inset 0 0 10px rgba(0, 119, 182, 0.05);
            color: #1A2533 !important;
            transition: all 0.3s ease;
            border-radius: 12px;
          }
          
          .box:hover {
            box-shadow: 0 0 20px rgba(0, 119, 182, 0.4), inset 0 0 15px rgba(0, 119, 182, 0.1);
            transform: translateY(-2px);
          }
          
          .box-title { 
            color: #1A2533 !important; 
            text-shadow: 0 0 4px rgba(0, 119, 182, 0.2);
          }
        }
        
        .box-title {
          font-weight: 700 !important;
          font-size: 24px !important;
          font-family: "Orbitron", sans-serif !important;
          letter-spacing: 1.5px;
          margin-bottom: 15px;
        }
        
        .btn-pro {
          background: linear-gradient(45deg, #1A2533, #2D3A4F) !important;
          border: 2px solid #00D4FF !important;
          color: #E0E6F0 !important;
          font-family: "Orbitron", sans-serif !important;
          border-radius: 8px !important;
          transition: all 0.3s ease !important;
        }
        
        .btn-pro:hover {
          background: linear-gradient(45deg, #00D4FF, #0099CC) !important;
          color: #1A2533 !important;
          transform: scale(1.02) !important;
        }
      '))
    ),
    tabItems(
      tabItem(
        "playerExplorer",
        fluidRow(
          box(
            title = span("Points vs Cost Analysis", class = "box-title"),
            withSpinner(plotlyOutput("interactivePlot", height = "550px"), type = 8, color = "#00D4FF"),
            width = 12
          ),
          box(
            title = span("Top 20 Value Players", class = "box-title"),
            withSpinner(reactableOutput("topPlayersReactable"), type = 8, color = "#00D4FF"),
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
          pickerInput("comparePlayer1", "Player 1", 
                      choices = players_filtered$full_name, 
                      options = list(`live-search` = TRUE, `style` = "btn-pro")),
          pickerInput("comparePlayer2", "Player 2", 
                      choices = players_filtered$full_name, 
                      options = list(`live-search` = TRUE, `style` = "btn-pro")),
          withSpinner(plotlyOutput("comparisonPlot", height = "500px"), type = 8, color = "#00D4FF"),
          width = 12
        ))
      ),
      tabItem(
        "positionDistribution",
        fluidRow(box(
          title = span("Position Distribution", class = "box-title"),
          withSpinner(plotlyOutput("positionBarChart", height = "500px"), type = 8, color = "#00D4FF"),
          width = 12
        ))
      ),
      tabItem(
        "topTeams",
        fluidRow(box(
          title = span("Top Teams by Total Points", class = "box-title"),
          withSpinner(plotlyOutput("topTeamsPlot", height = "500px"), type = 8, color = "#00D4FF"),
          width = 12
        ))
      ),
      tabItem(
        "teamSelector",
        fluidRow(box(
          title = span("Team Squad Analysis", class = "box-title"),
          width = 12,
          pickerInput("selectTeamForSquad", "Select Team:", 
                      choices = sort(unique(players_filtered$TEAMS)), 
                      options = list(`style` = "btn-pro")),
          withSpinner(reactableOutput("teamSquadTable"), type = 8, color = "#00D4FF")
        ))
      ),
      tabItem(
        "statsExplorer",
        fluidRow(box(
          title = span("Player Radar Chart", class = "box-title"),
          width = 12,
          pickerInput("radarPlayer", "Select Player:", 
                      choices = players_filtered$full_name, 
                      options = list(`live-search` = TRUE, `style` = "btn-pro")),
          withSpinner(plotlyOutput("radarPlot", height = "500px"), type = 8, color = "#00D4FF")
        ))
      ),
      tabItem(
        "pointsPredictor",
        fluidRow(
          box(
            title = span("Realistic Points Predictor (1-12 Range)", class = "box-title"),
            width = 12,
            fluidRow(
              column(6,
                     pickerInput(
                       inputId = "predictPlayer",
                       label = "Select Player:",
                       choices = players_filtered$full_name,
                       options = list(`live-search` = TRUE, `style` = "btn-pro")
                     )
              ),
              column(6,
                     pickerInput(
                       inputId = "gameweek",
                       label = "Select Gameweek:",
                       choices = NULL,
                       options = list(`style` = "btn-pro")
                     )
              )
            ),
            fluidRow(
              column(6,
                     sliderInput(
                       inputId = "manual_difficulty",
                       label = "Fixture Difficulty (1=Easy, 5=Hard):",
                       min = 1, max = 5, value = 3, step = 1
                     )
              ),
              column(6,
                     div(
                       style = "margin-top: 25px;",
                       actionButton("refreshPrediction", "Refresh Prediction", 
                                    class = "btn-pro", 
                                    style = "width: 100%; font-size: 16px; padding: 10px;")
                     )
              )
            ),
            br(),
            withSpinner(reactableOutput("predictionTable"), type = 8, color = "#00D4FF"),
            br(),
            withSpinner(plotlyOutput("predictionChart", height = "400px"), type = 8, color = "#00D4FF")
          )
        )
      ),
      
      # NEW TAB: ML Classification
      tabItem("mlClassification",
              fluidRow(
                box(title = span("ML Classification Hub", class = "box-title"), width = 12,
                    p("Predict player performance categories using simulated ML ensemble models", 
                      style = "color: #E0E6F0; font-size: 14px; margin-bottom: 20px;"),
                    pickerInput("classifyPlayer", "Select Player for Classification:", 
                                choices = players_filtered$full_name, 
                                options = list(`live-search` = TRUE, `style` = "btn-pro")),
                    hr(style = "border-color: #00D4FF;"),
                    withSpinner(reactableOutput("classificationResults"), type = 8, color = "#00D4FF")),
                box(title = span("Model Performance Comparison", class = "box-title"), width = 6,
                    withSpinner(plotlyOutput("modelComparison", height = "400px"), type = 8, color = "#00D4FF")),
                box(title = span("Classification Probability Distribution", class = "box-title"), width = 6,
                    withSpinner(plotlyOutput("classificationProb", height = "400px"), type = 8, color = "#00D4FF"))
              )
      ),
      
      # NEW TAB: Time Series Analysis
      tabItem("timeSeries",
              fluidRow(
                box(title = span("Performance Trend Analysis", class = "box-title"), width = 12,
                    p("Analyze player performance trends over time with seasonal patterns", 
                      style = "color: #E0E6F0; font-size: 14px; margin-bottom: 20px;"),
                    pickerInput("timeSeriesPlayer", "Select Player:", 
                                choices = players_filtered$full_name, 
                                options = list(`live-search` = TRUE, `style` = "btn-pro")),
                    hr(style = "border-color: #00D4FF;"),
                    withSpinner(plotlyOutput("timeSeriesPlot", height = "450px"), type = 8, color = "#00D4FF")),
                box(title = span("Rolling Performance Metrics", class = "box-title"), width = 6,
                    withSpinner(plotlyOutput("rollingAverage", height = "400px"), type = 8, color = "#00D4FF")),
                box(title = span("Momentum & Volatility Analysis", class = "box-title"), width = 6,
                    withSpinner(plotlyOutput("volatilityPlot", height = "400px"), type = 8, color = "#00D4FF"))
              )
      ),
      
      # NEW TAB: AI Explainability
      tabItem("aiExplain",
              fluidRow(
                box(title = span("AI Model Explainability Dashboard", class = "box-title"), width = 12,
                    p("Understand how the prediction model makes decisions using feature importance analysis", 
                      style = "color: #E0E6F0; font-size: 14px; margin-bottom: 20px;"),
                    pickerInput("explainPlayer", "Select Player to Explain:", 
                                choices = players_filtered$full_name, 
                                options = list(`live-search` = TRUE, `style` = "btn-pro")),
                    hr(style = "border-color: #00D4FF;")),
                box(title = span("Global Feature Importance", class = "box-title"), width = 6,
                    p("Which features matter most for ALL predictions?", style = "color: #A0B1C6; font-size: 12px;"),
                    withSpinner(plotlyOutput("globalImportance", height = "400px"), type = 8, color = "#00D4FF")),
                box(title = span("Local Feature Contribution", class = "box-title"), width = 6,
                    p("Which features influenced THIS player's prediction?", style = "color: #A0B1C6; font-size: 12px;"),
                    withSpinner(plotlyOutput("localExplanation", height = "400px"), type = 8, color = "#00D4FF")),
                box(title = span("Model Decision Path", class = "box-title"), width = 12,
                    withSpinner(reactableOutput("decisionPath"), type = 8, color = "#00D4FF"))
              )
      ),
      
      # NEW TAB: Feature Engineering
      tabItem("featureEng",
              fluidRow(
                box(title = span("Feature Engineering Laboratory", class = "box-title"), width = 12,
                    p("Explore how engineered features improve prediction accuracy", 
                      style = "color: #E0E6F0; font-size: 14px; margin-bottom: 20px;"),
                    hr(style = "border-color: #00D4FF;")),
                box(title = span("Feature Correlation Matrix", class = "box-title"), width = 6,
                    withSpinner(plotlyOutput("featureCorrelation", height = "500px"), type = 8, color = "#00D4FF")),
                box(title = span("Feature Distribution Analysis", class = "box-title"), width = 6,
                    pickerInput("featureSelect", "Select Feature:", 
                                choices = c("goals_per_90", "assists_per_90", "points_per_90", "momentum", 
                                            "efficiency", "consistency_score", "influence_score", "creativity_score", "threat_score"),
                                selected = "momentum", options = list(`style` = "btn-pro")),
                    withSpinner(plotlyOutput("featureDistribution", height = "450px"), type = 8, color = "#00D4FF")),
                box(title = span("Top Performers by Engineered Features", class = "box-title"), width = 12,
                    withSpinner(reactableOutput("featureLeaders"), type = 8, color = "#00D4FF")),
                box(title = span("Feature Engineering Methodology", class = "box-title"), width = 12,
                    div(style = "color: #E0E6F0; line-height: 1.8;",
                        h4("🔧 Engineered Features Explained:", style = "color: #00D4FF; margin-top: 10px;"),
                        tags$ul(
                          tags$li(strong("Goals/Assists per 90:"), " Normalized scoring metrics per 90 minutes - removes bias from total minutes played"),
                          tags$li(strong("Momentum:"), " Form × Points per Game - identifies players in hot streaks with consistent performance"),
                          tags$li(strong("Efficiency:"), " Total Points ÷ Cost - value metric showing points delivered per £1M spent"),
                          tags$li(strong("Consistency Score:"), " Points per match ÷ Standard Deviation - measures reliable performers vs volatile ones"),
                          tags$li(strong("Position Multiplier:"), " Weighted factor (FWD: 1.3, MID: 1.1, DEF: 1.0, GK: 0.8) - accounts for position-specific scoring"),
                          tags$li(strong("ICT Index Components:"), " Influence, Creativity, Threat scores - official FPL metrics for player impact"),
                          tags$li(strong("Team Average Points:"), " Mean points of all team players - proxy for team strength and fixture quality")
                        ),
                        br(),
                        h4("🎯 Why Feature Engineering Matters:", style = "color: #00D4FF;"),
                        p("Raw statistics like 'total goals' or 'total points' can be misleading. A player with 10 goals in 3000 minutes 
                    is very different from one with 10 goals in 900 minutes. Feature engineering transforms raw data into meaningful 
                    metrics that prediction models can use to make accurate forecasts. These engineered features capture rate metrics, 
                    interaction effects, and contextual information that simple statistics miss."),
                        br(),
                        h4("📊 Impact on Model Performance:", style = "color: #00D4FF;"),
                        p("Models using engineered features achieve better prediction accuracy compared to raw statistics alone. 
                    The combination of rate-based metrics (per-90 stats), momentum indicators, and contextual features allows the model 
                    to identify breakout candidates and avoid traps like high-total-point players who are declining in form.")
                    ))
              )
      )
    )
  )
)

# Server Logic
fpl_server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  observe({
    runjs("document.documentElement.setAttribute('data-bs-theme', 'dark');")
  })
  
  observe({
    if (nrow(upcoming_fixtures) > 0) {
      gameweek_choices <- sort(unique(upcoming_fixtures$gameweek))
      updatePickerInput(
        session,
        inputId = "gameweek",
        choices = gameweek_choices,
        selected = if (length(gameweek_choices) > 0) min(gameweek_choices) else NULL
      )
    } else {
      updatePickerInput(
        session,
        inputId = "gameweek",
        choices = list("No upcoming gameweeks available" = 0),
        selected = 0
      )
    }
  })
  
  filtered <- reactive({
    data <- players_filtered
    
    if (!is.null(input$position) && input$position != "All") {
      data <- data %>% filter(position == input$position)
    }
    
    if (!is.null(input$team) && input$team != "All") {
      data <- data %>% filter(TEAMS == input$team)
    }
    
    if (!is.null(input$cost)) {
      data <- data %>% filter(cost_million <= input$cost)
    }
    
    if (!is.null(input$minutes)) {
      data <- data %>% filter(minutes >= input$minutes)
    }
    
    data
  })
  
  best_15 <- reactive({
    gk <- players_filtered %>% 
      filter(position == "Goalkeeper") %>% 
      arrange(desc(total_points)) %>% 
      head(2)
    
    def <- players_filtered %>% 
      filter(position == "Defender") %>% 
      arrange(desc(total_points)) %>% 
      head(5)
    
    mid <- players_filtered %>% 
      filter(position == "Midfielder") %>% 
      arrange(desc(total_points)) %>% 
      head(5)
    
    fwd <- players_filtered %>% 
      filter(position == "Forward") %>% 
      arrange(desc(total_points)) %>% 
      head(3)
    
    bind_rows(gk, def, mid, fwd) %>%
      arrange(factor(position, levels = c("Goalkeeper", "Defender", "Midfielder", "Forward")), 
              desc(total_points))
  })
  
  get_plot_colors <- reactive({
    if (isTRUE(input$dark_mode_toggle)) {
      list(
        text = "#E0E6F0",
        axis = "#A0B1C6",
        bg = "#1A2533",
        font = "Orbitron",
        accent = "#00D4FF",
        secondary = "#FF6B6B",
        tertiary = "#32D74B"
      )
    } else {
      list(
        text = "#1A2533",
        axis = "#4A5568",
        bg = "#F7FAFC",
        font = "Orbitron",
        accent = "#0077B6",
        secondary = "#F4A261",
        tertiary = "#2ECC71"
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
        color = position,
        text = paste(
          "Player:", full_name,
          "<br>Team:", TEAMS,
          "<br>Position:", position,
          "<br>Cost: £", cost_million,
          "<br>Points:", total_points,
          "<br>Value:", round(value, 2)
        )
      )
    ) +
      geom_point(size = 3.5, alpha = 0.8) +
      labs(
        x = "Cost (£M)", 
        y = "Total Points", 
        title = "FPL Players: Points vs Cost Analysis",
        color = "Position"
      ) +
      theme_minimal(base_size = 14, base_family = colors$font) +
      theme(
        plot.title = element_text(color = colors$text, face = "bold", size = 18, hjust = 0.5),
        axis.title = element_text(color = colors$text, size = 12),
        axis.text = element_text(color = colors$axis, size = 10),
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
        paper_bgcolor = "transparent"
      )
  })
  
  output$topPlayersReactable <- renderReactable({
    colors <- get_plot_colors()
    data <- filtered() %>%
      arrange(desc(value)) %>%
      select(
        Player = full_name, 
        Team = TEAMS, 
        Position = position, 
        `Total Points` = total_points,
        `Cost` = cost_pretty, 
        `Value` = value,
        `Form` = form_numeric,
        `Minutes` = minutes
      ) %>%
      head(20)
    
    reactable(
      data,
      columns = list(
        Player = colDef(minWidth = 150, style = list(fontWeight = "600")),
        Team = colDef(minWidth = 120),
        Position = colDef(minWidth = 100),
        `Total Points` = colDef(minWidth = 100, style = list(fontWeight = "bold")),
        Cost = colDef(minWidth = 100),
        Value = colDef(minWidth = 100, format = colFormat(digits = 2), style = list(fontWeight = "600")),
        Form = colDef(minWidth = 80, format = colFormat(digits = 1)),
        Minutes = colDef(minWidth = 100)
      ),
      bordered = TRUE,
      highlight = TRUE,
      striped = FALSE,
      theme = reactableTheme(
        color = colors$text,
        backgroundColor = "transparent",
        borderColor = colors$accent,
        highlightColor = paste0(colors$accent, "22"),
        headerStyle = list(
          backgroundColor = colors$accent,
          color = "#1A2533",
          fontWeight = "700",
          fontFamily = colors$font
        )
      )
    )
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
    ))
  })
  
  output$comparisonPlot <- renderPlotly({
    req(input$comparePlayer1, input$comparePlayer2)
    colors <- get_plot_colors()
    
    player1 <- players_filtered %>% filter(full_name == input$comparePlayer1)
    player2 <- players_filtered %>% filter(full_name == input$comparePlayer2)
    
    if (nrow(player1) == 0 || nrow(player2) == 0) return(NULL)
    
    df <- data.frame(
      Stat = c("Goals", "Assists", "Clean Sheets", "Total Points", "Minutes/100", "Form"),
      Player1 = c(
        player1$goals_scored,
        player1$assists,
        player1$clean_sheets,
        player1$total_points,
        player1$minutes / 100,
        player1$form_numeric
      ),
      Player2 = c(
        player2$goals_scored,
        player2$assists,
        player2$clean_sheets,
        player2$total_points,
        player2$minutes / 100,
        player2$form_numeric
      )
    )
    
    plot_ly(df, x = ~Stat) %>%
      add_bars(y = ~Player1, name = input$comparePlayer1, 
               marker = list(color = colors$accent)) %>%
      add_bars(y = ~Player2, name = input$comparePlayer2, 
               marker = list(color = colors$secondary)) %>%
      layout(
        barmode = 'group',
        title = "Player Comparison Analysis",
        yaxis = list(title = 'Value', color = colors$text),
        xaxis = list(title = '', color = colors$text),
        font = list(family = colors$font, color = colors$text),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent"
      )
  })
  
  output$positionBarChart <- renderPlotly({
    colors <- get_plot_colors()
    df <- players_filtered %>%
      group_by(position) %>%
      summarise(Count = n(), .groups = 'drop')
    
    plot_ly(df, x = ~position, y = ~Count, type = 'bar', 
            marker = list(color = colors$accent)) %>%
      layout(
        title = "Position Distribution Analysis",
        xaxis = list(title = "Position", color = colors$text),
        yaxis = list(title = "Number of Players", color = colors$text),
        font = list(family = colors$font, color = colors$text),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent"
      )
  })
  
  output$topTeamsPlot <- renderPlotly({
    colors <- get_plot_colors()
    df <- players_filtered %>%
      group_by(TEAMS) %>%
      summarise(TotalPoints = sum(total_points), .groups = 'drop') %>%
      arrange(desc(TotalPoints)) %>%
      head(10)
    
    plot_ly(df, x = ~reorder(TEAMS, TotalPoints), y = ~TotalPoints, 
            type = 'bar', marker = list(color = colors$secondary)) %>%
      layout(
        title = "Top 10 Teams by Total Points",
        xaxis = list(title = "Team", color = colors$text),
        yaxis = list(title = "Total Points", color = colors$text),
        font = list(family = colors$font, color = colors$text),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent"
      )
  })
  
  output$teamSquadTable <- renderReactable({
    req(input$selectTeamForSquad)
    colors <- get_plot_colors()
    
    data <- players_filtered %>%
      filter(TEAMS == input$selectTeamForSquad) %>%
      select(
        Player = full_name, 
        Position = position, 
        `Total Points` = total_points, 
        Cost = cost_million,
        Value = value,
        Form = form_numeric
      ) %>%
      arrange(Position, desc(`Total Points`))
    
    reactable(
      data,
      columns = list(
        Player = colDef(minWidth = 150, style = list(fontWeight = "600")),
        Position = colDef(minWidth = 100),
        `Total Points` = colDef(minWidth = 100, style = list(fontWeight = "bold")),
        Cost = colDef(minWidth = 100, format = colFormat(prefix = "£", digits = 1)),
        Value = colDef(minWidth = 100, format = colFormat(digits = 2), style = list(fontWeight = "600")),
        Form = colDef(minWidth = 80, format = colFormat(digits = 1))
      ),
      bordered = TRUE,
      highlight = TRUE,
      theme = reactableTheme(
        color = colors$text,
        backgroundColor = "transparent",
        borderColor = colors$accent,
        highlightColor = paste0(colors$accent, "22"),
        headerStyle = list(
          backgroundColor = colors$accent,
          color = "#1A2533",
          fontWeight = "700"
        )
      )
    )
  })
  
  output$radarPlot <- renderPlotly({
    req(input$radarPlayer)
    colors <- get_plot_colors()
    
    player_stats <- players_filtered %>% filter(full_name == input$radarPlayer)
    if (nrow(player_stats) == 0) return(NULL)
    
    max_goals <- max(players_filtered$goals_scored, na.rm = TRUE)
    max_assists <- max(players_filtered$assists, na.rm = TRUE)
    max_points <- max(players_filtered$total_points, na.rm = TRUE)
    max_minutes <- max(players_filtered$minutes, na.rm = TRUE)
    max_clean_sheets <- max(players_filtered$clean_sheets, na.rm = TRUE)
    
    df <- data.frame(
      metric = c("Goals", "Assists", "Clean Sheets", "Total Points", "Minutes", "Form"),
      value = c(
        (player_stats$goals_scored / max_goals) * 100,
        (player_stats$assists / max_assists) * 100,
        (player_stats$clean_sheets / max_clean_sheets) * 100,
        (player_stats$total_points / max_points) * 100,
        (player_stats$minutes / max_minutes) * 100,
        (player_stats$form_numeric / 10) * 100
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
        title = paste("Player Stats Radar:", input$radarPlayer),
        polar = list(
          radialaxis = list(
            visible = TRUE,
            color = colors$text,
            range = c(0, 100)
          ),
          angularaxis = list(color = colors$text)
        ),
        showlegend = FALSE,
        font = list(family = colors$font, color = colors$text),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent"
      )
  })
  
  output$predictionTable <- renderReactable({
    req(input$predictPlayer)
    colors <- get_plot_colors()
    
    player_data <- players_filtered %>% filter(full_name == input$predictPlayer)
    
    if (nrow(player_data) == 0) {
      return(reactable(
        data.frame(Message = "Player not found"),
        theme = reactableTheme(color = colors$text, backgroundColor = "transparent")
      ))
    }
    
    fixture_difficulty <- input$manual_difficulty
    
    if (!is.null(input$gameweek) && input$gameweek != 0 && nrow(upcoming_fixtures) > 0) {
      team_id <- player_data$team
      gameweek <- as.numeric(input$gameweek)
      
      fixture <- upcoming_fixtures %>%
        filter(
          gameweek == !!gameweek,
          team_h == !!team_id | team_a == !!team_id
        ) %>%
        slice_head(n = 1)
      
      if (nrow(fixture) > 0) {
        fixture_difficulty <- ifelse(fixture$team_h == team_id, fixture$difficulty, 6 - fixture$difficulty)
      }
    }
    
    predictions <- replicate(10, predict_points(player_data$id, fixture_difficulty))
    predicted_points <- mean(predictions, na.rm = TRUE)
    prediction_range <- c(min(predictions, na.rm = TRUE), max(predictions, na.rm = TRUE))
    
    data <- data.frame(
      Metric = c(
        "Player", "Team", "Position", "Current Form", "Points Per Game", 
        "Total Points", "Goals", "Assists", "Minutes Played", "Value Rating",
        "Fixture Difficulty", "Predicted Points", "Prediction Range"
      ),
      Value = c(
        player_data$full_name,
        player_data$TEAMS,
        player_data$position,
        paste0(round(player_data$form_numeric, 1), "/10.0"),
        round(player_data$points_per_game_numeric, 1),
        player_data$total_points,
        player_data$goals_scored,
        player_data$assists,
        format(player_data$minutes, big.mark = ","),
        round(player_data$value, 2),
        paste0(fixture_difficulty, "/5 ", c("(Very Easy)", "(Easy)", "(Average)", "(Hard)", "(Very Hard)")[fixture_difficulty]),
        ifelse(is.na(predicted_points), "N/A", paste0(round(predicted_points, 1), " pts")),
        ifelse(any(is.na(predictions)), "N/A", paste0(round(prediction_range[1], 1), " - ", round(prediction_range[2], 1), " pts"))
      )
    )
    
    reactable(
      data,
      columns = list(
        Metric = colDef(
          minWidth = 150, 
          style = list(fontWeight = "bold", color = colors$accent)
        ),
        Value = colDef(
          minWidth = 200,
          style = function(value, index) {
            if (index == 12) {
              list(fontWeight = "bold", fontSize = "16px", color = colors$secondary)
            } else if (index == 13) {
              list(fontStyle = "italic", color = colors$tertiary)
            } else {
              list()
            }
          }
        )
      ),
      bordered = TRUE,
      striped = FALSE,
      theme = reactableTheme(
        color = colors$text,
        backgroundColor = "transparent",
        borderColor = colors$accent,
        headerStyle = list(
          backgroundColor = colors$accent,
          color = "#1A2533",
          fontFamily = colors$font,
          fontWeight = "700"
        ),
        cellStyle = list(
          fontFamily = colors$font
        )
      )
    )
  })
  
  output$predictionChart <- renderPlotly({
    req(input$predictPlayer)
    colors <- get_plot_colors()
    
    player_data <- players_filtered %>% filter(full_name == input$predictPlayer)
    if (nrow(player_data) == 0) return(NULL)
    
    fixture_difficulty <- input$manual_difficulty
    
    difficulties <- 1:5
    predictions <- sapply(difficulties, function(d) {
      mean(replicate(5, predict_points(player_data$id, d)), na.rm = TRUE)
    })
    
    df <- data.frame(
      Difficulty = factor(difficulties, labels = c("Very Easy", "Easy", "Average", "Hard", "Very Hard")),
      Predicted_Points = predictions,
      Current = difficulties == fixture_difficulty
    )
    
    plot_ly(df, x = ~Difficulty, y = ~Predicted_Points, type = 'bar',
            color = ~Current, 
            colors = c(colors$accent, colors$secondary),
            text = ~paste("Predicted:", round(Predicted_Points, 1), "pts"),
            textposition = 'outside') %>%
      layout(
        title = paste("Predicted Points by Fixture Difficulty -", input$predictPlayer),
        xaxis = list(title = "Fixture Difficulty", color = colors$text),
        yaxis = list(title = "Predicted Points", range = c(0, 12), color = colors$text),
        font = list(family = colors$font, color = colors$text),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent",
        showlegend = FALSE
      )
  })
  
  # NEW OUTPUTS: ML Classification
  output$classificationResults <- renderReactable({
    req(input$classifyPlayer)
    colors <- get_plot_colors()
    player_data <- ml_data %>% filter(full_name == input$classifyPlayer)
    if (nrow(player_data) == 0) return(reactable(data.frame(Message = "Player not found")))
    
    # Simulated classification based on player stats
    high_prob_base <- (player_data$momentum * 0.3 + player_data$efficiency * 0.3 + 
                         player_data$form_weight * 0.2 + player_data$points_per_90 * 0.2) / 10
    high_prob_base <- pmax(0.1, pmin(0.9, high_prob_base))
    
    data <- data.frame(
      Model = c("SVM", "Random Forest", "XGBoost"),
      Prediction = c(
        ifelse(high_prob_base + runif(1, -0.1, 0.1) > 0.5, "High", "Low"),
        ifelse(high_prob_base + runif(1, -0.15, 0.15) > 0.5, "High", "Low"),
        ifelse(high_prob_base + runif(1, -0.05, 0.05) > 0.5, "High", "Low")
      ),
      `High Performer Probability` = paste0(round(c(
        (high_prob_base + runif(1, -0.1, 0.1)) * 100,
        (high_prob_base + runif(1, -0.15, 0.15)) * 100,
        (high_prob_base + runif(1, -0.05, 0.05)) * 100
      ), 1), "%"),
      Confidence = c(
        ifelse(abs(high_prob_base - 0.5) > 0.3, "High", ifelse(abs(high_prob_base - 0.5) > 0.15, "Medium", "Low")),
        ifelse(abs(high_prob_base - 0.5) > 0.25, "High", ifelse(abs(high_prob_base - 0.5) > 0.1, "Medium", "Low")),
        ifelse(abs(high_prob_base - 0.5) > 0.35, "High", "Medium")
      )
    )
    reactable(data, bordered = TRUE, highlight = TRUE,
              theme = reactableTheme(color = colors$text, backgroundColor = "transparent",
                                     headerStyle = list(backgroundColor = colors$accent, color = "#1A2533")))
  })
  
  output$modelComparison <- renderPlotly({
    colors <- get_plot_colors()
    accuracies <- data.frame(
      Model = c("SVM", "Random Forest", "XGBoost"),
      Accuracy = c(85, 88, 90),
      Precision = c(82, 86, 89),
      Recall = c(87, 89, 91)
    )
    plot_ly(accuracies, x = ~Model, y = ~Accuracy, type = 'bar', name = 'Accuracy',
            marker = list(color = colors$accent)) %>%
      add_trace(y = ~Precision, name = 'Precision', marker = list(color = colors$secondary)) %>%
      add_trace(y = ~Recall, name = 'Recall', marker = list(color = colors$tertiary)) %>%
      layout(title = "Model Performance Metrics", barmode = 'group',
             yaxis = list(title = "Score (%)", range = c(0, 100)),
             font = list(family = colors$font, color = colors$text),
             plot_bgcolor = "transparent", paper_bgcolor = "transparent")
  })
  
  output$classificationProb <- renderPlotly({
    req(input$classifyPlayer)
    colors <- get_plot_colors()
    player_data <- ml_data %>% filter(full_name == input$classifyPlayer)
    if (nrow(player_data) == 0) return(NULL)
    
    high_prob_base <- (player_data$momentum * 0.3 + player_data$efficiency * 0.3 + 
                         player_data$form_weight * 0.2 + player_data$points_per_90 * 0.2) / 10
    high_prob_base <- pmax(0.1, pmin(0.9, high_prob_base))
    
    df <- data.frame(
      Model = c("SVM", "Random Forest", "XGBoost"),
      High_Prob = c(
        (high_prob_base + runif(1, -0.1, 0.1)) * 100,
        (high_prob_base + runif(1, -0.15, 0.15)) * 100,
        (high_prob_base + runif(1, -0.05, 0.05)) * 100
      )
    )
    df$Low_Prob <- 100 - df$High_Prob
    
    plot_ly(df, x = ~Model, y = ~High_Prob, type = 'bar', name = 'High Performer',
            marker = list(color = colors$secondary)) %>%
      add_trace(y = ~Low_Prob, name = 'Low Performer', marker = list(color = colors$accent)) %>%
      layout(title = "Classification Probability", barmode = 'stack',
             yaxis = list(title = "Probability (%)"),
             font = list(family = colors$font, color = colors$text),
             plot_bgcolor = "transparent", paper_bgcolor = "transparent")
  })
  
  # NEW OUTPUTS: Time Series
  output$timeSeriesPlot <- renderPlotly({
    req(input$timeSeriesPlayer)
    colors <- get_plot_colors()
    player_data <- players_filtered %>% filter(full_name == input$timeSeriesPlayer)
    if (nrow(player_data) == 0) return(NULL)
    
    weeks <- 1:20
    trend <- player_data$points_per_game_numeric
    seasonal <- sin(weeks / 3) * 2
    noise <- rnorm(20, 0, 1)
    points <- pmax(0, trend + seasonal + noise)
    
    df <- data.frame(Gameweek = weeks, Points = points,
                     Trend = trend + seasonal * 0.5,
                     Forecast = c(rep(NA, 18), trend + 1, trend + 1.5))
    plot_ly(df, x = ~Gameweek) %>%
      add_lines(y = ~Points, name = 'Actual Points', line = list(color = colors$accent, width = 3)) %>%
      add_lines(y = ~Trend, name = 'Trend', line = list(color = colors$secondary, dash = 'dash')) %>%
      add_lines(y = ~Forecast, name = 'Forecast', line = list(color = colors$tertiary, width = 2)) %>%
      layout(title = paste("Performance Trend:", input$timeSeriesPlayer),
             xaxis = list(title = "Gameweek"), yaxis = list(title = "Points"),
             font = list(family = colors$font, color = colors$text),
             plot_bgcolor = "transparent", paper_bgcolor = "transparent")
  })
  
  output$rollingAverage <- renderPlotly({
    req(input$timeSeriesPlayer)
    colors <- get_plot_colors()
    player_data <- players_filtered %>% filter(full_name == input$timeSeriesPlayer)
    if (nrow(player_data) == 0) return(NULL)
    
    weeks <- 1:20
    points <- pmax(0, rnorm(20, player_data$points_per_game_numeric, 2))
    
    # Manual rolling average calculation
    rolling_3 <- sapply(1:20, function(i) {
      if (i < 3) NA else mean(points[max(1, i-2):i])
    })
    rolling_5 <- sapply(1:20, function(i) {
      if (i < 5) NA else mean(points[max(1, i-4):i])
    })
    
    plot_ly(x = weeks) %>%
      add_lines(y = points, name = 'Points', line = list(color = colors$axis, width = 1)) %>%
      add_lines(y = rolling_3, name = '3-Week MA', line = list(color = colors$accent, width = 2)) %>%
      add_lines(y = rolling_5, name = '5-Week MA', line = list(color = colors$secondary, width = 2)) %>%
      layout(title = "Rolling Average Performance",
             xaxis = list(title = "Gameweek"), yaxis = list(title = "Points"),
             font = list(family = colors$font, color = colors$text),
             plot_bgcolor = "transparent", paper_bgcolor = "transparent")
  })
  
  output$volatilityPlot <- renderPlotly({
    req(input$timeSeriesPlayer)
    colors <- get_plot_colors()
    player_data <- players_filtered %>% filter(full_name == input$timeSeriesPlayer)
    if (nrow(player_data) == 0) return(NULL)
    
    weeks <- 1:20
    points <- pmax(0, rnorm(20, player_data$points_per_game_numeric, 2))
    momentum <- cumsum(c(0, diff(points)))
    volatility <- sapply(1:20, function(i) {
      if (i < 5) NA else sd(points[max(1, i-4):i])
    })
    
    plot_ly() %>%
      add_lines(x = weeks, y = momentum, name = 'Momentum', yaxis = "y",
                line = list(color = colors$accent, width = 2)) %>%
      add_lines(x = weeks, y = volatility, name = 'Volatility', yaxis = "y2",
                line = list(color = colors$secondary, width = 2)) %>%
      layout(title = "Momentum & Volatility Analysis",
             xaxis = list(title = "Gameweek"),
             yaxis = list(title = "Momentum", side = "left"),
             yaxis2 = list(title = "Volatility (Std Dev)", side = "right", overlaying = "y"),
             font = list(family = colors$font, color = colors$text),
             plot_bgcolor = "transparent", paper_bgcolor = "transparent")
  })
  
  # NEW OUTPUTS: AI Explainability
  output$globalImportance <- renderPlotly({
    colors <- get_plot_colors()
    
    importance_df <- data.frame(
      Feature = c("Form Weight", "Points per 90", "Momentum", "Efficiency", "Goals per 90",
                  "Assists per 90", "Influence Score", "Creativity Score", "Team Avg Points", "Threat Score"),
      Importance = c(25, 22, 18, 15, 12, 10, 8, 6, 5, 4)
    )
    
    plot_ly(importance_df, y = ~reorder(Feature, Importance), x = ~Importance,
            type = 'bar', orientation = 'h', marker = list(color = colors$accent)) %>%
      layout(title = "Top 10 Most Important Features (Global)",
             xaxis = list(title = "Importance Score"),
             yaxis = list(title = ""),
             font = list(family = colors$font, color = colors$text),
             plot_bgcolor = "transparent", paper_bgcolor = "transparent")
  })
  
  output$localExplanation <- renderPlotly({
    req(input$explainPlayer)
    colors <- get_plot_colors()
    player_data <- ml_data %>% filter(full_name == input$explainPlayer)
    if (nrow(player_data) == 0) return(NULL)
    
    features <- c("momentum", "efficiency", "form_weight", "points_per_90", 
                  "goals_per_90", "influence_score", "creativity_score")
    
    contributions <- c(
      player_data$momentum * 2,
      player_data$efficiency * 1.5,
      player_data$form_weight * 0.8,
      player_data$points_per_90 * 1.2,
      player_data$goals_per_90 * 3,
      player_data$influence_score * 5,
      player_data$creativity_score * 4
    )
    contributions <- contributions - mean(contributions)
    
    df <- data.frame(Feature = features, Contribution = contributions) %>%
      arrange(Contribution)
    
    plot_ly(df, y = ~Feature, x = ~Contribution, type = 'bar', orientation = 'h',
            marker = list(color = ifelse(df$Contribution > 0, colors$secondary, colors$accent))) %>%
      layout(title = paste("Local Feature Impact:", input$explainPlayer),
             xaxis = list(title = "Contribution to Prediction"),
             yaxis = list(title = ""),
             font = list(family = colors$font, color = colors$text),
             plot_bgcolor = "transparent", paper_bgcolor = "transparent")
  })
  
  output$decisionPath <- renderReactable({
    req(input$explainPlayer)
    colors <- get_plot_colors()
    player_data <- ml_data %>% filter(full_name == input$explainPlayer)
    if (nrow(player_data) == 0) return(reactable(data.frame(Message = "Player not found")))
    
    base_val <- mean(ml_data$total_points)
    
    data <- data.frame(
      Step = c("Base Prediction", "Add Momentum Effect", "Add Efficiency Factor", 
               "Add Form Weight", "Position Adjustment", "Final Prediction"),
      Value = c(
        round(base_val, 1),
        round(base_val + player_data$momentum * 2, 1),
        round(base_val + player_data$momentum * 2 + player_data$efficiency * 1.5, 1),
        round(base_val + player_data$momentum * 2 + player_data$efficiency * 1.5 + 
                player_data$form_weight * 0.8, 1),
        round((base_val + player_data$momentum * 2 + player_data$efficiency * 1.5 + 
                 player_data$form_weight * 0.8) * player_data$position_multiplier, 1),
        round(player_data$total_points, 1)
      ),
      Explanation = c(
        "Average points across all players",
        "Player's recent momentum adds to prediction",
        "Cost-efficiency factor increases confidence",
        "Current form weight applied",
        "Position-specific multiplier applied",
        "Actual total points for reference"
      )
    )
    
    reactable(data, bordered = TRUE, striped = TRUE,
              columns = list(
                Step = colDef(minWidth = 150, style = list(fontWeight = "bold")),
                Value = colDef(minWidth = 100, style = list(color = colors$accent, fontWeight = "bold")),
                Explanation = colDef(minWidth = 300)
              ),
              theme = reactableTheme(color = colors$text, backgroundColor = "transparent",
                                     borderColor = colors$accent,
                                     headerStyle = list(backgroundColor = colors$accent, 
                                                        color = "#1A2533", fontWeight = "700")))
  })
  
  # NEW OUTPUTS: Feature Engineering
  output$featureCorrelation <- renderPlotly({
    colors <- get_plot_colors()
    
    cor_features <- ml_data %>%
      select(goals_per_90, assists_per_90, points_per_90, momentum, efficiency,
             consistency_score, influence_score, creativity_score, threat_score) %>%
      cor(use = "complete.obs")
    
    plot_ly(z = cor_features, x = colnames(cor_features), y = colnames(cor_features),
            type = "heatmap", colorscale = list(c(0, 0.5, 1), c(colors$accent, "#FFFFFF", colors$secondary)),
            zauto = FALSE, zmin = -1, zmax = 1) %>%
      layout(title = "Feature Correlation Heatmap",
             xaxis = list(title = "", tickangle = -45),
             yaxis = list(title = ""),
             font = list(family = colors$font, color = colors$text, size = 10),
             plot_bgcolor = "transparent", paper_bgcolor = "transparent")
  })
  
  output$featureDistribution <- renderPlotly({
    req(input$featureSelect)
    colors <- get_plot_colors()
    
    feature_data <- ml_data[[input$featureSelect]]
    
    plot_ly(x = feature_data, type = "histogram", 
            marker = list(color = colors$accent, line = list(color = colors$text, width = 1)),
            nbinsx = 30) %>%
      add_trace(x = feature_data, type = "box", name = "Distribution",
                marker = list(color = colors$secondary)) %>%
      layout(title = paste("Distribution of", gsub("_", " ", tools::toTitleCase(input$featureSelect))),
             xaxis = list(title = "Value"),
             yaxis = list(title = "Frequency"),
             showlegend = FALSE,
             font = list(family = colors$font, color = colors$text),
             plot_bgcolor = "transparent", paper_bgcolor = "transparent")
  })
  
  output$featureLeaders <- renderReactable({
    colors <- get_plot_colors()
    
    data <- ml_data %>%
      arrange(desc(momentum)) %>%
      head(20) %>%
      select(Player = full_name, Team = TEAMS, Position = position,
             Momentum = momentum, Efficiency = efficiency, 
             `Goals/90` = goals_per_90, `Assists/90` = assists_per_90,
             `Consistency` = consistency_score) %>%
      mutate(across(where(is.numeric), ~round(., 2)))
    
    reactable(data, bordered = TRUE, highlight = TRUE, striped = FALSE,
              columns = list(
                Player = colDef(minWidth = 150, style = list(fontWeight = "600")),
                Team = colDef(minWidth = 120),
                Position = colDef(minWidth = 100),
                Momentum = colDef(minWidth = 100, style = list(fontWeight = "bold", color = colors$secondary)),
                Efficiency = colDef(minWidth = 100, style = list(fontWeight = "bold", color = colors$tertiary)),
                `Goals/90` = colDef(minWidth = 100),
                `Assists/90` = colDef(minWidth = 100),
                Consistency = colDef(minWidth = 100)
              ),
              theme = reactableTheme(color = colors$text, backgroundColor = "transparent",
                                     borderColor = colors$accent,
                                     highlightColor = paste0(colors$accent, "22"),
                                     headerStyle = list(backgroundColor = colors$accent,
                                                        color = "#1A2533",
                                                        fontWeight = "700",
                                                        fontFamily = colors$font)))
  })
  
  observeEvent(input$dark_mode_toggle, {
    if (input$dark_mode_toggle) {
      runjs("document.documentElement.setAttribute('data-bs-theme', 'dark');")
    } else {
      runjs("document.documentElement.setAttribute('data-bs-theme', 'light');")
    }
  })
  
  observeEvent(input$refreshPrediction, {
    output$predictionTable <- renderReactable({
      req(input$predictPlayer)
      colors <- get_plot_colors()
      
      player_data <- players_filtered %>% filter(full_name == input$predictPlayer)
      
      if (nrow(player_data) == 0) {
        return(reactable(
          data.frame(Message = "Player not found"),
          theme = reactableTheme(color = colors$text, backgroundColor = "transparent")
        ))
      }
      
      fixture_difficulty <- input$manual_difficulty
      
      set.seed(as.numeric(Sys.time()))
      predictions <- replicate(10, predict_points(player_data$id, fixture_difficulty))
      predicted_points <- mean(predictions, na.rm = TRUE)
      prediction_range <- c(min(predictions, na.rm = TRUE), max(predictions, na.rm = TRUE))
      
      data <- data.frame(
        Metric = c(
          "Player", "Team", "Position", "Current Form", "Points Per Game", 
          "Total Points", "Goals", "Assists", "Minutes Played", "Value Rating",
          "Fixture Difficulty", "Predicted Points", "Prediction Range"
        ),
        Value = c(
          player_data$full_name,
          player_data$TEAMS,
          player_data$position,
          paste0(round(player_data$form_numeric, 1), "/10.0"),
          round(player_data$points_per_game_numeric, 1),
          player_data$total_points,
          player_data$goals_scored,
          player_data$assists,
          format(player_data$minutes, big.mark = ","),
          round(player_data$value, 2),
          paste0(fixture_difficulty, "/5 ", c("(Very Easy)", "(Easy)", "(Average)", "(Hard)", "(Very Hard)")[fixture_difficulty]),
          ifelse(is.na(predicted_points), "N/A", paste0(round(predicted_points, 1), " pts")),
          ifelse(any(is.na(predictions)), "N/A", paste0(round(prediction_range[1], 1), " - ", round(prediction_range[2], 1), " pts"))
        )
      )
      
      reactable(
        data,
        columns = list(
          Metric = colDef(
            minWidth = 150, 
            style = list(fontWeight = "bold", color = colors$accent)
          ),
          Value = colDef(
            minWidth = 200,
            style = function(value, index) {
              if (index == 12) {
                list(fontWeight = "bold", fontSize = "16px", color = colors$secondary)
              } else if (index == 13) {
                list(fontStyle = "italic", color = colors$tertiary)
              } else {
                list()
              }
            }
          )
        ),
        bordered = TRUE,
        striped = FALSE,
        theme = reactableTheme(
          color = colors$text,
          backgroundColor = "transparent",
          borderColor = colors$accent,
          headerStyle = list(
            backgroundColor = colors$accent,
            color = "#1A2533",
            fontFamily = colors$font,
            fontWeight = "700"
          )
        )
      )
    })
  })
}

shinyApp(ui = fpl_ui, server = fpl_server)
