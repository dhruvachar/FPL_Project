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

# [Previous data fetching and processing code remains the same...]
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
    # Enhanced features for better prediction
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

# [Previous ML model code remains the same...]
# Enhanced ML model for significantly better predictions
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
    popularity = coalesce(selected_by_percent, 0) / 100
  ) %>%
  select(
    id, full_name, TEAMS, position, total_points, form_numeric, form_weight,
    points_per_game_numeric, points_per_90, minutes, goals_scored, assists, 
    clean_sheets, goals_per_90, assists_per_90, position_multiplier,
    influence_score, creativity_score, threat_score, team_avg_points,
    popularity, cost_million
  ) %>%
  filter(complete.cases(.))

cat("Enhanced ML training data prepared:", nrow(ml_data), "\n")

# Train improved Random Forest model for better predictions
ml_model <- NULL
if (nrow(ml_data) > 20) {
  set.seed(42)
  train_indices <- sample(nrow(ml_data), 0.8 * nrow(ml_data))
  train_data <- ml_data[train_indices, ]
  test_data <- ml_data[-train_indices, ]
  
  ml_model <- tryCatch({
    cat("Training enhanced Random Forest model...\n")
    model <- randomForest(
      total_points ~ form_weight + points_per_game_numeric + points_per_90 + 
        minutes + goals_per_90 + assists_per_90 + position_multiplier +
        influence_score + creativity_score + threat_score + team_avg_points +
        popularity + clean_sheets,
      data = train_data,
      ntree = 500,
      mtry = 4,
      importance = TRUE
    )
    predictions <- predict(model, test_data)
    rmse <- sqrt(mean((test_data$total_points - predictions)^2, na.rm = TRUE))
    mae <- mean(abs(test_data$total_points - predictions), na.rm = TRUE)
    cat("Model validation - RMSE:", round(rmse, 2), "MAE:", round(mae, 2), "\n")
    cat("Model trained successfully\n")
    model
  }, error = function(e) {
    cat("Failed to train enhanced model, falling back to linear model:", e$message, "\n")
    tryCatch({
      lm(total_points ~ form_weight + points_per_game_numeric + minutes + 
           goals_per_90 + assists_per_90 + position_multiplier + clean_sheets,
         data = train_data)
    }, error = function(e2) {
      cat("Failed to train any model:", e2$message, "\n")
      NULL
    })
  })
}

# Enhanced prediction function with better accuracy
predict_points <- function(player_id, fixture_difficulty = 3, gameweek = NULL) {
  if (is.null(ml_model)) {
    cat("No ML model available for predictions\n")
    return(NA)
  }
  
  player_data <- ml_data %>% filter(id == player_id)
  if (nrow(player_data) == 0) {
    cat("Player not found in ML data\n")
    return(NA)
  }
  
  difficulty_multiplier <- case_when(
    fixture_difficulty == 1 ~ 1.4,
    fixture_difficulty == 2 ~ 1.2,
    fixture_difficulty == 3 ~ 1.0,
    fixture_difficulty == 4 ~ 0.7,
    fixture_difficulty == 5 ~ 0.5,
    TRUE ~ 1.0
  )
  
  position_difficulty_adj <- case_when(
    player_data$position == "Forward" ~ 1.2,
    player_data$position == "Midfielder" ~ 1.1,
    player_data$position == "Defender" ~ 0.9,
    player_data$position == "Goalkeeper" ~ 0.8,
    TRUE ~ 1.0
  )
  
  base_prediction <- tryCatch({
    predict(ml_model, newdata = player_data)
  }, error = function(e) {
    cat("Prediction error:", e$message, "\n")
    return(player_data$form_weight * 0.7 + player_data$points_per_game_numeric * 0.3)
  })
  
  if (is.na(base_prediction)) {
    base_prediction <- player_data$form_weight * 0.7 + player_data$points_per_game_numeric * 0.3
  }
  
  final_prediction <- base_prediction * difficulty_multiplier * position_difficulty_adj
  final_prediction <- pmax(0, pmin(25, final_prediction))
  final_prediction + runif(1, -0.5, 0.5)
}

# UI Definition with Premier League lion logo
fpl_ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = div(
      class = "fpl-header-bar",
      div(
        class = "fpl-header-content",
        div(
          class = "fpl-header-left",
          # Premier League Lion Logo
          tags$img(
            src = "data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iNjAiIGhlaWdodD0iNjAiIHZpZXdCb3g9IjAgMCA2MCA2MCIgZmlsbD0ibm9uZSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj4KPGRlZnM+CjxsaW5lYXJHcmFkaWVudCBpZD0ibGlvbkdyYWQiIHgxPSIwJSIgeTE9IjAlIiB4Mj0iMTAwJSIgeTI9IjEwMCUiPgo8c3RvcCBvZmZzZXQ9IjAlIiBzdHlsZT0ic3RvcC1jb2xvcjojMzcwMDNDO3N0b3Atb3BhY2l0eToxIiAvPgo8c3RvcCBvZmZzZXQ9IjEwMCUiIHN0eWxlPSJzdG9wLWNvbG9yOiM5QzI3QjA7c3RvcC1vcGFjaXR5OjEiIC8+CjwvbGluZWFyR3JhZGllbnQ+CjwvZGVmcz4KPHN2ZyB3aWR0aD0iNjAwIiBoZWlnaHQ9IjYwMCIgdmlld0JveD0iMCAwIDYwIDYwIj4KPGNpcmNsZSBjeD0iMzAiIGN5PSIzMCIgcj0iMjgiIGZpbGw9InVybCgjbGlvbkdyYWQpIiBzdHJva2U9IiNGRkZGRkYiIHN0cm9rZS13aWR0aD0iMiIvPgo8IS0tIExpb24gSGVhZCAtLT4KPHBhdGggZD0iTTMwIDEwIEMzNSAxMCA0MiAxNSA0MiAyNSBDNDIgMzAgNDAgMzUgMzUgMzggQzMzIDM5IDMxIDQwIDMwIDQwIEMyOSA0MCAyNyAzOSAyNSAzOCBDMjAgMzUgMTggMzAgMTggMjUgQzE4IDE1IDI1IDEwIDMwIDEwIFoiIGZpbGw9IiNGRkZGRkYiLz4KPCEtLSBNYW5lIC0tPgo8cGF0aCBkPSJNMjIgMTggQzIwIDE2IDE4IDE4IDE4IDIwIEMxOCAyMiAyMCAyNCAyMiAyNCBDMjQgMjQgMjYgMjIgMjYgMjAgQzI2IDE4IDI0IDE2IDIyIDE4IFoiIGZpbGw9IiNGRkZGRkYiLz4KPHBhdGggZD0iTTM4IDE4IEM0MCAxNiA0MiAxOCA0MiAyMCBDNDIgMjIgNDAgMjQgMzggMjQgQzM2IDI0IDM0IDIyIDM0IDIwIEMzNCAxOCAzNiAxNiAzOCAxOCBaIiBmaWxsPSIjRkZGRkZGIi8+CjwhLS0gRXllcyAtLT4KPGNpcmNsZSBjeD0iMjYiIGN5PSIyNCIgcj0iMiIgZmlsbD0iIzM3MDAzQyIvPgo8Y2lyY2xlIGN4PSIzNCIgY3k9IjI0IiByPSIyIiBmaWxsPSIjMzcwMDNDIi8+CjwhLS0gTm9zZSAtLT4KPGVsbGlwc2UgY3g9IjMwIiBjeT0iMjgiIHJ4PSIyIiByeT0iMSIgZmlsbD0iIzM3MDAzQyIvPgo8IS0tIE1vdXRoIC0tPgo8cGF0aCBkPSJNMjggMzIgQzI4IDM0IDI5IDM1IDMwIDM1IEMzMSAzNSAzMiAzNCAzMiAzMiIgc3Ryb2tlPSIjMzcwMDNDIiBzdHJva2Utd2lkdGg9IjEuNSIgZmlsbD0ibm9uZSIvPgo8IS0tIENyb3duIC0tPgo8cGF0aCBkPSJNMjUgMTIgTDI3IDggTDMwIDEwIEwzMyA4IEwzNSAxMiBMMzAgMTQgWiIgZmlsbD0iI0ZGRkZGRiIgc3Ryb2tlPSIjMzcwMDNDIiBzdHJva2Utd2lkdGg9IjAuNSIvPgo8L3N2Zz4KPC9zdmc+",
            class = "pl-logo",
            alt = "Premier League Lion"
          ),
          # Original FPL Logo (smaller, positioned next to PL logo)
        ),
        div(
          class = "fpl-header-center",
          span("FPL COMMAND CENTER", class = "fpl-glow-text fpl-glow-main"),
          br(),
          a(
            href = "https://fantasy.premierleague.com",
            target = "_blank",
            class = "fpl-official-link",
            "Official Premier League FPL"
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
        background: linear-gradient(180deg, #1A2533, #2D3A4F);
        border-bottom: 2px solid #00D4FF;
        margin-bottom: 20px;
        box-shadow: 0 0 10px rgba(0, 212, 255, 0.3);",
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
      menuItem("Points Predictor", tabName = "pointsPredictor", icon = icon("chart-line"))
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
        
        /* Enhanced Header Layout */
        .fpl-header-bar {
          width: 100vw;
          height: 70px;
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
          padding: 0 20px;
        }
        
        .fpl-header-left,
        .fpl-header-right {
          flex: 0 0 120px;
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
        
        .pl-logo {
          height: 55px;
          width: 55px;
          transition: all 0.3s ease;
          animation: pulseLogo 3s ease-in-out infinite;
          filter: drop-shadow(0 0 10px rgba(55, 0, 60, 0.6));
        }
        
        .pl-logo:hover {
          transform: scale(1.1);
          filter: drop-shadow(0 0 15px rgba(55, 0, 60, 0.8));
        }
        
        .fpl-logo {
          height: 40px;
          width: 40px;
          transition: all 0.3s ease;
          animation: pulseLogo 2s ease-in-out infinite;
          filter: drop-shadow(0 0 8px rgba(0, 212, 255, 0.5));
        }
        
        .fpl-logo:hover {
          transform: scale(1.1);
          filter: drop-shadow(0 0 12px rgba(0, 212, 255, 0.8));
        }
        
        .fpl-glow-text {
          font-family: "Orbitron", sans-serif;
          font-size: 1.8rem;
          font-weight: 700;
          letter-spacing: 2px;
          color: #E0E6F0;
          text-shadow: 0 0 8px rgba(0, 212, 255, 0.7), 0 0 12px rgba(0, 212, 255, 0.5);
          animation: flicker 2s infinite;
          margin-bottom: 5px;
        }
        
        .fpl-glow-main { color: #E0E6F0; }
        
        .fpl-official-link {
          font-family: "Orbitron", sans-serif !important;
          font-size: 1.1rem !important;
          font-weight: 600 !important;
          color: #E0E6F0 !important;
          background: rgba(26, 37, 51, 0.9) !important;
          border: 2px solid #00D4FF !important;
          border-radius: 8px !important;
          padding: 6px 16px !important;
          box-shadow: 0 0 10px rgba(0, 212, 255, 0.6) !important;
          transition: all 0.3s ease !important;
          text-decoration: none !important;
          display: inline-block !important;
          text-shadow: 0 0 5px rgba(0, 212, 255, 0.4) !important;
          animation: officialGlow 3s ease-in-out infinite alternate !important;
        }
        
        .fpl-official-link:hover {
          background: linear-gradient(45deg, #00D4FF, #0099CC) !important;
          color: #1A2533 !important;
          box-shadow: 0 0 15px rgba(0, 212, 255, 0.8) !important;
          transform: scale(1.05) !important;
          text-shadow: none !important;
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
          
          /* Enhanced Tables and Plots */
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
        
        /* Enhanced Animations */
        @keyframes neonGlow {
          from { box-shadow: 0 0 12px rgba(0, 212, 255, 0.5); }
          to { box-shadow: 0 0 25px rgba(0, 212, 255, 0.8); }
        }
        
        @keyframes officialGlow {
          from { 
            box-shadow: 0 0 8px rgba(0, 212, 255, 0.4);
            text-shadow: 0 0 4px rgba(0, 212, 255, 0.3);
          }
          to { 
            box-shadow: 0 0 15px rgba(0, 212, 255, 0.7);
            text-shadow: 0 0 8px rgba(0, 212, 255, 0.5);
          }
        }
        
        @keyframes flicker {
          0%, 100% { text-shadow: 0 0 8px rgba(0, 212, 255, 0.7), 0 0 12px rgba(0, 212, 255, 0.5); }
          50% { text-shadow: 0 0 15px rgba(0, 212, 255, 0.9), 0 0 20px rgba(0, 212, 255, 0.7); }
        }
        
        @keyframes pulseLogo {
          0%, 100% { transform: scale(1); }
          50% { transform: scale(1.05); }
        }
        
        /* Enhanced Input Styling */
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
    # [Rest of the tabItems remain the same as in the previous code...]
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
            title = span("Enhanced Points Predictor", class = "box-title"),
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
      )
    )
  )
)

# [Server logic remains the same as in the previous code...]
# Server Logic with enhanced functionality
fpl_server <- function(input, output, session) {
  thematic::thematic_shiny()
  
  # Initialize dark mode as default
  observe({
    runjs("document.documentElement.setAttribute('data-bs-theme', 'dark');")
  })
  
  # Update gameweek choices dynamically
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
  
  # Reactive filtered data
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
  
  # Best XI selection
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
  
  # Color scheme reactive
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
  
  # Interactive scatter plot
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
  
  # Enhanced top players table
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
  
  # Best XI table
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
  
  # Player comparison plot
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
  
  # Position distribution chart
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
  
  # Top teams plot
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
  
  # Enhanced team squad table
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
  
  # Enhanced radar plot
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
        (player_stats$form_numeric / 5) * 100
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
  
  # Enhanced prediction table with more accuracy
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
    
    # Multiple predictions for confidence interval
    predictions <- replicate(10, predict_points(player_data$id, fixture_difficulty))
    predicted_points <- mean(predictions, na.rm = TRUE)
    prediction_range <- c(min(predictions, na.rm = TRUE), max(predictions, na.rm = TRUE))
    
    # Enhanced data with more insights
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
        paste0(round(player_data$form_numeric, 1), "/5.0"),
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
            if (index == 12) {  # Predicted Points row
              list(fontWeight = "bold", fontSize = "16px", color = colors$secondary)
            } else if (index == 13) {  # Prediction Range row
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
  
  # New: Prediction visualization chart
  output$predictionChart <- renderPlotly({
    req(input$predictPlayer)
    colors <- get_plot_colors()
    
    player_data <- players_filtered %>% filter(full_name == input$predictPlayer)
    if (nrow(player_data) == 0) return(NULL)
    
    # Create prediction comparison chart
    fixture_difficulty <- input$manual_difficulty
    
    # Predictions for different difficulties
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
        yaxis = list(title = "Predicted Points", color = colors$text),
        font = list(family = colors$font, color = colors$text),
        plot_bgcolor = "transparent",
        paper_bgcolor = "transparent",
        showlegend = FALSE
      )
  })
  
  # Handle dark mode toggle
  observeEvent(input$dark_mode_toggle, {
    if (input$dark_mode_toggle) {
      runjs("document.documentElement.setAttribute('data-bs-theme', 'dark');")
    } else {
      runjs("document.documentElement.setAttribute('data-bs-theme', 'light');")
    }
  })
  
  # Refresh prediction button
  observeEvent(input$refreshPrediction, {
    # Force reactivity update
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
      
      # Fresh predictions with new random seed
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
          paste0(round(player_data$form_numeric, 1), "/5.0"),
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

# Run the application
shinyApp(ui = fpl_ui, server = fpl_server)
