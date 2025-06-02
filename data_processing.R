library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

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

# Process FPL data
process_fpl_data <- function() {
  data <- fetch_fpl_data()
  players <- data$elements
  teams <- data$teams
  positions <- data$element_types
  
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
  
  list(players = players, players_filtered = players_filtered)
}

# Load data globally
fpl_data <- process_fpl_data()
players <- fpl_data$players
players_filtered <- fpl_data$players_filtered

# Function to select best 15 players
best_15 <- function() {
  gk <- players_filtered %>% filter(position == "Goalkeeper") %>% arrange(desc(total_points)) %>% head(2)
  def <- players_filtered %>% filter(position == "Defender") %>% arrange(desc(total_points)) %>% head(5)
  mid <- players_filtered %>% filter(position == "Midfielder") %>% arrange(desc(total_points)) %>% head(5)
  fwd <- players_filtered %>% filter(position == "Forward") %>% arrange(desc(total_points)) %>% head(3)
  bind_rows(gk, def, mid, fwd) %>%
    arrange(factor(position, levels = c("Goalkeeper", "Defender", "Midfielder", "Forward")), desc(total_points))
}
