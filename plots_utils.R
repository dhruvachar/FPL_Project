library(plotly)
library(ggplot2)
library(reactable)
library(dplyr)

# Color scheme function
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

# Interactive scatter plot
plot_interactive_scatter <- function(data, colors) {
  p <- ggplot(
    data,
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
}

# Top players table
plot_top_players_table <- function(data, colors) {
  data <- data %>%
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
}

# Radar chart for player stats
plot_radar_chart <- function(players_filtered, player_name, colors) {
  player_stats <- players_filtered %>% filter(full_name == player_name)
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
}

# Player comparison chart
plot_comparison_chart <- function(players_filtered, player1_name, player2_name, colors) {
  stats <- c("goals_scored", "assists", "clean_sheets", "total_points", "minutes")
  player1 <- players_filtered %>% filter(full_name == player1_name)
  player2 <- players_filtered %>% filter(full_name == player2_name)
  df <- data.frame(
    Stat = c("Goals", "Assists", "Clean Sheets", "Total Points", "Minutes"),
    Player1 = as.numeric(player1[stats]),
    Player2 = as.numeric(player2[stats])
  )
  plot_ly(df, x = ~Stat) %>%
    add_bars(y = ~Player1, name = player1_name, marker = list(color = colors$accent)) %>%
    add_bars(y = ~Player2, name = player2_name, marker = list(color = colors$secondary)) %>%
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
}

# Position distribution chart
plot_position_distribution <- function(players_filtered, colors) {
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
}

# Top teams chart
plot_top_teams <- function(players_filtered, colors) {
  df <- players_filtered %>%
    group_by(TEAMS) %>%
    summarise(TotalPoints = sum(total_points)) %>%
    arrange(desc(TotalPoints)) %>%
    head(10)
  plot_ly(df, x = ~reorder(TEAMS, TotalPoints), y = ~TotalPoints, type = 'bar', marker = list(color = colors$secondary)) %>%
    layout(
      xaxis = list(title = "Team", color = colors$text),
      yaxis = list(title = "Total Points", color = colors$text),
      title = list(text = "Top Teams", font = list(size = 20, color = colors$text, family = colors$font)),
      font = list(family = colors$font, color = colors$text),
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent"
    ) %>%
    animation_opts(500, easing = "cubic-in-out")
}

# Team squad table
plot_team_squad_table <- function(players_filtered, team_name, colors) {
  data <- players_filtered %>%
    filter(TEAMS == team_name) %>%
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
        backgroundColor = "transparent",
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
}
