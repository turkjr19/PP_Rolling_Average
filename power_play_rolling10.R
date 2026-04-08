library(tidyverse)
library(httr)
library(jsonlite)
library(purrr)
library(slider)

# -------------------------------
# Choose team.  Use the team short code to run script for that particular team.
# -------------------------------
selected_team <- "NB"

# -------------------------------
# Team lookup
# -------------------------------
teams <- tibble::tibble(
  team_short = c(
    "BAR", "BFD", "BRAM", "ER", "FLNT",
    "GUE", "KGN", "KIT", "LDN", "NB",
    "NIAG", "OS", "OSH", "OTT", "PBO",
    "SAG", "SAR", "SBY", "SOO", "WSR"
  ),
  team_code = c(
    7, 1, 18, 8, 13,
    9, 2, 10, 14, 19,
    20, 11, 4, 5, 6,
    34, 15, 12, 16, 17
  )
)

# -------------------------------
# Get selected team code
# -------------------------------
selected_team_code <- teams %>%
  filter(team_short == selected_team) %>%
  pull(team_code)

# -------------------------------
# Get selected team's games
# -------------------------------
games <- allGames %>%
  filter(started == 1) %>%
  filter(home == selected_team | away == selected_team) %>%
  select(game_id, date_played)

# -------------------------------
# Function to pull PP data
# -------------------------------
get_pp_summary <- function(game_id, date_played) {
  
  vstr1 <- "https://lscluster.hockeytech.com/feed/index.php?feed=gc&key=ff96010c143de1b1&client_code=ohl&game_id="
  vstr2 <- "&lang_code=en&fmt=json&tab=gamesummary"
  
  vurl <- paste0(vstr1, game_id, vstr2)
  
  verbose_call <- GET(vurl)
  verbose_char <- rawToChar(verbose_call$content)
  verbose <- fromJSON(verbose_char, flatten = TRUE)
  
  visitorID <- as.numeric(verbose[["GC"]][["Gamesummary"]][["visitor"]][["id"]])
  homeID    <- as.numeric(verbose[["GC"]][["Gamesummary"]][["home"]][["id"]])
  
  pp_count_raw <- verbose[["GC"]][["Gamesummary"]][["powerPlayCount"]]
  pp_goals_raw <- verbose[["GC"]][["Gamesummary"]][["powerPlayGoals"]]
  
  team_side <- case_when(
    selected_team_code == visitorID ~ "visitor",
    selected_team_code == homeID ~ "home",
    TRUE ~ NA_character_
  )
  
  attempts <- if (!is.na(team_side) && !is.null(pp_count_raw[[team_side]])) {
    as.numeric(pp_count_raw[[team_side]])
  } else {
    0
  }
  
  goals <- if (!is.na(team_side) && !is.null(pp_goals_raw[[team_side]])) {
    as.numeric(pp_goals_raw[[team_side]])
  } else {
    0
  }
  
  tibble(
    game_id = game_id,
    date_played = as.Date(date_played),
    team_short = selected_team,
    pp_attempts = attempts,
    pp_goals = goals,
    pp_pct = ifelse(
      attempts > 0,
      round((goals / attempts) * 100, 1),
      0
    )
  )
}

# -------------------------------
# Pull PP data
# -------------------------------
pp_summary <- pmap_dfr(
  games,
  ~ get_pp_summary(..1, ..2)
)

# -------------------------------
# Calculate rolling 10-game PP %
# -------------------------------
pp_summary <- pp_summary %>%
  arrange(date_played) %>%
  mutate(
    game_number = row_number(),
    
    roll10_attempts = slide_dbl(
      pp_attempts,
      ~ sum(.x, na.rm = TRUE),
      .before = 9,
      .complete = FALSE
    ),
    
    roll10_goals = slide_dbl(
      pp_goals,
      ~ sum(.x, na.rm = TRUE),
      .before = 9,
      .complete = FALSE
    ),
    
    rolling10_pp_pct = ifelse(
      roll10_attempts > 0,
      round((roll10_goals / roll10_attempts) * 100, 1),
      0
    )
  )

# -------------------------------
# Date of game 68
# -------------------------------
game68_date <- pp_summary$date_played[68]

# -------------------------------
# Plot
# -------------------------------
ggplot(pp_summary, aes(x = date_played, y = rolling10_pp_pct)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.3) +
  geom_vline(
    xintercept = as.Date("2026-01-10"),
    color = "green",
    linetype = "dashed",
    linewidth = 1.2
  ) +
  geom_vline(
    xintercept = game68_date,
    color = "red",
    linetype = "solid",
    linewidth = 1.2
  ) +
  scale_y_continuous(
    limits = c(0, 40),
    breaks = seq(0, 40, by = 2)
  ) +
  labs(
    title = paste(selected_team, "Rolling 10-Game Power Play %"),
    subtitle = "Green = Jan 10\nRed = Game 68",
    x = "",
    y = "Rolling 10-Game PP %"
  ) +
  theme_minimal()
