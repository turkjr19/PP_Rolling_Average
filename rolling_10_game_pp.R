library(dplyr)
library(purrr)
library(httr)
library(jsonlite)
library(tibble)
library(slider)
library(ggplot2)
library(readr)

# code below is to scrape site
# change season_id in url variable as necessary
# url is pulled from network tab using browser
url <- "https://lscluster.hockeytech.com/feed/?feed=modulekit&view=schedule&key=2976319eb44abe94&fmt=json&client_code=ohl&lang=en&season_id=85&team_id=&league_code=&fmt=json"

# get json
pull <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)

# all games dataframe
allRegGames_df <- tibble(
  game_id = as.numeric(pull[["SiteKit"]][["Schedule"]][["game_id"]]),
  season = as.numeric(pull[["SiteKit"]][["Parameters"]][["season_id"]]),
  date_played = as.Date(pull[["SiteKit"]][["Schedule"]][["date_played"]]),
  home_team_id = as.numeric(pull[["SiteKit"]][["Schedule"]][["home_team"]]),
  visiting_team_id = as.numeric(pull[["SiteKit"]][["Schedule"]][["visiting_team"]]),
  home_score = as.numeric(pull[["SiteKit"]][["Schedule"]][["home_goal_count"]]),
  home = pull[["SiteKit"]][["Schedule"]][["home_team_code"]],
  away_score = as.numeric(pull[["SiteKit"]][["Schedule"]][["visiting_goal_count"]]),
  away = pull[["SiteKit"]][["Schedule"]][["visiting_team_code"]],
  game_status = pull[["SiteKit"]][["Schedule"]][["game_status"]],
  attendance = as.numeric(pull[["SiteKit"]][["Schedule"]][["attendance"]]),
  started = as.numeric(pull[["SiteKit"]][["Schedule"]][["started"]])
)

# all games dataframe
allPlayoffGames_df <- tibble(
  game_id = as.numeric(pull[["SiteKit"]][["Schedule"]][["game_id"]]),
  season = as.numeric(pull[["SiteKit"]][["Parameters"]][["season_id"]]),
  date_played = as.Date(pull[["SiteKit"]][["Schedule"]][["date_played"]]),
  home_team_id = as.numeric(pull[["SiteKit"]][["Schedule"]][["home_team"]]),
  visiting_team_id = as.numeric(pull[["SiteKit"]][["Schedule"]][["visiting_team"]]),
  home_score = as.numeric(pull[["SiteKit"]][["Schedule"]][["home_goal_count"]]),
  home = pull[["SiteKit"]][["Schedule"]][["home_team_code"]],
  away_score = as.numeric(pull[["SiteKit"]][["Schedule"]][["visiting_goal_count"]]),
  away = pull[["SiteKit"]][["Schedule"]][["visiting_team_code"]],
  game_status = pull[["SiteKit"]][["Schedule"]][["game_status"]],
  attendance = as.numeric(pull[["SiteKit"]][["Schedule"]][["attendance"]]),
  started = as.numeric(pull[["SiteKit"]][["Schedule"]][["started"]])
)

# -------------------------------
# Combine regular season + playoffs
# -------------------------------
allGames <- bind_rows(allRegGames_df, allPlayoffGames_df)

# -------------------------------
# Get Petes games that started
# -------------------------------
games <- allGames %>% 
  filter(started == 1) %>%
  filter(home == "PBO" | away == "PBO") %>% 
  select(game_id, date_played)

# -------------------------------
# Petes team ID
# -------------------------------
petes_id <- 6

# -------------------------------
# Function to pull PP data
# -------------------------------
get_pp_summary <- function(game_id, date_played){
  
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
    petes_id == visitorID ~ "visitor",
    petes_id == homeID ~ "home",
    TRUE ~ NA_character_
  )
  
  attempts <- if (!is.null(pp_count_raw[[team_side]])) {
    as.numeric(pp_count_raw[[team_side]])
  } else {
    0
  }
  
  goals <- if (!is.null(pp_goals_raw[[team_side]])) {
    as.numeric(pp_goals_raw[[team_side]])
  } else {
    0
  }
  
  tibble(
    game_id = game_id,
    date_played = as.Date(date_played),
    team = "Petes",
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
# Loop through games
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
# View result
# -------------------------------
print(pp_summary)

# -------------------------------
# Season PP% Plot
# -------------------------------
ggplot(pp_summary, aes(x = date_played, y = pp_pct)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.3) +
  geom_vline(
    xintercept = as.Date("2026-01-10"),
    color = "green",
    linetype = "dashed",
    linewidth = 1.2
  ) +
  labs(
    title = "Petes Season Power Play %",
    x = "Date",
    y = "PP %"
  ) +
  theme_minimal()

# -------------------------------
# Calculate rolling 10-game PP %
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
    xintercept = pp_summary$date_played[68],
    color = "red",
    linetype = "solid",
    linewidth = 1.2
  ) +
  scale_y_continuous(
    limits = c(10, 40),
    breaks = seq(10, 40, by = 2)
  ) +
  labs(
    title = "Rolling 10-Game Power Play %",
    subtitle = "Green = Jan 10\nRed = Last regular season game",
    x = "",
    y = "Rolling 10-Game PP %"
  ) +
  theme_minimal()

# -------------------------------
# Save to CSV
# -------------------------------
write_csv(pp_summary, "rolling_10_game_pp_summary.csv")
