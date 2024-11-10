library(tidyverse)
library(reactable)
library(htmltools)

# data ----

load("data/hitting_stats.RData")

leaders <- hitter |> 
  filter(season == 2024) |> 
  arrange(desc(ops)) |> 
  mutate(
    name = player_full_name, 
    player_id, 
    team = team_name, 
    team_id = team_link |> 
      str_sub(15, 17) |> 
      as.numeric(), 
    position, 
    ops,
    .keep = "none"
  )


# logos ----

download_logo <- function(team_id, destination = str_glue("images/team_logos/{team_id}.svg")) {
  Sys.sleep(2)
  logo_url <- str_glue("https://www.mlbstatic.com/team-logos/{team_id}.svg")
  download.file(logo_url, destination, mode = "wb")
}

team_ids <- leaders |> 
  distinct(team_id) |> 
  pull(team_id) |> 
  set_names()

team_ids |> 
  map_dbl(download_logo)


# headshots ----

download_headshot <- function(player_id, destination = str_glue("images/player_headshots/{player_id}.png")) {
  Sys.sleep(2)
  headshot_url <- str_glue("https://content.mlb.com/images/headshots/current/60x60/{player_id}.png")
  download.file(headshot_url, destination, mode = "wb")
}

player_ids <- leaders |> 
  distinct(player_id) |> 
  pull(player_id) |> 
  set_names()

player_ids |> 
  map_dbl(download_headshot)
