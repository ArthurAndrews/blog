library(tidyverse)
library(reactable)
library(htmltools)

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

team_logo <- function(team_id, ...) {
  img(src = str_glue("https://www.mlbstatic.com/team-logos/{team_id}.svg"), ...)
}

player_headshot <- function(player_id, ...) {
  img(src = str_glue("https://content.mlb.com/images/headshots/current/60x60/{player_id}.png"), ...)
}

download_logo <- function(team_id, destination = str_glue("images/team_logos/{team_id}.svg")) {
  Sys.sleep(2)
  logo_url <- str_glue("https://www.mlbstatic.com/team-logos/{team_id}.svg")
  download.file(logo_url, destination)
}

team_ids <- leaders |> 
  distinct(team_id) |> 
  pull(team_id)

team_ids |> 
  map(download_logo)




x <- leaders |> 
  head(1) |> 
  mutate(
    headshot = player_id |> 
      player_headshot(style = css(width = "20%")) |> 
      as.character(),
    logo = team_id |> 
      team_logo(style = css(width = "20%")) |> 
      as.character()
  ) |> 
  print()

x|> 
  reactable(
    columns = list(
      headshot = colDef(html = TRUE),
      logo = colDef(html = TRUE)
    )
  ) |> 
  save_html("test.html")




"https://www.mlbstatic.com/team-logos/147.svg" # yankees logo
"https://content.mlb.com/images/headshots/current/60x60/592450.png" # judge headshot



leaders

icons <- dir("images", "*.png")

id <- 592450
headshot_url <- str_glue(
  "https://img.mlbstatic.com/mlb-photos/image/upload/w_213,d_people:generic:headshot:silo:current.png,q_auto:best,f_auto/v1/people/{id}/headshot/67/current"
)

"https://a.espncdn.com/combiner/i?img=/i/headshots/mlb/players/full/42470.png&h=96&w=96&scale=crop"

# https://baseballsavant.mlb.com/statcast_leaderboard
# this has nicer images

img(src = headshot_url) |> 
  browsable()

leaders |> 
  head(1) |> 
  mutate(
    headshot = as.character(img(src = headshot_url, style = css(width = "20%"))),
    logo = as.character(img(src = "images/yankees.png", style = css(width = "20%")))
  ) |> 
  reactable(
    columns = list(
      headshot = colDef(html = TRUE),
      logo = colDef(html = TRUE)
    )
  ) |> 
  save_html("test.html")
  
