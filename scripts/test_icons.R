source("tabulate_baseball.R")

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
    ops = as.numeric(ops),
    .keep = "none"
  )

x <- leaders |> 
  head(10) |> 
  mutate(
    headshot = make_player_headshot(player_id),
    logo = make_team_logo(team_id)
  ) |> 
  select(headshot, name, logo, team, ops) |> 
  print()

x |> 
  tabulate_players() |> 
  save_html("test.html")


