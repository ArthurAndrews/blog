# query season hitter stats over time range and player details (to calculate age)
# and save data frames to disk
# Arthur Andrews
# July 2024

library(tidyverse)
library(baseballr)
library(magrittr, include.only = "divide_by")


# query hitter stats
hitter <- tibble(season = 2005:2024) |> 
  pmap(
    partial(mlb_stats, stat_type = "season", stat_group = "hitting", player_pool = "All")
  ) |> 
  bind_rows() |> 
  filter(at_bats > 200)

# get unique ids
ids <- hitter |> 
  drop_na(player_id) |> 
  pull(player_id) |> 
  unique()

# query player data.  need this to calculate age.
# this fails.  i think the vector of ids is too long and the API complains.
# player <- ids |> 
#   mlb_people()

# query in chunks instead
player <- hitter |> 
  drop_na(player_id) |> 
  distinct(player_id) |> 
  mutate(q = ntile(player_id, 10)) |> 
  nest(id = player_id) |> 
  mutate(
    data = id |> 
      map(pull, player_id) |> 
      map(mlb_people)
  ) |> 
  select(-id) |> 
  unnest(data)

player_to_join <- player |> 
  select(id, birth_date, position = primary_position_abbreviation, height, weight)

# join birth date and calculate age at mid-season
hitter <- hitter |> 
  select(-any_of(colnames(player_to_join))) |> 
  left_join(
    player_to_join,
    by = join_by(player_id == id)
  ) |> 
  mutate(
    across(birth_date, as_date),
    mid_season = str_glue("{season}-07-1") |> 
      as_date(),
     age = (mid_season - birth_date) |> 
      as.numeric(units = "days") |>
      divide_by(365.25),
    across(c(avg, obp, slg, ops), as.numeric),
    team_id = team_link |> 
      str_sub(15, 17) |> 
      as.numeric(),
    ab = at_bats, pa = plate_appearances, name = player_full_name, id = player_id
  )

# smaller set of columns
hit <- hitter |> 
  select(
    season, 
    id, name, position, height, weight,
    team_name, team_id,
    ab, pa, age, avg, obp, slg, ops
  ) |> 
  arrange(id, season) |> 
  group_by(id) |> 
  mutate(n_seasons = n_distinct(season)) |> 
  ungroup() |> 
  mutate(
    mean_age = mean(age),
    centered_age = age - mean_age
  )

# save
save(hitter, player, hit, file = "data/hitting_stats.RData")
