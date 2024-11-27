# save a .RData file with 
#  - `team_palette` - a named character vector of the teams' primary color hex code
#  - `team_colors` - a dataframe with columns primary, secondary, tertiary colors

# I copied this text file from:
# https://chrislancaster.com/blog/design/colors-of-mlb-baseball-teams/

# Arthur Andrews

library(tidyverse)

team_colors <- scan("data/team-colors.txt", "text", sep = "\n") |> 
  str_trim()

team_colors <- team_colors[str_length(team_colors) > 0]

team_colors <- team_colors |> 
  matrix(ncol = 2, byrow = TRUE) |> 
  magrittr::set_colnames(c("team_name", "team_colors")) |> 
  as_tibble() |> 
  separate_wider_delim(
    team_colors, delim = ", ", names = c("primary", "secondary", "tertiary"), 
    too_few = "align_start", too_many = "drop"
  )

team_palette <- with(team_colors, set_names(primary, team_name))

save(team_colors, team_palette, file = "data/mlb_team_colors.RData")