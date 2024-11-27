library(tidyverse)
library(reactable)
library(htmltools)


#' Make html for team logo
#'
#' @param team_id numeric team id, i.e. New York Yankees == 147
#' @param style css style; defaults to `css(height = "25px")`
#' @param ... additional arguments passed to `img()`
#'
#' @return for length(team_id) == 1, an img tag; otherwise a character vector of html
make_team_logo <- function(team_id, style = css(height = "25px"), ...) {
  if (length(team_id) == 1) {
    img(src = str_glue("images/team_logos/{team_id}.svg"), style = style, ...)
  } else {
    team_id |> 
      map(make_team_logo, style, ...) |> 
      map_chr(as.character)
  }
}


#' Make html for player headshot
#'
#' @param player_id numeric player id, i.e. Aaron Judge == 592450
#' @param style css style; defaults to `css(height = "25px")`
#' @param ... additional arguments passed to `img()`
#'
#' @return for length(player_id) == 1, an img tag; otherwise a character vector of html
make_player_headshot <- function(player_id, style = css(height = "25px"), ...) {
  if (length(player_id) == 1) {
    img(src = str_glue("images/player_headshots/{player_id}.png"), style = style, ...)
  } else {
    player_id |> 
      map(make_player_headshot, style, ...) |> 
      map_chr(as.character)
  }
}


add_headshots_and_logos <- function(data) {
  data |> 
    mutate(
      headshot = make_player_headshot(player_id),
      logo = make_team_logo(team_id)
    ) |> 
    relocate(headshot, .after = name) |> 
    relocate(logo, .after = team_name) |> 
    select(-player_id, -team_id)
}


label_hitting_stat <- function(x) {
  scales::label_number(accuracy = 0.001, trim = FALSE)(x)
}


#' Tabulate players in a `reactable` table
#'
#' @param data data with columns for name, headshot, team, logo, ops, ...
#'
#' @return a reactable table
tabulate_players <- function(data, more_columns = NULL, ...) {
  
  if (!"headshot" %in% colnames(data)) {
    data <- data |> 
      add_headshots_and_logos()
  }
  
  cl <- list(
    headshot = colDef("", minWidth = 40, maxWidth = 40, html = TRUE),
    logo = colDef("", minWidth = 40, maxWidth = 40, html = TRUE),
    ops = colDef("ops", cell = label_hitting_stat)
  ) |> 
    c(more_columns)
  
  cl <- cl[intersect(names(cl), names(data))]
  
  data |> 
    reactable(
      highlight = TRUE,
      style = css(fontSize = "90%"),
      columns = cl,
      ...
    )  
}
