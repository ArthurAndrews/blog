# this was an aborted ML model

library(tidyverse)
library(tidymodels)

# if multiple teams in season?

load("data/hitting_stats.RData")
hit0 <- hit

# join with player details ----

player <- player |> 
  mutate(height1 = height) |> 
  separate_wider_delim(height1, "'", names = c("feet", "inches")) |> 
  mutate(
    feet = feet |> parse_number(),
    inches = inches |> 
      str_remove(fixed('\"')) |> 
      str_trim() |> 
      parse_number(),
    height_inches = 12 * feet + inches,
    kg = weight * 0.4535,
    m = height_inches / 39.37,
    bmi = kg / (m ^ 2),
    centered_bmi = bmi - mean(bmi),
    position = primary_position_abbreviation
  )

common_names <- intersect(names(hitter), names(player))

hit <- hitter |> 
  rename(id = player_id, name = player_full_name, ab = at_bats, pa = plate_appearances) |> 
  select(-all_of(common_names)) |> 
  left_join(player, by = join_by(id)) |> 
  group_by(id) |> 
  mutate(n_seasons = n_distinct(season)) |> 
  ungroup() |> 
  mutate(centered_age = age - mean(age))

hit <- hit |> 
  filter(n_seasons >= 5) |> 
  complete(nesting(id, name), season) |> #nesting(name, id)) |> 
  arrange(id, name, season) |> 
  group_by(id, name) |> 
  mutate(
    ops = as.numeric(ops),
    position = as.factor(position),
    across(
      c(avg, obp, slg, ops, pa), 
      list(lag1 = \(x) lag(x, 1), lag2 = \(x) lag(x, 2))
    )
  ) |> 
  ungroup() 

hit_complete <- hit |> 
  drop_na(ops, ops_lag1, ops_lag2) |> 
  filter(pa > 200, pa_lag1 > 200, pa_lag2 > 200)

form <- ops ~ age + ops_lag1 + ops_lag2 + bmi + position

model.frame(form, hit) |> 
  as_tibble()

model <- workflow() |> 
  add_recipe(
    recipe(form, data = hit) |> 
      step_dummy(position)
  ) |> 
  add_model(
    boost_tree(mtry = tune(), tree_depth = tune(), learn_rate = tune()) |> 
      set_mode("regression") |> 
      set_engine("xgboost")
  ) |> 
  tune_grid(
    resamples = group_vfold_cv(hit, group = name, v = 10),
    grid = 100,
    metrics = metric_set(rmse, rsq_trad),
    control = control_grid(save_workflow = TRUE, save_pred = TRUE)
  )

model |> 
  collect_metrics(type = "wide") |> 
  arrange(desc(rsq_trad))

final_model <- model |> 
  fit_best()

xgb <- final_model |> 
  extract_fit_engine()

xgb |> 
  xgboost::xgb.importance(model =)

library(lme4)
library(splines)

model2 <- lmer(
  ops ~ (centered_age | name) + bs(centered_age, df = 3),
  data = hit0 |> 
    filter(season < 2023)
)

test <- hit0 |> 
  filter(season == 2023) 

test <- test |> 
  mutate(
    pred = predict(model2, test, allow.new.levels = TRUE),
    res = ops - pred
  )

with(test, 1 - var(pred - ops) / var(ops))
with(test, sd(pred - ops))

test |> 
  arrange(desc(abs(res)))


model3 <- lmer(
  ops ~ (centered_age | position/name) + bs(centered_age, df = 3),
  data = hit0 |> 
    filter(season < 2023)
)

test <- hit0 |> 
  filter(season == 2023) 

test <- test |> 
  mutate(
    pred = predict(model3, test, allow.new.levels = TRUE),
    res = ops - pred
  )

with(test, 1 - var(pred - ops) / var(ops))
with(test, sd(pred - ops))

test |> 
  arrange(desc(abs(res))) |> 
  select(name, position, team_name, age, pa, ops, pred, res) |> 
  print(n = 40)


model4 <- lmer(
  ops ~ (centered_age | position/name) + bs(centered_age, df = 3),
  data = hit0 
)

f <- hit0 |> 
  filter(season == 2023) |> 
  mutate(season = 2023, age = age + 1, centered_age = centered_age + 1, ops_2023 = ops, ops = NA, pa = NA) 

f <- f |> 
  mutate(
    pred = predict(model4, test, allow.new.levels = TRUE),
    res = ops - pred,
    pred_delta = pred - ops_2023
  )

f |> 
  arrange(desc(pred)) |> 
  select(name, position, team_name, age, pa, ops, pred, res) |> 
  print(n = 40)

f |> 
  arrange(desc(abs(pred_delta))) |> 
  select(name, position, team_name, age, pa, ops, pred, pred_delta) |> 
  print(n = 40)

f |> 
  filter(team_name == "Philadelphia Phillies") |> 
  arrange(desc(pred)) |> 
  select(name, position, team_name, age, pa, ops, pred, res) |> 
  print(n = 40)

f |> 
  arrange()

hit <- hit |> 
  mutate(
    bmi_range = cut(bmi, 6)
  )

model5 <- lmer(
  ops ~ (centered_age | bmi_range/name) + bs(centered_age, df = 3),
  data = hit |> 
    drop_na(bmi_range)
)


slice <- dplyr::slice

hit |> 
  distinct(age, centered_age, bmi_range) |> 
  drop_na() |> 
  mutate(name = NA_character_) |> 
  add_prediction(model5, allow.new.levels = TRUE) |> 
  ggplot() + 
  aes(age, pred_ops, color = bmi_range) +
  geom_line()
