library(tidyverse)
library(tidymodels)
library(googlesheets4)
gs4_auth(email = "diabbluis@gmail.com", cache = ".gargle")

df <- read_csv("posts/premier-hijos-del-dado/dado_premier.csv")
data_path <- "https://docs.google.com/spreadsheets/d/1ZErNDZVSbIP8ekj-mmTpdmJ_Gbpo8yQaN5HeLBlqblE/edit?gid=1605468956#gid=1605468956"
ranking <- read_sheet(data_path, "ranking", col_types = "Dcd") |>
  filter(version == "2024-12-07")

df_prepared <-
  df |>
  mutate(across(contains("jugador_"), ~str_remove_all(.x, "\\s"))) |>
  relocate(1, 3, 2, 4, 6, 5, 8, 7) |>
  rename_with(\(x) colnames(df)) |>
  mutate(comienza_lgl = comienza == "b") |>
  bind_rows(df |> mutate(comienza_lgl = comienza == "a") |> mutate(across(contains("jugador_"), ~str_remove_all(.x, "\\s")))) |>
  mutate(victoria_lgl = resultado_a == 1) |>
  left_join(ranking |> select(-version), by = join_by(jugador_a == tor)) |>
  left_join(ranking |> select(-version), by = join_by(jugador_b == tor), suffix = c("_a", "_b")) |>
  mutate(
    delta_elo = elo_a - elo_b,
    delta_elo_pct = delta_elo / elo_a
  ) |>
  select(ronda, raza = raza_a, raza_oponente = raza_b, comienza_lgl, victoria_lgl, elo = elo_a, elo_oponente = elo_b, delta_elo, delta_elo_pct) |>
  mutate(across(where(is.logical), as.factor))


# Data partition and Bootstraps -------------------------------------------

set.seed(123)
boots <- bootstraps(df_prepared, strata = victoria_lgl)

library(bonsai)

orf_spec <-
  rand_forest(mtry = tune(), trees = 300, min_n = tune()) |>
  set_engine("aorsf") |>
  set_mode("classification")

rec <-
  recipe(victoria_lgl ~ ., data = df_prepared) |>
  update_role(ronda, new_role = "id")

orf_wf <-
  workflow() |>
  add_recipe(rec) |>
  add_model(orf_spec)

set.seed(234)
orf_grid <- grid_latin_hypercube(
  mtry(range = c(1, 5)),
  min_n(range = c(2, 20)),
  size = 40
)

res <- tune_grid(
  orf_wf,
  resamples = boots,
  grid = orf_grid
)

res |> autoplot()

best_orf <- select_best(res, metric = "roc_auc")
final_orf <-
  orf_wf |>
  finalize_workflow(best_orf) |>
  fit(data = df_prepared)

library(DALEXtra)

orf_explainer <- explain_tidymodels(final_orf, data = df_prepared[, -5], y = as.integer(df_prepared$victoria_lgl))

pdp_dado <- model_profile(
  orf_explainer,
  variables = "comienza_lgl",
  groups = c("raza_oponente"),
  type = "conditional"
)

pdp_dado$agr_profiles |>
  as_tibble() |>
  mutate(`_label_` = str_remove(`_label_`, "workflow_")) %>%
  ggplot(aes(`_x_`, `_yhat_`, color = `_label_`, group = `_label_`)) +
  geom_line(linewidth = 1.2, alpha = 0.8)

tree_spec <-
  decision_tree(cost_complexity = tune(), tree_depth = tune(), min_n = tune()) |>
  set_mode("classification") |>
  set_engine("rpart")

tree_wf <-
  workflow() |>
  add_recipe(rec) |>
  add_model(tree_spec)

res_tree <- tune_grid(
  tree_wf,
  resamples = boots,
  grid = 40
)

autoplot(res_tree)
best_tree <- select_by_one_std_err(res_tree, tree_depth, metric = "roc_auc")
final_tree <-
  tree_wf |>
  finalize_workflow(best_tree) |>
  fit(data = df_prepared)


library(rpart.plot)
rpart.plot(pull_workflow_fit(final_tree)$fit)

second_tree <-
  tree_wf |>
  finalize_workflow(show_best(res_tree, metric = "roc_auc", n = 30)[30, c(1:3, 9)]) |>
  fit(data = df_prepared)

rpart.plot(pull_workflow_fit(second_tree)$fit)
predict(second_tree, new_data = df_prepared)















