library(tidyverse)
library(googlesheets4)
library(gt)

gs4_auth(email = "diabbluis@gmail.com", cache = ".gargle")

data_path <- "https://docs.google.com/spreadsheets/d/1ZErNDZVSbIP8ekj-mmTpdmJ_Gbpo8yQaN5HeLBlqblE/edit?gid=1605468956#gid=1605468956"

resultados <- read_sheet(data_path, "resultados", col_types = "ciccii")
torneos <- read_sheet(data_path, "torneos", col_types = "ccDDcccccc")
jugadores <- read_sheet(data_path, "jugadores", col_types = "ccc")
ranking <- read_sheet(data_path, "ranking", col_types = "Dcd")
torneos_computados <- read_sheet(data_path, "torneos computados", col_types = "cD")

# Filtros
torneos <- torneos |> filter(id_torneo %in% torneos_computados$id_torneo)
resultados <- resultados |> filter(id_torneo %in% torneos_computados$id_torneo)

# Separar sub-razas druida, goblin y minotauro
torneos <-
  torneos |>
  mutate(
    raza = str_remove(raza, ".+ / "),
    raza = str_to_title(raza),
    raza = replace_na(raza, "")
  )

df <-
  resultados |>
  left_join(
    torneos |> select(id_torneo, fecha, tor, raza),
    by = join_by(id_torneo, tor_a == tor),
    relationship = "many-to-one"
  ) |>
  left_join(
    torneos |> select(id_torneo, tor, raza),
    by = join_by(id_torneo, tor_b == tor),
    relationship = "many-to-one",
    suffix = c("_a", "_b")
  ) |>
  filter(fecha > "2024-11-05") |>
  left_join(
    ranking,
    by = join_by(tor_a == tor, closest(fecha <= version)),
    relationship = "many-to-one"
  ) |>
  left_join(
    ranking,
    by = join_by(tor_b == tor, closest(fecha <= version)),
    relationship = "many-to-one",
    suffix = c("_a", "_b")
  ) |>
  select(matches("_[ab]"), -contains("version"), -contains("tor")) |>
  filter(raza_a != raza_b, raza_a != "", raza_b != "")

rs <-
  df |>
  relocate(2, 1, 4, 3, 6, 5) |>
  rename_with(\(x) colnames(df)) |>
  bind_rows(df) |>
  mutate(
    match = case_when(
      resultado_a > resultado_b ~ 1,
      resultado_a < resultado_b ~ 0,
      resultado_a == resultado_b ~ 0.5
    ),
    nivel = case_when(
      elo_a <= 1350 ~ "novato",
      elo_a <= 1427 ~ "intermedio",
      elo_a <= 1550 ~ "avanzado",
      .default = "experto"
    ),
    nivel = fct_relevel(nivel, c("experto", "avanzado", "intermedio", "novato"))
  ) |>
  group_by(nivel, raza = raza_a) |>
  summarize(wr = mean(match), sample = n(), .groups = "drop") |>
  filter(sample >= 100)

rs |>
  filter(raza != "Defensor") |>
  slice_max(wr, by = nivel, n = 3, with_ties = FALSE)

rs |>
  # filter(raza != "Defensor") |>
  slice_min(wr, by = nivel, n = 3, with_ties = FALSE)


















