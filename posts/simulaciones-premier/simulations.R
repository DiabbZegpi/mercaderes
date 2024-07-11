razas <- c(
  "Caballero" = 30, "Faerie" = 42, "Dragón" = 37,
  "Olímpico" = 99, "Titán" = 51, "Héroe" = 23,
  "Defensor" = 28, "Desafiante" = 10, "Sombra" = 6,
  "Eterno" = 27, "Faraón" = 10, "Sacerdote" = 13
)

n_players <- sum(razas)
players <- data.frame(
  id = paste0("CL", 10001:(10000 + n_players)),
  name = randomNames::randomNames(n_players),
  raza = rep(names(razas), razas)
)

a_zone <- sample(1:300, size = n_players / 2, replace = FALSE)
players_a <- players[a_zone, ]
players_b <- players[-a_zone, ]

# Swiss format
make_swiss <- function(n_rounds, players, top) {
  if (nrow(players) %% 2 != 0) {
    players <- rbind(players, data.frame(id = "bye", name = "bye", raza = NA))
  }
  n_players <- nrow(players)
  overall_matches <- data.frame(
    round = integer(),
    player1 = character(),
    player2 = character(),
    result_player1 = integer(),
    result_player2 = integer(),
    points_player1 = integer(),
    points_player2 = integer()
  )

  initial_order <- sample(n_players, replace = FALSE)
  new_order <- players[initial_order, ]

  for (i in seq_len(n_rounds)) {
    # Pairings
    player1 <- new_order[1:(n_players / 2), ][["id"]]
    player2 <- new_order[(1 + n_players / 2):n_players, ][["id"]]
    results <- replicate(n_players / 2, sample(c(0, 1, 1, 2), size = 2)) |> t()
    result_player1 <- results[, 1]
    result_player2 <- results[, 2]
    if (any(player1 == "bye")) {
      bye_index <- which(player1 == "bye")
      result_player1[bye_index] <- 0
      result_player2[bye_index] <- 2
    } else if (any(player2 == "bye")) {
      bye_index <- which(player2 == "bye")
      result_player1[bye_index] <- 2
      result_player2[bye_index] <- 0
    }
    points_player1 <- ifelse(result_player1 > result_player2, 3, ifelse(result_player1 == result_player2, 1, 0))
    points_player2 <- ifelse(points_player1 == 3, 0, ifelse(points_player1 == 1, 1, 3))
    pairing <- data.frame(
      round = rep(i, n_players / 2),
      player1,
      player2,
      result_player1,
      result_player2,
      points_player1,
      points_player2
    )
    overall_matches <- rbind(overall_matches, pairing)
    standing <- data.frame(
      player = c(overall_matches$player1, overall_matches$player2),
      points = c(overall_matches$points_player1, overall_matches$points_player2)
    )
    standing <- aggregate(points ~ player, data = standing, FUN = sum)
    standing <- standing[order(standing$points, decreasing = TRUE), ]
    new_order <- data.frame(id = standing$player, points = standing$points)
  }

  final_standing <- merge(standing, players[, c(1, 3)], by.x = "player", by.y = "id", sort = FALSE)
  top_n <- final_standing[1:top, ]
  top_n
}

top_a <- make_swiss(5, players_a, 16)
top_b <- make_swiss(5, players_b, 16)

# Knockout tournament
player1 <- top_a$player
player2 <- top_b[order(top_b$points, decreasing = FALSE), ]$player
knockout_vec <- c(rbind(player1, player2))

make_knockout <- function(player_list) {
  knockout_df <- data.frame(
    round = integer(),
    player1 = character(),
    player2 = character(),
    winner = character(),
    loser = character()
  )
  ko_round <- 1L

  while (length(player_list) > 1) {
    winner <- replicate(length(player_list) / 2, sample(1:2, size = 1))
    loser <- ifelse(winner == 2, 1, 2)
    result <- ifelse(c(rbind(winner, loser)) == 2, TRUE, FALSE)

    result_cols <- data.frame(
      round = rep(ko_round, length(winner)),
      player1 = player_list[c(TRUE, FALSE)],
      player2 = player_list[c(FALSE, TRUE)],
      winner = player_list[result],
      loser = player_list[!result]
    )

    ko_round <- ko_round + 1L
    knockout_df <- rbind(knockout_df, result_cols)
    player_list <- player_list[result]
  }
  knockout_df
}



# Who's the champ?
champion <- knockout_df[max(knockout_df$round), ][["winner"]]
champ_race <- players[players$id == champion, ][["raza"]]



# Complete simulation -----------------------------------------------------
razas <- c(
  "Caballero" = 30, "Faerie" = 42, "Dragón" = 37,
  "Olímpico" = 99, "Titán" = 51, "Héroe" = 23,
  "Defensor" = 28, "Desafiante" = 10, "Sombra" = 6,
  "Eterno" = 27, "Faraón" = 10, "Sacerdote" = 13
)

n_players <- sum(razas)
players <- data.frame(
  id = paste0("CL", 10001:(10000 + n_players)),
  name = randomNames::randomNames(n_players),
  raza = rep(names(razas), razas)
)

a_zone <- sample(1:n_players, size = n_players / 2, replace = FALSE)
players_a <- players[a_zone, ]
players_b <- players[-a_zone, ]

n_simulations <- 10000L
champions <- character()
champion_races <- character()

top_32 <- data.frame(
  simul = integer(),
  player = character(),
  points = integer(),
  raza = character()
)

final_table <- data.frame(
  simul = integer(),
  round = integer(),
  player1 = character(),
  player2 = character(),
  winner = character(),
  loser = character()
)

for (i in seq_len(n_simulations)) {
  top_a <- make_swiss(5, players_a, 16)
  top_b <- make_swiss(5, players_b, 16)
  top_32_simul <- cbind(simul = rep(i, nrow(top_a) + nrow(top_b)), rbind(top_a, top_b))
  top_32 <- rbind(top_32, top_32_simul)

  player1 <- top_a$player
  player2 <- top_b[order(top_b$points, decreasing = FALSE), ]$player
  knockout_vec <- c(rbind(player1, player2))
  knockout_df <- make_knockout(knockout_vec)
  knockout_simul <- cbind(simul = rep(i, nrow(knockout_df)), knockout_df)
  final_table <- rbind(final_table, knockout_simul)

  champ <- knockout_df[max(knockout_df$round), ][["winner"]]
  champ_race <-  players[players$id == champ, ][["raza"]]
  champions <- c(champions, champ)
  champion_races <- c(champion_races, champ_race)

  if (i %% 500 == 0) cat(i, "simulations completed!\tThis time", champ_race, "won\n")
}


# Analysis ----------------------------------------------------------------

library(tidyverse)
top_32 <- as_tibble(top_32)
final_table <- as_tibble(final_table)
champion_tbl <- tibble(champ = champions, race = champion_races)

theme_set(theme_minimal(base_size = 14, base_family = "Arial"))

edition <- tibble(race = names(razas), edition = c(
  rep("Espada Sagrada", 3), rep("Helénica", 3), rep("Hijos de Daana", 3), rep("Dominios de Ra", 3)
)) |>
  mutate(edition = fct_inorder(edition))

champion_tbl |>
  left_join(edition, by = "race") |>
  count(edition, race) |>
  mutate(race = fct_reorder(race, n)) |>
  ggplot(aes(race, n, fill = edition)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("deepskyblue4", "firebrick", "darkgreen", "gold3")) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )
