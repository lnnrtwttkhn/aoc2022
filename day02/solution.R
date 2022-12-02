if (!requireNamespace("pacman")) install.packages("pacman")
packages_cran = c("here", "magrittr", "data.table")
pacman::p_load(char = packages_cran)
# part one
column_names = c("opponent", "me")
data <- read.table(here::here("day02", "data", "input.txt"), sep = ' ', col.names = column_names)
data_clean <- setDT(data) %>%
  .[, opponent := fcase(
    opponent == "A", "Rock",
    opponent == "B", "Paper",
    opponent == "C", "Scissors"
  )] %>%
  .[, me := fcase(
    me == "X", "Rock",
    me == "Y", "Paper",
    me == "Z", "Scissors"
    )]
data_score = data_clean %>%
  .[, shape_score := fcase(
    me == "Rock", 1,
    me == "Paper", 2,
    me == "Scissors", 3
  )] %>%
  .[, outcome_score := fcase(
    me == "Rock" & opponent == "Paper", 0,
    me == "Rock" & opponent == "Scissors", 6,
    me == "Paper" & opponent == "Rock", 6,
    me == "Paper" & opponent == "Scissors", 0,
    me == "Scissors" & opponent == "Rock", 0,
    me == "Scissors" & opponent == "Paper", 6,
    me == "Rock" & opponent == "Rock", 3,
    me == "Paper" & opponent == "Paper", 3,
    me == "Scissors" & opponent == "Scissors", 3
  )] %>%
  .[, round_score := shape_score + outcome_score]

print(sum(data_score$round_score))

# part two
column_names = c("opponent", "outcome")
data <- read.table(here::here("day02", "data", "input.txt"), sep = ' ', col.names = column_names)
data_clean <- setDT(data) %>%
  .[, opponent := fcase(
    opponent == "A", "Rock",
    opponent == "B", "Paper",
    opponent == "C", "Scissors"
  )] %>%
  .[, outcome := fcase(
    outcome == "X", "Lose",
    outcome == "Y", "Draw",
    outcome == "Z", "Win"
  )]
data_score = data_clean %>%
  .[, me := fcase(
    outcome == "Lose" & opponent == "Rock", "Scissors",
    outcome == "Lose" & opponent == "Paper", "Rock",
    outcome == "Lose" & opponent == "Scissors", "Paper",
    outcome == "Draw" & opponent == "Rock", "Rock",
    outcome == "Draw" & opponent == "Paper", "Paper",
    outcome == "Draw" & opponent == "Scissors", "Scissors",
    outcome == "Win" & opponent == "Rock", "Paper",
    outcome == "Win" & opponent == "Paper", "Scissors",
    outcome == "Win" & opponent == "Scissors", "Rock"
  )] %>%
  .[, shape_score := fcase(
    me == "Rock", 1,
    me == "Paper", 2,
    me == "Scissors", 3
  )] %>%
  .[, outcome_score := fcase(
    me == "Rock" & opponent == "Paper", 0,
    me == "Rock" & opponent == "Scissors", 6,
    me == "Paper" & opponent == "Rock", 6,
    me == "Paper" & opponent == "Scissors", 0,
    me == "Scissors" & opponent == "Rock", 0,
    me == "Scissors" & opponent == "Paper", 6,
    me == "Rock" & opponent == "Rock", 3,
    me == "Paper" & opponent == "Paper", 3,
    me == "Scissors" & opponent == "Scissors", 3
  )] %>%
  .[, round_score := shape_score + outcome_score]

print(sum(data_score$round_score))