horse_colors = c(
  "2" = "#a6cee3",
  "12" = "#1f78b4",
  "3" = "#b2df8a",
  "11" = "#33a02c",
  "4" = "#fb9a99",
  "10" = "#e31a1c",
  "5" = "#fdbf6f",
  "9" = "#ff7f00",
  "6" = "#cab2d6",
  "8" = "#6a3d9a",
  "7" = "#b15928"
)

rolls_df = expand.grid(dice1 = 1:6, dice2 = 1:6) |>
  dplyr::mutate(roll = dice1 + dice2) |>
  dplyr::count(roll) |>
  dplyr::mutate(
    steps = c(3, 6, 8, 11, 14, 16, 14, 11, 8, 6, 3),
    prob = n / sum(n),
    prob_steps = steps / sum(steps)
  )

roll <- function(n, replace = TRUE, prob = rolls_df$prob) {
  # sample.int is faster than sample
  sample.int(11, size = n, replace = replace, prob = prob) + 1L
}

get_kitty <- function(base_value, scratches, rolls = NULL) {
  init = 4 * base_value * (4 + 3 + 2 + 1) # multiply by 4 for 4 suits in deck
  vals = NULL
  if (!is.null(rolls) && length(rolls) > 0) {
    vals = sapply(rolls, function(x) {
      mult = which(scratches == x) # index in scratches indicates multiplier
      if (length(mult) > 0) mult * base_value else 0
    })
  }
  sum(c(init, vals))
}

get_steps_remain <- function(scratches, rolls, rdf = rolls_df) {
  rolls = factor(rolls, levels = 2:12)
  counts = 0
  if (length(rolls) > 0) {
    counts = unclass(unname(table(rolls)))
  }
  steps_remain = setNames(rdf$steps - counts, as.character(2:12))
  steps_remain[as.character(scratches)] = NA
  steps_remain
}

sim_one_game <- function(scratches, rolls, winner_only = TRUE) {
  steps_remain = get_steps_remain(scratches, rolls)
  active = !is.na(steps_remain)
  sr = steps_remain
  if (any(sr[active] < 0)) {
    stop("Winner was already determined")
  }
  # conservative choice; highly unlikely to need 200 rolls to finish a game
  sim_rolls_pool = roll(200)
  i = 1
  while (all(sr[active] > 0)) {
    r = sim_rolls_pool[i] # integer 2–12
    i = i + 1
    idx = r - 1L # integer 1–11
    if (active[idx]) {
      sr[idx] = sr[idx] - 1L
    }
  }
  winner = names(which(sr == 0))
  if (winner_only) {
    winner
  } else {
    sim_rolls = sim_rolls_pool[seq_len(i - 1)] # trim to actual length
    list("winner" = winner, "rolls" = c(rolls, sim_rolls))
  }
}

# simulates win probability from any point in game (based on rolls)
sim_win_prob <- function(scratches, rolls = NULL, n_sim = 1000) {
  winners = replicate(n_sim, {
    sim_one_game(scratches, rolls)
  }) |>
    factor(levels = as.character(2:12))
  table(winners) / n_sim
}

# returns winner and kitty
# always starting at beginning of game
sim_new_game <- function(scratches, base_value = 0.25) {
  sim = sim_one_game(scratches, rolls = NULL, winner_only = FALSE)
  list(
    "winner" = sim$winner,
    "kitty" = get_kitty(base_value, scratches, sim$rolls)
  )
}
