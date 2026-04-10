library(dplyr)
library(ggplot2)
library(ggridges)

source("functions.R")

reps = 1:100000
base_value = 0.25

sim_list = lapply(
  cli::cli_progress_along(reps, "Simulating"),
  function(i) {
    sim_new_game(scratches = roll(4, FALSE), base_value)
  }
)

sim_df = data.frame(
  sim = reps,
  winner = sapply(sim_list, `[[`, "winner"),
  kitty = sapply(sim_list, `[[`, "kitty")
) |>
  # change factor levels to reflect paired probabilities
  mutate(
    winner = factor(winner, levels = rev(c(2, 12, 3, 11, 4, 10, 5, 9, 6, 8, 7)))
  )

winners <- sim_df |>
  count(winner) |>
  mutate(percent = round(n / sum(n) * 100, 2))

sim_df |>
  group_by(winner) |>
  summarise(kitty_avg = mean(kitty))

ggplot(
  left_join(sim_df, winners),
  aes(x = kitty, y = winner, height = after_stat(count), fill = winner)
) +
  geom_density_ridges(
    stat = "density",
    color = NA,
    scale = 2
  ) +
  labs(x = "Kitty", y = "Winner") +
  scale_fill_manual(values = horse_colors, guide = "none") +
  scale_x_continuous(breaks = seq(10, 70, 10)) +
  coord_cartesian(xlim = c(10, 50)) +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
ggsave("kitty-ggridges.png", width = 5, height = 6)
