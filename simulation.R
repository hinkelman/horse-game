library(progress)
library(dplyr)
library(ggplot2)

source("functions.R")

reps = 10000  
base_value = 0.25

sim_list = list()
pb <- progress_bar$new(total = reps)
for (i in 1:reps){
  pb$tick()
  sim_list[[i]] = sim_game(base_value)
}
sim_df = bind_rows(sim_list, .id = "Sim") |> 
  mutate(Winner = factor(Winner, levels = c(2, 12, 3, 11, 4, 10, 5, 9, 6, 8, 7)))

sim_df |> 
  count(Winner) |> 
  mutate(Percent = round(n/sum(n) * 100))

kitty_summ = sim_df |> 
  group_by(Winner) |> 
  summarise(Mean = mean(Kitty))
  
ggplot(left_join(sim_df, winners)) +
  geom_histogram(aes(x = Kitty), fill = "grey90", color = "black", binwidth = 2) +
  geom_vline(xintercept = mean(sim_df$Kitty), color = "grey50") +
  geom_vline(data = kitty_summ, aes(xintercept = Mean), color = "grey50", linetype = "dashed") +
  labs(x = "Kitty", y = "Number of Games") +
  scale_x_continuous(breaks = seq(10, 50, 4)) +
  facet_wrap(~ Winner, ncol = 2) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
