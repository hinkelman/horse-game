library(dplyr)
library(ggplot2)

source("functions.R")

# code is not optimized
# 1000 reps will take many minutes
reps = 1000  
base_value = 0.25

sim_list = list()
for (i in 1:reps){
  cat("\rRep", i, "of", reps)
  sim_list[[i]] = sim_game(base_value)
}
sim_df = bind_rows(sim_list, .id = "Sim")
saveRDS(sim_df, "sim_df.rds")

ggplot(sim_df, aes(x = Kitty)) +
  geom_histogram(alpha = 0.6, color = "black", binwidth = 2) +
  facet_wrap(~ Winner)
