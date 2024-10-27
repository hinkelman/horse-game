rolls_df = expand.grid(Dice1 = 1:6, Dice2 = 1:6) |> 
  dplyr::mutate(Roll = Dice1 + Dice2)  |> 
  dplyr::count(Roll) |> 
  # by making Roll a factor, table (see below) reports zero for values that haven't been rolled yet
  dplyr::mutate(Roll = as.factor(Roll),
                Prob = n/sum(n),
                Steps = c(3, 6, 8, 11, 14, 16, 14, 11, 8, 6, 3),
                StepsProb = Steps/sum(Steps))

roll <- function(n, replace = TRUE, rdf = rolls_df){
  sample(rdf$Roll, size = n, replace = replace, prob = rdf$Prob)
}

get_kitty <- function(base_value, scratches, rolls = NULL){
  init = 4 * base_value * (4 + 3 + 2 + 1) # multiply by 4 for 4 suits in deck
  vals = NULL
  if (!is.null(rolls) & length(rolls) > 0){
    vals = sapply(rolls, function(x){
      ind = which(scratches == x)
      val = if (length(ind) > 0) ind * base_value else 0})
  }
  cumsum(c(init, vals))
}

calc_win_prob <- function(roll_prob, steps_remain, scratches){
  probs = roll_prob^steps_remain
  # horse numbers range from 2:12 so need to sutract one to get index
  probs[as.numeric(scratches) - 1] = 0
  probs_scaled = probs/sum(probs)
  if (min(steps_remain) < 1) probs_scaled = ifelse(probs_scaled == max(probs_scaled), 1, 0)
  probs_scaled
}

get_win_prob <- function(scratches, rolls = NULL, rdf = rolls_df){
  if (!is.null(rolls) & length(rolls) > 0){
    counts = unclass(unname(table(rolls)))
    steps_remain = rdf$Steps - counts
  } else {
    steps_remain = rdf$Steps
  }
  data.frame(Horse = as.factor(2:12),
             StepsRemain = steps_remain,
             WinProb = calc_win_prob(rdf$Prob, steps_remain, scratches))
}

sim_game <- function(base_value){
  scratches = roll(4, replace = FALSE)
  rolls = roll(1)
  wp = get_win_prob(scratches, rolls)
  while(min(wp$StepsRemain) > 0){
    rolls = c(rolls, roll(1))
    wp = get_win_prob(scratches, rolls)
  }
  data.frame(Rolls = length(rolls),
             Kitty = max(get_kitty(base_value, scratches, rolls)), 
             Winner = wp$Horse[wp$StepsRemain == 0])
} 

