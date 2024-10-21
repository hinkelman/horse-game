rolls_df = expand.grid(Dice1 = 1:6, Dice2 = 1:6) |> 
  dplyr::mutate(Roll = Dice1 + Dice2)  |> 
  dplyr::count(Roll) |> 
  dplyr::mutate(Roll = as.factor(Roll),
                Prob = n/sum(n),
                Steps = c(3, 6, 8, 11, 14, 16, 14, 11, 8, 6, 3),
                StepsProb = Steps/sum(Steps))

roll <- function(n, rdf = rolls_df){
  sample(rdf$Roll, size = n, replace = TRUE, prob = rdf$Prob)
}

get_scratches <- function(){
  scratches = c()
  while(length(scratches) < 4){
    scratches = unique(c(scratches, roll(1)))
  }
  scratches
}

get_kitty <- function(base_value, scratches, rolls){
  out = 4 * base_value * (4 + 3 + 2 + 1)
  if (length(rolls) > 0){
    vals = sapply(rolls, function(x){
      ind = which(scratches == x)
      val = if (length(ind) > 0) ind * base_value else 0})
    out = out + sum(vals)
  }
  out
}

get_prob <- function(scratches, rolls, rdf = rolls_df){
  out = data.frame(Roll = as.factor(2:12), Freq = 0) |> 
    dplyr::bind_rows(table(rolls) |> 
                       as.data.frame() |> 
                       dplyr::rename(Roll = rolls)) |> 
    dplyr::filter(!(Roll %in% scratches)) |> 
    dplyr::group_by(Roll) |> 
    dplyr::summarise(Freq = sum(Freq, na.rm = TRUE)) |> 
    dplyr::left_join(dplyr::select(rdf, Roll, Prob, Steps),
                     by = dplyr::join_by(Roll)) |> 
    dplyr::rename(Horse = Roll) |> 
    dplyr::mutate(StepsRemain = Steps - Freq,
                  Prob = Prob^StepsRemain,
                  WinProb = Prob/sum(Prob)) |> 
    dplyr::arrange(dplyr::desc(WinProb))
  if (out$StepsRemain[1] == 0) out$WinProb = c(1, rep(0, nrow(out) - 1))
  out
}

sim_game <- function(base_value){
  scratches = get_scratches()
  rolls = roll(1)
  game = get_prob(scratches, rolls)
  while(min(game$StepsRemain) > 0){
    rolls = c(rolls, roll(1))
    game = get_prob(scratches, rolls)
  }
  data.frame(Rolls = length(rolls),
             Kitty = get_kitty(base_value, scratches, rolls), 
             Winner = game$Horse[game$StepsRemain == 0])
} 

