################################################################################
##
## == Advent of Code 2021 ==
## Day: 21
## Author: Jakob Dambon
##
################################################################################

# initialize starting positions and score
pos <- c(4, 6)
score <- c(0, 0)
# count rounds
round <- 0
while(all(score<1000)) {
  val <- sum(ifelse((3*round+1:3)%%100 == 0, 100, (3*round+1:3)%%100))
  if (round%%2 == 0) {
    # player 1
    pos[1] <- ifelse((pos[1] + val)%%10 == 0, 10, (pos[1] + val)%%10)
    score[1] <- score[1] + pos[1]
    print(score[1])
  } else {
    # player 2
    pos[2] <- ifelse((pos[2] + val)%%10 == 0, 10, (pos[2] + val)%%10)
    score[2] <- score[2] + pos[2]
    print(score[2])
  }
  round <- round+1
}

# each round 3 dice roll
3*round*min(score)


## -- part 2 -----

# frequency table: sum value of rolled 3 3-sided dice
# Therefore, we do not have to "create" 27 universes for each round,
# but only use the summed values and use the frequencies to weight each instance
df <- data.frame(
  val = 3:9, freq = c(1, 3, 6, 7, 6, 3, 1)
)

# initialize starting positions and score
pos <- c(4, 6)
score <- c(0, 0)

# recursive function
dirac_die <- function(player, score, pos, val) {
  pos[player] <- if ((pos[player] + val)%%10 == 0) 10 else (pos[player] + val)%%10
  score[player] <- score[player] + pos[player]
  if (score[player] >= 21) {
    # game over
    n_wins <- numeric(2)
    n_wins[player] <- 1
    return(n_wins)
  } else {
    # next roll
    wins <- sapply(df$val, function(val) {
      dirac_die(if (player == 1) 2 else 1, score, pos, val)
    })
    return(apply(wins*matrix(rep(df$freq, 2), nrow = 2, byrow = T), 1, sum))
  }
}

wins <- sapply(df$val, function(val) {
  print(val)
  dirac_die(1, score, pos, val)
})

options(scipen=999)
# output
max(apply(wins*matrix(rep(df$freq, 2), nrow = 2, byrow = T), 1, sum))
