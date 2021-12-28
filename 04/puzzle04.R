################################################################################
##
## == Advent of Code 2021 ==
## Day: 4
## Author: Jakob Dambon
##
################################################################################

N <- 5

# read in data
sqn_test <- 
  c(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)
mat_brd_test <- as.matrix(read.table("04/input2_test.csv"))
l_brd_test <- lapply(1:(nrow(mat_brd_test)/N), function(i){
  mat_brd_test[N*(i-1)+1:N, ]
})



sqn <- unlist(read.csv("04/input1.csv", sep = ",", header = FALSE))
mat_brd <- as.matrix(read.table("04/input2.csv"))
l_brd <- lapply(1:(nrow(mat_brd)/N), function(i){
  mat_brd[N*(i-1)+1:N, ]
})




bingo_game <- function(sqn, brd, N = 5) {
  rnd <- 0
  WIN <- FALSE
  # find ending round 
  while (rnd <= length(sqn)) {
    rnd <-  rnd + 1
    # marked board
    mrk <- matrix(brd %in% sqn[1:rnd], nrow = N)
    # any row or column with all TRUE?
    if (any(c(apply(mrk, 2, all), apply(mrk, 1, all)))) {
      WIN <- TRUE
      break()
    }
  }
  # return
  c(
    # winning round,
    win_round = if (WIN) rnd else NA, 
    # points
    points = if (WIN) sum(brd[!mrk])*sqn[rnd] else NA
  )
}


# -- part 1 ----
(result_games <- sapply(l_brd, bingo_game, sqn = sqn, N = N))
result_games[, which.min(result_games[1, ])]

# -- part 2 ----
test_games <- sapply(l_brd_test, bingo_game, sqn = sqn_test, N = N)
test_games


result_games[, which.max(result_games[1, ])]
