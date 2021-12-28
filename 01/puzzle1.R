################################################################################
##
## == Advent of Code 2021 ==
## Day: 1
## Author: Jakob Dambon
##
################################################################################

# read in data
x <- as.numeric(read.csv("01/input.txt", header = FALSE)$V1)

# -- part 1 -----
# answer
sum(diff(x)>0)

# -- part 2 -----
triple_inc <- function(x) {
  n <- 0L
  for (i in 1:(length(x)-3)) {
    if (sum(x[i + 1:3]) > sum(x[i-1 + 1:3]))
      n <- n + 1L
  }
  n
} 

# check
x_test <- c(607, 618, 618, 617, 647, 716, 769, 792)
triple_inc(x_test) == 5

# answer
triple_inc(x)
