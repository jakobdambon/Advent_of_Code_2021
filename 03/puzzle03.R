################################################################################
##
## == Advent of Code 2021 ==
## Day: 3
## Author: Jakob Dambon
##
################################################################################


# read in data
dgn <- read.table("03/input.txt", colClasses = "character")$V1

n <- length(dgn)

mat_dgn <- t(sapply(strsplit(dgn, split = ""), function(b) {
  as.logical(as.numeric(b))
}))

# -- part 1 -----

b_gamma <- (apply(mat_dgn, 2, sum) > n/2)
b_eps <- !b_gamma

gamma <- strtoi(
  paste0(as.character(as.numeric(b_gamma)), collapse = ""),
  base = 2
)

eps <- strtoi(
  paste0(as.character(as.numeric(b_eps)), collapse = ""),
  base = 2
)

eps*gamma


# -- part 2 -----

life_rat <- function(mat_dgn, least_common = FALSE) {
  iter_mat <- mat_dgn
  j <- 1
  while (!is.vector(iter_mat)) {
    pos <- if (least_common) !iter_mat[, j] else iter_mat[, j]
    pos <- if (sum(iter_mat[, j]) >= nrow(iter_mat)/2) {
      pos
    } else {
      !pos
    }
    iter_mat <- iter_mat[pos, ]
    j <- j+1
  }
  iter_mat
}

dgn_test <- c(
  "00100",
  "11110",
  "10110",
  "10111",
  "10101",
  "01111",
  "00111",
  "11100",
  "10000",
  "11001",
  "00010",
  "01010"
)
mat_dgn_test <- t(sapply(strsplit(dgn_test, split = ""), function(b) {
  as.logical(as.numeric(b))
}))

b_oxy_rat <- life_rat(mat_dgn)
b_co2_rat <- life_rat(mat_dgn, TRUE)

oxy_rat <- strtoi(
  paste0(as.character(as.numeric(b_oxy_rat)), collapse = ""),
  base = 2
)

co2_rat <- strtoi(
  paste0(as.character(as.numeric(b_co2_rat)), collapse = ""),
  base = 2
)

oxy_rat*co2_rat

