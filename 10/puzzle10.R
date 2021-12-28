################################################################################
##
## == Advent of Code ==
## Day: 10
## Author: Jakob Dambon
##
################################################################################

test <- read.table("10/input_test.txt", header = FALSE, sep = "\n")$V1


bracket_syntax <- function(s, completion = FALSE) {
  bra_mat <- matrix(
    c("(", ")", "[", "]", "{", "}", "<", ">"), 
    ncol = 2, byrow = TRUE
  )
  stop_at <- NULL
  exp_bracket <- character(0)
  for (i in 1:nchar(s)) {
    ch <- substr(s, i, i)
    if (any(bra_mat[, 1] == ch)) {
      exp_bracket <- c(exp_bracket, bra_mat[bra_mat[, 1] == ch, 2])
    } else {
      if (tail(exp_bracket, 1) == ch) {
        exp_bracket <- exp_bracket[-length(exp_bracket)]
      } else {
        stop_at <- i
        break
      }
    }
  }
  
  
  if (completion) {
    # here to complete string
    if (is.null(stop_at)) {
      pen <- 0
      for (i in length(exp_bracket):1) {
        pen <- 5*pen + switch(exp_bracket[i],
          ")" = 1,
          "]" = 2,
          "}" = 3,
          ">" = 4             
        )
      }
      return(pen)
    } else {
      return(NA)
    }
  } else {
    # here to compute penalty of syntax error
    if (is.null(stop_at)) {
      return(0)
    } else {
      return(
        switch(substr(s, stop_at, stop_at),
               ")" = 3, 
               "]" = 57, 
               "}" = 1197, 
               ">" = 25137
        )
      )
    }
  }
  
}





sum(sapply(test, bracket_syntax))

real <- read.table("10/input.txt", header = FALSE, sep = "\n")$V1
sum(sapply(real, bracket_syntax))


# -- puzzle 2 ----
median(sapply(test, bracket_syntax, completion = TRUE), na.rm = TRUE)

median(sapply(real, bracket_syntax, completion = TRUE), na.rm = TRUE)

