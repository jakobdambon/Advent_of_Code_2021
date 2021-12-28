################################################################################
##
## == Advent of Code 2021 ==
## Day: 15
## Author: Jakob Dambon
##
################################################################################

test_cave <- matrix(c(
  1, 1, 6, 3, 7, 5, 1, 7, 4, 2,
  1, 3, 8, 1, 3, 7, 3, 6, 7, 2,
  2, 1, 3, 6, 5, 1, 1, 3, 2, 8,
  3, 6, 9, 4, 9, 3, 1, 5, 6, 9,
  7, 4, 6, 3, 4, 1, 7, 1, 1, 1,
  1, 3, 1, 9, 1, 2, 8, 1, 3, 7,
  1, 3, 5, 9, 9, 1, 2, 4, 2, 1,
  3, 1, 2, 5, 4, 2, 1, 6, 3, 9,
  1, 2, 9, 3, 1, 3, 8, 5, 2, 1,
  2, 3, 1, 1, 9, 4, 4, 5, 8, 1
), ncol = 10, nrow = 10, byrow = TRUE)


test_cave

# 1st attempt. Faster, but does not get the correct value for part 2.
sum_risk_path <- function(cave, trace = TRUE) {
  n <- ncol(cave)
  ids <- expand.grid(
    r = 1:n, c = 1:n
  )
  sum_risk <- matrix(NA, ncol = n, nrow = n)
  sum_risk[1, 1] <- 0
  
  for (d in 1:(2*(n-1))) {
    sel_ids <- ids[ids$r+ids$c == d+2, ]
    nb_val <- apply(sel_ids, 1, function(id) {
      # 1st is left, 2nd is top
      nb_id <- matrix(rep(id, 2), ncol = 2, byrow = T) + 
        matrix(c(0, -1, -1, 0), ncol = 2, nrow = 2)
      c(
        # left val
        if (all(nb_id[1, ]>0)) sum_risk[nb_id[1, 1], nb_id[1, 2]] else NA,
        # top val
        if (all(nb_id[2, ]>0)) sum_risk[nb_id[2, 1], nb_id[2, 2]] else NA
      )
    })
    min_nb_val <- apply(nb_val, 2, min, na.rm = TRUE)
    for (i in 1:nrow(sel_ids)) {
      sum_risk[sel_ids[i, 1], sel_ids[i, 2]] <- 
        cave[sel_ids[i, 1], sel_ids[i, 2]] + min_nb_val[i]
    }
    if (trace) print(sum_risk)
  }
  sum_risk
}

# 2nd attempt. Quite slow, but retrieves correct value
get_nbhs <- function(id, sum_risk) {
  ids <- matrix(rep(id, 4), byrow = T, ncol = 2) +
    matrix(c(-1, 1, 0, 0, 0, 0, -1, 1), nrow = 4)
  # within subscript range and with NA in sum_risk
  valid_nbds <- apply(ids, 1, function(x) {
    if((x[1]>=1) & (x[1]<=nrow(sum_risk)) &
       (x[2]>=1) & (x[2]<=nrow(sum_risk))) {
      (is.na(sum_risk[x[1], x[2]]))
    } else {
      FALSE
    }
  })
  if (any(valid_nbds))
    return(matrix(ids[valid_nbds, ], ncol = 2))
  else 
    return(NULL)
}

library(tidyr)
library(dplyr)


sum_risk_path <- function(cave, trace = TRUE) {
  # initialize
  n <- ncol(cave)
  # matrix to fill in
  sum_risk <- matrix(NA, ncol = n, nrow = n)
  # starting position
  sum_risk[1, 1] <- 0
  mat_nbh <- as.data.frame(rbind(
    c(1, 2, cave[1, 2]),
    c(2, 1, cave[2, 1])
  ))
  colnames(mat_nbh) <- c("row", "col", "SR")
  # order by lowest risk, 1st col = row, 2nd col = col, 3rd col = sum_risk
  mat_nbh <- mat_nbh[order(mat_nbh[, 3]),]
  
  # Djistrka-like algorithm until lower right corner is reached
  while (is.na(sum_risk[n, n])) {
    print(mat_nbh[1, ])
    # fill in current index with lowest risk
    sum_risk[as.numeric(mat_nbh[1, 1]),
             as.numeric(mat_nbh[1, 2])] <- 
      as.numeric(mat_nbh[1, 3])
    # add all nbhs to list
    new_nbds <- get_nbhs(as.numeric(mat_nbh[1, 1:2]), sum_risk)
    
    
    # add new candidates to list
    if (is.null(new_nbds)) {
      mat_nbh <- mat_nbh[-1,]
    } else {
      df_new <- as.data.frame(cbind(new_nbds, 
                                    apply(new_nbds, 1, function(x) {
                                      sum_risk[as.numeric(mat_nbh[1, 1]), 
                                               as.numeric(mat_nbh[1, 2])] + 
                                        cave[x[1], x[2]]
                                    })))
      colnames(df_new) <- c("row", "col", "SR")
      mat_nbh <- rbind(mat_nbh[-1,], df_new)
    }
    
    
    # drop ids with multiple sum_risk (take min)
    mat_nbh <- mat_nbh %>% 
      group_by(row, col) %>% 
      arrange(SR) %>%
      slice(1) %>% 
      ungroup() %>% 
      arrange(SR)
    
  }
  sum_risk
}


sum_risk_path(test_cave)

## -- part 1 -----
cave <- t(sapply(
  read.table("15/input.txt", header = F, sep = "", colClasses = "character")$V1,
  function(x) {
    as.numeric(strsplit(as.character(x), "")[[1]])
  }))
row.names(cave) <- NULL

sum_path <- sum_risk_path(cave, trace = FALSE)
sum_path[100, 100]

## -- part 2 -----
# creates an N by N cave using block-matrix copies
n_by_n_cave <- function(cave, N = 5) {
  n <- nrow(cave)
  ids <- expand.grid(R = 0:(N-1), C = 0:(N-1))
  great_cave <- matrix(0, nrow = N*n, ncol = N*n)
  for (i in 1:nrow(ids)) {
    temp <- cave + sum(ids[i, ])
    temp[temp>9] <- temp[temp>9]-9
    great_cave[n*ids[i, 1]+1:n, n*ids[i, 2]+1:n] <- temp
  }
  great_cave
}

great_test_cave <- n_by_n_cave(test_cave)
test_sum_path <- sum_risk_path(great_test_cave, trace = FALSE)
test_sum_path[50, 50]


great_cave <- n_by_n_cave(cave)
sum_path <- sum_risk_path(great_cave, trace = FALSE)
sum_path[500, 500]

