################################################################################
##
## == Advent of Code 2021 ==
## Day: 8
## Author: Jakob Dambon
##
################################################################################


# read in data
raw_lns <- read.table("08/input.txt", colClasses = "character", sep = "\n")$V1
raw_lns_test <- read.table("08/input_test.txt", colClasses = "character", sep = "\n")$V1


raw_lns_test

# -- part 1 -----
find1478 <- function(s) {
  n1478 <- sapply(strsplit(s, " | "), function(s_split) {
    sum(nchar(s_split[11 + 1:4]) %in% c(2:4, 7))
  })
  sum(n1478)
}

find1478(raw_lns_test)

find1478(raw_lns)

## -- part 2 -----
decode <- function(s) {
  LHS <- unlist(strsplit(s, " "))[1:10]
  RHS <- unlist(strsplit(s, " "))[11 + 1:4]
  
  key <- numeric(10)
  names(key) <- LHS
  
  # unique
  key[nchar(LHS) == 2] <- 1
  key[nchar(LHS) == 3] <- 7
  key[nchar(LHS) == 4] <- 4
  key[nchar(LHS) == 7] <- 8
  
  # length 6 (candidates for 0, 6, 9)
  n6 <- LHS[nchar(LHS) == 6]
  l_n6 <- strsplit(n6, "")
  let_n4 <- unlist(strsplit(LHS[nchar(LHS) == 4], ""))
  let_n2 <- unlist(strsplit(LHS[nchar(LHS) == 2], ""))
  
  b24 <- sapply(l_n6, function(s) {
    c(sum(let_n2 %in% s), sum(let_n4 %in% s))
  })
  
  key[ n6[b24[2, ] == 4] ] <- 9
  key[ n6[(b24[2, ] != 4) & (b24[1, ] == 2)] ] <- 0
  key[ n6[(b24[2, ] != 4) & (b24[1, ] == 1)] ] <- 6
  
  # length 5 (candidates for 2, 3, 5)
  n5 <- LHS[nchar(LHS) == 5]
  l_n5 <- strsplit(n5, "")

  b24 <- sapply(l_n5, function(s) {
    c(sum(let_n2 %in% s), sum(let_n4 %in% s))
  })
  
  key[ n5[b24[1, ] == 2] ] <- 3
  key[ n5[b24[2, ] == 2] ] <- 2
  key[ n5[(b24[1, ] == 1) & (b24[2, ] == 3)] ] <- 5
  
  key_splt <- strsplit(names(key), "")
  
  
  out <- sapply(RHS, function(s) {
    s_splt <- unlist(strsplit(s, ""))
    sapply(key_splt, function(k) {
      all(s_splt %in% k) & (length(k) == length(s_splt))
    })
  })
  
  num_out <- apply(out, 2, function(bb) {
    key[bb]
  })
  
  sum(num_out*(10^(3:0)))
}


sum(sapply(raw_lns_test, decode))
sum(sapply(raw_lns, decode))



