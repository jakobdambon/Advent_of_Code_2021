################################################################################
##
## == Advent of Code 2021 ==
## Day: 14
## Author: Jakob Dambon
##
################################################################################

test_seq <- unlist(strsplit("NNCB", ""))
test_instr <- strsplit(
  read.table("14/input_test.txt", sep = "\n", header = FALSE)$V1, 
  " -> "
)


polymer <- function(start_seq, n_step, instr) {
  poly <- start_seq
  
  for (i in 1:n_step) {
    print(i)
    j <- 1
    while (j < length(poly)) {
      for (k in 1:length(instr)) {
        if (paste0(poly[j:(j+1)], collapse = "") == instr[[k]][1]) {
          poly <- c(poly[1:j], instr[[k]][2], poly[(j+1):length(poly)])
          j <- j + 1
          break
        } 
      } # end for (checking instructions)
      j <- j + 1
    } # end while (loop through current polymer at step i)
  } # end for (i-th step)
  
  poly
}


out <- polymer(test_seq, 10, test_instr)
t_out <- table(out)
max(t_out) - min(t_out)

# final
start_seq <- unlist(strsplit("BNBBNCFHHKOSCHBKKSHN", ""))
l_instr <- strsplit(
  read.table("14/input.txt", sep = "\n", header = FALSE)$V1, 
  " -> "
)
out <- polymer(start_seq, 10, l_instr)
t_out <- table(out)
max(t_out) - min(t_out)

## -- puzzle 2 -----


polymer2 <- function(start_seq, n_step, instr) {
  # set up
  mat <- matrix(0, ncol = 26, nrow = 26)
  rownames(mat) <- LETTERS; colnames(mat) <- LETTERS
  # copy for further computation
  mat00 <- mat
  
  for (i in 1:(length(start_seq)-1)) {
    mat[start_seq[i], start_seq[i+1]] <- 
      mat[start_seq[i], start_seq[i+1]] + 1
  }
  
  # iterations
  for (i in 1:n_step) {
    print(i)
    mat0 <- mat00
    for (k in 1:length(instr)) {
      if (mat[substr(instr[[k]][1], 1, 1), substr(instr[[k]][1], 2, 2)] > 0) {
        mat0[substr(instr[[k]][1], 1, 1), instr[[k]][2]] <- 
          mat0[substr(instr[[k]][1], 1, 1), instr[[k]][2]] + 
          mat[substr(instr[[k]][1], 1, 1), substr(instr[[k]][1], 2, 2)]
        mat0[instr[[k]][2], substr(instr[[k]][1], 2, 2)] <- 
          mat0[instr[[k]][2], substr(instr[[k]][1], 2, 2)] + 
          mat[substr(instr[[k]][1], 1, 1), substr(instr[[k]][1], 2, 2)]
        mat[substr(instr[[k]][1], 1, 1), substr(instr[[k]][1], 2, 2)] <- 0
      } 
    } # end for (checking instructions)
    mat <- mat + mat0
  } # end for (i-th step)
  
  mat
}

mat <- polymer2(test_seq, 40, test_instr)
t_out <- apply(mat, 1, sum)
t_out[t_out > 0]
final <- max(t_out) - min(t_out)

mat <- polymer2(start_seq, 40, l_instr)
t_out <- apply(mat, 1, sum)
t_out <- t_out[t_out > 0]
max(t_out)
min(t_out)
