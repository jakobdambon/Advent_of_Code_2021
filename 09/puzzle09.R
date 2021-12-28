################################################################################
##
## == Advent of Code 2021 ==
## Day: 9
## Author: Jakob Dambon
##
################################################################################

test_mat <- matrix(c(
  2, 1, 9, 9, 9, 4, 3, 2, 1, 0,
  3, 9, 8, 7, 8, 9, 4, 9, 2, 1,
  9, 8, 5, 6, 7, 8, 9, 8, 9, 2,
  8, 7, 6, 7, 8, 9, 6, 7, 8, 9,
  9, 8, 9, 9, 9, 6, 5, 6, 7, 8
), nrow = 5, byrow = TRUE)


mat_loc_min <- function(mat) {
  m <- dim(mat)[1]
  n <- dim(mat)[2]
  
  out <- matrix(logical(n*m), nrow = m, ncol = n)
  for (i in 1:m)
    for (j in 1:n) {
      bool <- logical(4)
      # top
      bool[1] <- if (i > 1) {
        (mat[i, j] < mat[i-1, j])
      } else {
        TRUE
      }
      # right
      bool[2] <- if (j < n) {
        (mat[i, j] < mat[i, j+1])
      } else {
        TRUE
      }
      # bottom
      bool[3] <- if (i < m) {
        (mat[i, j] < mat[i+1, j])
      } else {
        TRUE
      }
      # left
      bool[4] <- if (j > 1) {
        (mat[i, j] < mat[i, j-1])
      } else {
        TRUE
      }
      
      out[i, j] <- all(bool)
    }
  
  out
}

lp_test <- mat_loc_min(test_mat)
sum(test_mat[lp_test] + 1)

import <- read.table("09/input.txt", colClasses = "character", sep = "\n")$V1
mat <- Reduce(rbind, lapply(strsplit(import, ""), as.numeric))

lp <- mat_loc_min(mat)
sum(mat[lp] + 1)
              


## -- part 2 -----

mat2G <- function(mat) {
  m <- dim(mat)[1]
  n <- dim(mat)[2]
  
  V <- cbind(
    expand.grid(r = 1:m, c = 1:n), 
    id = 1:(n*m), 
    value = as.vector(mat)
  )
  
  E <- matrix(0, nrow = 2*m*n - m - n, ncol = 2)
  k <- 1
  for ( i in 1:m ) 
    for ( j in 1:n ) {
      # bottom edge
      if ( i < m ) {
        E[k, ] <- (j-1)*m + i + 0:1
        k <- k + 1
      }
      # right edge
      if ( j < n ) {
        E[k, ] <- (j-1)*m + i + c(0, m)
        k <- k + 1
      }
    }
  print(k)
  list( V = V, E = E)
}

G <- mat2G(test_mat)
G <- mat2G(mat)

# cut graph
nine_id <- G$V$id[G$V$value == 9]
drop_edge <- apply(G$E, 1, function(ee) {
  any(ee %in% nine_id)
})

E_cut <- G$E[!drop_edge, ]

library(igraph)

G <- graph_from_edgelist(as.matrix(E_cut), directed = FALSE)
print_all(G)
comp_G <- components(G)

# product of largest 3 basins
prod(head(sort(comp_G$csize, decreasing = TRUE), 3))

