################################################################################
##
## == Advent of Code 2021 ==
## Day: 20
## Author: Jakob Dambon
##
################################################################################

# read in data
input <- read.table( "20/input.txt", 
  header = FALSE, sep = "\n", colClasses = "character", comment.char = "%"
)$V1
# transform image to logical matrix
input_image <- matrix(
  strsplit(
    paste0(input[-1], collapse = ""), split = ""
  )[[1]] == "#", 
  nrow = length(input)-1, byrow = TRUE
)
# image enhancement algorithm (look-up)
iea <- strsplit(input[1], split = "")[[1]] == "#"
# algorithm
img_enh_alg <- function(img, iea, n_iter) {
  n_mat <- nrow(img)
  # initialize "infinite"-dimensional matrix 
  # (in correspondence to required number of steps)
  mat <- matrix(FALSE, 
                nrow = n_mat+2*(n_iter+1),
                ncol = n_mat+2*(n_iter+1))
  mat[n_iter+1 + 1:n_mat, n_iter+1 + 1:n_mat] <- img
  for (iter in 1:n_iter) {
    # depending on the first iea-entry
    # [...]
    # [...] => # or .
    # [...]
    # this is needed for the "infinite" part
    temp <- matrix(
      if (head(iea, 1)) {
        if (tail(iea, 1)) {
          TRUE
        } else {
          (iter%%2 == 1)
        }
      } else {
        FALSE
      }, 
       nrow = n_mat+2*(n_iter+1),
       ncol = n_mat+2*(n_iter+1))
    # go through each entry and compute updated values
    for (i in 2:(nrow(mat)-1)) {
      for (j in 2:(nrow(mat)-1)) {
        sub_img <- mat[i + c(-1, 0, 1), j+c(-1, 0, 1)]
        code <- sum(as.vector(t(sub_img)) * 2^(8:0))
        temp[i, j] <- iea[code+1]
      }
    }
    mat <- temp
  }
  mat
}

out <- img_enh_alg(input_image, iea, n_iter = 2)
sum(out)

## -- part 2 -----

out <- img_enh_alg(input_image, iea, n_iter = 50)
sum(out)
