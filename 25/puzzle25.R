################################################################################
##
## == Advent of Code 2021 ==
## Day: 25
## Author: Jakob Dambon
##
################################################################################

## -- part 1 -----
# read in data as character matrix
mat <- as.matrix(Reduce(
  rbind, 
  strsplit(read.table("25/input.txt", header = FALSE)$V1, split = "")
))

# define functions for East and South moves
fn_east <- function(mat) {
  ids <- which(mat == ">", arr.ind = T)
  ids <- cbind(ids, e_nbh = 
                 ifelse(ids[, "col"] + 1 > ncol(mat), 1, ids[, "col"] + 1))
  temp <- mat
  for (i in 1:nrow(ids)) {
    if (mat[ids[i, "row"], ids[i, "e_nbh"]] == "." ) {
      temp[ids[i, "row"], ids[i, "e_nbh"]] <- ">"
      temp[ids[i, "row"], ids[i, "col"]] <- "."
    }
  }
  temp
}

fn_south <- function(mat) {
  ids <- which(mat == "v", arr.ind = T)
  ids <- cbind(ids, s_nbh = 
                 ifelse(ids[, "row"] + 1 > nrow(mat), 1, ids[, "row"] + 1))
  ids <- ids[order(ids[, "row"]), ]
  temp <- mat
  for (i in 1:nrow(ids)) {
    if (mat[ids[i, "s_nbh"], ids[i, "col"]] == "." ) {
      temp[ids[i, "s_nbh"], ids[i, "col"]] <- "v"
      temp[ids[i, "row"], ids[i, "col"]] <- "."
    }
  }
  temp
}

# start loop until no change is detected
i <- 0
while (TRUE) {
  mat_old <- mat
  # 1st move East, 2nd move South
  mat <- fn_south(fn_east(mat))
  i <- i+1
  if (all(mat == mat_old))
    break
}

## -- part 2 -----
