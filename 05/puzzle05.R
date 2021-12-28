################################################################################
##
## == Advent of Code 2021 ==
## Day: 5
## Author: Jakob Dambon
##
################################################################################


# read in data
raw_lns <- read.table("05/input.txt", colClasses = "character", sep = "\n")$V1
raw_lns_test <- read.table("05/input_test.txt", colClasses = "character", sep = "\n")$V1

analyze_lines <- function(lns, diag = FALSE) {
  # bring into matrix format
  mat_lns <- sapply(lns, function(tuple) {
    as.numeric(unlist(strsplit(strsplit(tuple, " ")[[1]][c(1, 3)], c(","))))
  }) + 1
  
  ln_type <- apply(mat_lns, 2, function(x) {
    if (x[1] == x[3]) {
      "v" 
    } else if (x[2] == x[4]) {
      "h"
    } else {"d"} 
      
  })
  
  vents <- matrix(0, nrow = max(mat_lns), ncol = max(mat_lns))
  
  for (id in 1:length(ln_type)) {
    if (ln_type[id] == "d") {
      # browser()
      if (diag) {
        xypos <- cbind(
          x = mat_lns[1, id]:mat_lns[3, id],
          y = mat_lns[2, id]:mat_lns[4, id]
        )
        for (i in 1:nrow(xypos)) {
          vents[xypos[i, 2], xypos[i, 1]] <- vents[xypos[i, 2], xypos[i, 1]] + 1
        }
      } else {
        next
      }
    }
    
    if (ln_type[id] == "h") {
      vents[mat_lns[2, id], (mat_lns[1, id]):(mat_lns[3, id])] <-
        vents[mat_lns[2, id], (mat_lns[1, id]):(mat_lns[3, id])] + 1
    }
    if (ln_type[id] == "v") {
      vents[(mat_lns[2, id]):(mat_lns[4, id]), mat_lns[1, id]] <-
        vents[(mat_lns[2, id]):(mat_lns[4, id]), mat_lns[1, id]] + 1
    }
  }
  vents
}

# -- part 1 ---
analyze_lines(raw_lns_test)

out <- analyze_lines(raw_lns)

sum(out > 1)

# -- part 2 ---
analyze_lines(raw_lns_test, diag = TRUE)
sum(analyze_lines(raw_lns_test, diag = TRUE) > 1)


out <- analyze_lines(raw_lns, diag = TRUE)
sum(out > 1)



