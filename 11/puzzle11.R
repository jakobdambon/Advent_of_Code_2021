################################################################################
##
## == Advent of Code ==
## Day: 11
## Author: Jakob Dambon
##
################################################################################

test_mat <- matrix(
  c(
    5, 4, 8, 3, 1, 4, 3, 2, 2, 3,
    2, 7, 4, 5, 8, 5, 4, 7, 1, 1,
    5, 2, 6, 4, 5, 5, 6, 1, 7, 3,
    6, 1, 4, 1, 3, 3, 6, 1, 4, 6,
    6, 3, 5, 7, 3, 8, 5, 4, 7, 8,
    4, 1, 6, 7, 5, 2, 4, 6, 4, 5,
    2, 1, 7, 6, 8, 4, 1, 7, 2, 1,
    6, 8, 8, 2, 8, 8, 1, 1, 3, 4,
    4, 8, 4, 6, 8, 4, 8, 5, 5, 4,
    5, 2, 8, 3, 7, 5, 1, 5, 2, 6
  ), 
  ncol = 10, nrow = 10, byrow = TRUE
)

mat_energy_up <- function(mat, id) {
  n <- ncol(mat)
  mat01 <- matrix(0, ncol = n, nrow = n)
  mat01[abs(1:n - id[1]) <= 1, abs(1:n - id[2]) <= 1] <- 1
  
  mat + mat01
}

octopus_flashes <- function(mat, steps, trace, all_flash = FALSE) {
  n_flashes <- 0
  for (i in 1:steps) {
    mat_flashed <- matrix(FALSE, ncol = ncol(mat), nrow = nrow(mat))
    # increase all energy level by one
    mat <- mat + 1
    # lighten up
    while (sum(mat_flashed) < sum(mat>9)) {
      mat_fl_ids <- which((mat > 9) & !mat_flashed, arr.ind = TRUE)
      for (j in 1:nrow(mat_fl_ids)) {
        mat <- mat_energy_up(mat, mat_fl_ids[j, ])
        mat_flashed[mat_fl_ids[j, 1], mat_fl_ids[j, 2]] <- TRUE
      }
    } 
    # set to zero
    mat[mat_flashed] <- 0
    
    n_flashes <- n_flashes + sum(mat_flashed)
    
    if (trace) {
      print(paste0("Step ", i, ": ", sum(mat_flashed), 
                   "flashed in this step. New energy levels:"))
      print(mat)
    }
    
    if (all_flash) {
      if (sum(mat_flashed) == (ncol(mat))^2) {
        print(paste0("Step ", i, ": ", sum(mat_flashed), 
                     " (all) octopus flashed for the first time."))
        return()
      }
    }
  }
  n_flashes
}

octopus_flashes(test_mat, 100, trace = FALSE)


mat <- matrix(c(
  6, 6, 1, 7, 1, 1, 3, 5, 8, 4,
  6, 5, 4, 4, 2, 1, 8, 6, 3, 8,
  5, 4, 5, 7, 3, 3, 1, 4, 8, 8,
  1, 1, 3, 5, 6, 7, 5, 5, 8, 7,
  1, 2, 2, 1, 3, 5, 3, 2, 1, 6,
  1, 8, 1, 1, 1, 2, 4, 3, 7, 8,
  1, 3, 8, 7, 8, 6, 4, 3, 6, 8,
  4, 4, 2, 7, 6, 3, 7, 2, 6, 2,
  6, 7, 7, 8, 6, 4, 5, 4, 8, 6,
  3, 6, 8, 2, 1, 4, 6, 7, 4, 5
), ncol = 10, nrow = 10, byrow = TRUE)

octopus_flashes(mat, 100, trace = FALSE)

# -- part 2 ----
octopus_flashes(test_mat, 999, trace = FALSE, all_flash = TRUE)
octopus_flashes(mat, 999, trace = FALSE, all_flash = TRUE)
