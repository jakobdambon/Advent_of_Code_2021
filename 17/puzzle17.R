################################################################################
##
## == Advent of Code 2021 ==
## Day: 17
## Author: Jakob Dambon
##
################################################################################


target_area <- c(155, 215, -132, -72)
target_area <- c(20, 30, -10, -5)

traj <- function(velo, target_area) {
  i <- 1
  pos <- matrix(0, ncol = 3, nrow = 1)
  while ((pos[i, 1] <= target_area[2]) & 
         (pos[i, 2] >= target_area[3])) {
    
    new_pos <- pos[i, 1:2] + velo
    in_target <- (
      (new_pos[1] >= target_area[1]) &
      (new_pos[1] <= target_area[2]) &
      (new_pos[2] >= target_area[3]) &
      (new_pos[2] <= target_area[4])
    )
    pos <- rbind(pos, c(new_pos, in_target))
    
    velo[1] <- if (velo[1] > 0) {
      velo[1] - 1
    } else if (velo[1] < 0) {
      velo[1] + 1
    } else {0}
    velo[2] <- velo[2] -1
    
    i <- i + 1
  }
  pos
}

traj(c(17, -4), target_area = target_area)


init_velo <- expand.grid(0:100, (-100):100)

max_y <- apply(init_velo, 1, function(v) {
  traj_v <- traj(v, target_area)
  if (any(traj_v[, 3] == 1)) {
    max(traj_v[, 2])
  } else {
    NA
  }
})

max(max_y, na.rm = T)

## -- part 2 -----
# trial and error
init_velo <- expand.grid(15:25, 0:1000)
target_area <- c(155, 215, -132, -72)

max_y <- apply(init_velo, 1, function(v) {
  traj_v <- traj(v, target_area)
  if (any(traj_v[, 3] == 1)) {
    max(traj_v[, 2])
  } else {
    NA
  }
})

max(max_y, na.rm = T)
sum(!is.na(max_y))

init_velo2 <- expand.grid(15:25, (-132):(-1))
target_area <- c(155, 215, -132, -72)

max_y2 <- apply(init_velo2, 1, function(v) {
  traj_v <- traj(v, target_area)
  if (any(traj_v[, 3] == 1)) {
    max(traj_v[, 2])
  } else {
    NA
  }
})

max(max_y2, na.rm = T)

sum(!is.na(max_y2))


init_velo3 <- expand.grid(26:215, (-132):400)
target_area <- c(155, 215, -132, -72)

max_y3 <- apply(init_velo3, 1, function(v) {
  traj_v <- traj(v, target_area)
  if (any(traj_v[, 3] == 1)) {
    max(traj_v[, 2])
  } else {
    NA
  }
})

max(max_y3, na.rm = T)
sum(!is.na(max_y3))
