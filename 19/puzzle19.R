################################################################################
##
## == Advent of Code 2021 ==
## Day: 19
## Author: Jakob Dambon
##
################################################################################

# read in data
input <- read.table("19/input.txt", header = FALSE, sep = "\n")$V1
# get scanners
scanners <- list()
id <- 1
start <- 1
for (i in 2:length(input)) {
  if (substr(input[i], 1, 3) == "---") {
    scanners[[id]] <- matrix(
      as.numeric(Reduce(cbind, strsplit(input[(start+1):(i-1)], ","))),
      ncol = 3, byrow = TRUE
    )
    id <- id+1
    start <- i
  }
  if (i == length(input)) {
    scanners[[id]] <- matrix(
      as.numeric(Reduce(cbind, strsplit(input[(start+1):i], ","))), 
      ncol = 3, byrow = TRUE
    )
  }
}
scanners
# calculate paiw-wise distances
pw_minkow <- lapply(scanners, function(pos){
  as.matrix(dist(pos, method = "manhattan"))
})

# check number of same distances between distance matrices 
match_dist <- function(D1, D2) {
  match_id <- matrix(0, nrow = 0, ncol = 2)
  for (i1 in 1:nrow(D1)) {
    for (i2 in 1:nrow(D2)) {
      if (sum(D2[, i2] %in% D1[, i1]) >= 12L) {
        match_id <- rbind(match_id, c(i1, i2))
      }
    }
  }
  match_id
}
# number of same distances
l_matches <- list()
for (i in 1:(length(pw_minkow)-1)) {
  for (j in (i+1):length(pw_minkow)) {
    l_matches <- c(l_matches, list(match_dist(pw_minkow[[i]], pw_minkow[[j]])))
  }
}

sum(sapply(scanners, nrow)) - nrow(Reduce(rbind, l_matches))


## -- part 2 -----

l_matches <- lapply(pw_minkow, function(l1) {
  lapply(pw_minkow, function(l2) {
    match_dist(l1, l2)
  })
})

scanners2 <- scanners
# matrix that logs the scanner positions relative to scanner 0
s_pos <- matrix(NA, ncol = 3, nrow = length(scanners))
s_pos[1, ] <- 0
# index for next scanner position
i1 <- 2
# data frame of possible rotations
rot <- as.matrix(expand.grid(
  c(-1, 1),  c(2, -2), c(-3, 3)
))
colnames(rot) <- NULL
mat <- matrix(c(
  1:3, 
  1, 3, 2,
  2, 1, 3,
  2, 3, 1, 
  3, 2, 1, 
  3, 1, 2
), byrow = TRUE, ncol = 3)
df_rot <- Reduce(rbind, lapply(1:nrow(mat), function(i) {
  rot[, mat[i, ]]
}))


# go through all scanners and check for beacon mathches.
# Amend all matches to scanner 0 by transformation
while( length(scanners2)>1) {
  # check for matches
  for ( i2 in 2:(length(scanners2))) {
    if ( nrow(l_matches[[1]][[i2]]) >= 12 ) {
      # mathcing beacons in reference (0) and other scanner (i2)
      pos1 <- scanners2[[1]][l_matches[[1]][[i2]][, 1], ]
      pos2 <- scanners2[[i2]][l_matches[[1]][[i2]][, 2], ]
      # look for correct rotation
      match_found <- FALSE
      j <- 1
      while (!match_found) {
        # rotate positions no. 2
        pos2_rot <- t(ifelse(df_rot[j, ] < 0, -1, 1)*t(pos2))
        pos2_rot <- pos2_rot[, abs(df_rot[j, ])]
        
        if ( all( apply(pos1-pos2_rot, 2, function(x) {
          # differences all the same, i.e., correct rotation
          (min(x) == max(x))
        }))) {
          s_pos[i1, ] <- head(pos1-pos2_rot, 1)
          i1 <- i1+1
          # rotate all other scanners and beacons
          rot_pos <- t(ifelse(df_rot[j, ] < 0, -1, 1)*t(scanners2[[i2]]))
          rot_pos <- rot_pos[, abs(df_rot[j, ])]
          
          scanners2[[1]] <- unique(rbind(
            scanners2[[1]], 
            rot_pos + matrix(head(pos1-pos2_rot, 1), 
                             ncol = 3, nrow = nrow(rot_pos), byrow = T)))
          scanners2[[i2]] <- NA
          
          match_found <- TRUE
        } else {
          # if rotation doe not match, continue
          j <- j+1
        } 
      } # end while (i.e. end looking for rotation)
    }
  } # continue with next possible match
  
  # reduce to scanners that were not united
  scanners2 <- scanners2[sapply(scanners2, is.matrix)]
  # compute new distance matches between separated scanners
  pw_minkow <- lapply(scanners2, function(pos){
    as.matrix(dist(pos, method = "manhattan"))
  })
  # compute new matches
  l_matches <- lapply(pw_minkow, function(l1) {
    lapply(pw_minkow, function(l2) {
      match_dist(l1, l2)
    })
  })
}

# answer
max(dist(s_pos, method = "manhattan"))

