################################################################################
##
## == Advent of Code 2021 ==
## Day: 18
## Author: Jakob Dambon
##
################################################################################

# function that adds two snailfish numbers
sf_add <- function(sf_num1, sf_num2) {
  # simple addition due to their definition via value and depth
  sf_sum <- rbind(sf_num1, sf_num2)
  sf_sum$depth <- sf_sum$depth + 1
  n <- nrow(sf_sum)
  
  sf_reduce <- TRUE
  exploded <- FALSE
  splitted <- FALSE
  while(sf_reduce) {
    # explode
    for (i in 1:(n-1)) {
      if (all(sf_sum[i + 0:1, 2] == 5)) {
        # to the left
        if (i-1 >= 1) {
          sf_sum$value[i-1] <- sf_sum$value[i-1] + sf_sum$value[i]
        }
        # to the right
        if (i+2 <= n) {
          sf_sum$value[i+2] <- sf_sum$value[i+2] + sf_sum$value[i+1]
        }
        # drop pair
        sf_sum <- sf_sum[-(i+1), ]
        sf_sum[i, ] <- data.frame(
          value = 0, depth = 4
        )
        n <- nrow(sf_sum)
        exploded <- TRUE
        break
      }
    }
    
    if (exploded) {
      exploded <- FALSE
      next
    }

    # split
    for (i in 1:n) {
      if (sf_sum$value[i] >= 10) {
        if (i == 1) {
          sf_sum <- rbind(
            data.frame(
              value = c(floor(sf_sum[i, 1]/2), ceiling(sf_sum[i, 1]/2)),
              depth = sf_sum$depth[i] + 1
            ), 
            sf_sum[-1, ]
          )
        } else if (i == n) {
          sf_sum <- rbind(
            sf_sum[-n, ], 
            data.frame(
              value = c(floor(sf_sum[i, 1]/2), ceiling(sf_sum[i, 1]/2)),
              depth = sf_sum$depth[i] + 1
            )
          )
        } else {
          sf_sum <- rbind(
            sf_sum[1:(i-1), ],
            data.frame(
              value = c(floor(sf_sum[i, 1]/2), ceiling(sf_sum[i, 1]/2)),
              depth = sf_sum$depth[i] + 1
            ), 
            sf_sum[(i+1):n, ]
          )
        }
        n <- nrow(sf_sum)
        splitted <- TRUE
        break
      }
    }
    
    if (splitted) {
      splitted <- FALSE
      next
    }
    
    if (!exploded & !splitted) {
      sf_reduce <- FALSE
    }
  }
  
  sf_sum
}

# Snailfish numbers are defined by their value and their depth, i.e.,
# in how many brackets they are.
sf1 <- data.frame(
  value = c(4, 3, 4, 4, 7, 8, 4, 9),
  depth = c(4, 4, 3, 2, 2, 4, 4, 3)
)
sf2 <- data.frame(
  value = c(1, 1),
  depth = c(1, 1)
)
sf1
sf2
sf_add(sf1, sf2)

# function that transforms the representation in characters 
# to above mentioned data frame representation with value and depth
trans_sf <- function(s) {
  df <- NULL
  depth <- 0
  for ( i in 1:nchar (s)) {
    ch <- substr(s, i, i)
    if (ch == "[") {
      depth <- depth + 1
    } else if (ch == "]") {
      depth <- depth - 1
    } else if (ch == ",") {
      
    } else {
      if (is.null(df)) {
        df <- data.frame(
          value = as.numeric(ch),
          depth = depth
        )
      } else {
        df <- rbind(
          df, 
          data.frame(
            value = as.numeric(ch),
            depth = depth
          )
        )
      }
    }
  }
  df
}

## -- part 1 -----
# read in data
input <- read.table("18/input.txt", header = FALSE)$V1
# transfrom into snail fish number representation using a data frame
l_sf <- lapply(input, trans_sf)
# add dem up
for (i in 1:(length(l_sf)-1)) {
  if ( i == 1 ) {
    s <- sf_add(l_sf[[i]], l_sf[[i+1]])
  } else {
    s <- sf_add(s,  l_sf[[i+1]])
  }
  
  s
}
# function to compute the magnitude
calc_magnitude <- function(sf) {
  max_depth <- 4
  n <- nrow(sf)
  i <- 0
  summed <- FALSE
  while(max_depth>0) {
    for (i in 1:(n-1)) {
      if (all(c(sf$depth[i+0:1] == max_depth))) {
        if (i == 1) {
          sf <- rbind(
            data.frame(
              value = sum(3:2 * sf$value[i+0:1]),
              depth = max_depth-1
            ),
            sf[-(i+0:1), ]
          )
        } else if (i == n-1) {
          sf <- rbind(
            sf[-(c(n-1, n)), ],
            data.frame(
              value = sum(3:2 * sf$value[i+0:1]),
              depth = max_depth-1
            )
          )
        } else {
          sf <- rbind(
            sf[1:(i-1), ], 
            data.frame(
              value = sum(3:2 * sf$value[i+0:1]),
              depth = max_depth-1
            ),
            sf[(i+2):n, ]
          )
        }
        n <- nrow(sf)
        summed <- TRUE
        break
      }
    }
    if (summed) {
      summed <- FALSE
    } else {
      max_depth <- max_depth-1
    }
    
  }
  sf
}
# answer
calc_magnitude(s)


## -- part 2 -----
# function that combines pairwise addition of SF numbers and 
# and gives back magnitude
sum_value <- function(a, b) {
  sum <- sf_add(a, b)
  val <- calc_magnitude(sum)
  val$value
}

# compute all pairs (not commutative)
pw_add <- matrix(NA, ncol = length(l_sf), nrow = length(l_sf))
for (i in 1:length(l_sf))
  for (j in 1:length(l_sf)) {
    print(paste0("(", i, ",", j, ")"))
    pw_add[i, j] <- if (i == j) 0 else sum_value(l_sf[[i]], l_sf[[j]])
    
  }
# answer    
pw_add
max(pw_add)
