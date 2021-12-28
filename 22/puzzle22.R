################################################################################
##
## == Advent of Code 2021 ==
## Day: 22
## Author: Jakob Dambon
##
################################################################################

# read in data
input <- read.table("22/input.txt", header = FALSE)
# extract cuboids
cuboid <- sapply(strsplit(input$V2, split = ","), function(instr) {
  as.numeric(unlist(strsplit(substring(instr, first = 3), split = "..", fixed = TRUE)))
})
# data frame with operations and cuboids
df_instr <- data.frame(input$V1, t(cuboid))

## -- part 1 -----

# function on an small extract (-50, ..., 50)^3
reboot <- function(df_instr, min = -50, max = 50) {
  # only on an extract
  check_interval <- function(x, bounds = c(min, max)) {
    !((x[2]<bounds[1]) | (x[1]>bounds[2]))
  }
  
  assign_cuboid <- function(value, x, array, min) {
    
    array[
      seq(x[1], x[2])-min+1, 
      seq(x[3], x[4])-min+1,
      seq(x[5], x[6])-min+1
    ] <- value
    array
  }
  # initialize and go through instructions
  core <- array(FALSE, dim = rep(max - min + 1, 3))
  for (i in 1:nrow(df_instr)) {
    cuboid_dim <- df_instr[i, -1]
    if (check_interval(cuboid_dim[1:2]) & 
        check_interval(cuboid_dim[3:4]) & 
        check_interval(cuboid_dim[5:6])) {
      # within min-max-cuboid
      cuboid_dim[2*(1:3)] <- ifelse(cuboid_dim[2*(1:3)] > max, max, cuboid_dim[2*(1:3)])
      cuboid_dim[1+2*(0:2)] <- ifelse(cuboid_dim[1+2*(0:2)] < min, min, cuboid_dim[1+2*(0:2)])
      core <- assign_cuboid(df_instr[i, 1] == "on", as.numeric(cuboid_dim), core, min)
    } else {
      # outside
      next
    }
  }
  core
}

out <- reboot(df_instr)
sum(out)

## -- part 2 -----
input <- read.table("22/input.txt", header = FALSE)
# like before, read-in and get instructions
cuboid <- sapply(strsplit(input$V2, split = ","), function(instr) {
  as.numeric(unlist(strsplit(substring(instr, first = 3), split = "..", fixed = TRUE)))
})

df_instr <- data.frame(
  input$V1, 
  t(cuboid)
)

# As the cuboids cannot be expressed using an array like before we do the following
# 1. Go through instructions backwards
# 2. If "on", count only the "new" cuboids being turned on
# 2.1. That is, if i0 instruction is "on", the number of newly turned on voxels
#       is #{C_i0 \ {C_i0+1 u ... u C_n}} = #{C_i0} - #{C_i0 n {C_i0+1 u ... u C_n}} 
#       = #{C_i0} - #{{C_i0 n C_i0+1} u ... u {C_i0 n C_n}}
# 2.2. We use the following properties:
#       - Intersection of two cuboids is again a cuboid or an empty set
#       - Iteratively: #{A u B} = #A + #B - #{A n B}

# intersection function
intersect_cuboids <- function(c1, c2) {
  if (is.null(c1) | is.null(c2)) {
    return(NULL)
  } else {
    A <- cbind(c1, c2)
    mins <-  apply(A[c(1, 3, 5), ], 1, max)
    maxs <-  apply(A[c(2, 4, 6), ], 1, min)
    if (any(mins>maxs)) return(NULL) else 
      return(
        as.vector(rbind(mins, maxs))
      )
  }
}

# number of elements in cuboid (or empty set)
n_cuboid <- function(cuboid) {
  if (is.null(cuboid)) return(0) else {
    prod(cuboid[2*(1:3)] -cuboid[c(1, 3, 5)] + 1)
  }
}

# iterative intersections
power_intersect <- function(l_cuboid) {
  if (length(l_cuboid) == 0) {
    return(0) 
  } else if (length(l_cuboid) == 1) {
    return(n_cuboid(l_cuboid[[1]]))
  } else {
    
    s_single <- sapply(l_cuboid, n_cuboid)
    s_pw_inter <- sapply(seq_along(l_cuboid), function(i) {
      
      l2 <- lapply(l_cuboid[-(1:i)], function(cube) 
        intersect_cuboids(cube, l_cuboid[[i]])
      )
      s2 <- power_intersect(l2[!sapply(l2, is.null)])
      return(sum(s2))
    })
    return(sum(s_single)-sum(s_pw_inter))
  }
}


# adding "new" to s
s <- 0
for (i in nrow(df_instr):1) {
  if (df_instr[i, 1] == "on") {
    print(i)
    x <- as.numeric(df_instr[i, -1])
    s_full <- n_cuboid(x)
    
    
    l_inter <- apply(df_instr[nrow(df_instr):(i+1), -1], 1, function(y) {
      intersect_cuboids(y, x)
    })
    l_inter <- l_inter[!sapply(l_inter, is.null)]
    s_inter <- if (is.null(l_inter)) {
      0
    } else {
      sum(power_intersect(l_inter))
    }
    
    s <- s + s_full - s_inter
  }
}
