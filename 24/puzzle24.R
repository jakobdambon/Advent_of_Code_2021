################################################################################
##
## == Advent of Code 2021 ==
## Day: 24
## Author: Jakob Dambon
##
################################################################################

# read in operations
instr <- read.table("24/input.txt", header = FALSE, sep = "\n")
df_instr <- data.frame(
  op = substr(instr$V1, 1, 3),
  i1 = substr(instr$V1, 5, 5),
  i2 = ifelse(nchar(instr$V1)<=5, NA, substr(instr$V1, 7, nchar(instr$V1)))
)

# get sub operations, i.e.parts between "inp"
mat <- matrix(NA, ncol = 2, nrow = 0)
colnames(mat) <- c("start", "end")
i <- 1
while (i < nrow(df_instr)) {
    j <- i+head(which(df_instr$op[-(1:i)] == "inp"), 1)
    if (length(j) == 0) {
      j <- nrow(df_instr)
    } else j <- j-1
    
    mat <- rbind(mat, c(i, j))
    i <- j+1
  }
    
# function that acts on the current state of PU with operations from imported list
MONAD <- 
  function(PU, start, stop) {
    
    for (i in (start+1):stop) {
      b <- if (df_instr$i2[i] %in% letters[23:26]) {
        PU[df_instr$i2[i]]
      } else {
        as.numeric(df_instr$i2[i])
      }
      
      switch(df_instr$op[i],
             "add" = {PU[df_instr$i1[i]] <- PU[df_instr$i1[i]] + b},
             "mul" = {PU[df_instr$i1[i]] <- PU[df_instr$i1[i]] * b},
             "div" = {
               if (b == 0)
                 return(NA)
               else 
                 PU[df_instr$i1[i]] <- floor(PU[df_instr$i1[i]] / b)
             },
             "mod" = {
               if ((PU[df_instr$i1[i]] < 0) | (b <= 0))
                 return(NA)
               else 
                 PU[df_instr$i1[i]] <- floor(PU[df_instr$i1[i]] / b)
             },
             "eql" = {PU[df_instr$i1[i]] <- PU[df_instr$i1[i]] == b}
      )
    }
    return(PU)
  }

df_PU <- expand.grid(
  w = 9:1, 
  x = 0, #seq(-10, 10), 
  y = 0, #seq(-10, 10), 
  z = 0 #seq(-10, 10)
)

library(tidyr)
library(dplyr)
# does not complete in time
for (i in 1:14) {
  print(i)
  print(dim(df_PU))
  out <- as.data.frame(
    t(apply(df_PU[,1:4], 1, MONAD, start = mat[i, 1], stop = mat[i, 2]))
  )
  if (ncol(df_PU)>4) {
    out <- cbind(out, df_PU[, 5:ncol(df_PU)], df_PU$w)
  } else {
    out <- cbind(out, df_PU$w)
  }
  colnames(out) <- c(letters[23:26], paste0("x", 1:i))
  
  df_PU <- out %>%
    group_by(x, y, z) %>% 
    arrange(desc(w)) %>% 
    slice(1) %>% 
    ungroup()
  df_PU <- cbind(
    w = rep(9:1, each = nrow(df_PU)), 
    df_PU[, -1]
  )
}


