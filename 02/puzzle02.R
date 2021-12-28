################################################################################
##
## == Advent of Code 2021 ==
## Day: 2
## Author: Jakob Dambon
##
################################################################################

# read in data
df <- read.csv("02/input.txt", header = FALSE, sep = " ")

# -- part 1 -----
h_pos <- c(0, cumsum(ifelse(df$V1 == "forward", 1, 0)*df$V2))
v_pos <- c(0, cumsum(
  ifelse(df$V1 == "down", 1, ifelse(df$V1 == "up", -1, 0))*df$V2
))

plot(h_pos, v_pos, type = "l")
# answer
tail(h_pos, 1)*tail(v_pos, 1)


# -- part 2 -----
df_test <- data.frame(
  V1 = c("forward", "down", "forward", "up", "down", "forward"),
  V2 = c(5, 5, 8, 3, 8, 2)
)


pilot_fn <- function(df) {
  pos <- c("h" = 0, "v" = 0, "a" = 0)
  
  for (i in 1:nrow(df)) {
    
    if (df[i, 1] == "down") {
      pos["a"] <-  pos["a"] + df[i, 2]
    } else if (df[i, 1] == "forward") {
      pos["h"] = pos["h"] + df[i, 2]
      pos["v"] = pos["v"] + pos["a"]*df[i, 2]
    } else {
      pos["a"] = pos["a"] - df[i, 2]
    }
  }
  return(pos)
}

pilot_fn(df_test)

final_pos <- pilot_fn(df)
final_pos[1] * final_pos[2]
