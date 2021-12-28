################################################################################
##
## == Advent of Code 2021 ==
## Day: 13
## Author: Jakob Dambon
##
################################################################################

df_dots <- read.table("13/input_dots.txt", header = FALSE, sep = ",")
colnames(df_dots) <- c("x", "y")
head(df_dots)


df_dots_test <- read.table("13/input_dots_test.txt", header = FALSE, sep = ",")
colnames(df_dots_test) <- c("x", "y")

fold <- function(df, axis, pos) {
  out <- apply(df, 1, function(dot) {
    if (dot[axis] > pos) {
      dot[axis] <- pos - (dot[axis] - pos)
    } 
    return(dot)
  })
  unique(t(out))
}

mat1 <- fold(df_dots_test, 2, 7)
mat2 <- fold(mat1, 1, 5)

plot(mat2)

mat1 <- fold(df_dots, 1, 655)

s_instr <- read.table("13/input_instr.txt", header = FALSE, sep = "\n")$V1
 
df_instr <- data.frame(
  axis = ifelse(substring(s_instr, 12, 12) == "x", 1, 2),
  pos = as.numeric(substring(s_instr, 14))
)

mat <- df_dots
for (i in 1:nrow(df_instr)) {
  mat <- fold(mat, df_instr[i, 1], df_instr[i, 2])
}

# Read plot for answer
plot(mat, asp = 1, type = "p", pch = ".")
