################################################################################
##
## == Advent of Code ==
## Day: 12
## Author: Jakob Dambon
##
################################################################################


test <- read.table("12/input_test.txt", header = FALSE, sep = "\n")$V1

str2adj_mat <- function(s) {
  l_spl <- strsplit(s, "-")
  cave <- unique(unlist(l_spl))
  adj_mat <- matrix(FALSE, nrow = length(cave), ncol = length(cave))
  row.names(adj_mat) <- cave
  colnames(adj_mat) <- cave
  
  for (i in 1:length(l_spl)) {
    adj_mat[l_spl[[i]][1], l_spl[[i]][2]] <- TRUE
    adj_mat[l_spl[[i]][2], l_spl[[i]][1]] <- TRUE
  }
  adj_mat
}

find_path <- function(adj_mat, pos, path, visit_twice = FALSE) {
  # stop condition
  if (pos == "end")
    return(unlist(c(path, "end", "#")))
  
  next_step <- adj_mat[, pos == colnames(adj_mat)]
  next_step["start" == colnames(adj_mat)] <- FALSE
  
  if (sum(next_step) == 0) {
    # no way out
    return(c(path, "#"))
  } else {
    if (pos == tolower(pos)) {
      adj_mat_copy <- adj_mat
      adj_mat[pos, ] <- adj_mat[, pos] <- FALSE
    }
    p <- sapply(seq_along(next_step), function(i) {
      if (next_step[i]) {
        p1 <- find_path(adj_mat, rownames(adj_mat)[i], c(path, pos), visit_twice)
        if (visit_twice & (pos == tolower(pos))) {
          p2 <- find_path(adj_mat_copy, rownames(adj_mat)[i], c(path, pos), FALSE)
          return(list(p1, p2))
        } else {
          return(p1)
        }
      }
    })
    return(p)
  }
}

tidy_paths <- function(s) {
  p <- list()
  i <- 1
  for (j in 2:length(s)) {
    if (s[j] == "#") {
      # valid path?
      if (s[j-1] == "end") {
        p <- c(p, list(s[i:j]))
        i <- j + 1
      } else {
        i <- j + 1
      } 
    }
  }
  p
}

# -- puzzle 1 ----

# tests
test <- read.table("12/input_test1.txt", header = FALSE, sep = "\n")$V1
adj_mat <- str2adj_mat(test)
raw_paths <- unlist(find_path(adj_mat, pos = "start", path = ""))
length(tidy_paths(raw_paths))

test <- read.table("12/input_test2.txt", header = FALSE, sep = "\n")$V1
adj_mat <- str2adj_mat(test)
raw_paths <- unlist(find_path(adj_mat, pos = "start", path = ""))
length(tidy_paths(raw_paths))

test <- read.table("12/input_test3.txt", header = FALSE, sep = "\n")$V1
adj_mat <- str2adj_mat(test)
raw_paths <- unlist(find_path(adj_mat, pos = "start", path = ""))
length(tidy_paths(raw_paths))

# final
test <- read.table("12/input.txt", header = FALSE, sep = "\n")$V1
adj_mat <- str2adj_mat(test)
raw_paths <- unlist(find_path(adj_mat, pos = "start", path = ""))
length(tidy_paths(raw_paths))


# -- part 2 -----
# tests
test <- read.table("12/input_test1.txt", header = FALSE, sep = "\n")$V1
adj_mat <- str2adj_mat(test)
raw_paths <- unlist(find_path(adj_mat, pos = "start", path = "", TRUE))
length(unique(tidy_paths(raw_paths)))

test <- read.table("12/input_test2.txt", header = FALSE, sep = "\n")$V1
adj_mat <- str2adj_mat(test)
raw_paths <- unlist(find_path(adj_mat, pos = "start", path = "", TRUE))
length(unique(tidy_paths(raw_paths)))


test <- read.table("12/input_test3.txt", header = FALSE, sep = "\n")$V1
adj_mat <- str2adj_mat(test)
raw_paths <- unlist(find_path(adj_mat, pos = "start", path = "", TRUE))
length(unique(tidy_paths(raw_paths)))

# final
test <- read.table("12/input.txt", header = FALSE, sep = "\n")$V1
adj_mat <- str2adj_mat(test)
raw_paths <- unlist(find_path(adj_mat, pos = "start", path = "", TRUE))
length(unique(tidy_paths(raw_paths)))