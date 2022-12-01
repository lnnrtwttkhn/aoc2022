if (!requireNamespace("pacman")) install.packages("pacman")
packages_cran = c("here")
pacman::p_load(char = packages_cran)
data <- readLines(here::here("day01", "data.txt"))
example <- readLines(here::here("day01", "example.txt"))
# part one
get_sums = function(x) {
  # credit: https://stackoverflow.com/a/71130251
  i1 <- !nzchar(x)
  x_split <- unname(split(x[!i1], cumsum(i1)[!i1]))
  x_sums <- unlist(lapply(x_split, function(x) sum(as.integer(x))))
  return(x_sums)
}
solution <- max(get_sums(data))
example_solution <- max(get_sums(example))
# part two
sums <- get_sums(data)
solution <- sum(sort(sums)[seq(length(sums) - 2, length(sums))])