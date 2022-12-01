if (!requireNamespace("pacman")) install.packages("pacman")
packages_cran = c("here")
pacman::p_load(char = packages_cran)
data <- readLines(here::here("day01", "data.txt"))
# credit: https://stackoverflow.com/a/71130251
i1 <- !nzchar(data)
data_split <- unname(split(data[!i1], cumsum(i1)[!i1]))
data_sum <- lapply(data_split, function(x) sum(as.integer(x)))
solution <- max(unlist(data_sum))