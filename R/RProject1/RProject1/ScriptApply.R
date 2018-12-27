x <- list(a = 1:5, b = rnorm(10))
lapply(x, mean)

x <- 1:4
lapply(x, runif)
lapply(x, runif, min = 0, max = 10)

x <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2))
lapply(x, function(elt) elt[, 1])
sapply(x, function(elt) elt[, 1])

x <- list(a = 1:5, b = rnorm(10))
lapply(x, mean)
sapply(x, mean)


x <- matrix(rnorm(200), 20, 10)
apply(x, 2, mean)
apply(x, 1, mean)
apply(x, 2, sum)
apply(x, 1, sum)


x <- matrix(rnorm(200), 20, 10)
apply(x, 1, quantile, probs = c(0.25, 0.75))

a <- array(rnorm(2 * 2 * 10), c(2, 2, 10))
apply(a, c(1, 2), mean)
rowMeans(a, dims = 2)


x <- c(rnorm(10), runif(10), rnorm(10, 1))
f <- gl(3, 10)
tapply(x, f, mean)
tapply(x, f, range)
tapply(x, f, mean, simplify = FALSE)
tapply(x, f, mean, simplify = TRUE)

str(split)

split(x, f)
lapply(split(x, f), sum, simplify = TRUE)
sapply(split(x, f), sum)

df <- read.csv(file = "TextFile.csv")
s <- split(df, df$d)
sapply(s, function(x) colMeans(x[, c("b", "e", "f")]))
df

