C <- matrix(c(1, 0.7, 0.7, 1), 2, 2)

L <- chol(C)

t(L) %*% L

tau <- diag(c(1, 4))

Lambda <- tau %*% t(L)

Z <- rbind(rnorm(5000), rnorm(5000))

X <- Lambda %*% Z

cor(X[1,], X[2,])

par(mfrow = c(2, 1))
plot(Z[1,], Z[2,], pch = 16, cex = 0.4, col = "red")
plot(X[1,], X[2,], pch = 16, cex = 0.4, col = "green")
