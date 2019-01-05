# W = z * sqrt(dt), z ~ N(0,1)

T <- 10;
N <- 10;
dt <- T / N;

res <- rnorm(N, 0, sqrt(dt))
# generate many paths
M <- 10;
BB <- matrix(0, nrow = N, ncol = M);

for (i in 1:M) {
    dB <- rnorm(N, 0, sqrt(dt));
    BB[,i] <- rep(0, N);
    for (j in 2:N) {
        t <- j * (T / N);
        BB[j, i] = sum(dB[1:j]) - (t / T) * sum(dB);
    }
}

par(mfrow = c(1, 1))
matplot(1:10, BB[,1:10], col = "red", type = "l")
legend("topleft", c(gettextf("mu = %f,  sigma = %f", 0, T), gettextf("mu = %f,  sigma = %f", mean(BB), sd(BB) ^ 2)), cex = 0.8, col = c("red", "green"), pch = 1, lty = 1);