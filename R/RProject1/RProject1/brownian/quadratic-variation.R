simItos <- function(T, N) {
    # tim step size
    dt <- T / N;

    X <- rnorm(N, 0, 1);
    W <- rep(0, N)
    QV <- rep(0, N)
    TV <- rep(0, N);

    for (i in 2:N) {
        W[i] <- W[i - 1] + sqrt(dt) * X[i];
        QV[i] <- QV[i - 1] + (W[i] - W[i - 1]) ^ 2;
        TV[i] <- TV[i - 1] + abs(W[i] - W[i - 1]);
    }

    return(list(W = W, QV = QV, TV = TV));
}

T <- 10;
N <- 100000;
dt <- T / N;

res <- simItos(T, N);

par(mfrow = c(3, 1))
plot(seq(1:N), res$W, xlab = "time", ylab = "W", col = "red", type = "l");
plot(seq(1:N), res$QV, xlab = "time", ylab = "QV", col = "green", type = "l");
plot(seq(1:N), res$TV, xlab = "time", ylab = "TV", col = "blue", type = "l");
