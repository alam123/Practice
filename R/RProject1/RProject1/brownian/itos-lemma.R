simItos <- function(T, N) {
    # tim step size
    dt <- T / N;

    X <- rnorm(N, 0, 1);
    W <- rep(0, N)
    IWdW <- rep(0, N)
    ReimanIWdW <- rep(0, N);

    for (i in 2:N) {
        W[i] <- W[i - 1] + sqrt(dt) * X[i];
        IWdW[i] <- IWdW[i - 1] + W[i] * (W[i] - W[i - 1]);
        ReimanIWdW[i] <- 0.5 * (W[i] ^ 2);
    }

    return(list(W = W, IWdW = IWdW, ReimanIWdW = ReimanIWdW));
}

T <- 10;
N <- 10000;
dt <- T / N;

res <- simItos(T, N);

par(mfrow = c(2, 1))
plot(seq(1:N), res$IWdW, xlab = "time", ylab = "phenotype", col = "green", type = "l");
lines(seq(1:N), res$ReimanIWdW, col = "red");
plot(seq(1:N), res$IWdW - res$ReimanIWdW, xlab = "time", ylab = "phenotype", col = "green", type = "l");
