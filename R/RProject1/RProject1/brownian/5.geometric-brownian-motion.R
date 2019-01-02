# SDE -> dX = sigma * X * dW 
# Solution -> X = exp ( -0.5 * t ^ 2 + sigma + W)
# mean(X) = 1
# variance(X) = exp( sigma ^ 2 * t )
# p = 0.5


simBrownian <- function(T, N) {
    # tim step size
    dt <- T / N;

    X <- rnorm(N, 0, 1);
    cumx <- cumsum(X);

    X <- rep(0, N)

    for (i in 2:N) {
        X[i] <- sqrt(dt) * cumx[i - 1];
    }

    return(B = X);
}

T <- 10;
N <- 1000;
dt <- T / N;
sigma <- 0.2

# generate many paths
M <- 1000;
W <- rep(0, M);
GB <- rep(0, M);

for (i in 1:M) {
    res <- simBrownian(T, N);
    W[i] <- res[N];
}

GB <- exp(-0.5 * T ^ 2 + sigma + W)

par(mfrow = c(2, 1))
hist(W, col = "green")
hist(GB, col = "blue")

sprintf("Mean=%f Variance=%f", 1, exp(sigma ^ 2 * T))
sprintf("Mean=%f Variance=%f", mean(GB), sd(GB) ^ 2)