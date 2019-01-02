# W = z * sqrt(dt), z ~ N(0,1)

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

#W.W

T <- 10;
N <- 1000;
dt <- T / N;

M <- 1000;
W.W <- rep(0, M);

for (i in 1:M) {
    W <- simBrownian(T, N);
    W.W[i] = W[N] ^ 2;
}

sprintf("Mean=%f Variance=%f", T , 2 * T ^ 2)
sprintf("Mean=%f Variance=%f", mean(W.W), sd(W.W) ^ 2)

par(mfrow = c(2, 1))
hist(W, col = "green")
hist(W.W, col = "blue")


#WT.WS

T <- 10;
N <- 1000;
S <- 3;
dt <- T / N;

M <- 1000;
WT <- rep(0, M);
WS <- rep(0, M);
WT.WS <- rep(0, M);

for (i in 1:M) {
    W <- simBrownian(T, N);
    WT <- W;
    WS <- head(W, (S / T) * N);
    WT.WS[i] = WT[N] * WS[(S / T) * N];
}

sprintf("Mean=%f Variance=%f", S, S * (T + S))
sprintf("Mean=%f Variance=%f", mean(WT.WS), sd(WT.WS) ^ 2)

par(mfrow = c(3, 1))
hist(WT, col = "red")
hist(WS, col = "green")
hist(WT.WS, col = "blue")