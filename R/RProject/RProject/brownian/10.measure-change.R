#Girsanov

# WA  = -sum(sigmaA * dt) + WQ
# dA / A = r * dt + sigmaA * dW
# WB  = -sum(sigmaB * dt) + WQ
# dB / B = r * dt + sigmaB * dW
# dWA = -sigmaA * dt + dW
# dWB = -sigmaB * dt + dW

T <- 10;
N <- 1000;
dt <- T / N;
r <- 0.02;
muA <- 0.2;
sigmaA <- 0.2;
muB <- 0.5;
sigmaB <- 0.5;

# generate many paths
M <- 1000;
W <- rep(0, M);
AS <- rep(1, M);
BS <- rep(1, M);

for (i in 2:M) {
    dW <- rnorm(N, mean = 0, sd = sqrt(dt));
    
    for (j in 2:N) {
        AS[j] <- AS[j - 1] + AS[j - 1] * (r * dt + sigmaA * dW[j - 1]);
        BS[j] <- BS[j - 1] + BS[j - 1] * (r * dt + sigmaB * dW[j - 1]);
    }

    W[i] <- sum(dW);
}

WA <- -sigmaA * T + W
WB <- -sigmaB * T + W

par(mfrow = c(5, 1))
hist(W, col = "green", breaks = 100)

hist(AS, col = "blue", breaks = 100)
hist(WA, col = "blue", breaks = 100)

hist(BS, col = "purple", breaks = 100)
hist(WB, col = "purple", breaks = 100)

sprintf("Mean=%f Variance=%f", 0, T)
sprintf("Mean=%f Variance=%f", mean(W), sd(W) ^ 2)

sprintf("Mean=%f Variance=%f", muA, sigmaA)
sprintf("Mean=%f Variance=%f", mean(AS), sd(AS) ^ 2)

sprintf("Mean=%f Variance=%f", muB, sigmaB)
sprintf("Mean=%f Variance=%f", mean(BS), sd(BS) ^ 2)




