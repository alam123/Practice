#Girsanov

# dW = N(0,dt)
# dWQ = N(lambda * dt ,dt)
# dS/S = mu * dt + sigma * dW
# dS/S = mu * dt + sigma * (dWQ - lambda * dt)
# dS/S = r * dt + sigma * dWQ
# dB / B = r * dt + sigmaB * dW
# dWA = -sigmaA * dt + dW
# dWB = -sigmaB * dt + dW

T <- 10;
N <- 1000;
dt <- T / N;
r <- 0.02;
mu <- 0.2;
sigma <- 0.2;
lambda <- (mu - r) / sigma;

# generate many paths
M <- 1000;
W <- rep(0, M);
WQ <- rep(0, M);
SP <- rep(1, M);
SQ <- rep(1, M);

for (i in 2:M) {
    dW <- rnorm(N, mean = lambda * dt, sd = sqrt(dt));
    dWQ <- rnorm(N, mean = 0, sd = sqrt(dt));

    W[i] <- sum(dW);
    WQ[i] <- sum(dWQ);

    for (j in 2:N) {
        SP[i] <- SP[i] + SP[i] * (mu * dt + sigma * dW[j - 1]);
        #SQ[i] <- SQ[i] + SQ[i] * (mu * dt + sigma * (dWQ[j - 1] - lambda * dt));
        SQ[i] <- SQ[i] + SQ[i] * (r * dt + sigma * dWQ[j - 1]);
    }
}

par(mfrow = c(4, 1), mar = c(1, 1, 1, 1))
hist(W, col = "green", breaks = 100)
hist(WQ, col = "blue", breaks = 100)
hist(SP, col = "red", breaks = 100)
hist(SQ, col = "purple", breaks = 100)


sprintf("W: Mean=%f Variance=%f", lambda * T, T)
sprintf("W: Mean=%f Variance=%f", mean(W), sd(W) ^ 2)

sprintf("WQ: Mean=%f Variance=%f", r * T, T)
sprintf("WQ: Mean=%f Variance=%f", mean(WQ), sd(WQ) ^ 2)

sprintf("SP: Mean=%f Variance=%f", mu * T, sigma ^ 2 * T)
sprintf("SP: Mean=%f Variance=%f", mean(log(SP)), sd(log(SP)) ^ 2)

sprintf("SQ: Mean=%f Variance=%f", (r - 0.5 * sigma ^ 2) * T, sigma ^ 2 * T)
sprintf("SQ: Mean=%f Variance=%f", mean(log(SQ)), sd(log(SQ)) ^ 2)




