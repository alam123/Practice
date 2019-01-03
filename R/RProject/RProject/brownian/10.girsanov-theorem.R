#Girsanov

# dB = N(0,dt)
# dZ/Z = -lambda^T * dW
# Q = B + sum(lambda * dt)

# dQ = N(lambda * dt ,dt)
# dB / B = r * dt + sigmaB * dW
# WQ = W + sum(lambda * dt)


T <- 10;
N <- 1000;
dt <- T / N;
r <- 0.02;
mu <- 0.2;
sigma <- 0.2;
lambda <- (mu - r) / sigma;

# generate many paths
M <- 1000;
B <- rep(0, M);
Q <- rep(0, M);


for (i in 2:M) {
    dB <- rnorm(N, mean = 0, sd = sqrt(dt));
    dQ <- rnorm(N, mean = lambda * dt, sd = sqrt(dt));

    B[i] <- sum(dB);
    Q[i] <- sum(dQ);
}



par(mfrow = c(2, 1), mar = c(1, 1, 1, 1))
hist(B, col = "green", breaks = 100)
legend("topleft", c(gettextf("mu = %f,  sigma = %f", 0, sigma), gettextf("mu = %f,  sigma = %f", mean(B), sd(B) ^ 2)), cex = 0.8, col = c("red", "green"), pch = 1, lty = 1);

hist(Q, col = "blue", breaks = 100)
legend("topleft", c(gettextf("mu = %f,  sigma = %f", lambda * T, sigma), gettextf("mu = %f,  sigma = %f", mean(Q), sd(Q) ^ 2)), cex = 0.8, col = c("red", "green"), pch = 1, lty = 1);
