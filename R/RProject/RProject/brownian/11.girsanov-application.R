#Girsanov

# dB = N(0,dt)
# dS/S = mu * dt + sigma * dB
# dS/S = mu * dt + sigma * (dQ - ((mu-r)/sigma) * dt)
# dS/S = r * dt + sigma * dQ
# p <- 0.5;
# q <- 0.5 * (1 + ((r - mu) / sigma) * sqrt(dt));

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
SB <- rep(1, M);
SQ <- rep(1, M);

for (i in 2:M) {
    dB <- rnorm(N, mean = 0, sd = sqrt(dt));
    dQ <- rnorm(N, mean = lambda * dt, sd = sqrt(dt));

    B[i] <- sum(dB);
    Q[i] <- sum(dQ);

    for (j in 1:N) {
        SB[i] <- SB[i] + SB[i] * (mu * dt + sigma * dB[j]);
        SQ[i] <- SQ[i] + SQ[i] * (r * dt + sigma * dQ[j]);
    }
}



par(mfrow = c(4, 1), mar = c(1, 1, 1, 1))
hist(B, col = "green", breaks = 100)
legend("topleft", c(gettextf("mu = %f,  sigma = %f", 0, sqrt(T)), gettextf("mu = %f,  sigma = %f", mean(B), sd(B))), cex = 0.8, col = c("red", "green"), pch = 1, lty = 1);

hist(Q, col = "blue", breaks = 100)
legend("topleft", c(gettextf("mu = %f,  sigma = %f", lambda * T, sqrt(T)), gettextf("mu = %f,  sigma = %f", mean(Q), sd(Q))), cex = 0.8, col = c("red", "green"), pch = 1, lty = 1);


hist(SB, col = "blue", breaks = 100)
legend("topleft", c(gettextf("mu = %f,  sigma = %f", exp((mu + 0.5 * sigma ^ 2) * T), sigma * sqrt(T)), gettextf("mu = %f,  sigma = %f", mean(SB), sd(log(SB)))), cex = 0.8, col = c("red", "green"), pch = 1, lty = 1);

hist(SQ, col = "blue", breaks = 100)
legend("topleft", c(gettextf("mu = %f,  sigma = %f", exp(r * T), sigma * sqrt(T)), gettextf("mu = %f,  sigma = %f", mean(log(SQ)), sd(log(SQ)))), cex = 0.8, col = c("red", "green"), pch = 1, lty = 1);

