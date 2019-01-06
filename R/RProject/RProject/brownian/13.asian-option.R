delta <- 1 / 12
T <- 2
S0 <- 100
sigma <- 0.20
K <- 100
r <- 0.01
n <- 10 ^ 4
m <- T / delta
S <- numeric(m + 1)
S[1] <- S0
asian_price <- function() {
    for (j in 1:m) {
        W <- rnorm(1)
        S[j + 1] <- S[j] * exp((r - 0.5 * sigma ^ 2) * delta + sigma * sqrt(delta) * W)
    }
    Si.bar <- mean(S[-1])
    exp(-r * T) * max(Si.bar - K, 0)
}

C <- rep(0, n);

for (j in 1:n) { 
    C[j] <- asian_price();
}


par(mfrow = c(1, 1))

hist(C, col = "green", breaks = 100)
legend("topleft", c(gettextf("mu = %f,  sigma = %f", 0, 0), gettextf("mu = %f,  sigma = %f", mean(C), sd(C) ^ 2)), cex = 0.8, col = c("red", "green"), pch = 1, lty = 1);
