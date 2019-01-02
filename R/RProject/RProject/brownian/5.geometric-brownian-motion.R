# SDE -> dX / X = sigma * dW 
# Solution -> X = exp ( -0.5 * (sigma ^ 2) * 2 + sigma + W)
# mean(X) = 1
# variance(X) = exp( sigma ^ 2 * t )
# p = 0.5


T <- 10;
N <- 1000;
dt <- T / N;
sigma <- 0.2

# generate many paths for simulation and closed form
M <- 1000;
W <- rep(0, M);
XS <- rep(0, M);
XC <- rep(0, M);

for (i in 1:M) {
    dW <- rnorm(N, mean = 0, sd = sqrt(dt));
    W[i] <- sum(dW);
    X <- rep(0, M);

    X[1] <- 1;
    for (j in 2:N) {
        X[j] <- X[j - 1] + X[j - 1] * (sigma * dW[j - 1]);
    }

    XS[i] = X[N];
    XC[i] = exp((- 0.5 * (sigma ^ 2)) * T + sigma * W[i]);
}

par(mfrow = c(2, 1))
hist(XS, col = "green", breaks = 100)
hist(XC, col = "blue", breaks = 100)

sprintf("Mean=%f Variance=%f", mean(XS), sd(XS) ^ 2)
sprintf("Mean=%f Variance=%f", mean(XC), sd(XC) ^ 2)

# SDE -> dX / X = mu * dt + sigma * dW 
# Solution -> X = X0 * exp ((mu - 0.5 * sigma ^ 2) * T + sigma + W)
# mean(X) = mu
# variance(X) = sigma ^ 2 ????
# p = 0.5

T <- 10;
N <- 1000;
dt <- T / N;
sigma <- 0.2
mu <- 0.2

# generate many paths for simulation and closed form
M <- 1000;
W <- rep(0, M);
XS <- rep(0, M);
XC <- rep(0, M);

for (i in 1:M) {
    dW <- rnorm(N, mean = 0, sd = sqrt(dt));
    W[i] <- sum(dW);
    X <- rep(0, M);

    X[1] <- 1;
    for (j in 2:N) {
        X[j] <- X[j - 1] + X[j - 1] * (mu * dt + sigma * dW[j - 1]);
    }

    XS[i] = X[N];
    XC[i] = exp((mu - 0.5 * (sigma ^ 2)) * T + sigma * W[i]);
}

par(mfrow = c(2, 1))
hist(XS, col = "green", breaks = 100)
hist(XC, col = "blue", breaks = 100)

sprintf("Mean=%f Variance=%f", mean(XS), sd(XS) ^ 2)
sprintf("Mean=%f Variance=%f", mean(XC), sd(XC) ^ 2)


#Geometric Brownian Motion
maturity <- 15
simulation.length <- 10001
dt <- maturity / (simulation.length - 1)

timeline <- seq(0, maturity, dt)

S0 <- 1
r <- 0.05
mu <- 0.1
mu0 <- 0.2
sigma <- 0.2
sigma0 <- 0.375

f <- g <- g0 <- h <- h0 <- rep(0, times = simulation.length)
g0[1] <- h0[1] <- g[1] <- h[1] <- S0

for (i in 2:simulation.length) {
    f[i] <- f[i - 1] + sqrt(dt) * rnorm(1)
    g[i] <- g[1] * exp((mu - (sigma ^ 2) / 2) * (i - 1) * dt + sigma * f[i])
    g0[i] <- g0[1] * exp(mu * (i - 1) * dt)
    h[i] <- h[1] * exp((mu0 - (sigma0 ^ 2) / 2) * (i - 1) * dt + sigma0 * f[i])
    h0[i] <- h0[1] * exp(mu0 * (i - 1) * dt)
}

o_range <- range(f, g, g0, h, h0)

plot(timeline, f, ylim = o_range, type = "l", col = "black")
lines(timeline, g0, col = "red")
lines(timeline, g, col = "green")
lines(timeline, h, col = "blue")
lines(timeline, h0, col = "orange")

title(main = "Geometric Brownian Motion trajectories", col.main = "red", font.main = 4)



legend(1, o_range[2], c("mu = 0.2,  sigma = 0.375", "mu = 0.1,  sigma = 0.2", "Brownian motion"), cex = 0.8,
   col = c("blue", "green", "black"), pch = 1, lty = 1);