# SDE -> dX = sigma * X * dW 
# Solution -> X = exp ( -0.5 * (sigma ^ 2) * 2 + sigma + W)
# mean(X) = 1
# variance(X) = exp( sigma ^ 2 * t )
# p = 0.5

T <- 10;
N <- 1000;
dt <- T / N;
sigma <- 0.2

# generate many paths
M <- 1000;
W <- rep(0, M);
GB <- rep(0, M);

for (i in 1:M) {
    dW <- rnorm(N, 0, sqrt(dt));
    W[i] <- sum(dW);
}

GB <- exp(-0.5 * (sigma ^ 2) * T + sigma + W)

par(mfrow = c(2, 1))
hist(W, col = "green")
hist(GB, col = "blue")

sprintf("Mean=%f Variance=%f", 1, exp(sigma ^ 2 * T))
sprintf("Mean=%f Variance=%f", mean(GB), sd(GB) ^ 2)


# SDE -> dX = mu * X * dt + sigma * X * dW 
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
M <- 10;
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
    XC[i] = exp((mu - 0.5 * (sigma ^ 2)) * T + sigma + W[i]);
}

par(mfrow = c(2, 1))
hist(XS, col = "green")
hist(XC, col = "blue")

sprintf("Mean=%f Variance=%f", mean(XS), sd(XS) ^ 2)
sprintf("Mean=%f Variance=%f", mean(XC), sd(XC) ^ 2)



##inputs:
alpha = 0;
sigma = 1;
T = 1;
n = 2 ^ (12);
X0 = 0.1;
#############Generate 1 trajectory
dt = T / n
t = seq(0, T, by = dt)
x = c(X0, alpha * dt + sigma * sqrt(dt) * rnorm(n, mean = 0, sd = 1))
Xt = cumsum(x)
par(mfrow = c(1, 1))
plot(t, Xt, type = 'l', xlab = "time")
sprintf("Mean=%f Variance=%f", mean(Xt), sd(Xt) ^ 2)



set.seed(12345)
f0 <- 102
mu <- (0.05)
sigma <- 0.08
T <- 0.5
t <- seq(1 / 365, T
f <- numeric(n)
f <- f0 * exp(cumsum((mu - sigma * sigma / 2) * T / n + sigma * sqrt(T / n) * rnorm(n)))
diff <- f - f0
plot(t, f, type = "l")