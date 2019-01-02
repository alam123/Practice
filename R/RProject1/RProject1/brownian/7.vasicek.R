# W = z * sqrt(dt), z ~ N(0,1)

simVasicek <- function(T, N, r0, kappa, theta) {
    # tim step size
    dt <- T / N;

    dW <- rnorm(N, 0, sqrt(dt));

    r <- rep(0, N);
    r[0] <- r0;

    for (i in 2:N) {
        r[i] <- r[i-1] + kappa * (theta - r[i-1]) * dt + sigma * dW[i]
    }

    return(r = r);
}

T <- 10;
N <- 1000;
dt <- T / N;
mu <- 0.1
sigma <- 0.02
theta <- 0.02
kappa <- 0.02
r0 <- 0.05


r <- simVasicek(T, N, r0, kappa, theta);

par(mfrow = c(1, 1))
plot(r, col = "green", type="l")
mean(r)

