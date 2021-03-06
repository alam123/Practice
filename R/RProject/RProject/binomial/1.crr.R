simCRR <- function(T, N, mu, sigma, r) {
    # tim step size
    dt <- T / N;

    # branching probabilities
    p <- 0.5 * (1 + mu / sigma * sqrt(dt));
    q <- 0.5 * (1 + ((r - 0.5 * (sigma ^ 2)) / sigma) * sqrt(dt));

    # generate Bernoullis
    u <- runif(N);
    xp <- 2 * (u < p) - 1;
    xq <- 2 * (u < q) - 1;
    cumxp <- cumsum(xp);
    cumxq <- cumsum(xq);

    Sp <- rep(0, N)
    Sq <- rep(0, N)
    Sp[1] <- 1;
    Sq[1] <- 1;

    for (i in 2:N) {
        Sp[i] <- Sp[1] * exp(sigma * sqrt(dt) * cumxp[i - 1]);
        Sq[i] <- Sq[1] * exp(sigma * sqrt(dt) * cumxq[i - 1]);
    }

    return(list(Sp = Sp, Sq = Sq));
}


T = 1;
N = 100;
dt = T / N;
sigma <- 0.5
mu <- 0.1
r <- 0.01

result <- simCRR(T, N, mu, sigma, r)

# generate many paths
M <- 5000;
SpT <- rep(0, M);
SqT = rep(0, M);
for (i in 1:M) {
    res <- simCRR(T, N, mu, sigma, r);
    SpT[i] = res$Sp[N];
    SqT[i] = res$Sq[N];
}

par(mfrow = c(2, 1))

hist(SpT, col = "green", breaks = 100)
legend("topleft", c(gettextf("mu = %f,  sigma = %f", mu * T, (sigma ^ 2) * T), gettextf("mu = %f,  sigma = %f", mean(log(SpT)), sd(log(SpT)) ^ 2)), cex = 0.8, col = c("red", "green"), pch = 1, lty = 1);

hist(SqT, col = "green", breaks = 100)
hist(SpT, col = "blue", breaks = 100, add = T)
legend("topleft", c(gettextf("mu = %f,  sigma = %f", (r - 0.5 * (sigma ^ 2)) * T, (sigma ^ 2) * T), gettextf("mu = %f,  sigma = %f", mean(log(SqT)), sd(log(SqT)) ^ 2)), cex = 0.8, col = c("red", "green"), pch = 1, lty = 1);

