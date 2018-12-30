#CRR
#A[n+1] = A[n] * exp (mu * dt + sigma * sqrt(dt) * x[n])

simCRR <- function(T, N, mu, sigma, r) {
    # tim step size
    dt <- T / N;

    # branching probabilities
    p <- 0.5
    #q <- 0.5 * (1 + (r - mu) / sigma * sqrt(dt));
    q <- 0.5 * (1 + ((r - mu) / sigma) * sqrt(dt));

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
        Sp[i] <- Sp[1] * exp((mu - 0.5 * sigma ^ 2) * dt * i + sigma * sqrt(dt) * cumxp[i - 1]);
        Sq[i] <- Sq[1] * exp((mu - 0.5 * sigma ^ 2) * dt * i + sigma * sqrt(dt) * cumxq[i - 1]);
    }

    return(list(Sp = Sp, Sq = Sq));
}


T = 1;
N = 1000;
dt = T / N;
sigma <- 0.5
mu <- 0.1
r <- 0.01

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
p1 <- hist(SpT, breaks = seq(0, 10, 0.1))
p2 <- hist(SqT, breaks = seq(0, 10, 0.1))
plot(p1, col = "green") # first histogram
plot(p2, col = "red") # second
plot(p1, col = "green", add = T) # second

mean(SpT)
exp(mu)
mean(SqT)
exp(r)
