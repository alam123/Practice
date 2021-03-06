simCRR <- function(T, N, mu, sigma, r) {
    # tim step size
    dt <- T / N;

    # branching probabilities
    q <- 0.5 * (1 + ((r - 0.5 * sigma ^ 2) / sigma) * sqrt(dt));

    # generate Bernoullis
    u <- runif(N);
    xq <- 2 * (u < q) - 1;
    cumxq <- cumsum(xq);

    Sq <- rep(0, N)
    Sq[1] <- 1;

    for (i in 2:N) {
        Sq[i] <- Sq[1] * exp(sigma * sqrt(dt) * cumxq[i - 1]);
    }

    return(Sq = Sq);
}

simAltCRR <- function(T, N, mu, sigma, r) {
    # tim step size
    dt <- T / N;

    # branching probabilities
    q <- 0.5 * (1 + ((r - mu) / sigma) * sqrt(dt));

    # generate Bernoullis
    u <- runif(N);
    xq <- 2 * (u < q) - 1;
    cumxq <- cumsum(xq);

    Sq <- rep(0, N)
    Sq[1] <- 1;

    for (i in 2:N) {
        Sq[i] <- exp((mu - 0.5 * sigma ^ 2) * dt * i + sigma * sqrt(dt) * cumxq[i - 1]);
    }

    return(Sq = Sq);
}


T = 1.0;
N = 100;
dt = T / N;
sigma = 1.5
mu = 0.1
r = 0.01
S0 = 100
K = 20

#closed form solution
#V0 = S0 * phi(d1) - K * exp(-r*T) * phi(d2))
#d1 = (ln(S0/K) + (r + 0.5*sigma^2)(T2-T1))/(sigma*(sqrt(T2-T1))
#d2 = (ln(S0/K) + (r - 0.5*sigma^2)(T2-T1))/(sigma*(sqrt(T2-T1))

d1 <- (log(S0 / K) + (r + 0.5 * sigma ^ 2) * T) / (sigma * (sqrt(T)))
d2 <- (log(S0 / K) + (r - 0.5 * sigma ^ 2) * T) / (sigma * (sqrt(T)))
V0C <- S0 * pnorm(d1) - K * exp(-r * T) * pnorm(d2)

sprintf("d1=%f d2=%f V0=%f", d1, d2, V0C)

# CRR
M <- 100;
SqT <- rep(0, M);
VT <- rep(0, M);

for (i in 1:M) {
    res <- simCRR(T, N, mu, sigma, r);
    SqT[i] <- S0 * res[N];
    VT[i] = max(0, S0 * res[N] - K)
}

V0M <- exp(-r * T) * mean(VT)
sprintf("V0M=%f", V0M)

par(mfrow = c(4, 2))
plot(SqT, col = "green")
plot(VT, col = "red")
hist(VT, col = "blue")
hist(S0 * res, col = "purple")

# Alt CRR
M <- 100;
SqT <- rep(0, M);
VT <- rep(0, M);

for (i in 1:M) {
    res <- simAltCRR(T, N, mu, sigma, r);
    SqT[i] <- S0 * res[N];
    VT[i] = max(0, S0 * res[N] - K)
}

V0M <- exp(-r * T) * mean(VT)
sprintf("V0M=%f", V0M)

plot(SqT, col = "green")
plot(VT, col = "red")
hist(VT, col = "blue")
hist(S0 * res, col = "purple")