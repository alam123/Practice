#Alternate CRR
#Q = MAX(0, ST-K)
#A[n+1] = A[n] * exp (mu * dt + sigma * sqrt(dt) * x[n])

simCRR <- function(T, N, mu, sigma, r) {
    # tim step size
    dt <- T / N;

    # branching probabilities
    q <- 0.5 * (1 + (r - mu) / sigma * sqrt(dt));

    # generate Bernoullis
    u <- runif(N);
    xq <- 2 * (u < q) - 1;
    cumxq <- cumsum(xq);

    Sq <- rep(0, N)
    Sq[1] <- 1;

    for (i in 2:N) {
        Sq[i] <- Sq[1] * exp((mu - 0.5 * sigma ^ 2) * dt + sigma * sqrt(dt) * cumxq[i - 1]);
    }

    return(Sq = Sq);
}


T = 1.0;
N = 1000;
dt = T2 / N;
sigma = 0.5
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

# generate many paths
M <- 5000;
SqT <- rep(0, M);
VT <- rep(0, M);

for (i in 1:M) {
    res <- simCRR(T2, N, mu, sigma, r);
    SqT[i] <- res[N];
    VT[i] = max(0, S0 * res[N] - K)
}

V0M <- exp(-r * T) * mean(VT)
sprintf("V0M=%f", V0M)

par(mfrow = c(2, 1))
plot(VT, col = "green") # first histogram
hist(VT, col = "red") # second