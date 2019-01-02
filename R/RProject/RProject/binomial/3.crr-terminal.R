
#CRR
#A[n+1] = A[n] * exp (sigma * sqrt(dt) * x[n])
T = 1;
N = 10;
M = 10;
dt = T / N;
sigma <- 0.2
mu <- 0.1
r <- 0.01

p = 0.5 * (1 + ((mu - 0.5 * sigma ^ 2) / sigma) * sqrt(dt))
q = 0.5 * (1 + ((r - 0.5 * sigma ^ 2) / sigma) * sqrt(dt))

spT <- rep(0, M);
sqT <- rep(0, M);

sp <- 1
sq <- 1



for (i in 1:M) {
    for (j in 1:N) {
        u <- runif(1, 0, 1)
        xp <- 2 * (u[j] < p) - 1
        xq <- 2 * (u[j] < q) - 1

        sp <- sp * exp(sigma * sqrt(dt) * xp)
        sq <- sq * exp(sigma * sqrt(dt) * xq)
    }
    spT[i] <- sp
    sqT[i] <- sq
}


par(mfrow = c(2, 1))
hist(spT)
hist(sqT)
