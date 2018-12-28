for (i in 1:N) {
}


#Simple Brownian
T = 1;
N = 10;
dt = T / N;
dW = sqrt(dt) * rnorm(N)
W = cumsum(dW)
plot(W, type = "l")


#CRR
#A[n+1] = A[n] * exp (sigma * sqrt(dt) * x[n])
T = 1;
N = 100000;
dt = T / N;
sigma <- 0.2
mu <- 0.1
r <- 0.01

p = 0.5 * (1 + ((mu - 0.5 * sigma ^ 2) / sigma) * sqrt(dt))
q = 0.5 * (1 + ((r - 0.5 * sigma ^ 2) / sigma) * sqrt(dt))

u <- runif(N, 0, 1)

spT <- rep(0, N);
sqT = rep(0, N);


spT[1] = 1
sqT[1] = 1

for (i in 1:N) {
    xp <- 2 * (u[i] < p) - 1
    xq <- 2 * (u[i] < q) - 1

    spT[i + 1] <- spT[i] * exp(sigma * sqrt(dt) * xp)
    sqT[i + 1] <- sqT[i] * exp(sigma * sqrt(dt) * xq)
}

par(mfrow = c(2, 1))
plot(spT, type = "l", col = "red")
plot(spT, type = "l", col = "red")
points(sqT, type = "l", col = "blue")




hist(spT)
hist(sqT)

plot(density(u))
