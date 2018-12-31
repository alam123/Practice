# W = z * sqrt(dt), z ~ N(0,1)

simBrownian <- function(T, N) {
    # tim step size
    dt <- T / N;

    X <- rnorm(N, 0, 1);
    cumx <- cumsum(x);

    X <- rep(0, N)

    for (i in 2:N) {
        X[i] <- sqrt(dt) * cumx[i - 1];
    }

    return(B = X);
}

T <- 10;
N <- 1000;
dt <- T / N;

res <- simBrownian(T, N);

# generate many paths
M <- 1000;
ST <- rep(0, M);
for (i in 1:M) {
    res <- simBrownian(T, N);
    ST[i] = res[N];
}

par(mfrow = c(2, 1))
plot(ST, col = "green") 
hist(ST, col = "blue", breaks = seq(-20, 20, 0.3))

sprintf("Mean=%f Variance=%f", mean(ST), sd(ST)^2)
