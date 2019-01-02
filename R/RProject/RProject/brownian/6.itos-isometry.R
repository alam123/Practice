G <- function(t) {
    return (sin(t) ^ 2 + t);
}

H <- function(t) {
    return(t ^ 3 + t - t / 2);
}

simItosIsemetry <- function(T, N) {
    # tim step size
    dt <- T / N;

    t <- rep(0, N);

    XG <- rnorm(N, 0, 1);
    XH <- rnorm(N, 0, 1);

    dW <- rep(0, N);

    GdW <- rep(0, N);
    HdW <- rep(0, N);
    GHdT <- rep(0, N);

    for (i in 2:N) {
        t[i] <- i * dt;

        dW[i] <- sqrt(dt) * XH[i];

        GdW[i] <- G(t[i]) * dW[i];
        HdW[i] <- H(t[i]) * dW[i];
        GHdT[i] <- G(t[i]) * H(t[i]) * dt;
    }

    return(list(GdW = GdW, HdW = HdW, GHdT = GHdT));
}

T <- 1;
N <- 1000;

res <- simItosIsemetry(T, N);

# generate many paths
M <- 1000;
IGdW <- rep(0, M);
IHdW <- rep(0, M);
IGHdT <- rep(0, M);

for (i in 1:M) {
    res <- simItosIsemetry(T, N);
    IGdW[i] <- sum(res$GdW);
    IHdW[i] <- sum(res$HdW);
    IGHdT[i] <- sum(res$GHdT);
}


par(mfrow = c(3, 1))
hist(IGdW, xlab = "time", ylab = "IGdW", col = "red");
hist(IHdW, xlab = "time", ylab = "IHdW", col = "green");
hist(IGHdT, xlab = "time", ylab = "IGHdT", col = "purple");

mean(IGdW)
mean(IHdW)
mean(IGdW * IHdW)
mean(IGHdT)