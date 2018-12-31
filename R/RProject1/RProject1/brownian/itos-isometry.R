simItosIsemetry <- function(T, N) {
    # tim step size
    dt <- T / N;

    XG <- rnorm(N, 0, 1);
    XH <- rnorm(N, 0, 1);

    dWG <- rep(0, N);
    dWH <- rep(0, N);

    GdWG <- rep(0, N);
    HdWH <- rep(0, N);
    GHdT <- rep(0, N);

    for (i in 2:N) {
        t <- i * dt;

        dWG[i] <- sqrt(dt) * XG[i];
        dWH[i] <- sqrt(dt) * XH[i];

        GdWG[i] <- (t ^ 2 + sin(t)) * dWG[i];
        HdWH[i] <- (t ^ 3 + cos(t)) * dWH[i];
        GHdT[i] <- (t ^ 2 + sin(t)) * (t ^ 3 + cos(t)) * dt;
    }

    return(list(GdWG = GdWG, HdWH = HdWH, GHdT = GHdT));
}

T <- 10;
N <- 100;
dt <- T / N;

res <- simItosIsemetry(T, N);

# generate many paths
M <- 100;
IGdWG <- rep(0, M);
IHdWH <- rep(0, M);
IGHdT <- rep(0, M);

for (i in 1:M) {
    res <- simItosIsemetry(T, N);
    IGdWG[i] <- sum(res$GdWG);
    IHdWH[i] <- sum(res$HdWH);
    IGHdT[i] <- sum(res$GHdT);
}


par(mfrow = c(3, 1))
plot(seq(1:N), IGdWG, xlab = "time", ylab = "IGdWG", col = "red", type = "l");
plot(seq(1:N), IHdWH, xlab = "time", ylab = "IHdWH", col = "green", type = "l");
plot(seq(1:N), IGHdT, xlab = "time", ylab = "IGHdT", col = "purple", type = "l");

mean(IGdWG * IHdWH)
mean(IGHdT)