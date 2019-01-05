# W = z * sqrt(dt), z ~ N(0,1)

T <- 10;
N <- 1000;
dt <- T / N;

res <- rnorm(N, 0, sqrt(dt))
# generate many paths
M <- 3000;
ST <- rep(0, M);
for (i in 1:M) {
    res <- rnorm(N, 0, sqrt(dt));
    ST[i] =sum(res);
}

par(mfrow = c(2, 1))
plot(ST, col = "green") 
hist(ST, col = "blue", breaks = seq(-20, 20, 0.3))
legend("topleft", c(gettextf("mu = %f,  sigma = %f", 0, T), gettextf("mu = %f,  sigma = %f", mean(ST), sd(ST) ^ 2)), cex = 0.8, col = c("red", "green"), pch = 1, lty = 1);