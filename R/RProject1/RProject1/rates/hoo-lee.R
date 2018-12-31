#r[n] = r[n-1] + theta[n-1]*dt + sigma*sqrt(dt)x[n]
#Q branching probability = 1/2, Q(x=1,x=-1) = 1/2
#theta calibrated from bond prices

simHooLee <- function(T, N, mu, sigma) {
    dv <- rep(0, N)
    r <- rep(0, N)
    # Ho-Lee Short Rate Model
    for (i in 2:T) {
        dv[i] = sigma * sqrt(dt / T) * rnorm(1, mean = 0, sd = 1)
        r[i] = r[i - 1] + delta(i - 1) * (dt / T) + dv[i]
    }

    return(list(Sp = Sp, Sq = Sq));
}

T = 1;
N = 1000;
dt = T / N;
sigma <- 0.5
mu <- 0.1

simHooLee(T, mu, sigma, r)

plot(t, r, type = "l", col = "blue", xlab = "Time", ylab = "Instantaneous Short Rate",
       main = "Simulation of a One Factor Short-Rate Model 
       \n Ho Lee Model")