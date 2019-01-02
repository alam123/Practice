###################################################
# Édouard Tallent @ TaGoMa.Tech, November 2013    #
# QuantCorner @ https://quantcorner.wordpress.com  #
###################################################

# Least Squares Calibration
# Please see http://www.sitmo.com/article/calibrating-the-ornstein-uhlenbeck-model/

S <- c(3, 1.76, 1.2693, 1.196, 0.9468,
        0.9532, 0.6252, 0.8604, 1.0984,
        1.431, 1.3019, 1.4005, 1.2686,
        0.7147, 0.9237, 0.7297, 0.7105,
        0.8683, 0.7406, 0.7314, 0.6232)
n <- 20
delta <- 0.25

Sx <- sum(S[1:length(S) - 1])
Sy <- sum(S[2:length(S)])
Sxx <- crossprod(S[1:length(S) - 1], S[1:length(S) - 1])
Sxy <- crossprod(S[1:length(S) - 1], S[2:length(S)])
Syy <- crossprod(S[2:length(S)], S[2:length(S)])

a <- (n * Sxy - Sx * Sy) / (n * Sxx - Sx ^ 2)
b <- (Sy - a * Sx) / n
sd <- sqrt((n * Syy - Sy ^ 2 - a * (n * Sxy - Sx * Sy)) / (n * (n - 2)))

lambda <- -log(a) / delta
mu <- b / (1 - a)
sigma <- sd * sqrt(-2 * log(a) / delta / (1 - a ^ 2))


###################################################
# Édouard Tallent @ TaGoMa.Tech, November 2013    #
# QuantCorner @ https://quantcorner.wordpress.com  #
###################################################

# Maximum Likelihood Calibration
# PLease, see http://www.sitmo.com/article/calibrating-the-ornstein-uhlenbeck-model/

S <- c(3, 1.76, 1.2693, 1.196, 0.9468,
        0.9532, 0.6252, 0.8604, 1.0984,
        1.431, 1.3019, 1.4005, 1.2686,
        0.7147, 0.9237, 0.7297, 0.7105,
        0.8683, 0.7406, 0.7314, 0.6232)
n <- 20
delta <- 0.25

Sx <- sum(S[1:length(S) - 1])
Sy <- sum(S[2:length(S)])
Sxx <- crossprod(S[1:length(S) - 1], S[1:length(S) - 1])
Sxy <- crossprod(S[1:length(S) - 1], S[2:length(S)])
Syy <- crossprod(S[2:length(S)], S[2:length(S)])

mu <- (Sy * Sxx - Sx * Sxy) / (n * (Sxx - Sxy) - (Sx ^ 2 - Sx * Sy))
lambda <- -log((Sxy - mu * Sx - mu * Sy + n * mu ^ 2) / (Sxx - 2 * mu * Sx + n * mu ^ 2)) / delta
a <- exp(-lambda * delta)
sigmah2 <- (Syy - 2 * a * Sxy + a ^ 2 * Sxx - 2 * mu * (1 - a) * (Sy - a * Sx) + n * mu ^ 2 * (1 - a) ^ 2) / n
sigma <- sqrt(sigmah2 * 2 * lambda / (1 - a ^ 2))