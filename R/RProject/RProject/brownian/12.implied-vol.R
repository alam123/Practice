install.packages("RQuantLib");
, "quantmod", "rgl", "akima", "", "ggplot2");


library(RQuantLib)
library(quantmod)
library(rgl)
library(akima)
library(ggplot2)
library(plyr)

GetIV <- function(type, value,
                  underlying, strike, dividendYield, riskFreeRate, maturity, volatility,
                  timeSteps = 150, gridPoints = 151) {

    AmericanOptionImpliedVolatility(type, value,
                                    underlying, strike, dividendYield, riskFreeRate, maturity, volatility,
                                    timeSteps = 150, gridPoints = 151)$impliedVol
}


GetDelta <- function(type, underlying, strike,
                     dividendYield, riskFreeRate, maturity, volatility,
                     timeSteps = 150, gridPoints = 149, engine = "CrankNicolson") {

    AmericanOption(type, underlying, strike, dividendYield, riskFreeRate, maturity, volatility,
                   timeSteps = 150, gridPoints = 149, engine = "CrankNicolson")$delta
}
# set what symbol you want vol surface for
underlying <- 'GOOG'
# set what your volatility forcast or assumption is
volforcast <- .25
# Get symbols current price
underlying.price <- getQuote(underlying, what = yahooQF("Last Trade (Price Only)"))$Last

OC <- getOptionChain(underlying, NULL)
#check data
head(OC)
lputs <- lapply(OC, FUN = function(x) x$puts[grep("[A-Z]\\d{6}[CP]\\d{8}$", rownames(x$puts)),])
head(lputs) #check for NA values, yahoo returns all NA values sometimes
puts <- do.call('rbind', lputs)
#check data
head(puts, 5)

symbols <- as.vector(unlist(lapply(lputs, rownames)))
expiries <- unlist(lapply(symbols, FUN = function(x) regmatches(x = x, regexpr('[0-9]{6}', x))))
puts$maturity <- as.numeric((as.Date(expiries, "%y%m%d") - Sys.Date()) / 365)

puts$IV <- mapply(GetIV, value = puts$Ask, strike = puts$Strike, maturity = puts$maturity,
                  MoreArgs = list(type = 'put', underlying = underlying.price,
                                 dividendYield = 0, riskFreeRate = 0.01,
                                 volatility = volforcast), SIMPLIFY = TRUE)

puts$delta <- mapply(GetDelta, strike = puts$Strike, volatility = puts$IV,
                     maturity = puts$maturity, MoreArgs = list(type = 'put',
                                                              underlying = underlying.price, dividendYield = 0,
                                                              riskFreeRate = 0.01), SIMPLIFY = TRUE)

# subset out itm puts
puts <- subset(puts, delta < -.09 & delta > -.5)

expiries.formated <- format(as.Date(levels(factor(expiries)), format = '%y%m%d'), "%B %d, %Y")

fractionofyear.levels <- levels(factor(puts$maturity))

xyz <- with(puts, interp(x = maturity, y = delta * 100, z = IV * 100,
                         xo = sort(unique(maturity)), extrap = FALSE))

with(xyz, persp3d(x, y, z, col = heat.colors(length(z))[rank(z)], xlab = 'maturity',
                  ylab = 'delta', zlab = 'IV', main = 'IV Surface'))

putsplot <- ggplot(puts, aes(delta, IV, group = factor(maturity), color = factor(maturity))) +
    labs(x = "Delta", y = "Implied Volatilty", title = "Volatility Smile", color = "GooG \nExpiration") +
    scale_colour_discrete(breaks = c(fractionofyear.levels),
                           labels = c(expiries.formated)) +
                           geom_line() +
                           geom_point()

putsplot
