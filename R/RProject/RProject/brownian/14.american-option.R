q_prob = function(r, delta_t, sigma) {
    u = exp(sigma * sqrt(delta_t))
    d = exp(-sigma * sqrt(delta_t))

    return((exp(r * delta_t) - d) / (u - d))
}

build_stock_tree <- function(S, sigma, delta_t, div_yield, T, N) {
    tree <- matrix(0, nrow = N + 1, ncol = N + 1)

    u <- exp(+sigma * sqrt(delta_t))
    d <- exp(-sigma * sqrt(delta_t))

    for (i in 1:(N + 1)) {
        j <- 1:i
        tree[i, j] <- S * u ^ (j - 1) * d ^ ((i - 1) - (j - 1)) * exp(-div_yield * delta_t * (i - 1))
    }
    tree
}

value_binomial_option <- function(tree, sigma, delta_t, r, X, type) {
    q <- q_prob(r, delta_t, sigma)
    option_tree <- matrix(0, nrow = nrow(tree), ncol = ncol(tree))
    option_tree[nrow(option_tree),] <- if (type == 'put') pmax(X - tree[nrow(tree),], 0) else pmax(tree[nrow(tree),] - X, 0)
    for (i in (nrow(tree) - 1):1) {
        j <- 1:i
        exercise.payoff <- if (type == 'put') pmax(X - tree[i, j], 0) else pmax(tree[i, j] - X, 0)
        hold.payoff <- ((1 - q) * option_tree[i + 1, j] + q * option_tree[i + 1, j + 1]) / exp(r * delta_t)
        option_tree[i, j] <- pmax(exercise.payoff, hold.payoff)
    }
    option_tree
}


stock =

binomial_option = function(type, sigma, T, r, X, S, div_yield, N) {
    q = q_prob(r = r, delta_t = T / N, sigma = sigma)
    tree = build_stock_tree(S = S, sigma = sigma, delta_t = T / N, div_yield = div_yield, N = N)
    option = value_binomial_option(tree, sigma = sigma, delta_t = T / N, r = r, X = X, type = type)
    delta = (option[2, 2] - option[2, 1]) / (tree[2, 2] - tree[2, 1])
    return(list(q = q, stock = tree, option = option, price = option[1, 1], delta = delta))
}

delta = function(binomial_option, row, col) {
    stock_tree = binomial_option$stock
    option_tree = binomial_option$option
    return((option_tree[row + 1, col + 1] - option_tree[row + 1, col]) / (stock_tree[row + 1, col + 1] - stock_tree[row + 1, col]))
}


binomial_option(type = 'call', sigma = 0.2, T = 3, r = 0.1, X = 100, S = 150, div_yield = 0.05, N = 3)