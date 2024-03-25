
calc_posterior <- function(samples, p_grid, prior) {
  water <- sum(samples == "W")
  likelihood <- dbinom(water, length(samples), prob = p_grid)
  posterior <- likelihood * prior
  posterior <- posterior / sum(posterior)
  data.frame(x = p_grid, posterior = posterior)
}

plot_it <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x, posterior)) +
    ggplot2::geom_point(ggplot2::aes(shape = ".", alpha = 0.1)) +
    ggplot2::geom_line() +
    ggplot2::xlab("Probability of water") +
    ggplot2::ylab("Posterior probability")
}

## Q 2M1.
p_grid <- seq(0, 1, length.out = grid_points)
prior <- rep(1, length(p_grid))

sample_1 <- c("W", "W", "W")
posterior <- calc_posterior(sample_1, p_grid, prior)
plot_it(posterior)

sample_2 <- c("W", "W", "W", "L")
posterior <- calc_posterior(sample_2, p_grid, prior)
plot_it(posterior)

sample_3 <- c("L", "W", "W", "L", "W", "W", "W")
posterior <- calc_posterior(sample_3, p_grid, prior)
plot_it(posterior)


## Q 2M2
calc_new_prior <- function(p_grid) {
  prior <- rep(1, length(p_grid))
  prior[p_grid < 0.5] = 0
  prior
}
prior <- calc_new_prior(p_grid)

posterior <- calc_posterior(sample_1, p_grid, prior)
plot_it(posterior)

posterior <- calc_posterior(sample_2, p_grid, prior)
plot_it(posterior)

posterior <- calc_posterior(sample_3, p_grid, prior)
plot_it(posterior)

## Q 2M3

# Assume equal chance of Earth or Mars globe being tossed, given that
# we have observed land, what is the probability we tossed the Earth globe?

# P(E|l) = P(l|E)*P(E) / P(l) by Bayes theorem
# P(l|E) is 0.3, P(E) is 0.5, P(L) is P(l|E) + P(l|M) = 0.5 * 0.3 + 0.5

# gives

prob = (0.3 * 0.5) / (0.5 * 0.3 + 0.5)


