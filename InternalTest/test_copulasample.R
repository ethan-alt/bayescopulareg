n <- 100
M <- 10

x <- runif(n, 1, 2)
y1 <- 0.25 * x + rnorm(100)
y2 <- rpois(n, exp(0.25 * x))

formula.list <- list(y1 ~ 0 + x, y2 ~ 0 + x)
family.list <- list(gaussian(), poisson())
data = data.frame(y1, y2, x)

try <- sample_copula(
  formula.list, family.list, data, M = M
)