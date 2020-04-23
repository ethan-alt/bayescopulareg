#' @examples
set.seed(1234)
n <- 100
M <- 1000

x <- runif(n, 1, 2)
y1 <- 0.25 * x + rnorm(100)
y2 <- rpois(n, exp(0.25 * x))

formula.list <- list(y1 ~ 0 + x, y2 ~ 0 + x)
family.list <- list(gaussian(), poisson())
data = data.frame(y1, y2, x)

## Perform copula regression sampling with default
## (noninformative) priors
sample <- mvbayesglm(
  formula.list, family.list, data, M = M
)
## Regression coefficients
summary(do.call(cbind, sample$betasample))

## Dispersion parameters
summary(sample$phisample)

## Posterior mean correlation matrix
apply(sample$Gammasample, c(1,2), mean)

## Fraction of accepted betas
colMeans(sample$betaaccept)

## Fraction of accepted dispersion parameters
colMeans(sample$phiaccept)

