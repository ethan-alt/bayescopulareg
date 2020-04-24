#' @examples
set.seed(123)
n <- 500
M <- 100
thin <- 50

x <- runif(n, 1, 2)
y1 <- 0.25 * x + rnorm(100)
y2 <- rpois(n, exp(0.25 * x))
y3 <- rgamma(n, 1, scale = exp(0.25 * x))
y4 <- rbinom(n, size = 1, prob = boot::inv.logit(0.25 * x))

formula.list <- list(y1 ~ 0 + x, y2 ~ 0 + x, y3 ~ 0 + x, y4 ~ 0 + x)
family.list <- list(gaussian(), poisson(), Gamma('log'), binomial())
data = data.frame(y1, y2, y3, y4, x)

## Perform copula regression sampling with default
## (noninformative) priors
sample <- bayescopulaglm(
  formula.list, family.list, data, M = M, thin = thin
)

postmean <- function(smpl) {
  post.beta <- do.call(cbind, smpl$betasample)
  post.beta <- colMeans(post.beta)
  post.disp <- colMeans(smpl$phisample)
  res <- rbind('beta' = post.beta, 'dispersion' = post.disp)
  return(res)
}

glm.list <- list()
for (i in 1:length(formula.list)) {
  glm.list[[i]] <- glm(formula.list[[i]], family.list[[i]], data)
}
get_params <- function(fit) {
  c('beta' = fit$coefficients, 'dispersion' = summary(fit)$dispersion)
}
sapply(glm.list, get_params)
postmean(sample)
apply(sample$Gammasample, c(1, 2), mean)
# 
# 
# 
# sample2 <- mvbayesglm_wrapper(
#   formula.list[c(1, 3)], family.list[c(1, 3)], data, M = M
# )
# sapply(glm.list[c(1, 3)], get_params)
# postmean(sample2, burn = 2000)
# 
# 
# Gamma <- rbind(
#   c(1, 0.5, 0.25),
#   c(0.5, 1, 0.125),
#   c(0.25, 0.125, 1)
# )
# Z <- mvtnorm::rmvnorm(n = n, sigma = Gamma[1:2, 1:2])
# U <- pnorm(Z)
# Y <- U
# Y[, 1] <- qnorm(U[, 1], mean = 0.25 * x, sd = 1)
# Y[, 2] <- qgamma(U[, 2], shape = 1, scale = exp(0.25 * x))
# data <- data.frame(Y, x)
# names(data)[1:2] <- c('y1', 'y2')
# 
# 
# formula.list <- list(y1 ~ 0+x, y2 ~ 0+x)
# family.list <- list(gaussian(), Gamma())
# sample3 <- mvbayesglm(
#   formula.list, family.list, data, M = M
# )
# 
# for( i in 1:2 ) {
#   glm.list[[i]] <- glm(formula.list[[i]], family.list[[i]], data)
# }
# 
# sapply(glm.list[1:2], get_params)
# postmean(sample3, burn = 2000)
# apply(sample3$Gammasample, c(1, 2), mean)
# 
# 
# 
# 
# 
# data$y2 <- qnorm(U[, 2], mean = 0.5 * x, sd = 2)
# formula.list <- list(y1 ~ 0+x, y2 ~ 0+x)
# family.list <- list(gaussian(), gaussian())
# sample4 <- mvbayesglm(
#   formula.list, family.list, data, M = M
# )
# 
# for( i in 1:2 ) {
#   glm.list[[i]] <- glm(formula.list[[i]], family.list[[i]], data)
# }
# 
# sapply(glm.list, get_params)
# postmean(sample4, burn = 2000)
# apply(sample4$Gammasample, c(1, 2), mean)
# 
# library(systemfit)
# freq <- systemfit::systemfit(
#   formula.list,
#   method = 'SUR',
#   data = data
# )
# cov2cor(freq$residCov)


