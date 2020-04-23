#' @examples
set.seed(123)
n <- 500
M <- 10000

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
sample <- mvbayesglm(
  formula.list, family.list, data, M = M
)

postmean <- function(smpl, fam.list, burn = 1000) {
  post.beta <- do.call(cbind, smpl$betasample)
  post.beta <- post.beta[-(1:burn), ]
  post.beta <- colMeans(post.beta)
  post.disp <- colMeans(smpl$phisample[-(1:burn), ])
  res <- rbind('beta' = post.beta, 'dispersion' = post.disp)
  colnames(res) <- lapply(fam.list, function(f) f$family)
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
postmean(sample, family.list, burn = 2000)



sample2 <- mvbayesglm(
  formula.list[c(1, 3)], family.list[c(1, 3)], data, M = M
)
sapply(glm.list[c(1, 3)], get_params)
postmean(sample2, family.list[c(1, 3)], burn = 2000)


