################# Comparing nlme vs Bayesian estimation ####################

# packages
library(nlme)
library(brms)
library(ggmcmc)
library(MASS)
# Create dataset ------------------
n2 <- 40                                                   # number of clusters (should be even so that half of the clusters in control and other half in intervention)
n1 <- 5                                                    # common cluster size
cluster_id <- rep(1:n2, each = n1)                                  # ID for clusters
condit <- c(rep(0, each = (n1*n2)/2), rep(1,each = (n1*n2)/2))    # treatment condition

cov1 <- matrix(c(35, 15, 15, 40), 2, 2)                          # covariance matrix level 1
cov2 <- matrix(c(10, 4, 4, 12), 2, 2)

u <- mvrnorm(n2, mu = c(0,0), Sigma = cov2)
u1 <- u[,1]
u2 <- u[,2]

u1 <- rep(u1, each = n1)
u2 <- rep(u2, each = n1)

e <- mvrnorm(n1*n2, mu = c(0,0), Sigma = cov1)
e1 <- e[,1]
e2 <- e[,2]

y1 <- round(50 + 5*condit + u1 + e1)
y2 <- round(60 + 8*condit + u2 + e2)

dataset <- cbind(y1, y2, cluster_id, condit)
dataset <- as.data.frame(dataset)


## Long data format---------
dataset$obs_id <- 1:nrow(dataset)
long_data <- dataset %>%
    pivot_longer(
        cols = c(y1, y2),
        names_to = "outcome",
        values_to = "value"
    ) %>%
    mutate(
        outcome_fac = factor(outcome),
        outcome_num = as.numeric(outcome_fac)
    )
#------------------ Model with nlme ---------------------
model_nlme <- lme(
    value ~ 0 + outcome_fac + outcome_fac:condit,
    # Random intercept
    random = list(cluster_id = pdSymm(~ outcome_fac - 1)),
    # Correlation of observations within the clusters and within the same subject
    correlation = corSymm(form = ~ outcome_num | cluster_id/obs_id),
    # Variance of residuals per outcome
    weights = varIdent(form = ~ 1 | outcome_fac),
    data = long_data,
    control = lmeControl(opt = "optim", msMaxIter = 1000)
)
summary(model_nlme)

# variance-covariance of the fixed effects
var_cov_fixeff <- vcov(model_nlme)

# Variance-covariance matrix of random effects
var_random_eff <- getVarCov(model_nlme)

#------------------- Model with brms ------------------------
bf_model <- bf(mvbind(y1, y2) ~ condit + (1|int|cluster_id)) + set_rescor(TRUE)

fit_1 <- brm(bf_model, data = dataset, chains = 2, cores = 2)
