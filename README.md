# TSCI
R Package of TSCI

# Data generation
```
library(MASS)
library(TSCI)

# set seeds for reproducibility
RNGkind("L'Ecuyer-CMRG")
set.seed(1)
# dimension
p <- 10
# sample size
n <- 1000
# interaction value
inter_val <- 1
# the IV strength
a <- 1
# violation strength
tau <- 1
f <- function(x) {a * (1 * sin(2 * pi * x) + 1.5 * cos(2 * pi * x))}
rho <- 0.5
Cov <- stats::toeplitz(rho^c(0 : p))
mu <- rep(0, p + 1)
# true effect
beta <- 1
alpha <- as.matrix(rep(-0.3, p))
gamma <- as.matrix(rep(0.2, p))
inter <- as.matrix(c(rep(inter_val, 5),rep(0, p - 5)))


# generate the data
mu_error <- rep(0,2)
Cov_error <- matrix(c(1, 0.5, 0.5,1), 2, 2)
Error <- MASS::mvrnorm(n, mu_error, Cov_error)
W_original <- MASS::mvrnorm(n, mu, Cov)
W <- pnorm(W_original)
# instrument variable
Z <- W[, 1]
# baseline covariates
X <- W[, -1]
# generate the treatment variable D
D <- f(Z) + X %*% alpha + Z * X %*% inter + Error[, 1]
# generate the outcome variable Y
Y <- D * beta + tau * Z + X %*% gamma + Error[, 2]
```

# Create violation space candidates
```
vio_space <- create_monomials(Z, 4, "monomials_main")
```

# TSCI with random forest
```
output_RF <- tsci_forest(Y = Y, D = D, Z = Z, X = X, vio_space = vio_space)
summary(output_RF)
```

# TSCI with boosting
```
output_BO <- tsci_boosting(Y = Y, D = D, Z 0 Z, X = X, vio_space = vio_space)
summary(output_BO)
```

# TSCI with polynomials
```
output_PY <- tsci_poly(Y = Y, D = D, Z = Z, X = X)
summary(output_PY)
```

# TSCI with user defined hat matrix
```
A <- cbind(1, Z, Z^2, Z^3, Z^4, X)
weight <- A %*% chol2inv(chol(t(A) %*% A)) %*% t(A)
output_UD <- tsci_secondstage(Y = Y, D = D, Z = Z, X = X, vio_space = vio_space, weight = weight)
summary(output_UD)
```
