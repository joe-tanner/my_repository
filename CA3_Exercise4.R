## Question 3 

df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))

design_matrix <- df[, -1]
ones <- rep(1, times = nrow(df))
design_matrix <- cbind(ones, design_matrix)
data <- cbind(df$y, design_matrix)
data <- apply(data, 2, as.numeric)

# 5 parameters - B0, B1, B2, B3, sigma

nll_lm <- function(data, par) {
  
  beta <- par[1:4]
  X <- data[, -1]
  y <- data[, 1]
  sigma <- par[5]
  temp <- y - X %*% beta
  
  nll <- -sum(dnorm(temp, mean = 0, sd = sigma, log = TRUE))
  
  return(nll)
}

beta_hat <- mean(df$y)
inits <- c(beta_hat, 0, 0, 0, 5)

fit <- optim(inits, nll_lm, data = data, hessian = TRUE, method = "L-BFGS-B", lower = c(-50, -50, -50, -50, 0.01), upper = c(50, 50, 50, 50, 50)) 

ans1 <- fit$par

design_matrix <- df[, -1]
ones <- rep(1, times = nrow(df))
design_matrix <- cbind(ones, design_matrix)
X <- as.matrix(design_matrix)
y <- df[, 1]

ans2 <- solve(crossprod(X)) %*% crossprod(X, y)

B0 <- abs(ans1[1] - ans2[1])
B1 <- abs(ans1[2] - ans2[2])
B2 <- abs(ans1[3] - ans2[3])
B3 <- abs(ans1[4] - ans2[4])

print(c(B0, B1, B2, B3))


beta_hat <- ans2
n <- nrow(df)
mat <- y - X %*% beta_hat
k <- 4 # estimating 4 other parameters

sigma_hat2 <- (1/(n - k))*crossprod(mat)
sigma_hat <- sqrt(sigma_hat2)

sigma_ans1 <- ans1[5]
sigma_ans2 <- sigma_hat

S <- abs(sigma_ans1 - sigma_ans2)
print(S)

std_errors <- sqrt(diag(solve(fit$hessian)))
print(std_errors)

# Add values from lm
model <- lm(y ~ x1 + x2 + x3, df)

# Beta estimates
print(model$coefficients)

# Sigma estimate
print(summary(model)$sigma)
