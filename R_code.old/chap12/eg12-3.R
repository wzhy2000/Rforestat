# 构建数据集
library(brms)
set.seed(123)
n <- 200
X1 <- rnorm(n)
X2 <- rnorm(n)
y <- 3 * sin(2 * X1) + 2 * cos(3 * X2) + rnorm(n)
data <- data.frame(X1 = X1, X2 = X2, y = y)

# 划分数据集
train_indices <- sample(1:n, size = 0.7 * n)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# 模型构建
model <- brm(
  y ~ sin(2 * X1) + cos(3 * X2),
  data = train_data,
  family = gaussian(),
  iter = 2000,
  warmup = 1000,
  chains = 4
)
summary(model)

# 模型预测
train_preds <- predict(model, newdata = train_data)[, "Estimate"]
test_preds <- predict(model, newdata = test_data)[, "Estimate"]

# 模型评估
train_mse <- mean((train_data$y - train_preds)^2)
train_cor <- cor(train_data$y, train_preds)
test_mse <- mean((test_data$y - test_preds)^2)
test_cor <- cor(test_data$y, test_preds)

# 采样结果分析
plot(model)
stan_fit <- model$fit
samples_with_warmup <- rstan::extract(stan_fit, inc_warmup = TRUE, permuted = FALSE)

library(bayesplot)
# mcmc_trace(samples_with_warmup)

mcmc_trace(samples_with_warmup, pars = c("b_Intercept", "b_sin2MUX1", "b_cos3MUX2", "sigma", "lprior", "lp__"))


model1 <- brm(
  y ~ sin(2 * X1) + cos(3 * X2),
  data = train_data,
  family = gaussian(),
  iter = 2000,
  warmup = 100,
  chains = 4)
plot(model1)
