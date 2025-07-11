library(nlme)
library(dplyr)
library(performance)
library(forestat)
data(larch)
colnames(larch) <- make.names(colnames(larch))
larch <- larch %>%
  select(PLOT, AGE.GROUP, D, H, CW) %>%
  rename(AgeGroup = AGE.GROUP) %>%
  mutate(PLOT     = factor(PLOT),
         AgeGroup = factor(AgeGroup))
print(head(larch))
print(table(larch$AgeGroup, larch$PLOT))
lm_init <- lm(log(CW) ~ log(D) + log(H), data = larch)
start_vals <- list(a = exp(coef(lm_init)[1]),
                   b = coef(lm_init)[2],
                   c = coef(lm_init)[3])
print(start_vals)
mod_plot <- nlme(
  CW ~ a * D^b * H^c,
  data   = groupedData(CW ~ D | PLOT, data = larch),
  fixed  = a + b + c ~ 1,
  random = a ~ 1 | PLOT,   # random intercept only
  start  = unlist(start_vals),
  method = "ML"
)
summary(mod_plot)
mod_nested <- nlme(
  CW ~ a * D^b * H^c,
  data   = groupedData(CW ~ D | AgeGroup/PLOT, data = larch),
  fixed  = a + b + c ~ 1,
  random = a ~ 1 | AgeGroup/PLOT,
  start  = unlist(start_vals),
  method = "ML"
)
summary(mod_nested)
print(AIC(mod_plot, mod_nested))
pseudo_r2 <- function(model, data, group = NULL) {
  y <- data$CW
  y_fixed <- predict(model, level = 0)    
  y_mixed <- predict(model)               
  ss_tot  <- sum((y - mean(y))^2)
  R2_marg <- 1 - sum((y - y_fixed)^2) / ss_tot
  R2_cond <- 1 - sum((y - y_mixed)^2) / ss_tot
  c(marginal = R2_marg, conditional = R2_cond)
}
R2_plot   <- pseudo_r2(mod_plot,   larch)
R2_nested <- pseudo_r2(mod_nested, larch)
print(list(R2_plot   = R2_plot,
           R2_nested = R2_nested))
