library(tidyverse)        
library(brms)            
library(posterior)       
library(pROC)     
library(PRROC)
library(tidyr)      
library(forestat)

# 数据加载与预处理
obj <- load("JFSP_all_df.rda ")
raw <- get(obj)

seedling <- raw %>% 
  select(Species, LiveDead, heat_load, slope, roughness, wyr1_suVPDmu) %>% 
  mutate(
    LiveDead = as.integer(LiveDead),
    Species  = factor(Species)
  ) %>% 
  drop_na()


# pdf("不同存活状态下样本的热负荷分布.pdf", width = 10, height = 6, family = "GB1")
ggplot(seedling, aes(heat_load, fill = factor(LiveDead))) +
  geom_density(alpha = 0.4) +
  labs(x = "热负荷", y = "密度", fill = "死亡(1) / 存活(0)") + 
  theme(
    axis.title.x = element_text(size = 26, color = "black"),  # x轴标题字体大小
    axis.title.y = element_text(size = 26, color = "black"),  # y轴标题字体大小
    axis.text.x = element_text(size = 26, color = "black"),   # x轴文本字体大小
    axis.text.y = element_text(size = 26, color = "black"),   # y轴文本字体大小
    legend.text = element_text(size = 26),
    legend.title = element_text(size = 26)
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank()
  )

# dev.off()

# 数据集划分
set.seed(123)
idx <- sample(1:nrow(seedling), 0.7 * nrow(seedling))
train <- seedling[idx, ]
test <- seedling[-idx, ]

# 模型构建
prior.weak <- set_prior("normal(0, 2)", class = "b")
bayes.mod <- brm(
  LiveDead ~ Species + heat_load + slope + roughness + wyr1_suVPDmu,
  data   = train,
  family = bernoulli(link = "logit"),
  prior  = prior.weak,
  chains = 4, iter = 3000, warmup = 1000, cores = 4, seed = 123,
  control = list(adapt_delta = 0.95, max_treedepth = 12))

summary(bayes.mod)

pdf("森林图.pdf", width = 8, height = 6)
mcmc_plot(bayes.mod) + 
  theme(
    text = element_text(size = 26)
  )
dev.off()

pdf("后验预测检验的密度叠加图.pdf", width = 8, height = 6)
pp_check(bayes.mod, type = "dens_overlay")+ 
  theme(
    text = element_text(size = 26),
    legend.text = element_text(size = 26),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 40)  # 上右下左，单位为“pt”
  )
dev.off()


library(posterior)
# 把 brmsfit 转成 draws_df 
draws.df <- as_draws_df(bayes.mod)

# 取出所有 b_ 开头的系数列 
sel.vars <- grep("^b_", variables(draws.df), value = TRUE)
draw.b   <- subset_draws(draws.df, variable = sel.vars)

# 计算 OR 及 95% 可信区间
OR.tbl <- summarise_draws(
  draw.b,
  OR      = ~median(exp(.x)),
  .lower  = ~quantile(exp(.x), 0.025),
  .upper  = ~quantile(exp(.x), 0.975)
)

print(OR.tbl)

pdf("固定效应系数的优势比及其95可信区间.pdf", width = 12, height = 6)
ggplot(OR.tbl, aes(reorder(variable, OR), OR)) +
  geom_pointrange(aes(ymin = `2.5%`, ymax = `97.5%`)) +
  coord_flip() +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(x = NULL, y = "OR") + 
  theme(
    axis.title.x = element_text(size = 26, color = "black"),  # x轴标题字体大小
    axis.title.y = element_text(size = 26, color = "black"),  # y轴标题字体大小
    axis.text.x = element_text(size = 26, color = "black"),   # x轴文本字体大小
    axis.text.y = element_text(size = 26, color = "black"),   # y轴文本字体大小
    # panel.grid.major = element_blank(),                         # 去掉主网格线
    # panel.grid.minor = element_blank()
  )
dev.off()



# 模型性能评估
pre.train <- posterior_predict(bayes.mod, newdata = train) %>% colMeans()
FittingEvaluationIndex(pre.train, train$LiveDead)

loo.res <- loo(bayes.mod)
print(loo.res)

# 模型测试
pre.test <- posterior_predict(bayes.mod, newdata = test) %>% colMeans()
FittingEvaluationIndex(pre.test, test$LiveDead)


# ROC曲线
roc.train <- roc(train$LiveDead, pre.train)
roc.test <- roc(test$LiveDead, pre.test)
pdf("roc贝叶斯.pdf", width = 8, height = 8)
plot(roc.train, col = "blue", lwd = 3, legacy.axes = TRUE,
     mar = c(6, 6, 2, 2), mgp = c(4, 1, 0),
     cex.lab = 2.2, cex.axis = 2.2)
plot(roc.test, col = "red", lwd = 3, add = TRUE)
legend("bottomright",
       legend = c(paste0("Train AUC = ", round(auc(roc.train), 3)),
                  paste0("Test AUC = ",  round(auc(roc.test),  3))),
       col = c("blue", "red"), lwd = 3, cex = 1.5)
dev.off()


# PR曲线
pr.train <- pr.curve(scores.class0 = pre.train[train$LiveDead == 1],
                     scores.class1 = pre.train[train$LiveDead == 0],
                     curve = TRUE)
pr.test <- pr.curve(scores.class0 = pre.test[test$LiveDead == 1],
                    scores.class1 = pre.test[test$LiveDead == 0],
                    curve = TRUE)
pdf("PR贝叶斯.pdf", width = 8, height = 8)
par(mar = c(6, 6, 2, 2), mgp = c(4, 1, 0))
plot(pr.train, col = "blue", lwd = 3, auc.main = FALSE,
     cex.lab = 2.2, cex.axis = 2.2, main = "", xlim = c(0, 1), ylim = c(0, 1))
# 添加测试集 PR 曲线
plot(pr.test, col = "red", lwd = 3, add = TRUE)
abline(a = 1, b = -1, col = "gray", lty = 2, lwd = 2)
legend("bottomright",
       legend = c(paste0("Train AUC = ", round(pr.train$auc.integral, 3)),
                  paste0("Test AUC = ",  round(pr.test$auc.integral, 3))),
       col = c("blue", "red"), lty = 1, lwd = 3, cex = 1.5)
dev.off()



# 情景预测
newdat <- expand_grid(
  Species = levels(seedling$Species),
  heat_load = seq(min(seedling$heat_load), max(seedling$heat_load), length.out = 30),
  slope = median(seedling$slope),
  roughness = median(seedling$roughness),
  wyr1_suVPDmu = median(seedling$wyr1_suVPDmu)
)


# newdat$death_prob <- fitted(bayes.mod, newdata = newdat)[, "Estimate"]
newdat$death_prob2 <- posterior_predict(bayes.mod, newdata = newdat) %>% colMeans()

pdf("不同物种随热负荷的死亡概率曲线.pdf", width = 12, height = 6, family = "GB1")
ggplot(newdat, aes(heat_load, death_prob, colour = Species)) +
  geom_line(size = 1) +
  labs(x = "热负荷", y = "死亡概率") + 
  theme(
    axis.title.x = element_text(size = 26, color = "black"),  # x轴标题字体大小
    axis.title.y = element_text(size = 26, color = "black"),  # y轴标题字体大小
    axis.text.x = element_text(size = 26, color = "black"),   # x轴文本字体大小
    axis.text.y = element_text(size = 26, color = "black"),   # y轴文本字体大小
    legend.text = element_text(size = 26),
    legend.title = element_text(size = 26)
  )
dev.off()








