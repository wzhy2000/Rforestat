NH <- H~(c0 / (1 + c1 * exp(-c2 * D))) /
  (1 - 1 / (1 + exp(b0 + b1 * D + b2 * cw)))
NHCB <- HCB~H / (1 + exp(b0 + b1 * D + b2 * cw))
NCL <- CL~c0 / (1 + c1 * exp(-c2 * D))

models <- list(NH, NHCB, NCL)
startvalues <- c(b0 = 2, b1 = 0.3, b2 = 0.5, c0 = 6, c1 = 3, c2 = 0.08)

m1.SUR <- nlsystemfit(
  "SUR", models, startvalues,
  data = train.up5.HCB, eqnlabels = list("H", "HCB", "CL")
)

print(m0.SUR$b)
print(m1.SUR$b)
print(m0.SUR$covb)
print(m1.SUR$covb)

library(ggplot2) # 调用ggplot2包

data <- data.frame(
  Method = c(
    rep("Simultaneous", length(m0.SUR$resids)),
    rep("Additive", length(m1.SUR$resids))
  ),
  X = c(
    rep("H", length(m0.SUR$resids[, 1])),
    rep("HCB", length(m0.SUR$resids[, 2])),
    rep("CL", length(m0.SUR$resids[, 3])),
    rep("H", length(m1.SUR$resids[, 1])),
    rep("HCB", length(m1.SUR$resids[, 2])),
    rep("CL", length(m1.SUR$resids[, 3]))
  ),
  Residuals = c(
    m0.SUR$resids[, 1],
    m0.SUR$resids[, 2],
    m0.SUR$resids[, 3],
    m1.SUR$resids[, 1],
    m1.SUR$resids[, 2],
    m1.SUR$resids[, 3]
  )
) # 将残差、对应的模型、方程整理到一个数据中

p <- ggplot(data, aes(X, Residuals, fill = Method)) +
  geom_boxplot() # 绘制箱线图

ggsave("plot.png", plot = p, height = 5, units = "in") # 保存图像



