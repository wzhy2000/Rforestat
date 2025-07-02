library(agridat)
library(lme4)
library(ggplot2)

# 数据加载与预处理
data(hanover.whitepine)
str(hanover.whitepine)

# 模型构建
model <- lmer(length ~ (1 | rep) + (1 | male) + (1 | female) + (1 | male:female), 
              data = hanover.whitepine)
summary(model)

# 参数估计
fixef(model)
# ranef(model)$female
# coef(model)$female

# 提取模型中 female（母本）随机效应的 BLUP 值
blup <- ranef(model)$female
blup
# 将 BLUP 转换为数据框格式，并添加家系编号列
blup.df <- data.frame(
  female = rownames(blup),  # 母本家系编号
  blup = blup[, 1]            # 对应的 BLUP 值
)

# pdf("母本家系对白松主干长度的BLUP估计.pdf", width = 12 ,height = 6, family = "GB1")
ggplot(blup.df, aes(x = reorder(female, blup), y = blup)) +
  geom_col(fill = "forestgreen") +       # 绿色条形图
  coord_flip() +                         # 翻转坐标轴，横向展示家系编号
  labs(
    x = "母本编号（female）",                # 横轴标签
    y = "BLUP值") +                        # 纵轴标签
  theme_minimal() +                          # 使用简洁主题
  theme(
    axis.title.x = element_text(size = 26, color = "black"),  # x轴标题字体大小
    axis.title.y = element_text(size = 26, color = "black"),  # y轴标题字体大小
    axis.text.x = element_text(size = 26, color = "black"),   # x轴文本字体大小
    axis.text.y = element_text(size = 26, color = "black"),   # y轴文本字体大小
  )
# dev.off()

# 区间估计
confint(model, method = "profile")

# 遗传力计算
# 提取模型的方差分量（随机效应和残差方差），并转换为数据框格式
varcomp <- as.data.frame(VarCorr(model))
# 提取母本家系（female）的方差估计值（在数据框中的第1行）
sigma2.female <- varcomp$vcov[1]
sigma2.total <- varcomp$vcov[2] + varcomp$vcov[3] + varcomp$vcov[4] + varcomp$vcov[5]
# 计算狭义遗传力 h²（家系方差 / 总方差）
h2 <- sigma2.female / (sigma2.female + sigma2.total)
round(h2, 4)





