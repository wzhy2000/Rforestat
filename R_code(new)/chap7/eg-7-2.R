# 零膨胀泊松模型与 hurdle 泊松模型示例
# 数据：虚拟松树幼苗样方调查数据

# 1. 检查并加载软件包 -------------------------------------------------------
if (!requireNamespace("pscl", quietly = TRUE)) {
  stop("请先安装 pscl 软件包：install.packages(\"pscl\")")
}

library(pscl)

# 2. 生成可复现的虚拟数据 -------------------------------------------------
set.seed(123)
n <- 400

pineSeedling <- data.frame(
  openness = runif(n, min = 0.10, max = 0.90),
  distance = runif(n, min = 0, max = 500)
)

# 将距离转换为以100 m为单位，便于解释回归系数
pineSeedling$distance100 <- pineSeedling$distance / 100

# 计数过程：林冠开度越大，幼苗期望株数越多
lambda <- exp(0.40 + 1.40 * pineSeedling$openness)

# 结构零过程：距母树林越远，产生结构零的概率越高
p.zero <- plogis(-1.20 + 0.70 * pineSeedling$distance100)
structural.zero <- rbinom(n, size = 1, prob = p.zero)

# 结构零样方的幼苗数为0；其他样方服从泊松分布
pineSeedling$seedlings <- ifelse(
  structural.zero == 1,
  0,
  rpois(n, lambda = lambda)
)

# 仅保留分析中需要的观测变量
pineSeedling <- pineSeedling[
  c("seedlings", "openness", "distance", "distance100")
]

# 3. 描述数据中的零膨胀现象 ------------------------------------------------
cat("\n数据摘要：\n")
print(summary(pineSeedling))

observed.zero <- mean(pineSeedling$seedlings == 0)
poisson.zero <- exp(-mean(pineSeedling$seedlings))

cat(sprintf("\n样本零值比例：%.4f（%.2f%%）\n",
            observed.zero, 100 * observed.zero))
cat(sprintf("按普通泊松分布估计的零值概率：%.4f（%.2f%%）\n",
            poisson.zero, 100 * poisson.zero))

# 4. 拟合零膨胀泊松模型 ----------------------------------------------------
# 竖线左侧为泊松计数部分，右侧为结构零部分。
model.zif <- zeroinfl(
  seedlings ~ openness | distance100,
  dist = "poisson",
  data = pineSeedling
)

cat("\n================ 零膨胀泊松模型 ================\n")
print(summary(model.zif))

# 5. 拟合 hurdle 泊松模型 --------------------------------------------------
# 竖线左侧为零截断泊松计数部分，右侧为是否出现正计数的二项部分。
model.hurdle <- hurdle(
  seedlings ~ openness | distance100,
  dist = "poisson",
  zero.dist = "binomial",
  data = pineSeedling
)

cat("\n================ hurdle 泊松模型 ================\n")
print(summary(model.hurdle))

# 6. 将回归系数转换为便于解释的比值 --------------------------------------
zip.count.effect <- exp(0.1 * coef(model.zif, model = "count")["openness"])
zip.zero.effect <- exp(coef(model.zif, model = "zero")["distance100"])

hurdle.count.effect <- exp(
  0.1 * coef(model.hurdle, model = "count")["openness"]
)
hurdle.positive.effect <- exp(
  coef(model.hurdle, model = "zero")["distance100"]
)

cat("\n================ 系数解释 ================\n")
cat(sprintf(
  "ZIP：林冠开度每增加0.1，计数部分的幼苗期望株数乘以 %.3f。\n",
  zip.count.effect
))
cat(sprintf(
  "ZIP：距母树林每增加100 m，成为结构零的优势比乘以 %.3f。\n",
  zip.zero.effect
))
cat(sprintf(
  "hurdle：林冠开度每增加0.1，正计数部分的参数乘以 %.3f。\n",
  hurdle.count.effect
))
cat(sprintf(
  "hurdle：距母树林每增加100 m，出现幼苗的优势比乘以 %.3f。\n",
  hurdle.positive.effect
))

# 7. 模型预测 --------------------------------------------------------------
# response：综合两个过程后的总体期望株数
# count：计数部分的均值参数
# zero：ZIP中为结构零概率；hurdle中的含义由模型参数化决定
pre.response <- predict(model.zif, type = "response")
head(pre.response)
pre.prob <- predict(model.zif, type = "prob")
round(head(pre.prob, n = 3)[, 1:5], 4)
pre.count <- predict(model.zif, type = "count")
head(pre.count)
pre.zero <- predict(model.zif, type = "zero")
head(pre.zero)


# 8. 模型比较 --------------------------------------------------------------
cat("\n================ AIC比较 ================\n")
print(AIC(model.zif, model.hurdle))

cat("\n================ ZIP伪R方 ================\n")
print(pR2(model.zif))

cat("\n================ hurdle伪R方 ================\n")
print(pR2(model.hurdle))



# 9. 残差和协方差矩阵 ------------------------------------------------------
res.response <- residuals(model.zif, type = "response")
head(res.response)

res.pearson <- residuals(model.zif, type = "pearson")
head(res.pearson)

round(vcov(model.zif), 4)

