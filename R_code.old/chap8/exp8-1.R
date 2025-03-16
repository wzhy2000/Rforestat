library(forestat)
library(dplyr)
library(MASS)
# 处理数据
data("birch")
pt <- unique(birch$plot)

hdo_list <- lapply(pt, function(p) {
  max(birch[birch$plot == p, "H"], na.rm = TRUE)
})

# plot_counts <- table(birch$plot)
# hdo_repeated <- rep(unlist(hdo_list), times = plot_counts)
hdo_df <- data.frame(plot = pt, hdo = unlist(hdo_list))
data <- birch %>% left_join(hdo_df, by = "plot")
data <- data[!is.na(data$D) & !is.na(data$H), ]  # 移除 D 和 H 中的 NA
data <- data[order(data$D), ]  # 按照 D 升序排列数据


myInitial <- function(mCall, LHS, data, ...) {
  D <- data[["D"]]
  y <- data[["H"]]
  hdo <- data[["hdo"]]

  b1 <- max(y, na.rm = TRUE) / max(hdo, na.rm = TRUE) 
  b2 <- coef(lm(log(y) ~ log(hdo)))[2]  # 初始 b2，假设中性值
  b3 <- 1 / mean(D, na.rm = TRUE)       # 初始 b3，控制增长速率
  b4 <- sd(y, na.rm = TRUE) / mean(y, na.rm = TRUE)    # 初始 b4，假设线性效应
  
  value <- c(b1 = b1, b2 = b2, b3 = b3, b4 = b4)
  names(value) <- mCall[c("b1", "b2", "b3", "b4")]
  return(value)
}


# 定义 selfStart 模型
mySelfStart <- selfStart(
  ~ 1.3 + b1 * hdo^b2 * (1 - exp(-b3 * D))^b4,
  initial = myInitial,
  parameters = c("b1", "b2", "b3", "b4")
)

start <- getInitial(H ~ mySelfStart(hdo = hdo, D = D, b1, b2, b3, b4), data = data)
start
# 非线性回归模型拟合
# 定义支持 deriv3 的非线性模型
my_model <- deriv3(~ 1.3 + b1 * hdo^b2 * (1 - exp(-b3 * D))^b4, 
                   namevec = c("b1", "b2", "b3", "b4"),  # 参数名称
                   function(b1, b2, b3, b4, hdo, D) NULL  # 创建函数模板
                   )
model <- nls(H ~ my_model(b1, b2, b3, b4, hdo, D),data = data,start = start)
# model <- nls(H ~ 1.3 + b1 * hdo^b2 * (1 - exp(-b3 * D))^b4, data = data, start = start)

# 结果分析
summary(model)
rms.curv(model)
# 伪R^2
RSS <- sum(residuals(model)^2)
TSS <- sum((data$H - mean(data$H))^2)
pseudo_R2 <- 1 - (RSS / TSS)
pseudo_R2

data$fitted_value <- predict(model)
library(ggplot2)
pdf("图8.4.pdf", width = 8, height = 6, family = "GB1")
ggplot(data, aes(x = D, y = H)) + 
  geom_point(color = "blue", size = 2, alpha = 0.7) +  
  geom_line(aes(x = D, y = fitted_value), color = "red", size = 1) +  
  labs(
    title = "模型拟合结果",
    x = "树高（D）",
    y = "胸径（H）"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 24, face = "bold"),  # 标题字体大小
    axis.title.x = element_text(size = 24),               # x 轴标签字体大小
    axis.title.y = element_text(size = 24),               # y 轴标签字体大小
    axis.text.x = element_text(size = 24),                # x 轴刻度字体大小
    axis.text.y = element_text(size = 24),                # y 轴刻度字体大小
    legend.title = element_text(size = 24),               # 图例标题字体大小
    legend.text = element_text(size = 24)                 # 图例内容字体大小
  )
dev.off()

data$residuals <- residuals(model)
pdf("图8.5.pdf", width = 8, height = 6, family = "GB1")
ggplot(data, aes(x = D, y = residuals)) +
  geom_point(color = "blue", size = 2, alpha = 0.7) +  # 残差点
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +  # 零线
  labs(
    title = "残差图",
    x = "拟合值",
    y = "残差"
  ) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 24, face = "bold"),  # 标题字体大小
    axis.title.x = element_text(size = 24),               # x 轴标签字体大小
    axis.title.y = element_text(size = 24),               # y 轴标签字体大小
    axis.text.x = element_text(size = 24),                # x 轴刻度字体大小
    axis.text.y = element_text(size = 24),                # y 轴刻度字体大小
    legend.title = element_text(size = 24),               # 图例标题字体大小
    legend.text = element_text(size = 24)                 # 图例内容字体大小
  )
dev.off()