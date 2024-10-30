# 加载必要的库
library(mgcv)
library(ggplot2)
library(Metrics)
library(car)
library(broom)
library(cowplot)
library(pdp)
library(gridExtra)

# 定义模型评估函数
FittingEvaluationIndex <- function(EstiH, ObsH) {
  Index <- array(dim=5)
  e <- ObsH - EstiH
  e1 <- ObsH - mean(ObsH)
  pe <- mean(e)
  var2 <- var(e)
  RMSE <- sqrt(pe^2 + var2)
  R2 <- 1 - sum(e^2) / sum((e1)^2)
  TRE <- 100 * sum(e^2) / sum((EstiH)^2)
  Index[1] <- pe
  Index[2] <- RMSE
  Index[3] <- R2
  Index[4] <- var2
  Index[5] <- TRE
  dimnames(Index) <- list(c("pe", "RMSE", "R2", "Var", "TRE"))
  return(Index)
}

# 读取数据
sm21 <- read.csv('la_m.csv', header=TRUE)
sm21v <- read.csv('la_v.csv', header=TRUE)
sm22 <- read.csv('wh_m.csv', header=TRUE)
sm22v <- read.csv('wh_v.csv', header=TRUE)

# 构建第一个广义可加模型
mgcv1634 <- gam(log(h) ~ s(hdo, k=30, bs="cr") + s(d, k=40, bs="cr") + ti(hdo, d, k=34, bs="cr"), data=sm21)

# 计算训练数据的预测值和评估指标
p.mgcv1634 <- exp(fitted(mgcv1634))
FittingEvaluationIndex(p.mgcv1634, sm21$h)

# 计算新数据上的预测值和评估指标
p.newdatam1634 <- predict(mgcv1634, newdata=sm21v, type="response")
FittingEvaluationIndex(exp(p.newdatam1634), sm21v$h)

# 输出模型摘要信息
summary(mgcv1634)

# 绘制部分依赖图
pd1634 <- partial(mgcv1634, pred.var=c("d", "hdo"))
rwb <- colorRampPalette(c("red", "white", "blue"))
pdp21634 <- plotPartial(pd1634, contour=TRUE, col.regions=rwb, xlab="胸径 DBH/cm \n 落叶松广义可加模型 \n GAM-larch", ylab="优势高 \n hdo/m")

# 保存部分依赖图
png(filename='Rpl.png', width=2000, height=1000, res=300)
grid.arrange(pdp21634, ncol=1)
dev.off()

# 计算残差并绘制预测残差图
pr.mgcv1634 <- exp(predict(mgcv1634, newdata=sm21v))
rs.mgcv1634 <- pr.mgcv1634 - sm21v$h
tt.mgcv1634 <- data.frame(pr.mgcv1634, rs.mgcv1634)
rp.mgcv1634 <- ggplot(tt.mgcv1634, aes(pr.mgcv1634, rs.mgcv1634)) +
  geom_point() +
  scale_x_continuous(name='树高预测值 \n Predicted value of height/m', limits=c(0,20)) +
  geom_point(color='#999999') +
  theme(panel.background = element_rect(fill='white'), axis.line = element_line(colour='black'), 
        axis.ticks.length=unit(-10,"pt"), axis.text.y=element_text(size=18), 
        axis.text.x=element_text(size=18), text=element_text(size=18)) +
  scale_y_continuous(name='残差\n Residual error/m', limits=c(-10,10)) +
  geom_hline(aes(yintercept=0), size=1.3) +
  ggtitle("落叶松广义可加模型\n GAM-larch") +
  theme(plot.title=element_text(vjust=-10, size=18), plot.title=element_text(hjust=0.1, size=18))

# 保存预测残差图
png(filename='Rp.png', width=3700, height=1700, res=300)
grid.arrange(rp.mgcv1634, ncol=1)
dev.off()
