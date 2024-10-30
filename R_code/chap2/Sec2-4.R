# 2.4 绘图基础

# 2.4.1 基础绘图
# 绘图边距设置
par(oma=c(3,3,3,3))
par(mar=c(5,4,4,2))
plot(0:10, 0:10, type="n", xlab="X", ylab="Y")
text(5, 5, "Plot", col="red", cex=2)
box(col="red")
mtext("Margins", side=3, line=2, cex=2, col="forestgreen")  
box("figure", col="forestgreen")    
mtext("Outer Margin Area", side=1, line=1, cex=2, col="blue", outer=TRUE)  
box("outer", col="blue")

# 常见的绘图函数
# eg 2.1 基础绘图示例
# 散点图
x <- 1:10
y <- x + rnorm(10)
plot(x, y, main = "基础散点图", xlab = "X轴", ylab = "Y轴", pch = 19, col = "blue")
points(x, y, pch = 17, col = "red")
lines(x, y, col = "green")
text(5, 8, "示例文本")
legend("topright", legend = c("点", "线"), col = c("red", "green"), pch = c(17, NA), lty = 1)

# 柱状图
categories <- c("A", "B", "C", "D", "E", "F")
values <- c(1, 2, 3, 4, 5, 6)
bp <- barplot(values, names.arg = categories, main = "基础柱状图", xlab = "分类", 
              ylab = "值", col = "lightblue", ylim = c(0, max(values) + 2))
text(bp, values + 0.3, labels = values, col = "black")

# 直方图
data <- rnorm(100)
hist(data, main = "基础直方图", xlab = "值", ylab = "频率", 
     col = "orange", xlim = c(-3, 3), ylim = c(0, 30), breaks = 10)
abline(v = mean(data), col = "red", lwd = 2)

# 2.4.2 进阶绘图
# eg2.2 进阶绘图示例
# 使用qplot创建箱线图
library(ggplot2)
data(iris)
qplot(data = iris, x = Species, y = Sepal.Length, geom = "boxplot",
      main = "不同种类鸢尾花的萼片长度分布",
      xlab = "种类",
      ylab = "萼片长度 (cm)")

# 使用ggplot()函数创建散点图
p <- ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = Species)) +
  labs(title = "鸢尾花萼片长度与宽度的关系", 
       x = "萼片长度 (cm)", 
       y = "萼片宽度 (cm)", 
       color = "Species")
print(p)

# 综合使用散点图、平滑曲线和小提琴图
data(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
ggplot(mtcars, aes(x = cyl, y = mpg)) + 
  geom_violin(aes(fill = cyl), trim = FALSE, alpha = 0.3) + 
  geom_jitter(aes(color = cyl), width = 0.2, size = 3) + 
  stat_smooth(aes(group = 1), method = "lm", se = FALSE, 
              formula = y ~ x, color = "red", linetype = "dashed") + 
  labs(title = "汽车缸数与每加仑英里数的关系", 
       x = "缸数", 
       y = "每加仑英里数 (mpg)", 
       color = "缸数", 
       fill = "缸数") + 
  theme_minimal()

# 2.4.3 绘图导出
# 普通导出
x <- 1:10
y <- x * 2 + rnorm(10)
png(file = "plot.png")
plot(x, y, main = "Example Plot")
dev.off()

jpeg(file = "plot.jpeg", width = 500, height = 500, units = "px", pointsize = 12)
plot(x, y, main = "Example Plot")
dev.off()

pdf(file = "plot.pdf")
plot(x, y, main = "Example Plot")
dev.off()

# 利用export包导出
install.packages("export")
library(export)
help(package="export")
x <- 1:10
y <- x * 2 + rnorm(10)
plot(x, y, main = "Example Plot")       
graph2ppt(file="Plot.pptx")
graph2doc()
graph2svg()
graph2pdf(file="Plot.pdf", font="GB1")
graph2eps()
graph2png()
graph2tif()
graph2jpg()

# 导出表格
fit=aov(yield ~ block + N * P + K, npk)
x=summary(fit)
table2ppt(x=x,file="table.pptx")
table2doc(x=x)
table2excel(x=x, file = "table.xlsx",digits=4,digitspvals=1, sheetName = "Anova_table", add.rownames = TRUE)
table2tex(x=x)
table2html(x=x)
