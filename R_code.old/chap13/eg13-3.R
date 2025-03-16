library(neuralnet)
model <- neuralnet(Species ~ . ,data = iris,hidden = c(5, 3),linear.output = FALSE)

pdf("neuralnet.pdf",width = 10,height = 6)
plot(model, rep = "best", 
     col.entry = "blue",       # 输入节点颜色
     col.hidden = "green",     # 隐藏层节点颜色
     col.out = "red",          # 输出节点颜色
     arrow.length = 0.2,       # 调整箭头长度
     cex = 0.8)                # 调整节点文本大小
dev.off()