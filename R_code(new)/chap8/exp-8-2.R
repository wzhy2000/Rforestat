data.train <- read.csv("la_m.csv")
data.test <- read.csv("la_v.csv")
data.total <- read.csv("la_tot.csv")

summary(data.train)
summary(data.test)
summary(data.total) 

library(mgcv)
library(forestat)
library(ggplot2)
library(pdp)
library(gridExtra)
model.gamh <- gam(log(h) ~ s(hdo, k = 30, bs = "cr") + s(d, k = 40, bs = "cr")   
                  + ti(hdo, d, k = 34, bs = "cr"), data = data.train)

summary(model.gamh)
y.pred <- exp(fitted(model.gamh))  
FittingEvaluationIndex(y.pred, data.train$h)

y.pred.log <- predict(model.gamh, newdata = data.test, type = "response")
FittingEvaluationIndex(exp(y.pred.log), data.test$h)

y.pred <- exp(predict(model.gamh, newdata = data.test))
df.pred <- data.frame(y.pred, y.pred - data.test$h)  

pdf("8.7a.pdf", width = 8, height = 6, family = "GB1")
ggplot(df.pred, aes(y.pred, y.pred - data.test$h)) +  
        geom_point() +
        scale_x_continuous(name = '树高预测值(m)', limits = c(0, 20)) +  
        geom_point(color = '#999999') +
        theme(panel.background = element_rect(fill = 'white'),  
              axis.line = element_line(colour = 'black'), 
              axis.ticks.length = unit(-10, "pt"), 
              axis.text.y = element_text(size = 24), 
              axis.text.x = element_text(size = 24),   
              text = element_text(size = 24)) +
        scale_y_continuous(name = '残差(m)', limits = c(-10, 10)) +     
        geom_hline(aes(yintercept = 0), size = 1.3)
dev.off()


y.part <- partial(model.gamh, pred.var = c("d", "hdo"))
colspace <- colorRampPalette(c("white", "gray50", "black"))
plot.part <- plotPartial(y.part, contour = TRUE, col.regions = colspace, 
                         xlab = list("胸径(cm)", cex = 2), 
                         ylab = list("优势木平均高(m)", cex = 2), 
                         scales = list(cex = 2))

pdf("8.7b.pdf", width = 8, height = 6, family = "GB1")
grid.arrange(plot.part, ncol = 1)
dev.off()
