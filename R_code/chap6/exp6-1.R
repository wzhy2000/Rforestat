data <- read.csv("D1.冬奥核心区-白桦data_all.csv", header = TRUE,  fileEncoding = "GBK",sep = ',')
dim(data)
names(data)
data$H <- round(data$H * 100) 
set.seed(123)
data <- data[sample(nrow(data), 500), ]

attach(data)

null_model <- glm(H~1, family = poisson())
model <- glm(H~D+CBH+CW, family = poisson())
summary(model)
p_value <- pchisq(model$null.deviance - model$deviance, model$df.null - model$df.residual, lower.tail = FALSE)
p_value 


model_1a <- glm(H~D+CBH+CW, family = poisson(link = "identity"))
summary(model_1a)
p_value_1a <- pchisq(model_1a$null.deviance - model_1a$deviance, model_1a$df.null - model_1a$df.residual, lower.tail = FALSE)
p_value_1a


model_1b <- glm(H~D+CBH+CW, family = poisson(link = "sqrt"))
summary(model_1b)
p_value_1b <- pchisq(model_1b$null.deviance - model_1b$deviance, model_1b$df.null - model_1b$df.residual, lower.tail = FALSE)
p_value_1b 

anova(model, model_1a, model_1b, test = "Chisq")

predict_height <- predict(model_1a, type = "response")

png("D_H_fitting_plot.png", width = 800, height = 800)
par(mgp = c(2.8,1,0))
plot(D, H, xlab = "胸径/cm", ylab = "树高/cm", las = 1, 
     pch = 16, col = "green", cex = 1, main = "胸径-树高拟合图", 
     cex.main = 1.5, cex.lab = 1.4, cex.axis = 1.2)
points(D, predict_height, pch = 16, col = "red", cex = 1)
dev.off()

residuals <- residuals(model_1a, type = "response")
png("Residuals.png", width = 800, height = 800)
par(mgp = c(2.8,1,0))
plot(predict_height, residuals, main = "残差图",xlab = "拟合值", 
     ylab = "残差", pch = 16, col = "green", cex = 1, cex.main = 1.5, 
     cex.lab = 1.4, cex.axis = 1.2)
abline(h = 0, col = "red")
dev.off()