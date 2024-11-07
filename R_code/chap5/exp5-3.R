library(openxlsx)
library(dplyr)
library(ggcorrplot)
library(ggsci)

data1=read.csv("data-{30m}-5-3.csv",header =TRUE, sep = ",")
data1$gx11<-(data1$x11-min(data1$x11))/(max(data1$x11)-min(data1$x11))
data1$gx37<-(data1$x37-min(data1$x37))/(max(data1$x37)-min(data1$x37))
data1$gx46<-(data1$x46-min(data1$x46))/(max(data1$x46)-min(data1$x46))
data1$gx62<-(data1$x62-min(data1$x62))/(max(data1$x62)-min(data1$x62))
data1$gx75<-(data1$x75-min(data1$x75))/(max(data1$x75)-min(data1$x75))


lmde=lm(agb~., data = data1)
datacor=select(data1,agb,x22,x42,x38,x61,x41,x40,x24,x60,x39,x19,x84,x49,x18,x48,x4)

ggcorrplot(cor(datacor),hc.order=TRUE,type="lower",
           outline.color="white",ggtheme=ggplot2::theme_gray,
           colors=c("#6D9EC1","white","#E46726"),lab=T)

mydata=select(data1,agb,x4,x7,x19,x22,x11,x37,x46,x75,x62)
lmde=lm(agb~., data = mydata)
lm.step=step(lmde, direction = "backward") 

#逐步回归前
before=summary(lmde)  
round(before$coefficients,4)
before$r.squared
before$adj.r.squared
before$fstatistic

#逐步回归后
after=summary(lm.step)
round(after$coefficients,4)
after$r.squared
after$adj.r.squared
after$fstatistic

round(before$coefficients,4)
before$r.squared
before$adj.r.squared
before$fstatistic

round(after$coefficients,4)
after$r.squared
after$adj.r.squared
after$fstatistic

AIC(lmde)
BIC(lmde)
AIC(lm.step)
BIC(lm.step)


