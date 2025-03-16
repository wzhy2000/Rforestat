# 加载数据
library(brms)
library(Rcpp)
library(forestat)

data("birch")
bhdata_cross <- birch[, c("CW", "D", "SD", "CLR", "plot")]


# 定义交叉验证函数
CV <- function(n,Z,seed=888){
     z <- rep(1:Z, ceiling(n/Z))[1:n]
     set.seed(seed)
     z <- sample(z,n)
     mm <- list()
     for (i in 1:Z) 
         mm[[i]] <- (1:n)[z==i];
         return(mm)
}

# 处理数据
bhdata_cross$plot <- as.factor(bhdata_cross$plot)
w <- bhdata_cross
n <- nrow(w)
Z <- 3
mm <- CV(n,Z)
result <- 1

# 初始化存储变量
brms_RMSE_train <- rep(0,Z)
brms_RMSE_test <- rep(0,Z)
brms_r2_train <- rep(0,Z)
brms_r2_test <- rep(0,Z)
brms_MAPE_train <- rep(0,Z)
brms_MAPE_test <- rep(0,Z)
brms_MAE_train <- rep(0,Z)
brms_MAE_test <- rep(0,Z)
brms_cross_model_bh <- list()

# 定义贝叶斯模型的先验分布
prior2_cross <- prior(student_t(4, 0, 2.5), nlpar = "p") +
prior(normal(4, 2), nlpar = "a1") +
prior(normal(2, 2), nlpar = "a2") +
prior(normal(2, 2), nlpar = "b1") +
prior(normal(1, 2), nlpar = "c1") +
prior(normal(-0.0000002, 0.01), nlpar = "c2")

# 交叉验证及模型训练
for(i in 1:Z){
 	m <- mm[[i]];
 	a <- brm(bf(CW ~p*(a1+a2*CLR)/(1+b1*exp(-(c1+c2*SD)*D)), p ~ 1 + (1|plot),a1~1, a2~1,b1~1,c1~1,c2~1,nl = TRUE),data = w[-m,], prior = prior2_cross)
   train_pre <- predict(a,newdata =  w[-m,])[,1]
   test_pre <- predict(a,newdata = w[m,],allow_new_levels=T)[,1]
   brms_cross_model_bh[[i]] <- a
   brms_RMSE_train[i] <- rmse(w[-m,result],train_pre,nrow(w[-m,]))
   brms_RMSE_test[i] <- rmse(w[m,result],test_pre,nrow(w[m,]))
   brms_r2_train[i] <- r2(w[-m,result],train_pre)
   brms_r2_test[i] <- r2(w[m,result],test_pre)
   brms_MAPE_train[i] <- MAPE(w[-m,result],train_pre)
   brms_MAPE_test[i] <- MAPE(w[m,result],test_pre)
   brms_MAE_train[i] <- MAE(w[-m,result],train_pre)
   brms_MAE_test[i] <- MAE(w[m,result],test_pre)
}

# 计算并总结交叉验证结果
brms_RMSE_train_nor <- mean(brms_RMSE_train)
brms_RMSE_test_nor <- mean(brms_RMSE_test)
brms_r2_train_nor <- mean(brms_r2_train)
brms_r2_test_nor <-  mean(brms_r2_test)
brms_MAPE_train_nor <- mean(brms_MAPE_train)
brms_MAPE_test_nor <- mean(brms_MAPE_test)
brms_MAE_train_nor <- mean(brms_MAE_train)
brms_MAE_test_nor <- mean(brms_MAE_test)

brms_train_indicator <- rbind(brms_RMSE_train_nor,brms_r2_train_nor,brms_MAPE_train_nor,brms_MAE_train_nor)
brms_test_indicator <- rbind(brms_RMSE_test_nor,brms_r2_test_nor,brms_MAPE_test_nor,brms_MAE_test_nor)

brms_train_indicator
brms_test_indicator