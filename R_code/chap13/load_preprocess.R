# load_preprocess.R

library(openxlsx)
data_m <- read.xlsx("30m.xlsx", sheet = 1, colNames = TRUE)
data_m <- data_m[, -c(2, 3)]  # 删除不需要的列
set.seed(63) # 设定随机种子
datapartde <- sample(nrow(data_m), 0.7 * nrow(data_m))
trainde_m <- data_m[datapartde, ]
testde_m <- data_m[-datapartde, ]
trainde_m1 <- trainde_m[, -c(29)]
testde_m1 <- testde_m[, -c(29)]

FittingEvaluationIndex <- function(EstiH, ObsH) {
  Index <- array(dim = 6)
  e <- ObsH - EstiH
  e1 <- ObsH - mean(ObsH)
  pe <- mean(e)
  var2 <- var(e)
  var <- sqrt(var(e))
  RMSE <- sqrt(pe^2 + var2)
  R2 <- 1 - sum(e^2) / sum((e1)^2)
  TRE <- 100 * sum(e^2) / sum((EstiH)^2)
  Index[1] <- pe
  Index[2] <- RMSE
  Index[3] <- R2
  Index[4] <- var2
  Index[5] <- TRE
  Index[6] <- var
  dimnames(Index) <- list(c("pe", "RMSE", "R2", "Var", "TRE", "sd"))
  return(Index)
}
