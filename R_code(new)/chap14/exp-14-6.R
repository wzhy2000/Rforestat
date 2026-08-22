##### 该部分内容比较耗时，可以选择加载 ../../data/case-14.6.rdata 文件 #############
# load("../../data/case-14.6.rdata")

library(foreach)
library(doParallel)
library(neuralnet)
library(forestat)
library(stringr)
library(caret)
library(ggplot2)

data("picea")
picea$AGB <- picea$STEM + picea$BRANCH + picea$FOLIAGE + picea$FRUIT

set.seed(123)
idx.train <- sample(nrow(picea), floor(0.7 * nrow(picea)))
model.vars <- c("LH", "LHCB", "CPA", "D0", "H0", "HCB0", "CW", "AGB")
picea.train.raw <- picea[idx.train, model.vars]
picea.test.raw <- picea[-idx.train, model.vars]
y.test.original <- picea.test.raw$AGB

# 只用训练数据估计归一化参数，再将同一参数应用于新数据。
fit_minmax <- function(train, newdata) {
  min.train <- vapply(train, min, numeric(1), na.rm = TRUE)
  max.train <- vapply(train, max, numeric(1), na.rm = TRUE)
  if (any(max.train <= min.train)) {
    stop("归一化变量在训练数据中没有正的变化范围")
  }

  transform_data <- function(x) {
    out <- x
    for (col in names(out)) {
      out[[col]] <- (out[[col]] - min.train[col]) /
        (max.train[col] - min.train[col])
    }
    out
  }

  list(
    train = transform_data(train),
    newdata = transform_data(newdata),
    min = min.train,
    max = max.train
  )
}


########################################## 如果加载了.RData文件，后续可不运行 #########################################
# 每个验证折只能使用对应训练折的归一化参数。
cross_validate_neuralnet <- function(data, folds, hidden,
                                     learningrate, threshold, stepmax) {
  fold.result <- lapply(seq_along(folds), function(i) {
    valid.id <- folds[[i]]
    scaled <- fit_minmax(data[-valid.id, ], data[valid.id, ])
    train.fold <- scaled$train
    valid.fold <- scaled$newdata

    tryCatch({
      set.seed(123 + i)
      fit <- neuralnet(
        AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW,
        data = train.fold,
        hidden = hidden,
        linear.output = TRUE,
        err.fct = "sse",
        act.fct = "logistic",
        algorithm = "backprop",
        learningrate = learningrate,
        threshold = threshold,
        stepmax = stepmax
      )

      pred <- as.numeric(predict(fit, valid.fold))
      obs <- valid.fold$AGB
      c(
        avg_rmse = sqrt(mean((obs - pred)^2)),
        avg_r_squared = 1 - sum((obs - pred)^2) /
          sum((obs - mean(obs))^2)
      )
    }, error = function(e) {
      c(avg_rmse = NA_real_, avg_r_squared = NA_real_)
    })
  })

  fold.result <- do.call(rbind, fold.result)
  if (!all(complete.cases(fold.result))) {
    return(c(avg_rmse = NA_real_, avg_r_squared = NA_real_))
  }
  colMeans(fold.result)
}

set.seed(123)
folds <- createFolds(picea.train.raw$AGB, k = 5, returnTrain = FALSE)
param.grid <- expand.grid(
  hidden = "10,10",
  learningrate = seq(1e-4, 1e-3, by = 1e-4),
  threshold = c(1e-1, 1e-2, 1e-3, 1e-4),
  stepmax = c(500000, 1200000),
  stringsAsFactors = FALSE
)

# 参数组合之间相互独立，外层并行不改变每折内设置的随机种子。
num.cores <- min(12L, parallel::detectCores())
cl <- makeCluster(num.cores)
registerDoParallel(cl)
grid_search_results <- tryCatch(
  foreach(
    i = seq_len(nrow(param.grid)),
    .combine = rbind,
    .packages = "neuralnet"
  ) %dopar% {
    par <- param.grid[i, ]
    score <- cross_validate_neuralnet(
      data = picea.train.raw,
      folds = folds,
      hidden = as.numeric(strsplit(par$hidden, ",")[[1]]),
      learningrate = par$learningrate,
      threshold = par$threshold,
      stepmax = par$stepmax
    )
    cbind(par, as.data.frame(as.list(score)))
  },
  finally = stopCluster(cl)
)

#########################################################################################

successful.results <- grid_search_results[
  complete.cases(grid_search_results[c("avg_rmse", "avg_r_squared")]),
]
if (nrow(successful.results) == 0L) {
  stop("所有参数组合均未完成5折拟合")
}
print(successful.results)

modela.nn.best <- successful.results[
  which.min(successful.results$avg_rmse),
]
print(modela.nn.best)

# 最终模型只使用完整训练集的参数处理测试集。
final.scaled <- fit_minmax(picea.train.raw, picea.test.raw)
picea.train <- final.scaled$train
picea.test <- final.scaled$newdata
agb.min <- final.scaled$min["AGB"]
agb.max <- final.scaled$max["AGB"]

set.seed(123)
training_log <- capture.output({
  modela.nn <- neuralnet(
    AGB ~ LH + LHCB + CPA + D0 + H0 + HCB0 + CW,
    data = picea.train,
    hidden = as.numeric(strsplit(modela.nn.best$hidden, ",")[[1]]),
    linear.output = TRUE,
    err.fct = "sse",
    act.fct = "logistic",
    algorithm = "backprop",
    threshold = as.numeric(modela.nn.best$threshold),
    learningrate = as.numeric(modela.nn.best$learningrate),
    stepmax = as.numeric(modela.nn.best$stepmax),
    lifesign = "full",
    lifesign.step = 1000
  )
}, type = "message")
writeLines(training_log, "test.log")

y.pred <- as.numeric(predict(modela.nn, picea.test))
y.pred.original <- y.pred * (agb.max - agb.min) + agb.min
evaluation.metrics <- FittingEvaluationIndex(y.pred.original, y.test.original)
print(evaluation.metrics)

data.nn <- data.frame(
  x = y.pred.original,
  y = y.test.original - y.pred.original
)
p.nn <- ggplot(data.nn, aes(x = x, y = y)) +
  theme_light() +
  geom_point(color = "steelblue", size = 3, show.legend = FALSE) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x = "地上生物量（kg/株）", y = "预测误差（kg/株）") +
  theme(
    axis.title.x = element_text(size = 26, color = "black"),  # x轴标题字体大???
    axis.title.y = element_text(size = 26, color = "black"),  # y轴标题字体大???
    axis.text.x = element_text(size = 26, color = "black"),   # x轴文本字体大???
    axis.text.y = element_text(size = 26, color = "black"),   # y轴文本字体大???
    plot.title = element_text(size = 26, color = "black"),     # 图表标题字体大小
    legend.title = element_text(size = 26, color = "black"),   # 图例标题字体大小
    legend.text = element_text(size = 26, color = "black"),      # 图例文本字体大小
    panel.grid.major = element_blank(),                         # 去掉主网格线
    panel.grid.minor = element_blank()                          # 去掉次网格线
  )
pdf("图14.15a.pdf", width = 8, height = 6)
print(p.nn)
dev.off()

thresh <- stringr::str_extract(training_log, "min thresh: [0-9.]+")
thresh <- as.numeric(stringr::str_replace(thresh, "min thresh: ", ""))
thresh <- thresh[is.finite(thresh)]
iteration <- seq_along(thresh) * 1000
convergence.data <- data.frame(Iteration = iteration, MinThresh = thresh)
p.convergence <- ggplot(convergence.data, aes(x = Iteration, y = MinThresh)) +
  theme_light() +
  geom_line(color = "black") +
  labs(
    title = "Training min thresh Curve",
    x = "Iteration",
    y = "min thresh"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 26, color = "black"),  # x轴标题字体大小
    axis.title.y = element_text(size = 26, color = "black"),  # y轴标题字体大小
    axis.text.x = element_text(size = 26, color = "black"),   # x轴文本字体大小
    axis.text.y = element_text(size = 26, color = "black"),   # y轴文本字体大小
    plot.title = element_text(size = 26, color = "black"),     # 图表标题字体大小
    legend.title = element_text(size = 26, color = "black"),   # 图例标题字体大小
    legend.text = element_text(size = 26, color = "black"),      # 图例文本字体大小
  )
pdf("图14.15b.pdf", width = 8, height = 6)
print(p.convergence)
dev.off()

# save(
#   grid_search_results,
#   successful.results,
#   modela.nn.best,
#   modela.nn,
#   final.scaled,
#   y.pred.original,
#   y.test.original,
#   evaluation.metrics,
#   file = "../../data/case-14.6.rdata"
# )
