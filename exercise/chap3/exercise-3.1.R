# 习题 3.1：CO2 数据重编码、派生变量、排序与导出

library(dplyr)

# （1）核对变量类型和缺失值；保留 Treatment 原列并新增中文列。
d <- datasets::CO2 |>
  mutate(
    Treatment_cn = recode(
      as.character(Treatment),
      chilled = "冷处理",
      nonchilled = "常温处理"
    ),
    # （2）该比值仅用于练习变量构造，不解释为生理效率。
    uptake_per_conc = uptake / conc
  )

print(vapply(d, function(x) paste(class(x), collapse = "/"), character(1)))
print(colSums(is.na(d)))
stopifnot(nrow(d) == 84L, !anyNA(d$Treatment_cn))

# （3）按 uptake 降序输出前 10 行。
top10 <- d |> arrange(desc(uptake)) |> head(10)
print(top10)
stopifnot(top10$uptake[1] == max(d$uptake))

# （4）导出并重新读入，核对记录数和新增变量。
output_file <- "../../data/exercise-3.1.csv"
write.csv(d, output_file, row.names = FALSE, fileEncoding = "UTF-8")
d_reloaded <- read.csv(output_file, check.names = FALSE, fileEncoding = "UTF-8")
print(c(nrow = nrow(d_reloaded), ncol = ncol(d_reloaded)))
print(vapply(d_reloaded, function(x) paste(class(x), collapse = "/"), character(1)))
stopifnot(
  nrow(d_reloaded) == nrow(d),
  all(c("Treatment_cn", "uptake_per_conc") %in% names(d_reloaded))
)
cat("CSV 不保存因子属性；重新分析时应依据数据字典恢复分类变量。\n")
