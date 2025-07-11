# 载入内置数据集
library(dplyr)
data(CO2)
# 将 Treatment 列重编码为“冷处理”和“常温处理”；
CO2 <- CO2 %>%
  mutate(Treatment = recode(Treatment,
                            chilled = "冷处理",
                            nonchilled = "常温处理"))
# 2. 创建单位浓度吸收率变量
CO2 <- CO2 %>%
  mutate(uptake_per_conc = uptake / conc)
# 3. 按照 uptake 降序排列前 10 行
CO2_top <- CO2 %>% arrange(desc(uptake)) %>% head(10)
#4. 保存处理后的数据
write.csv(CO2, file = "CO2_processed.csv", row.names = FALSE)

