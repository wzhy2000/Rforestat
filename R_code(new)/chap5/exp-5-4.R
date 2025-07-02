library(rFIA)
library(psych)
library(tidyverse)


me_data <- readFIA("./ME_CSV")
df <- me_data$TREE %>%
  select(PLT_CN, DIA, HT, CARBON_AG, TPA_UNADJ, VOLBFGRS) %>%  
  group_by(PLT_CN) %>%  
  summarise(
    DIAmean = mean(DIA, na.rm = TRUE),          
    HTmean = mean(HT, na.rm = TRUE),            
    CARBONsum = sum(CARBON_AG, na.rm = TRUE),   
    TPAmean = mean(TPA_UNADJ, na.rm = TRUE),    
    BAsum = sum(pi*(DIA/2)^2 * TPA_UNADJ, na.rm = TRUE),
    VOLsum = sum(VOLBFGRS, na.rm = TRUE)        
  ) %>%
  na.omit() %>%  
  column_to_rownames("PLT_CN") %>%
  scale()  
head(df, n = 5)

KMO(df)

fa.parallel(df, fa = "pc", n.iter = 100, show.legend = FALSE) 

pca_result <- principal(df, nfactors = 2, rotate = "none")
print(pca_result$loadings)

pca_rotated <- principal(df, nfactors = 2, rotate = "varimax")
print(pca_rotated$loadings)


mle_result <- factanal(df, factors = 2, rotation = "none", scores = "regression")
print(mle_result$loadings)



mle_varimax <- factanal(df, factors = 2, rotation = "varimax", scores 
                        = "regression")
print(mle_varimax$loadings)