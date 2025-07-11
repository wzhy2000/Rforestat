library(vegan)
data(mite)
data(mite.env)

mites_count <- mite[, "Brachy"]
other_count <- rowSums(mite[, -which(colnames(mite) == "Brachy")])
Topo <- mite.env$Topo

glm1 <- glm(mites_count ~ Topo, family = poisson(link = "log"))
summary(glm1)

total_mites <- rowSums(mite)
rel_abund <- mites_count / total_mites

model.prop <- glm(rel_abund ~ Topo, family = binomial, weights = total_mites)
boxplot(rel_abund ~ Topo, data = mite.env,
        col = "lightblue",
        main = "Brachy相对丰度 vs 地形",
        ylab = "相对丰度", xlab = "地形类型")