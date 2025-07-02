library(MASS)
library(caret)
library(ggplot2)
library(klaR)
data <- read.csv("ASTER.csv", header = T)
str(data)
table(data$class)
set.seed(123)
idx <- createDataPartition(data$class, p = 0.7, list = FALSE)
train <- data[idx, ]
test <- data[-idx, ]

model.lda <- lda(class ~ ., data = train)
model.qda <- qda(class ~ ., data = train)

print(model.lda)
print(model.qda)

model.ldacv <- lda(class ~ ., data = train, CV = TRUE)
table(CV = model.ldacv$class, True = train$class)

model.qdacv <- qda(class ~ ., data = train, CV = TRUE)
table(CV = model.qdacv$class, True = train$class)


scores.train <- predict(model.lda, newdata = train)
head(round(scores.train$posterior, 3))

plot.train <- data.frame( LD1 = scores.train$x[, 1],
                          LD2 = scores.train$x[, 2], class = scores.train[["class"]] )
ggplot(plot.train, aes(x = LD1, y = LD2, color = class)) +
  geom_point(size = 3) +
  labs(x = "LD1", y = "LD2", color = "Class") +
  theme_classic()
scores.test <- predict(model.lda, newdata = test)
plot.test <- data.frame(
  LD1 = scores.test$x[, 1],
  LD2 = scores.test$x[, 2],
  class = scores.test[["class"]]
)
ggplot(plot.test, aes(x = LD1, y = LD2, color = class)) +
  geom_point(size = 3) +
  labs(x = "LD1", y = "LD2", color = "Class") +
  theme_classic()

pm <- stepclass(class ~ ., data = train, method = "lda", criterion = "AS")
print(pm)
