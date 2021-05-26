data = read.csv("c://data//data2a.csv")
View(data)
pairs(data)
cor(data)

model = lm(data$Y~data$x2+data$x3+data$x4)
summary(model)

model2 = lm(data$Y~data$x2+data$x3)
summary(model2)

res = residuals(model)
qqnorm(res)
qqline(res)
hist(res)
shapiro.test(res)

plot(data$x2, res)
plot(data$x3, res)
plot(data$x4, res)

confint(model)
AIC(model, model2)
BIC(model, model2)

library(MASS)
stepAIC(model)

library(leaps)
regsubsets(Y~.,data=data)
summary(regsubsets(data[,-1], data[,1]))
res.sum = summary(regsubsets(data[,-1], data[,1]))
data.frame(Adj.R2 = which.max(res.sum$adjr2),
           CP = which.min(res.sum$cp),
           BIC = which.min(res.sum$bic))

#question 2
data2 = read.csv("c://data//data2b.csv")
View(data2)
head(data2)
pairs(data2)
str(data2)
cor(data2)

library(ggplot2)
require(corrplot)
colSums(sapply(data2, is.na))
summary(data2)
cat("The number of duplicated rows are", nrow(data2) - nrow(unique(data2)))

corr.df <- cbind(data2, data2['SMR_resp'])
correlations <- cor(corr.df)

corr.SMR_resp <- as.matrix(sort(correlations[,'SMR_resp'], decreasing = TRUE))

corr.idx <- names(which(apply(corr.SMR_resp, 1, function(x) (x > 0.5 | x < -0.5))))

corrplot(as.matrix(correlations[corr.idx,corr.idx]), type = 'upper', method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)

train1 <- data2[!is.na(data2$SMR_resp),]
test1 <- data2[is.na(data2$SMR_resp),]
library(randomForest)
library(xgboost)
xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)
label_train <- data2$SMR_resp[!is.na(data2$SMR_resp)]
dtrain <- xgb.DMatrix(data = as.matrix(train1), label= label_train)
dtest <- xgb.DMatrix(data = as.matrix(test1))