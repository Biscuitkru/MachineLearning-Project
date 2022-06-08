school1=read.table("student-mat.csv",sep=";",header=TRUE)
school2=read.table("student-por.csv",sep=";",header=TRUE)
schools=merge(school1,school2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

library(corrplot)
## First we want to find which Variables are useful in predicting G3 for School 1 and 2, GLM then Ridge Vs Lasso 
library(glmnet)
library(caTools)
## We first check useful parameters for School 1 in predicting G3 score
Math <- school1[,-c(31,32)]
Math[sapply(school1, is.character)] <- lapply(school1[sapply(school1, is.character)], as.factor)

x <- data.matrix(school1[,1:30])
y <- as.matrix(school1[,31])

# 70-30 Train test split
set.seed(2021)
train <- sample.split(Y = school1$G3, SplitRatio = 0.7)
trainset1 <- subset(school1, train == T)
testset1 <- subset(school1, train == F)
## Do a multiple linear regression model for both schools then can improve the models via lasso/ridge
mlr1 <- lm(G3 ~ ., data=trainset1)
summary(mlr1)
rmse <- mean(residuals(mlr1)^2)
Mathd <- data.matrix(Math)

# 70-30 Train test split for ridge/lasso
set.seed(2021)
train <- sample.split(Y = school1$G3, SplitRatio = 0.7)
trainset1 <- subset(Mathd, train == T)
testset1 <- subset(Mathd, train == F)

## Ridge Regression
grid <- 10^seq(10,-2,length=100)
ridge.mod <- cv.glmnet(x = trainset1[, 1:30], y = trainset1[,31], alpha = 0, lambda = grid)

cv.r1 = cv.glmnet(x = trainset1[, 1:30], y = trainset1[, 31], alpha = 0)
plot(cv.r1)
bestlambda=cv.r1$lambda.min
bestlambda
## 3.72951
ridge.prediction=predict(ridge.mod, s= bestlambda, newx = testset1[, 1:30])
head(ridge.prediction)
RMSE.ridge <- sqrt(mean((ridge.prediction-testset1[,31])^2))
RMSE.ridge
# 4.222624
outcome <- glmnet(x,y,alpha=0, lambda=grid)
ridge.coef <- predict(outcome,type="coefficients",s=bestlambda)
ridge.coef

## Lasso Regression
lasso.mod <- glmnet(x = trainset1[, 1:30], y = trainset1[,31], alpha = 1, lambda = grid)
cv.l1 = cv.glmnet(x = trainset1[, 1:30], y = trainset1[,31], alpha = 1)
plot(cv.l1)
bestlambda=cv.l1$lambda.min
bestlambda
## 0.2693008
lasso.prediction=predict(lasso.mod, s = bestlambda, newx = testset1[,1:30])
head(lasso.prediction)
RMSE.lasso <- sqrt(mean((lasso.prediction-testset1[,31])^2))
RMSE.lasso
## 4.287612

outcome <- glmnet(x,y,alpha=1, lambda=grid)
lasso.coef <- predict(outcome,type="coefficients",s=bestlambda)
lasso.coef
plot(lasso.prediction)
## Coeffs that are 0, School, Age. Address. Pstatus, Mjob, Fjob, Guardian, Traveltime, Paid, Activities
## Nursery, internet, romantic, famrel, freetime, Dalc, Health, Absences.

## Although Rmse of ridge is lower than lasso, the difference in Rmse is not a very significant amount
## So we will be continuing the regression using lasso keeping in mind that the variables
## that we will be using to predict G3 score for in this case.

## Now we check for useful parameters for Port in predicting G3 scores
Port <- school2[,-c(31,32)]
Port[sapply(Port, is.character)] <- lapply(Port[sapply(Port, is.character)], as.factor)

Portd <- data.matrix(Port)

x <- data.matrix(Port[,1:30])
y <- as.matrix(Port[,31])

# 70-30 Train test split
set.seed(2021)
train <- sample.split(Y = school2$G3, SplitRatio = 0.7)
trainset2 <- subset(Portd, train == T)
testset2 <- subset(Portd, train == F)

## Ridge Regression
grid <- 10^seq(10,-2,length=100)
ridge.mod <- cv.glmnet(x = trainset2[, 1:30], y = trainset2[,31], alpha = 0, lambda = grid)

cv.r2 = cv.glmnet(x = trainset2[, 1:30], y = trainset2[, 31], alpha = 0)
plot(cv.r2)
bestlambda=cv.r2$lambda.min
bestlambda
## 1.011083
ridge.prediction=predict(ridge.mod, s= bestlambda, newx = testset2[, 1:30])
RMSE.ridge <- sqrt(mean((ridge.prediction-testset2[,31])^2))
RMSE.ridge
# 2.809792

outcome <- glmnet(x,y,alpha=0, lambda=grid)
ridge.coef <- predict(outcome,type="coefficients",s=bestlambda)
ridge.coef

## Lasso Regression
lasso.mod <- glmnet(x = trainset2[, 1:30], y = trainset2[,31], alpha = 1, lambda = grid)
cv.l2 = cv.glmnet(x = trainset2[, 1:30], y = trainset2[,31], alpha = 1)
plot(cv.l2)
bestlambda=cv.l2$lambda.min
bestlambda
## 0.1162499
lasso.prediction=predict(lasso.mod, s = bestlambda, newx = testset2[,1:30])
head(lasso.prediction)
RMSE.lasso <- sqrt(mean((lasso.prediction-testset2[,31])^2))
RMSE.lasso
# 2.810955

outcome <- glmnet(x,y,alpha=1, lambda=grid)
lasso.coef <- predict(outcome,type="coefficients",s=bestlambda)
lasso.coef

# Doing CART method
library(rpart)
library(rpart.plot)

# Math
set.seed(2021)
M1 <- rpart(G3 ~ ., data = Math, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))

printcp(M1)

plotcp(M1)
# Finding the optimal tree
CVerror.cap <- M1$cptable[which.min(M1$cptable[,"xerror"]), "xerror"] + M1$cptable[which.min(M1$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (M1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

cp.opt = ifelse(i > 1, sqrt(M1$cptable[i,1] * M1$cptable[i-1,1]), 1)
# Can see from i that the optimal tree to use is the 3rd tree

PruneM1 <- prune(M1, cp = cp.opt)
printcp(PruneM1, digits = 3)
# Root node error: 8270/395 = 20.9
# M1 trainset MSE = 0.760 * 20.9 = 15.884
# M1 CV MSE = 0.768 * 20.9 = 16.0512

rpart.plot(PruneM1, nn = T, main = "Tree for School 1")
PruneM1$variable.importance
# Number of past class failures have the highest importance followed by number of absences.

# Port
set.seed(2021)
P1 <- rpart(G3 ~ ., data = Port, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))

printcp(P1)

plotcp(P1)
# Optimal tree for P1
CVerror.cap <- P1$cptable[which.min(P1$cptable[,"xerror"]), "xerror"] + P1$cptable[which.min(P1$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (P1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

cp.opt = ifelse(i > 1, sqrt(P1$cptable[i,1] * P1$cptable[i-1,1]), 1)
# Optimal tree this time is tree 2

PruneP1 <- prune(P1, cp = cp.opt)
printcp(PruneP1, digits = 3)
# Root node error: 6763/649 = 10.4
# P1 trainset MSE = 0.808 * 10.4 = 8.4032
# P1 CV MSE = 0.813 * 10.4 = 8.4552

rpart.plot(PruneP1, nn = T, main = "Tree for School 2")
PruneP1$variable.importance
# For Port, the most importable variable is the number of failures followed by their age

