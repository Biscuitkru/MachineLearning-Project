bank=read.table("bank.csv",sep=";",header=TRUE)

b1 <- bank
b1[sapply(b1, is.character)] <- lapply(bank[sapply(b1, is.character)], as.factor)

summary(b1$y)
# Binary classification with the use of Logistic regression, KNN, Trees, SVM
library(ISLR)

x <- data.frame(b1[,1:16])
y <- (b1[,17])

set.seed(2021)
#70-30 train-test set
library(caTools)
train <- sample.split(Y = b1$y, SplitRatio = 0.70)
trainset1 <- subset(b1, train == T)
testset1 <- subset(b1, train == F)

# Logistic Regression 
glm.Y <- glm(y ~ ., data=b1, family = binomial, subset = train)

glm.probability=predict(glm.Y,testset1,type="response")
summary(glm.probability)
# Since mean is 0.116, i will go with the assumption of 0.11 as the cutoff point where y is yes
contrasts(b1$y)
glm.predicted=rep("no",1356)
glm.predicted[glm.probability>.11]="yes"
glm.predicted <- factor(glm.predicted)
  
table(glm.predicted, testset1$y)
# (994+124)/(1365)
mean(glm.predicted==testset1$y)
# Accuracy of 82.4%

# LDA
library(MASS)
ldaY <- lda(y~., data = b1, subset = train)

lda.predicted <- predict(ldaY, testset1)$class
table(lda.predicted,testset1$y)
# (1159+70)/1356
mean(lda.predicted==testset1$y)
# Accuracy of 90.6%
library(ROCR)
lda.predicted <- predict(ldaY, testset1)
predicted <- prediction(lda.predicted$posterior[,2], testset1$y)
lda.perf <- performance(predicted, "tpr","fpr")
plot(lda.perf,colorize = T)
# Depiction of true positive rate against false positive rate for LDA model

# QDA
qdaY <- qda(y~., data = b1, subset = train)

qda.pred <- predict(qdaY, testset1)$class
table(qda.pred,testset1$y)
# (1095+69)/1356
mean(qda.pred==testset1$y)
# Accuracy of 85.8%

# KNN
library(class)
b2 <- as.data.frame(b1)
# Extracting Y term 
library(dplyr)
yterm <- b2 %>% select(y)
b2 <- b2 %>% select(-y)
# Scaling of variables
b2[,c("age","day","balance","previous","campaign","duration","pdays")] <- scale(b2[,c("age","day","balance","previous","campaign","duration","pdays")])
# Turning factors into numerical
b2$default <- ifelse(b2$default == "yes", 1, 0)
b2$housing <- ifelse(b2$housing == "yes", 1, 0)
b2$loan <- ifelse(b2$loan == "yes", 1, 0)

library(psych)
job <- as.data.frame(dummy.code(b2$job))
marital <- as.data.frame(dummy.code(b2$marital))
month <- as.data.frame(dummy.code(b2$month))
education <- as.data.frame(dummy.code(b2$education))
contact <- as.data.frame(dummy.code(b2$contact))
poutcome <- as.data.frame(dummy.code(b2$poutcome))

b2 <- b2 %>% select(-one_of(c("job", "marital","month", "education","contact","poutcome")))
b2 <- cbind(b2, job, marital, education, contact, month, poutcome)

set.seed(2021)
training <- floor(0.7002*nrow(b2))
training <- sample(seq_len(nrow(b2)), size = training)

trainset2 <- b2[training,]
testset2 <- b2[-training,]
yterm_train <- yterm[training,]
yterm_test <- yterm[-training,]

# Set K = 56, square root of 3164
ypredknn <- knn(train = trainset2, test = testset2, cl = yterm_train, k=56)
ypredknn <- data.frame(ypredknn)
Cross_table <- data.frame(ypredknn, yterm_test)
names(Cross_table) <- c("PredictedY", "ObservedY")
table(Cross_table$PredictedY, Cross_table$ObservedY)
mean(Cross_table$PredictedY==Cross_table$ObservedY)
# Accuracy rate of 89.2%

#SVMFIT
library(e1071)
xydata=data.frame(x,y=as.factor(y))
svm.Y=svm(y~., data=xydata[training,], kernel="linear",gamma = 1, cost=1)
svm_prediction <- predict(svm.Y, newdata = xydata[-training,])
table(svm_prediction, testset1$y)
mean(svm_prediction == testset1$y)
## Svmfit gives an accuracy of 86.1%