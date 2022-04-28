install.packages("kernlab")
library(kernlab)
library(e1071)
library(caret)
library(DAAG)
data("spam7")
str(spam7)
#학습용, 평가용 데이터 구분
idx <- createDataPartition(spam7$yesno, p=0.7, list=F)
spam_train <- spam7[idx,]
spam_test <- spam7[-idx,]
#데이터셋 표준화
#yesno변수 제거
spam_train_scale <- data.frame(scale(spam_train[-7]))
spam_test_scale <- data.frame(scale(spam_test[-7]))
#yesno변수 다시 포함
spam_train_scale$yesno <- spam_train$yesno
spam_test_scale$yesno <- spam_test$yesno
#모델용 데이터셋 정리
train <- spam_train_scale
test <- spam_test_scale

#linear SVM
linear.tune <- tune.svm(yesno ~., data=train, kernel="linear",
                        cost=c(0.1,0.5,1,5,10))
summary(linear.tune)
#cost값 10, 오차비율 0.15
best.linear <- linear.tune$best.model
tune.test <- predict(best.linear, newdata=test)

confusionMatrix(tune.test, test$yesno)

#polynomial kernal svm
set.seed(200)
poly.tune <- tune.svm(yesno~., data=train, kernel="polynomial",
                      degree=c(2,3,4), coef0 = c(0.1,0.5,1,2,3,4,5))
summary(poly.tune)
#degree 4, coef0 5, 오차비율 12.9%
best.poly <- poly.tune$best.model
poly.test <- predict(best.poly, newdata=test)
confusionMatrix(poly.test, test$yesno)

#radial kernal svm
set.seed(200)
rbf.tune <- tune.svm(yesno~., data=train, kernel="radial", gamma=c(0.1,0.5,1,2,3,4,5))
summary(rbf.tune)
#gamma : 5, 오차비율 : 12.8
best.rbf <- rbf.tune$best.model
rbf.test <- predict(best.rbf, newdata=test)
confusionMatrix(rbf.test, test$yesno)

#sigmoid kernal svm
set.seed(200)
sigmoid.tune <- tune.svm(yesno~., data=train, kernel="sigmoid",
                         gamma=c(0.1,0.5,1,2,3,4,5), coef0=c(0.1,0.5,1,2,3,4,5) )
summary(sigmoid.tune)
#gamma : 0.1, coef0 : 2, 오차비율 : 18.7%
best.sigmoid <- sigmoid.tune$best.model
sigmoid.test <- predict(best.sigmoid, newdata=test)
confusionMatrix(sigmoid.test, test$yesno)