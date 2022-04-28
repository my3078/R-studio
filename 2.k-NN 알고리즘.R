install.packages("kknn")
library(kknn)
library(class)
library(caret)
library(reshape2)
library(ggplot2)
library(corrplot)
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
#yesno변수만 객체로 만들기
spam_train_labels <- spam_train$yesno
spam_test_labels <- spam_test$yesno
#모델용 데이터셋 정리
train <- spam_train_scale[-7]
test <- spam_test_scale[-7]
#k값 구하기
grid1 <- expand.grid(.k=seq(1,20,by=1))
control <- trainControl(method="cv")
set.seed(200)
knn.train <- train(yesno ~., data=spam_train_scale,
                   method="knn", trControl=control, tuneGrid=grid1)
knn.train
#k = 11인 분류 모델 작성 및 정확도 평가
pred <- knn(train=train, test=test, cl=spam_train_labels, k=11, prob=TRUE)
confusionMatrix(pred, spam_test_labels)

#가중치 이웃법
set.seed(200)
kknn.train <- train.kknn(yesno ~., data=spam_train_scale,
                         kmax=25, distance=2, kernel=c("rectangular","triangular","epanechnikov" ))
plot(kknn.train)
kknn.train
#k = 7인 모델 정확도 평가
kknn.pred <- predict(kknn.train, newdata=test)
table(kknn.pred, spam_test_labels)
confusionMatrix(kknn.pred, spam_test_labels)