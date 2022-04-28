#의사결정나무
install.packages("rpart.plot")
library(rpart.plot)
library(DAAG)
library(caret)
library(rpart)
data("spam7")
head(spam7)
#학습용, 평가용 데이터 구분
idx <- createDataPartition(spam7$yesno, p=0.7, list=F)
spam_train <- spam7[idx,]
spam_test <- spam7[-idx,]
#의사결정나무 만들기
treeFit <- rpart(yesno~., data=spam_train, method="class")
print(treeFit)
#의사결정나무 그림
rpart.plot(treeFit, box.col=c("gray","white"))
#정확도 측정
pred <- predict(treeFit, newdata=spam_test, type="class")
confusionMatrix(pred, spam_test$yesno)


#조건부 추론 트리
install.packages("party")
library(party)
myF <- yesno~crl.tot+dollar+bang+money+n000+make
ctreeResult <- ctree(myF, data=spam_train)
table(predict(ctreeResult), spam_train$yesno)
plot(ctreeResult)
#모델 정확도 측정
pred2 <- predict(ctreeResult, newdata=spam_test)
table(pred2, spam_test$yesno)
confusionMatrix(pred2, spam_test$yesno)

#앙상블
#배깅
#부트스트랩 자료 반들기
data1 <- spam7[sample(1:nrow(spam7), replace=T),]
data2 <- spam7[sample(1:nrow(spam7), replace=T),]
data3 <- spam7[sample(1:nrow(spam7), replace=T),]
#예측 모델 작성
ctree1 <- ctree(yesno~., data1)
ctree2 <- ctree(yesno~., data2)
ctree3 <- ctree(yesno~., data3)
#예측 수행
pred1 <- predict(ctree1, spam7)
pred2 <- predict(ctree2, spam7)
pred3 <- predict(ctree3, spam7)
#예측 모델 결합하여 새로운 모델 작성
test <- data.frame(yesno=spam7$yesno, pred1, pred2, pred3)
head(test)
funcResultValue <- function(x) {

    result <- NULL
  for(i in 1:nrow(x)) {
    xtab <- table(t(x[i,]))
    rvalue <- names(sort(xtab, decreasing = T) [1])
    result <- c(result, rvalue)
  }
  return(result)
}
#최종 결과 산출
test$result <- funcResultValue((test[,2:4]))
plot(ctree2)

#정확도와 kappa값 계산
table(test$result, test$yesno)
agree <- (2672 + 1385)/4601
agree
chance <- ((2672+428)/4601)*((2627+116)/4601)+((428+1385)/4601)*((116+1385)/4601)
chance
kappa <- (agree-chance)/(1-chance)
kappa

#랜덤포레스트
install.packages("randomForest")
library(randomForest)
#학습용, 평가용 데이터 구분
idx <- createDataPartition(spam7$yesno, p=0.7, list=F)
spam_train <- spam7[idx,]
spam_test <- spam7[-idx,]
#모델 작성
random <- randomForest(yesno~., data=spam_train, ntree=100, proximity=T)
random
plot(random, main="RandomForest Model of spam")
#주요 피처 확인
importance(random)
varImpPlot(random)
#예측 정확성 확인
table(spam_train$yesno, predict(random))
pred_random <- predict(random, newdata=spam_test)
table(spam_test$yesno, pred_random)
plot(margin(random, spam_test$yesno))
confusionMatrix(pred_random, spam_test$yesno)
