install.packages("DAAG")
library(DAAG)
data("ais")
ais.1 <- ais[,-(12:13)]
head(ais.1)
attach(ais.1)
str(ais.1)
#상관관계 분석
install.packages("corrplot")
library(corrplot)
ais.cor <- cor(ais.1)
ais.cor
corrplot(ais.cor, method="ellipse")
pairs(~., data=ais.1)
#독립변수 선택
install.packages("leaps")
library(leaps)
sub.fit <- regsubsets(hc~., data=ais.1)
a.summary <- summary(sub.fit)
names(a.summary)
#rss 사용
which.min(a.summary$rss)
#cp 사용
#rcc, hg, ssf, pcBfat, ht 사용
which.min(a.summary$cp)
par(mfrow=c(1,1))
plot(a.summary$cp, xlab="number of features", ylab="cp")
plot(sub.fit, scale="Cp")
#cp로 선택한 피처로 모델 만들기
fit.1 <- lm(hc~rcc+hg+ssf+pcBfat+wt, data=ais.1)
summary(fit.1)
#진단용 도표 작성(선형성, 정규성, 등분산성)
par(mfrow=c(2,2))
plot(fit.1)
#공선성 파악
install.packages("car")
library(car)
vif(fit.1)
#ssf변수와 pcBaf변수 간의 상관관계
par(mfrow=c(1,1))
plot(ssf,pcBfat,xlab="ssf",ylab="pcBfat", main="ssf와 pcBaf 상관관계")
#adjr2를 이용한 변수 탈락
a.summary$adjr2
plot(sub.fit, scale="adjr2")
#2개의 독립변수를 가진 모델 만들기
fit.2 <- lm(hc~rcc+hg, data=ais.1)
summary(fit.2)
#진단형 도표 작성 및 공선성 확인
par(mfrow=c(2,2))
plot(fit.2)
vif(fit.2)
par(mfrow=c(1,1))
plot(fit.2$fitted.values, hc, xlab="predicted",ylab="actual", main="predicted vs. actual")
#교차검증기법
install.packages("MPV")
library(MPV)
PRESS(fit.1)
PRESS(fit.2)

#정규화
#상관관계 확인
plot(ais.1)
install.packages("caret")
library(caret)
#학습용, 평가용 데이터 구분
idx <- createDataPartition(ais.1$hc, p=0.7, list=F)
ais_train <- ais.1[idx,]
ais_test <- ais.1[-idx,]
str(ais_test)
str(ais_train)
#최량부분 집합
subfit <- regsubsets(hc~., data=ais_train)
best.summary <- summary(subfit)
which.min(best.summary$bic)
par(mfrow=c(1,1))
plot(best.summary$bic, type="l", xlab="Features N", ylab="BIC")
plot(subfit, scale="bic", main="Best subset features")

#최량 부분 집합 모델의 선형성 확인
ols <- lm(hc~rcc+hg, data=ais_train)
plot(ols$fitted.values, ais_train$hc, xlab="predicted", 
     ylab="actual", main="predicted vs. actual")
pred.subfit <- predict(ols, newdata=ais_test)
plot(pred.subfit, ais_test$hc, xlab="predicted", ylab="actual", main="predicted vs. actual")
#평균 제곱 차이(MSE) 확인
resid.subfit <- ais_test$hc - pred.subfit
mean(resid.subfit^2)

#능형 회귀 분석
install.packages("glmnet")
library(glmnet)
x <- as.matrix(ais_train[,-3])
y <- ais_train[,3]
ridge <- glmnet(x,y, family="gaussian", alpha=0)
print(ridge)
plot(ridge, label=TRUE)
plot(ridge,xvar="lambda", label=TRUE)
#회귀계수 확인(편차 백분율)
ridge.coef <- coef(ridge, s=0.4, exact=TRUE, x=x, y=y)
ridge.coef
plot(ridge, xvar="dev",label=TRUE)
#능형 회귀 분석 모델의 선형성 확인
newx <- as.matrix(ais_test[,-3])
ridge.y <- predict(ridge, newx=newx, type="response",s=0.4)
plot(ridge.y, ais_test$hc, xlab="Predicted", ylab="Actual", main="ridge regression")
#평균 제곱 차이(MSE) 확인
ridge.resid <- ridge.y - ais_test$hc
mean(ridge.resid^2)

#LASSO
lasso <- glmnet(x,y, family="gaussian", alpha=1)
print(lasso)
plot(lasso, xvar="lambda", label=TRUE)
lasso.coef <- coef(lasso, s=0.02, exact=TRUE, x=x, y=y)
lasso.coef
#lasso모델의 선형성 확인
lasso.y <- predict(lasso, newx=newx, type="response", s=0.02)
plot(lasso.y, ais_test$hc, xlab="predicted", ylab="actual", 
     main="LASSO")
#평균 제곱 차이(MSE) 확인
lasso.resid <- lasso.y - ais_test$hc
mean(lasso.resid^2)

#일래스틱 넷
grid <- expand.grid(.alpha=seq(0,1,by=0.5), .lambda=seq(0,0.5, by=0.05))
table(grid)
control <- trainControl(method="LOOCV")
enet.train <- train(hc ~ ., data=ais_train, method="glmnet", trControl=control, tuneGrid=grid)
enet.train
#회귀계수 확인
enet <- glmnet(x,y,family="gaussian", alpha=0.5, lambda=0.05)
enet.coef <- coef(enet, s=0.05, exact=TRUE)
enet.coef
#bmi, pcBfat, lbm, wt 제외
enet.y <- predict(enet, newx=newx, type="response", s=0.05)
plot(enet.y, ais_test$hc, xlab="predicted", 
     ylab="actual", main="Elastic Net")
#평균 제곱 차이(MSE) 확인
enet.resid <- enet.y - ais_test$hc
mean(enet.resid^2)