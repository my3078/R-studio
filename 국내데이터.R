getwd()
setwd("C:/Users/choim/Desktop")
accident <- read.csv("accident.csv")
accident

#기술통계
summary(accident)
vars <- c("발생건수", "사망자수", "부상자수")
vehicle_accident <- accident[vars]
vehicle_accident
#sapply함수를 이용한 평균 구하기
sapply(vehicle_accident, mean)
#lapply함수를 이용한 평균 구하기
lapply(vehicle_accident, mean)
#vapply함수를 이용한 최소값, 최댓값
vapply(vehicle_accident, range, c(min = 0, max = 0))
#describe함수
install.packages("psych")
library(psych)
describe(vehicle_accident)

#t-검정
#H0 : 주간 사고와 야간 사고의 사망자 수는 차이가 없다.
wilcox.test(사망자수~주야, data = accident)

#상관분석
cor(accident, method = "spearman")
#부분 상관관계
library(ggm)
colnames(accident)
#발생건수와 경상 간 상관관계
pcor(c(3, 7), cov(accident))
#상관관계 유의도 검정
cor.test(accident[,3], accident[,7])
plot(accident[,3], accident[,7])

#회귀분석
slight_wound <- lm(formula=경상~발생건수, data = accident)
summary(slight_wound)
plot(accident$경상, accident$발생건수, xlab = "경상자수", ylab = "발생건수")
abline(slight_wound)