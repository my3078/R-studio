getwd()
setwd("C:/Users/choim/Desktop")
heart <- read.csv("heart.csv")
heart

#데이터 현황
summary(heart)
# sex, age, thalach, target 데이터 현황
vars <- c("sex", "age","thalach", "target")
#새로운 hd 객체 생성
hd <- heart[vars]
hd
summary(hd)
#lapply함수를 이용한 평균 구하기
lapply(hd, mean)
#vapply함수를 이용한 최소값, 최대값 구하기
vapply(hd,range,c(min=0,max=0))
install.packages("pastecs")
library(pastecs)
#pastecs패키지의 stat.desc()로 기술통계 나타내기
#각 변수의 관측치 개수, 결측치 값, 분산, 표준편차
stat.desc(hd)

#일원표(분할표)
table(hd$target)
#변수 이름만으로 접근 가능하게 해줌
exist <- with(hd, table(target))
exist
#빈도를 비율로 표시
prop.table(exist)

#상관분석(스피어만)
cor(heart, method = "spearman")
#부분 상관관계
install.packages("ggm")
library(ggm)
colnames(heart)
#age변수와 thalach변수 간 상관관계
#나머지 변수는 통제
pcor(c(1, 6, 2, 3, 4, 5, 7, 8, 9), cov(heart))
#상관관계 유의도 검정
#H0 : age변수와 thalach변수 간의 피어슨 상관계수가 0이다.
cor.test(heart[,1], heart[,6])
plot(heart[,1], heart[,6])

#카이제곱 독립성 검정
#H0 : 심장병 유무는 성별과 연관성이 없다.
table <- xtabs(~target+sex, data=heart)
chisq.test(table)

#t-검정
#H0 : 심장병 유무에 따라 혈압 수치는 차이가 없다. 
t.test(trestbps~target, data = heart)

#선형회귀분석
str(heart)
thalach <- lm(formula=thalach ~ age, data=heart)
summary(thalach)
#잔차값
residuals(thalach)
plot(heart$age, heart$thalach, xlab="age", ylab="thalach")
abline(thalach)