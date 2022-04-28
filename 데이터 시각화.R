install.packages("Stat2Data")
library(Stat2Data)
data("WalkTheDogs")

#ggplot
library(ggplot2)
#산점도
p1 <- ggplot(WalkTheDogs, aes(StepCount,Kcal))
p2 <- p1 + geom_point(col="blue", shape=17) 
p3 <- p2 + ggtitle("StepCount&Kcal")
p3
#가로형 누적 막대 그래프
p4 <- ggplot(WalkTheDogs, aes(factor(Weather)))
p5 <- p4 + geom_bar(aes(fill=factor(Walk)))
p6 <- p5 + coord_flip()
p7 <- p6 + ggtitle("날씨에 따른 산책 분포")
p7
#백분율을 표시한 파이 차트
library(dplyr)
count.day <- data.frame(
  class = c("cold", "rain", "shine"),
  n = c(69, 35, 119),
  prop=c(31,16,53))
count.day
count.day <- count.day %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
count.day
p8 <- ggplot(count.day, aes(x="",y = prop, fill = class))
p9 <- p8 + geom_bar(width=1, stat="identity",color="black")  
p10 <- p9 + coord_polar("y", start=0)  
p11 <- p10 + geom_text(aes(y=lab.ypos, label=paste(prop, "%")), color="white")
p12 <- p11 + ggtitle("날씨 분포포")
p12
#그룹별 밀도 그래프
p13 <- ggplot(WalkTheDogs, aes(x=Steps))
p14 <- p13 + geom_density(aes(fill=factor(Walk)), alpha=0.5)
p15 <- p14 + ggtitle("강아지 산책 유무에 따른 걸음단계")
p15
#집단화 박스 그래프
p16 <- ggplot(WalkTheDogs, aes(factor(Weather), Miles))
p17 <- p16 + geom_boxplot()
p18 <- p17 + facet_grid(~Walk)
p19 <- p18 + ggtitle("산책유형 별 날씨에 따른 걸은 거리")
p19

#육각형 플롯(hexbin plot)
p20 <- ggplot(WalkTheDogs, aes(x=StepCount, y=Kcal))
p21 <- p20 + geom_hex()
p21

#R그래픽스
#산점도
attach(WalkTheDogs)
plot(Kcal~StepCount, col="blue", pch=17, type="p",main="StepCount&Kcal")
grid()
#가로형 누적 막대 그래프
counts <- table(Walk, Weather)
counts
barplot(axes=F, counts, horiz=TRUE, main="날씨에 따른 산책 분포", col=c("red","blue"),legend=rownames(counts))
axis(side=1, col.axis="black")

#백분율을 표시한 파이차트
slices <- c(69, 35, 119)
lbls <- c("cold","rain","shine")
pct <- round(slices/sum(slices)*100)
lbls2 <- paste(lbls, " ", pct, "%", "")
pie(slices, labels=lbls2, main="날씨 분포")

#그룹별 밀도 그래프
install.packages("sm")
library(sm)
sm.density.compare(Steps, Walk)
grid()
legend("topright", fill=c("red","green"),legend=c("0","1"))
title(main="강아지 산책 유무에 따른 걸음수")

#집단화 박스 그래프
boxplot(Miles~Weather+Walk, data=WalkTheDogs, 
        at=c(1:3, 5:7), col=c("red", "blue", "green"),
        names=c("", "0", "", "", "1", ""),main="산책유형 별 날씨에 따른 걸은 거리")
legend("topleft", fill=c("red","blue","green"),legend=c("cold","rain","shine"))
abline(v=4, col="black")
grid()

#육각형 플롯(hexbin plot)
install.packages("hexbin")
library(hexbin)
h <- hexbin(StepCount, Kcal)
plot(h)