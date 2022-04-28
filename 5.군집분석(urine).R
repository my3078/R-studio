install.packages("compareGroups")
install.packages("NbClust")
install.packages("sparcl")
library(compareGroups)
library(NbClust)
library(sparcl)
library(boot)
library(cluster)

data(urine)
str(urine)
urine.1 <- na.omit(urine)
head(urine.1)
summary(urine.1)
#데이터 표준화
A <- urine.1[,-1]
A.1 <- scale(A)
A.df <- as.data.frame(A.1)
str(A.df)

#완전측정법
#군집수 결정(3개)
numComplete <- NbClust(A.df, distance="euclidean",min.nc = 2, max.nc = 7, method="complete", index="all")
numComplete$Best.nc
#클러스터 덴드로그램 작성
#거리 행렬 계산
dis <- dist(A.1, method="euclidean")
hc <- hclust(dis, method="complete")
plot(hc, hang=-1, labels=FALSE, main="complete-linkage")
comp <- cutree(hc,3)
#군집 시각화
ColorDendrogram(hc, y=comp, main="complete", branchlength = 30)
#새로운 데이터의 클러스터 확인
urine.1$cluster <- comp
head(urine.1)

#Ward.D2(군집수 3개)
ward <- NbClust(A.df, distance="euclidean",min.nc = 2, max.nc = 7, method="ward.D2",index="all")
#클러스터 덴드로그램 작성
#거리 행렬 계산
dis.2 <- dist(A.1, method="euclidean")
#군집화
hcward <- hclust(dis.2, method="ward.D2")
plot(hcward, labels=FALSE, main="ward")
#군집 시각화
ward2 <- cutree(hcward,3)
ColorDendrogram(hcward, y=ward2,main="Ward",branchlength = 30)
table(ward2, urine.1$r)

#모델 비교
par(mfrow=c(1,2))
boxplot(urine.1$ph ~ comp, data=urine.1, main="complete")
boxplot(urine.1$ph ~ ward2, data=urine.1, main="ward")

#k-평균 군집화(군집수 3개)
kmeans <- NbClust(A.df, distance="euclidean",min.nc = 2, max.nc = 7, method="kmeans", index="all")

#k-평균 군집화의 또다른 모델
set.seed(2000)
km <- kmeans(A.df, 3, nstart=25)
table(km$cluster)
km$centers

#모델 비교
par(mfrow=c(1,2))
boxplot(urine.1$ph ~ km$cluster, data=urine.1, main="k-means")
boxplot(urine.1$ph ~ comp, data=urine.1, main="complete")
boxplot(urine.1$urea ~ km$cluster, data=urine.1, main="k-means")
boxplot(urine.1$urea ~ comp, data=urine.1, main="complete")
plot(urine.1[,3:4], pch=km$cluster, col=km$cluster)