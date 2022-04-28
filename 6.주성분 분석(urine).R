library(psych)
library(ggplot2)
library(data.table)
library(corrplot)
library(boot)
data(urine)
str(urine)
urine.1 <- na.omit(urine)
#데이터 표준화
u <- urine.1[,-1]
u.1 <- scale(u)
u.df <- as.data.frame(u.1)
str(u.df)
#상관관계 확인
urine.cor <- cor(u.df)
urine.cor
corrplot(urine.cor, method="ellipse")
#주성분 추출
pca <- principal(u.df, rotate="none")
pca
par(mfrow=c(1,1))
plot(pca$values, type="b", xlab="component", ylab="eigenvalues")
#주성분 회전
pca.rotate <- principal(u.df, nfactors=2, rotate="varimax")
pca.rotate
#주성분 점수 추출
head(pca.rotate$scores)
