

Package_Data_List <- data(package = .packages(all.available = TRUE))
head(Package_Data_List$results)
# 패키지별 데이터 셋 정보의 상위 6개 항목을 확인

data("mtcars")
head(mtcars)
# inner data set 

?mtcars

str(mtcars)

#데이터의 변수를 직접 이용 
attach(mtcars)

#그래프의 창 크기를 설정
win.graph(7,5)

#단순 선점도 (중량 대비 연비)
plot(wt , mpg , main="자동차 중량 대비 연비 산점도", xlab="자동차 중량", ylab="연비(mpg)", pch=19, cex=1.2)

grid(col=3)

#단순회귀 적합선 추가 : lm(y~x) 함수 , abline()함수 

sr <- lm(mpg~wt)
sr

abline(sr , lty=2, lwd=2, col="red")

#단순회귀 적합선 식 표시 

text(4,27, labels = paste0("Y=" , round(sr$coef[[1]],4),"(+)", round(sr$coef[[2]],4), " * X"), col=2, cex=1.2)

# R패키지에 있는 'mtcars' 데이터를 이용하여 , 자동차 연비(mpg), 기통(cyl) , 출력(hp) , 중량(wt) 등 4가지간의 선점도를 작성한다.


# 산점 앵렬도 : 여러 변수들의 상관관계를 동시에 나타낸다.

pairs(mtcars[c(1, 2, 4, 6)])

pairs(~mpg+cyl+hp+wt, data = mtcars, main="자동차 특성치 산점행렬도", panel=function(x,y) 
  {points(x,y, pch=19, col=4); abline(lm(y~x), col=2)})