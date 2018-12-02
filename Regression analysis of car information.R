install.packages("readxl")
library(readxl)

# readxl 패키지를 설치하여 read.xlsx()함수를 사용한다.

Data <- read_excel("C:/Users/user/Desktop/3학년2학기/데이터마이닝과통계/기말고사/기말_HW4/Modify2_Sample.xlsx", sheet = "Database",range = "B3:K108",
col_names = TRUE,
col_types ="guess",
na = "NA")

str(Data)


#데이터의 변수를 직접 이용 
attach(Data)


#그래프의 창 크기를 설정
win.graph(7,5)


plot(`Base Curb Weight (lbs)` , `Fuel Economy Est-Combined (MPG)` , main="Ibs 대비 MPG 산점도", xlab="Base Curb Weight", ylab="Fuel Economy Est-Combined (MPG)", pch=19, cex=1.2)

grid(col=3)

#단순회귀 적합선 추가 : lm(y~x) 함수 , abline()함수 
sr <- lm(`Fuel Economy Est-Combined (MPG)`~`Base Curb Weight (lbs)`)
sr

abline(sr , col="red")


pairs(Data[c(8,9,10)])
# year , ibs , MPG  데이터 통합 

pairs(~`Fuel Economy Est-Combined (MPG)`+`Base Curb Weight (lbs)`+Year, data = Data, main="자동차 특성 산점행렬도", panel=function(x,y) 
{points(x,y, pch=19, col=4); abline(lm(y~x), col=2)})