####################################################
# 확률분포 그래프(Probability distribution plots)
#
# 출처: http://rfriend.tistory.com/
#
#      15 / Nov. / 2018
#
#######################
#distribution R name 
#=====================
#Normal        norm 
#Student t     t 
#Uniform       unif 
#Exponential   exp
#Poisson       pois 
#Beta          beta 
#Lognormal     lnorm 
#Binomial      binom 
#Negative Binomial  nbinom 
#Cauchy        cauchy 
#Chisquare     chisq 
#F             f 
#Gamma         gamma 
#Tukey         tukey 
#Geometric     geom 
#Weibull       weib 
#Hypergeometric hyper 
#Wilcoxon      wilcox 
#Logistic      logis     
#######################


library(ggplot2)


#####################################################################################
# (1) 정규분포그래프(Normal Distribution plot)

# 정규분포(normal distribution)는 
# 추정과 검정을 하는 추정통계학, 회귀분석과 같은 모형 적합시 근간이 되는 확률 분포

# 우리의 일상 주변에서 흔히 접할 수 있는 확률분포이며, 
# 중심 극한의 정리(Central Limit Theorem)에 따라 샘플의 갯수 n이 증가하면 
# 이항분포, 초기하분포, 포아송분포 등의 이산형 확률분포와 
# t-분포, F-분포 등의 연속형 확률분포가 정규분포로 근사하게 됨.  

# 정규분포는 통계에 있어서 중요하고 많이 사용되는 확률분포임.
#
# 출처: http://rfriend.tistory.com/102?category=605867 [R, Python 분석과 프로그래밍 (by R Friend)]
#####################################################################################



x<-seq(-3, 3, length=200)
plot(x, dnorm(x, mean =0, sd=1), type ='l', main="Normal Distribution, X~N(0,1)")



# 누적정규분포 그래프(Cumulative Normal Distribution plot), X~N(0,1)

x<-seq(-3, 3, length=200)
plot(x, pnorm(x, mean =0, sd=1), type ='l', main="Cumulative Normal Distribution, X~N(0,1)")


# 100s random sampling from normal distribution X~N(0,1)
random_norm_100 <- rnorm(100, mean=0, sd=1)
random_norm_100


# Histogram of random_norm_100
hist(random_norm_100)




################################################################
# (2) t-분포그래프(t-Distribution plot)
# fun = dt
#

# 정규분포에서는 모분산(σ2)을 알고 있다고 가정하는데, 
# 실전의 현실 세계에서는 모분산(σ2)을 모르는 경우가 대부분이다보니 
# 표본을 추출해서 표본분산(s2)을 계산하여 사용하는 경우가 다반사입니다.
# 
# 이러다 보니 표준정규분포 통계량 Z 값을 사용할 수 없고, 표본 확률분포를 사용해야 하는데 
# 그중의 하나가 T통계량을 사용하는 t-분포임.
#
# 출처: http://rfriend.tistory.com/110 [R, Python 분석과 프로그래밍 (by R Friend)]


###############################################################
ggplot(data.frame(x=c(-3,3)), aes(x=x)) +
  stat_function(fun=dnorm, colour="blue", size=1) +
  stat_function(fun=dt, args=list(df=3), colour="red", size=2) +
  stat_function(fun=dt, args=list(df=1), colour="yellow", size=3) +
  annotate("segment", x=1.5, xend=2, y=0.4, yend=0.4, colour="blue", size=1) +
  annotate("segment", x=1.5, xend=2, y=0.37, yend=0.37, colour="red", size=2) + 
  annotate("segment", x=1.5, xend=2, y=0.34, yend=0.34, colour="yellow", size=3) + 
  annotate("text", x=2.4, y=0.4, label="N(0,1)") +
  annotate("text", x=2.4, y=0.37, label="t(3)") + 
  annotate("text", x=2.4, y=0.34, label="t(1)") + 
  ggtitle("Normal Distribution, t-distribution")



# 누적 t분포 그래프 (Cumulative t-distribution plot) : fun=pt
ggplot(data.frame(x=c(-3,3)), aes(x=x)) +
  stat_function(fun=pt, args=list(df=1), colour="brown", size=1.5) +
  ggtitle("Cumulative t-Distribution : t(1)")


# t분포 난수 발생 : rt(n, df) 
rt <- rt(50, df=1)
rt

# Histogram of hist(rt, breaks=20)
hist(rt, breaks=20)



################################################################
# (3) 균등분포 그래프(Uniform Distribution plot) (min=0, max=10)
# fun = dunif
#
# 균등분포(uniform distribution)은 
# 연속형 확률 분포 중에서 가장 간단한 형태로서, 
# 구간 [mi=a, max=b]에서 값이 균등하게 퍼져 있는 집단, 일어날 확률이 균등한 분포를 말합니다.  
#
# 사례 :
# 김포공항에서 제주도 공항까지 비행기로 이륙에서 착륙까지 걸리는 총 비행시간이 1시간~1시간5분 사이라고 하면, 
# 0시~59분59초까지는 비행기가 도착할 확률이 0, 
# 1시간~1시간5분 사이에 도착할 확률은 1, 
# 1시간 5분 이후는 다시 확률이 0이 되는 균등분포를 따른다고 할 수 있겠습니다.
#
#############################################################################

# 출처: http://rfriend.tistory.com/106 [R, Python 분석과 프로그래밍 (by R Friend)]


ggplot(data.frame(x=c(-2,20)), aes(x=x)) + 
  stat_function(fun=dunif, args=list(min=0, max=10), colour="black", size=1) + 
  ggtitle("Uniform Distribution of (min=1, max=10)")


# 누적 균등분포 그래프(Cumulative Uniform Distribution plot) (min=0, max=10)
# fun = punif

ggplot(data.frame(x=c(-2,20)), aes(x=x)) + 
  stat_function(fun=punif, args=list(min=0, max=10), colour="black", size=1) + 
  ggtitle("Cumulative Uniform Distribution of (min=1, max=10)")


# 100s random sampling from uniform distribution
ru_100 <- runif(n=100, min=0, max=10)
ru_100


# Histogram of hist(ru_100, freq=FALSE, breaks=10, col="yellow")
hist(ru_100, freq=FALSE, breaks=10, col="yellow")

abline(h=0.1, lty=3, lwd=3, col="red")



####################################################################################################
# (4) 지수분포그래프(Exponential Distribution plot)
# fun = dexp
#
#
# 지수분포 (exponential distribution)는 연속형 확률 분포 중 하나로서
#         어떤 특정 사건이 발생하기 전까지 걸리는 시간 (즉, 특정 사건과 사건 사이의 간격)을 
#         나타내기 위해 많이 사용하는 확률분포 입니다.  
#
# 지수분포의 예 : 
# 1. 전자레인지의 수명시간, 
# 2. 콜센터에 전화가 걸려 올 때까지 걸리는 시간, 
# 3. 경부고속도로 안성 나들목에서 다음번 교통사고가 발생할 때까지 걸리는 시간, 
# 4. 은행 지점에 고객이 내방하는데 걸리는 시간 등이 있겠습니다. 
#
# 참고로, 이산형 확률분포 중에서 포아송 분포 (Poisson distribution)는 
#         단위 시간 혹은 단위 공간에서 특정 사건이 발생하는 횟수에 대한 분포를 나타낼 때 주로 사용함. 
#
# 출처: http://rfriend.tistory.com/107 [R, Python 분석과 프로그래밍 (by R Friend)]
#
####################################################################################################

ggplot(data.frame(x=c(0,10)), aes(x=x)) + 
  stat_function(fun=dexp, args=list(rate=1), colour="brown", size=1.5) + 
  ggtitle("Exponential Distribution")

# 누적 지수분포 그래프(Cumulative Exponential Distribution plot)
# fun = pexp

ggplot(data.frame(x=c(0,10)), aes(x=x)) + 
  stat_function(fun=pexp, args=list(rate=1), colour="brown", size=1.5) + 
  ggtitle("Cumulative Exponential Distribution")



# 100s random sampling from exponential distribution
rexp(100, rate=1)



# Histogram of rexp(100, rate=1)
hist(rexp(100, rate=1), breaks=10)


####################################################################################################
# (5) 포아송 분포그래프(Poisson Distribution plot)
# fun = pois
#
# 연속형 확률 분포 중 지수 분포(exponential distribution)는 특정 사건과 사건 사이의 간격에 대한 분포 
####################################################################################################

# 포아송 분포(Poisson distribution)는 
# 일정한 단위 시간, 단위 공간에서 어떤 사건이 랜덤하게 발생하는 경우에 사용할 수 있는 이산형 확률분포 
#
# 포아송 분포에서 모수 λ (lambda 라고 발음함)는 일정한 단위 시간 또는 단위 공간에서 랜덤하게 발생하는 사건의 평균 횟수

# 대표적인 사례:
# +++++++++++++++++++++++++++++++++++++
# 1시간 동안 은행에 방문하는 고객의 수, 
# 1시간 동안 콜센터로 걸려오는 전화의 수, 
# 1달 동안 경부고속도로에서 교통사고가 발생하는 건수, 
# 1년 동안 비행기 사고가 발생하는 건수, 
# 책 1페이지당 오탈자가 발생하는 건수, 
# 반도체 웨이퍼 25장 당 불량 건수 등과 같이 단위 시간 혹은 단위 공간에서의 랜덤한 사건에 대해 사용 
#
####################################################################################################

#λ = 3 인 포아송 분포 그래프 (Poisson distribution plot of lambda = 3)

# (1) 포아송 분포 그래프 (Poisson distribution plot)
plot(dpois(x=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), lambda = 3), type='h', main = "Poisson distribution, lambda = 3")


######### ------
# 문제)  어느 은행의 1시간 당 방문 고객 수가 λ = 20 인 포아송 분포를 따른다고 한다.  
# 그럼 1시간 당 방문고객수가 15명일 확률은? 

# (2) P(X = 15) in Poisson distribution with lambda = 20
  
dpois(x=15, lambda = 20)

## result: [1] 0.05164885


#
# 난수 발생 : rpois(n, lambda)

# 문제 ) λ = 20 인 포아송 분포에서 n = 1000 개의 난수를 발생시키고, 도수분포표를 구하고, 도수별 막대그래프를 그려보아라. 

## 난수 10000 개 생성
rpois(n=1000, lambda = 20)

## 도수분포표
table(rpois(n=1000, lambda = 20))

## 결과
# 7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 
# 1  2  3  6 13 17 33 31 45 74 76 77 87 95 86 73 65 59 37 36 28 16 14 12  4  8  2 

## 도수별 막대 그래프
## 결과: 아래의 그래프를 보면 λ = 20 이므로 평균이 20 인 위치에서 가장 높게 모여있고, 
##       오른쪽으로 꼬리가 긴 포아송 분포를 따르고 있음. 

plot(table(rpois(n=1000, lambda = 20)))


## 출처: http://rfriend.tistory.com/101 [R, Python 분석과 프로그래밍 (by R Friend)]


