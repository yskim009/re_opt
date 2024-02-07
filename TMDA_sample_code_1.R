#경로 설정
setwd("C:/r_workspace/econometric_analysis")
#파일 불러오기
mydata <- read.csv("sample_data_1_it_firm.csv", fileEncoding='euc-kr')
#산포도 그려보기
plot(SALES ~ IT.STOCK, data=mydata)
#상관관계 분석
# cor 함수는 이렇게 column끼리 비교도 되지만 그냥 DF 전체에 적용도 가능
# DF에 적용시엔 각 변수끼리의 상관관계를 다룬 매트릭스가 출력
cor(mydata$SALES, mydata$IT.STOCK)
cor11 <-  cor(mydata)
class(cor11)



# 선형회귀용 lm(linear model) 함수:  인자 2개
# formula = 앞(종속변수) ~ 뒤(독립변수)
# MLR시에는 뒤(독립변수)에 +로 여러개의 독립변수를 이어서 추가해준다.
# data = 말그대로 저장된 DF 불러오기

# summary함수: call, residuals, coefficients, 성능지표( RMSE, M R제곱 등등)

#regresult에 회귀분석 결과 저장 및 요약결과 출력
regresult <- lm(formula = SALES ~ IT.STOCK, data=mydata)
summary(regresult)
class(regresult) # 자료형 자체가 lm이므로 주의

#regresult에 회귀분석 결과를 바탕으로 신뢰구간 계산
#신뢰수준 설정 가능 (95%, 99%, 90%), default=95%
confint(regresult, level = 0.95)
confint(regresult, level = 0.99)
confint(regresult, level = 0.90)
# confint 함수 : 인자 2개
# lm 자료형의 데이터 / level: 신뢰수준 설정



#우리가 회귀분석 결과를 저장한 regresult 안에 저장되어 있음
# 내부의 data 이름들은 알아두자!!
#잔차
regresult$residuals
#예측치
regresult$fitted.values
#실제 X, Y, 예측된 Y, 잔차를 포함하는 dataframe을 만들어보자
# cbind :  column(열)결합 / rbind : row(행)결합 / merge: key 값을 기준으로 column 병합/방법이 다양
newdf <- cbind(mydata$IT.STOCK, mydata$SALES, regresult$fitted.values, regresult$residuals)
#칼럼명 지정 및 데이터프레임으로 형식 변경 (아직은 array, matrix 형태이다)
colnames(newdf) <- c("obs x", "obs y", "fitted y", "residual")
newdf <- data.frame(newdf)

#abline: 직선 그리기 함수
abline(a= 14.41633, b= 0.76529) #summary에서의 estimated Intercept(y절편), Estimated IT.stock(기울기)


#앞서 만든 newdf에 있는 잔차 및 y 관측치를 이용하여 rss 및 tss 계산 
rss <- sum(newdf$residual^2)
rss #잔차 제곱합 만들기(aka SSR)
tss <- sum((newdf$obs.y - mean(newdf$obs.y))^2)
tss # 편차 제곱합 만들기 (aka TSS)
#R-squared
Rsq <- 1 - rss/tss
Rsq
#adjusted R-squared
AdRsq <- 1 - ((rss/tss)*504/503)
AdRsq
#RMSE
rmse <- sqrt(rss/505)
rmse


#########################아래는 기타 코드 (수업자료 1 시각화)

install.packages("corrplot")
library(corrplot)

#mtcars는 기본 데이터임 (수업 내용 아님)
mcor <-cor(mtcars)

corrplot(mcor)

corrplot(cor(mydata))

