#set working directory
setwd("C:/r_workspace/econometric_analysis")

#(p.6)
#read file
mydata1 <- read.csv("sample_data_1_it_firm.csv", fileEncoding='euc-kr')

#regresult(previous simple linear regression model)
regresult <- lm(formula = SALES ~ IT.STOCK, data=mydata1)
summary(regresult)

#regres1 종속변수 3개: IT.STOCK, LABOUR, CAPITAL
regres1 <- lm(formula = SALES ~ IT.STOCK + LABOUR + CAPITAL, data=mydata1)
summary(regres1)

#(p.7)
#summarize data
summary(mydata1)

#calculate RMSE
length(regres1$residual)
rmse1 <- sum(regres1$residual^2)/length(regres1$residual)
rmse1

#(p.16)
#make new data 
mydata1$AGE_MONTH = mydata1$AGE * 12 #단순한 12배 => 완전 공산성 case!!

regres2 <- lm(formula = SALES ~ IT.STOCK + AGE + AGE_MONTH, data=mydata1)
summary(regres2)

#(p.20)
#draw histogram
hist(regres1$residuals, main="histogram of residuals")
#draw residual plot
plot(mydata1$IT.STOCK, regres1$residuals, main="residual plot")
abline(a=0, b=0, col="red")


#(p.21) draw abline plot using rmse
plot(mydata1$IT.STOCK, mydata1$SALES)
rmse <- sum(regresult$residual^2)/length(regresult$residual)
rmse
abline(a=regresult$coefficients[1], b=regresult$coefficients[2], col="blue", lwd=2)

abline(a=regresult$coefficients[1]+rmse, b=regresult$coefficients[2], col="green", lwd=2)
abline(a=regresult$coefficients[1]-rmse, b=regresult$coefficients[2], col="green", lwd=2)

abline(a=regresult$coefficients[1]+2*rmse, b=regresult$coefficients[2], col="red", lwd=2)
abline(a=regresult$coefficients[1]-2*rmse, b=regresult$coefficients[2], col="red", lwd=2)

#(p.31)
#regresult <- lm(formula = SALES ~ IT.STOCK, data=mydata)
summary(regresult)

#regres1 <- lm(formula = SALES ~ IT.STOCK + LABOUR + CAPITAL, data=mydata1)
summary(regres1)



#(p.33) model 3-5
regres3 <- lm(formula = SALES ~ IT.STOCK + LABOUR + CAPITAL + IT.LABOUR, data=mydata1)
summary(regres3)
rmse3 <- sum(regres3$residual^2)/length(regres3$residual)
rmse3

regres4 <- lm(formula = SALES ~ IT.STOCK + LABOUR + CAPITAL + IT.LABOUR + AGE, data=mydata1)
summary(regres4)
rmse4 <- sum(regres4$residual^2)/length(regres4$residual)
rmse4

regres5 <- lm(formula = SALES ~ AGE, data=mydata1)
summary(regres5)
rmse5 <- sum(regres5$residual^2)/length(regres5$residual)
rmse5



#(p.43)
#include all dummy variables
regres6 <- lm(formula = SALES ~ NOT + ERP + CRM + BOTH, data=mydata1)
summary(regres6)

#exclude one dummy variable (NOT)
regres7 <- lm(formula = SALES ~ ERP + CRM + BOTH, data=mydata1)
summary(regres7)
confint(regres7)

#(p.44)
confint(regres6)
confint(regres7)

#(p.46)
mydata1$ERP_D = mydata1$ERP + mydata1$BOTH
regres8 <- lm(formula = SALES ~ ERP_D, data=mydata1)
summary(regres8)

mydata1$CRM_D = mydata1$CRM + mydata1$BOTH
regres9 <- lm(formula = SALES ~ ERP_D + CRM_D, data=mydata1)
summary(regres8)

#(p.47)
regres10 <- lm(formula = SALES ~ SIZE, data=mydata1)
summary(regres10)

#(p.49)
mydata1$SIZE_SMALL = ifelse(mydata1$SIZE==0, 1, 0)
mydata1$SIZE_MID = ifelse(mydata1$SIZE==1, 1, 0)
mydata1$SIZE_BIG = ifelse(mydata1$SIZE==2, 1, 0)

regres11 <- lm(formula = SALES ~ SIZE_BIG + SIZE_MID, data = mydata1)
summary(regres11)


