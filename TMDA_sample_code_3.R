#set working directory
setwd("C:/r_workspace/econometric_analysis")

################################################
#power function
################################################
#(p.10)
#read education data
mydata2 <- read.csv("sample_data_2_edu_school.csv", fileEncoding='euc-kr')

#data plot
plot(mydata2$avginc, mydata2$testscr)


#(p.11)
#linear
regres1 <- lm(formula = testscr ~ avginc, data=mydata2)
summary(regres1)

#nonlinear (quadratic)
mydata2$avginc_sq = mydata2$avginc * mydata2$avginc
regres2 <- lm(formula = testscr ~ avginc + avginc_sq, data = mydata2)
summary(regres2)


#(p.13)
#data plot
plot(mydata2$avginc, mydata2$testscr, xlab="Average Income", ylab="Average Test Score")
#regression line for linear model
abline( a=regres1$coefficients[1], b = regres1$coefficients[2], col="blue", lwd=3)
#estimated points for nonlinear model
points(mydata2$avginc, regres2$fitted.values, col="Red")


################################################
#logarithmic function
################################################
#(p.17)
#read firm data
mydata1 <- read.csv("sample_data_1_it_firm.csv", fileEncoding='euc-kr')


#(p.19)
regres3 <- lm(exp(SALES) ~ IT.STOCK, data=mydata1)
summary(regres3)


#(p.22)
regres4 <- lm(SALES ~ exp(IT.STOCK), data=mydata1)
summary(regres4)


#(p.25)
regres5 <- lm(SALES ~ IT.STOCK, data=mydata1)
summary(regres5)


#(p.30)
#pic1
plot(mydata1$IT.STOCK, exp(mydata1$SALES))
abline(a=regres3$coefficients[1], b=regres3$coefficients[2], col="red", lwd=3)
#pic2
plot(exp(mydata1$IT.STOCK), mydata1$SALES)
abline(a=regres4$coefficients[1], b=regres4$coefficients[2], col="red", lwd=3)
#pic3
plot(mydata1$IT.STOCK, mydata1$SALES)
abline(a=regres5$coefficients[1], b=regres5$coefficients[2], col="red", lwd=3)


#(p.31)
regres6 <- lm(exp(SALES) ~ exp(IT.STOCK), data=mydata1)
summary(regres6)
plot(exp(mydata1$IT.STOCK), exp(mydata1$SALES))
abline(a=regres6$coefficients[1], b=regres6$coefficients[2], col="red", lwd=3)


#(p.34)
regres7 <- lm(SALES ~ IT.STOCK + LABOUR + CAPITAL + IT.LABOUR, data=mydata1)
summary(regres7)

################################################
#interaction term
################################################
#(p.40)
#check descriptive statistics
summary(mydata2)
#about bottom 25%
mydata2$smallclass = ifelse(mydata2$str<19,1,0)

#about bottom 25% 
mydata2$lowinc = ifelse(mydata2$avginc<10,1,0)

#(p.41)
#basic model
regres8 <- lm(testscr ~ smallclass + lowinc, data=mydata2)
summary(regres8)

#with interaction
regres9 <- lm(testscr ~ smallclass + lowinc + smallclass:lowinc, data=mydata2)
summary(regres9)

#(p.46)
#basic model
regres10 <- lm(testscr ~ str + avginc, data=mydata2)
summary(regres10)

#with interaction
regres11 <- lm(testscr ~ str + avginc + str:avginc, data=mydata2)
summary(regres11)

#(p.51)
#basic model
regres12 <- lm(testscr ~ str + lowinc, data=mydata2)
summary(regres12)

#with interaction
regres13 <- lm(testscr ~ str + lowinc + str:lowinc, data=mydata2)
summary(regres13)

#(p.54)
#extension
regres14 <- lm(testscr ~ str + str:lowinc, data=mydata2)
summary(regres14)

#(p.56)
#first row
plot(mydata2$str, mydata2$testscr, col=factor(mydata2$lowinc))
abline(a = regres12$coefficients[1], b=  regres12$coefficients[2], col="blue", lwd=3)
abline(a = regres12$coefficients[1] + regres12$coefficients[3], b=  regres12$coefficients[2], col="red", lwd=3)

plot(mydata2$str, mydata2$testscr, col=factor(mydata2$lowinc))
abline(a = regres13$coefficients[1], b=  regres13$coefficients[2], col="blue", lwd=3)
abline(a = regres13$coefficients[1] + regres13$coefficients[3], b=  regres13$coefficients[2] + regres13$coefficients[4], col="red", lwd=3)

plot(mydata2$str, mydata2$testscr, col=factor(mydata2$lowinc))
abline(a = regres14$coefficients[1], b=  regres14$coefficients[2], col="blue", lwd=3)
abline(a = regres14$coefficients[1], b=  regres14$coefficients[2] + regres14$coefficients[3], col="red", lwd=3)

#second row
plot(mydata2$str, mydata2$testscr, col=factor(mydata2$lowinc), xlim=c(0,26), ylim=c(0,800))
abline(a = regres12$coefficients[1], b=  regres12$coefficients[2], col="blue", lwd=3)
abline(a = regres12$coefficients[1] + regres12$coefficients[3], b=  regres12$coefficients[2], col="red", lwd=3)

plot(mydata2$str, mydata2$testscr, col=factor(mydata2$lowinc), xlim=c(0,26), ylim=c(0,800))
abline(a = regres13$coefficients[1], b=  regres13$coefficients[2], col="blue", lwd=3)
abline(a = regres13$coefficients[1] + regres13$coefficients[3], b=  regres13$coefficients[2] + regres13$coefficients[4], col="red", lwd=3)

plot(mydata2$str, mydata2$testscr, col=factor(mydata2$lowinc), xlim=c(0,26), ylim=c(0,800))
abline(a = regres14$coefficients[1], b=  regres14$coefficients[2], col="blue", lwd=3)
abline(a = regres14$coefficients[1], b=  regres14$coefficients[2] + regres14$coefficients[3], col="red", lwd=3)

################################################
#making prediction
################################################
#(p.60)
#regres5 <- lm(SALES ~ IT.STOCK, data=mydata1)
#predict for a given value
predict(regres5, newdata = data.frame(IT.STOCK=7), interval="confidence")
#predict for a list of values
predict(regres5, newdata = data.frame(IT.STOCK=c(7,8,9,10,11)), interval="confidence")
#predict for the original value
predict(regres5, newdata = data.frame(IT.STOCK=log(50000)), interval="confidence")


#(p.61)
#regres7 <- lm(SALES ~ IT.STOCK + LABOUR + CAPITAL + IT.LABOUR, data=mydata1)
#give all values
predict(regres7, newdata = data.frame(IT.STOCK=5, LABOUR=10, CAPITAL=10, IT.LABOUR=0.5), interval="confidence")

#read new data
pdata <- read.csv("sample_data_1_it_firm_add.csv")
#predict with new data
predict(regres7, newdata = pdata, interval="confidence")


