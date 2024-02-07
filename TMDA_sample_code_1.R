#��� ����
setwd("C:/r_workspace/econometric_analysis")
#���� �ҷ�����
mydata <- read.csv("sample_data_1_it_firm.csv", fileEncoding='euc-kr')
#������ �׷�����
plot(SALES ~ IT.STOCK, data=mydata)
#������� �м�
# cor �Լ��� �̷��� column���� �񱳵� ������ �׳� DF ��ü�� ���뵵 ����
# DF�� ����ÿ� �� ���������� ������踦 �ٷ� ��Ʈ������ ���
cor(mydata$SALES, mydata$IT.STOCK)
cor11 <-  cor(mydata)
class(cor11)



# ����ȸ�Ϳ� lm(linear model) �Լ�:  ���� 2��
# formula = ��(���Ӻ���) ~ ��(��������)
# MLR�ÿ��� ��(��������)�� +�� �������� ���������� �̾ �߰����ش�.
# data = ���״�� ����� DF �ҷ�����

# summary�Լ�: call, residuals, coefficients, ������ǥ( RMSE, M R���� ���)

#regresult�� ȸ�ͺм� ��� ���� �� ����� ���
regresult <- lm(formula = SALES ~ IT.STOCK, data=mydata)
summary(regresult)
class(regresult) # �ڷ��� ��ü�� lm�̹Ƿ� ����

#regresult�� ȸ�ͺм� ����� �������� �ŷڱ��� ���
#�ŷڼ��� ���� ���� (95%, 99%, 90%), default=95%
confint(regresult, level = 0.95)
confint(regresult, level = 0.99)
confint(regresult, level = 0.90)
# confint �Լ� : ���� 2��
# lm �ڷ����� ������ / level: �ŷڼ��� ����



#�츮�� ȸ�ͺм� ����� ������ regresult �ȿ� ����Ǿ� ����
# ������ data �̸����� �˾Ƶ���!!
#����
regresult$residuals
#����ġ
regresult$fitted.values
#���� X, Y, ������ Y, ������ �����ϴ� dataframe�� ������
# cbind :  column(��)���� / rbind : row(��)���� / merge: key ���� �������� column ����/����� �پ�
newdf <- cbind(mydata$IT.STOCK, mydata$SALES, regresult$fitted.values, regresult$residuals)
#Į���� ���� �� ���������������� ���� ���� (������ array, matrix �����̴�)
colnames(newdf) <- c("obs x", "obs y", "fitted y", "residual")
newdf <- data.frame(newdf)

#abline: ���� �׸��� �Լ�
abline(a= 14.41633, b= 0.76529) #summary������ estimated Intercept(y����), Estimated IT.stock(����)


#�ռ� ���� newdf�� �ִ� ���� �� y ����ġ�� �̿��Ͽ� rss �� tss ��� 
rss <- sum(newdf$residual^2)
rss #���� ������ �����(aka SSR)
tss <- sum((newdf$obs.y - mean(newdf$obs.y))^2)
tss # ���� ������ ����� (aka TSS)
#R-squared
Rsq <- 1 - rss/tss
Rsq
#adjusted R-squared
AdRsq <- 1 - ((rss/tss)*504/503)
AdRsq
#RMSE
rmse <- sqrt(rss/505)
rmse


#########################�Ʒ��� ��Ÿ �ڵ� (�����ڷ� 1 �ð�ȭ)

install.packages("corrplot")
library(corrplot)

#mtcars�� �⺻ �������� (���� ���� �ƴ�)
mcor <-cor(mtcars)

corrplot(mcor)

corrplot(cor(mydata))
