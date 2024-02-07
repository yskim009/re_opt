#(p.44)
setwd("C:/r_workspace/econometric_analysis")
#read data
mydata3 <- read.csv("sample_data_3_pd_country.csv", fileEncoding='euc-kr')
#(p.52)
#install sem package
#LAtent VAriable ANalysis 
##install.packages("lavaan")
library(lavaan)
##install.packages("tidyverse")
library(tidyverse)
##install.packages("dplyr")
library(dplyr)
#political democracy data is available in R

mydata3 <- PoliticalDemocracy
?PoliticalDemocracy

#(p.54)
#measurement model
mycfa1 <-"#measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8"

#fit model
fit1 <- cfa(model=mycfa1, data=mydata3)
summary(fit1)


#(p.56)
#see descriptive statistics
summary(mydata3)
#show standardized solution
fit1 %>%
  standardizedSolution()
#same
filter(standardizedSolution(fit1))


#(p.57)
#show standardized factor loading
fit1 %>%
  standardizedSolution() %>%
  filter(op == "=~")
#same
filter(standardizedSolution(fit1), op=="=~")


#(p.58)
#residual (correlation) matrix
residuals(fit1, type="cor")$cov


#(p.60)
#show all fitmeasures
fit1 %>%
  fitMeasures()

#show some fit measures 
fit1 %>%
  fitMeasures(c("chisq","pvalue", "rmsea", "gfi", "cfi"))


#(p.61)
#revised measurement model
mycfa2 <-"#measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
y1 ~~ y5
y2 ~~ y6
y3 ~~ y7
y4 ~~ y8
"

#fit model
fit2 <- cfa(model=mycfa2, data=mydata3)


#(p.62)
#residual (correlation) matrix
residuals(fit2, type="cor")$cov

#show some fit measures
fit2 %>%
  fitMeasures(c("chisq","pvalue", "rmsea", "gfi", "cfi"))


#(p.63)
#modindices
#show all cases
modindices(fit2)
#sort
modindices(fit2, sort. = TRUE)
#sort & show mi over 3
modindices(fit2, sort. = TRUE, minimum.value = 3)


#(p.65)
#final measurement model
mycfa3 <-"#measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
y1 ~~ y5
y2 ~~ y6 
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
y2 ~~ y4
"

#alternative form
mycfa3 <-"#measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
y1 ~~ y5
y2 ~~ y6 + y4
y3 ~~ y7 
y4 ~~ y8
y6 ~~ y8
"

#fit model
fit3 <- cfa(model=mycfa3, data=mydata3)


#(p.66)
#residual (correlation) matrix
residuals(fit3, type="cor")$cov

#show some fit measures
fit3 %>%
  fitMeasures(c("chisq","pvalue", "rmsea", "gfi", "cfi"))


#(p.67)
#show standardized factor loading
fit3 %>%
  standardizedSolution() %>%
  filter(op == "=~")


#(p.68)
#final model
mymodel <-"# mesaurement model
ind 60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
#regression
dem60 ~ ind60
dem65 ~ ind60 + dem60
#correlation 
y1 ~~ y5
y2 ~~ y6 + y4
y3 ~~ y7 
y4 ~~ y8
y6 ~~ y8
"


#(p.69)
#fit model
sem1 <- sem(model=mymodel, data=mydata3)

#show factor loading
sem1 %>%
  standardizedSolution() %>%
  filter(op == "=~")

#residual (correlation) matrix
residuals(sem1, type="cor")$cov

#show fit measures
sem1 %>%
  fitMeasures(c("chisq","pvalue", "rmsea", "gfi", "cfi"))


#(p.70)
#show regression result
sem1 %>%
  standardizedSolution() %>%
  filter(op == "~")


#(p.73)
#install plotting package
install.packages("semPlot")
library(semPlot)

#draw plot
semPaths(sem1, what="std", layout="tree2", edg.label.cex=1, edge.color="blue",
         color=list(lat="red", man="green"), fade=FALSE,
         style="lisrel", curvature=2)


#(p.74)
#show standardized result
sem1 %>%
  standardizedSolution()
