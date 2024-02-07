#(p.29)
install.packages("Benchmarking")
library(Benchmarking)

#read data
mydata4 <- read.csv("sample_data_4_dea.csv", fileEncoding='euc-kr')



#(p.30)
#help text for function dea()
?dea()
#make input output 
input1 = mydata4['Labor']
output1 = mydata4['Revenue']

#input-based CCR: 1 input - 1 output
deares1 <- dea(X=input1, Y=output1, SLACK = TRUE, ORIENTATION = "in", RTS = "crs")



#(p.31)
#show efficiency range summary
summary(deares1)



#(p.32)
#efficiency (theta)
deares1$eff

#whether there is slack
deares1$slack

#slack for input 
deares1$sx

#slack for input 
deares1$sy

#reference set 
deares1$lambda



#(p.33)
#total df
df1 <- cbind(theta = deares1$eff, slack = deares1$slack, deares1$sx, deares1$lambda)
#write csv file
write.csv(df1,"myresult.csv")



#(p.34)
#visualize (1 input, 1 output) - CRS
dea.plot.frontier(x=mydata4['Labor'], y=mydata4['Revenue'], RTS="crs", txt=T)

#visualize (1 input, 1 output) - in case of VRS
dea.plot.frontier(x=mydata4['Labor'], y=mydata4['Revenue'], RTS="vrs", txt=T)



#(p.35)
#input-based CCR: 2 input -1 output  
input2 = mydata4[ , c("Labor", "Capital")]
output2 = mydata4['Revenue']
deares2 <- dea(X=input2, Y=output2, SLACK = TRUE, ORIENTATION = "in", RTS = "crs")

#efficiency (theta)
deares2$eff
#whether there is slack
deares2$slack
#slack for input 
deares2$sx
#slack for output 
deares2$sy
#reference set 
deares2$lambda
#total df
df2 <- cbind(theta = deares2$eff, slack = deares2$slack, sx = deares2$sx, sy = deares2$sy,  deares2$lambda)
#write csv file
write.csv(df2,"myresult2.csv")



#(p.37)
#visualize (2 input) - wrong!
dea.plot.isoquant(x1=mydata4['Labor'], x2=mydata4['Capital'], RTS="crs", txt=T)



#(p.38)
#normalize value
mydata4$Labor_n = mydata4$Labor/mydata4$Revenue
mydata4$Capital_n = mydata4$Capital/mydata4$Revenue
dea.plot.isoquant(x1=mydata4['Labor_n'], x2=mydata4['Capital_n'], RTS="crs", txt=T)



#(p.39)
#2 input-2 output:  input-based CCR
input3 = mydata4[,c("Labor", "Capital")]
output3 = mydata4[,c('Revenue', "Patent")]
deares3 <- dea(X=input3, Y=output3, SLACK = TRUE, ORIENTATION = "in", RTS = "crs")

#total df
df3 <- cbind(theta = deares3$eff, slack = deares3$slack, sx = deares3$sx, sy = deares3$sy, deares3$lambda)
#write csv file
write.csv(df3,"myresult3.csv")



########################################
#(p.53)
#output-based CCR: 1 input-1 output 
input4 = mydata4["Labor"]
output4 = mydata4['Revenue']
deares4 <- dea(X=input4, Y=output4, SLACK = TRUE, ORIENTATION = "out", RTS = "crs")

#total df
df4 <- cbind(phi = deares4$eff, slack = deares4$slack, sx = deares4$sx, sy = deares4$sy, deares4$lambda)
#write csv file
write.csv(df4,"myresult4.csv")



#(p.54)
1/deares4$eff
deares1$eff

dfcompare = cbind(theta= deares1$eff, phi = 1/deares4$eff)
dfcompare


#(p.55)
dea.plot.frontier(x=mydata4['Labor'], y=mydata4['Revenue'], RTS="crs", txt=T)



#(p.56)
#output-based CCR: 1 input-2 output 
input5 = mydata4["Labor"]
output5 = mydata4[,c('Revenue', "Patent")]
deares5 <- dea(X=input5, Y=output5, SLACK = TRUE, ORIENTATION = "out", RTS = "crs")

#total df
df5 <- cbind(phi = deares5$eff, slack = deares5$slack, sx = deares5$sx, sy = deares5$sy, deares5$lambda)
#write csv file
write.csv(df5,"myresult5.csv")



#(p.57)
#plot? - wrong!
dea.plot.transform(y1=mydata4['Revenue'], y2=mydata4['Patent'], RTS="crs", txt=T)

#normalize
mydata4$Revenue_n = mydata4$Revenue/mydata4$Labor
mydata4$Patent_n = mydata4$Patent/mydata4$Labor
#then plot
dea.plot.transform(y1=mydata4['Revenue_n'], y2=mydata4['Patent_n'], RTS="crs", txt=T)



#(p.59)
#output-based CCR: 2 input-2 output 
input6 = mydata4[,c("Labor", "Capital")]
output6 = mydata4[,c('Revenue', "Patent")]
deares6 <- dea(X=input6, Y=output6, SLACK = TRUE, ORIENTATION = "out", RTS = "crs")

#total df
df6 <- cbind(phi = deares6$eff, slack = deares6$slack, sx = deares6$sx, sy = deares6$sy, deares6$lambda)
#write csv file
write.csv(df6,"myresult6.csv")

