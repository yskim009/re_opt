##########################
#p.23
install.packages("stm")
install.packages("stringr")
install.packages("stopwords")
library(stm)
library(stringr)
library(stopwords)

#p.24
mydata5 <- read.csv("sample_data_5_hydrogenproduction.csv", fileEncoding='euc-kr')

#make text data of concern
#data with title + abstract
mydata5$text = paste(mydata5$Title, " ", mydata5$Abstract)

#data with title + abstract + claims
mydata5$text_wc = paste(mydata5$Title, " ", mydata5$Abstract, " ", mydata5$Claims)

#data with title + abstract with double weight on title
mydata5$text_tt = paste(mydata5$Title, " ", mydata5$Title, " ", mydata5$Abstract)



#p.25
#replace 
mydata5$text <- str_replace_all(mydata5$text, '-', ' a ')
mydata5$text <- str_replace_all(mydata5$text, '_', ' a ')

#stopwords from the stopword library
stwds <- stopwords(language = "en", source = "smart")
#custom library 
custom = c('invention', 'thereof', 'therefore', 'therefrom')
#merge stopwords
csw <- c(stwds, custom)



#p.27
#textProcessor
mypreprocess <- textProcessor(mydata5$text, metadata = mydata5
                              , lowercase = TRUE
                              , removepunctuation = TRUE
                              , customstopwords = csw
                              , removestopwords = TRUE
                              , removenumbers = TRUE
                              , stem = TRUE
                              , wordLengths = c(2,Inf))

#prepDocuments
myout <-prepDocuments(mypreprocess$documents,
                      mypreprocess$vocab, mypreprocess$meta,
                      lower.thresh = 22)



#p.28
myout$vocab



################################################################################
################################################################################
#p.29
#textProcessor
mypreprocess <- textProcessor(mydata5$text, metadata = mydata5
                              , lowercase = TRUE
                              , removepunctuation = TRUE
                              , customstopwords = csw
                              , removestopwords = TRUE
                              , removenumbers = FALSE
                              , stem = TRUE
                              , wordLengths = c(2,Inf))

#prepDocuments
myout <-prepDocuments(mypreprocess$documents,
                      mypreprocess$vocab, mypreprocess$meta,
                      lower.thresh = 22)

myout$vocab
################################################################################
################################################################################



################################################################################
################################################################################
#p.30
#additional custom list
custom_add = c('10', '100', '11', '12', '14', '16', '20', '40', '50', 'ii')
#merge stopwords
csw <- c(csw, custom_add)

#textProcessor
mypreprocess <- textProcessor(mydata5$text, metadata = mydata5
                              , lowercase = TRUE
                              , removepunctuation = TRUE
                              , customstopwords = csw
                              , removestopwords = TRUE
                              , removenumbers = FALSE
                              , stem = TRUE
                              , wordLengths = c(2,Inf))

#prepDocuments
myout <-prepDocuments(mypreprocess$documents,
                      mypreprocess$vocab, mypreprocess$meta,
                      lower.thresh = 22)

myout$vocab
################################################################################
################################################################################



#p.32
#make new dummy variable
myout$meta$KR <- ifelse(myout$meta$Current.Assignee.Country == 'KR', 1, 0)

#set number of topics for stm
kno = 4

#STM
mystm <- stm(myout$documents, myout$vocab, data=myout$meta,
             K=kno,
             prevalence = ~ Publication.Year + KR ,
             seed = 16)



#p.34, 36
labelTopics(mystm, topics=1:kno, n=7)



#p.37
plot(mystm, type = "summary", labeltype = "prob", text.cex = 1)
plot(mystm, type = "labels", labeltype = "prob", text.cex = 1)




################################################################################
################################################################################
#p.39
K<-c(5,10,15,20,25,30,35,40,45,50) 
kresult <- searchK(myout$documents, myout$vocab, data=myout$meta,
                   K, 
                   prevalence = ~ Publication.Year + KR,
                   seed = 16)

plot(kresult)
################################################################################
################################################################################



################################################################################
################################################################################
#p.41
kno = 25

#STM
mystm <- stm(myout$documents, myout$vocab, data=myout$meta,
             K=kno,
             prevalence = ~ Publication.Year + KR ,
             seed = 16)

plot(mystm, type = "summary", text.cex = 1)



#p.42
labelTopics(mystm, topics=1:kno, n=7)



#p.43
myresult <- estimateEffect(c(1:kno) ~ Publication.Year + KR, mystm, myout$meta)
summary(myresult)



#p.46
findTopic(mystm, c('fuel','cell'), n=10)
findTopic(mystm, c('electrolysi'), n=10)

toi = 14
findThoughts(mystm, texts = myout$meta$Title, topics= toi, n=10)
findThoughts(mystm, texts = myout$meta$Abstract, topics= toi, n=10)
findThoughts(mystm, texts = myout$meta$CPC, topics= toi, n=10)



#p.48
#topic composition
sink(file='mytopics.txt')
labelTopics(mystm, topics=1:kno, n=10)
sink()

#postestimation results
sink(file='myeffects.txt')
summary(myresult)
sink()



#p.49
#WRITE CSV (BETA, THETA)
#logbeta
logbeta_export <- mystm[["beta"]][["logbeta"]][[1]]
logbeta_export <- as.data.frame(logbeta_export)
colnames(logbeta_export) <- myout$vocab
write.csv(logbeta_export, file= 'logbeta.csv', row.names=TRUE)

#beta
logbeta <- as.data.frame(mystm[["beta"]][["logbeta"]][[1]])
beta <- as.data.frame(exp(logbeta))
colnames(beta) <- myout$vocab
write.csv(beta, file = 'beta.csv', row.names=TRUE)

#theta
theta <- as.data.frame(mystm[["theta"]])
theta <- cbind(theta, mydata5["Title"], mydata5["Publication.Number"], mydata5["Publication.Year"], mydata5["Current.Assignee"], mydata5["Current.Assignee.Country"])
write.csv(theta, file = 'theta.csv', row.names=TRUE)



#p.50
#calculate distance 
dd <- dist(scale(logbeta), method = "euclidean")

#make cluster dendrogram
hc <- hclust(dd, method = "ward.D2")
plot(hc)
plot(hc, hang = -1)




#p.51
#make interactive plot
toLDAvis(mystm, myout$documents)



