
#######------Part 1 Time Series Analysis----------####
#personal consumption expenditure file
pce <- read.csv("pce.csv", header = TRUE)

library(forecast)
library(ggplot2)
library(imputeTS)

# converting dataframe into time series object - personal consumption expenditure time series (pcets)
pcets <- ts(pce$PCE, start = c(1959, 1), end = c(2021, 12), frequency = 12)

# plotting the initial time series data and checking if there are any missing values
plot(pcets, main = "PCE")
statsNA(pce$PCE)
ggplot_na_distribution(pce$PCE) #visualizing missing observations

# Handle missing values
pcets1 <- na_interpolation(pcets)
pcets2 <- na_ma(pcets, k =4, weighting = "exponential")
pcets3 <- na_kalman(pcets)
pcets4 <- na_kalman(pcets, model = "auto.arima")
statsNA(pcets4)
#
pceNew<- cbind(pcets,pcets1,pcets2, pcets3, pcets4)

#
par(mfrow=c(2,2))
plot(pcets1)
plot(pcets2)
plot(pcets3)
plot(pcets4)


#
par(mfrow=c(1,1))
seasonplot(pcets4) #no seasonality observed, hence no need to use additive/multiplicative

ggseasonplot(pcets4)

# detrend
mapcets4 <- ma(pcets4, 5) # 4 obersvations are lost

plot(mapcets4)

plot(pcets4)
lines(mapcets4, col= "blue", lwd = 1)

detrendpce <- pcets4 - mapcets4
plot(detrendpce)

#additive #alternate approach
decomposepce <- decompose(pcets4, type = "additive")
plot(decomposepce)

plot(pcets4)
lines(decomposepce$trend, col =2)
lines(decomposepce$seasonal, col = 4)

#--------- Best model for forecasting-----------

#creating training and testing data (85:15 ratio)
pcets4train <- window (pcets4, end= c(2012, 6)) #642 observations
pcets4test <- window(pcets4, start=c(2012, 7)) #114 observations

#----------Simple Forecasting Techniques--------
averageforecast <- meanf(pcets4train, h = 114)
naiveforecast <- naive(pcets4train, h = 114)
simplenaiveforecast <- snaive(pcets4train, h = 114)
driftforecast <- rwf(pcets4train, drift = TRUE, h = 114)

#testing accuracy for simple forecasting techniques
accuracy(averageforecast, pcets4test)
accuracy(naiveforecast, pcets4test)
accuracy(simplenaiveforecast, pcets4test)
accuracy(driftforecast, pcets4test) #minimum RMSE, ME, MAE, MPE, MAPE, MASE

autoplot(pcets4) +
  autolayer(averageforecast, series = "Average Forecast") + 
  autolayer(naiveforecast, series = "Naive Forecast") + 
  autolayer(simplenaiveforecast, series = "Simple Naive Forecast") + 
  autolayer(driftforecast, series = "Drift Forecast") +
  guides(colour = guide_legend("Model"))

#---------Exponential Smoothing Techniques-------

etsAAN <- ets(pcets4train, model = "AAN") #used ets AAN on trained data set
etsforecast <- forecast(etsAAN, h =114)
accuracy(etsforecast, pcets4test) #accuracy check with ets h = 114 and test data set

#---------ARIMA Model-----------------------------

autoplot(pcets4)

library(tseries)
adf.test(x = diff(diff(pcets4train)), alternative = "stationary", k = 12) 
tsdisplay(pcets4train)
tsdisplay(diff(diff(pcets4train)))

adf.test(x = pcets4train, alternative = "stationary", k = 12)
adf.test(x = diff(pcets4train), alternative = "stationary", k = 12)

# p = 0, d = 2, q = 1

arima <- arima(pcets4train, order = c(0,2,1))
arimaforecast <- forecast(arima, h = 114)
checkresiduals(arimaforecast)

#alternate approach using automated function
Autoarima <- auto.arima(pcets4train)
Autoarimaforecast <- forecast(Autoarima, h = 114)
checkresiduals(Autoarimaforecast)

#---------Plot all model in one graph----------
autoplot(pcets4) +
  autolayer(driftforecast, series = "drift forecast") + 
  autolayer(etsforecast, series = "ETS forecast") + 
  autolayer(arimaforecast, series = "ARIMA forecast") + 
  guides(colour = guide_legend("Model"))

#------------Best-Fit Model Accuracy Check------
accuracy(driftforecast, pcets4test) #accuracy check for drift 
accuracy(etsforecast, pcets4test) #accuracy check for ets model
accuracy(arimaforecast, pcets4test) #accuracy check for arima model #best-fit model

#---------Prediction for October'2022-------
PC <- arima(pcets4, order = c(0,2,1))
PCEOctober2022 <- forecast(PC, h = 10)
autoplot(pcets4) +
  autolayer(PCEOctober2022, col = 13)

#----------One Step Rolling without re-estimation of parameters-----
library(fpp)
#ARIMA
onesteparima<- auto.arima(pcets4train)
onesteparimarefit<- Arima(pcets4, model=onesteparima)
onestepforecastarima<- window(fitted(onesteparimarefit), start=c(2012, 7))

#Drift
onestepdrift <- rwf(pcets4train, drift = TRUE)
onestepdriftfit <- rwf(pcets4, drift = TRUE, model=onestepdrift)
onestepforecastdrift <- window(fitted(onestepdriftfit), start=c(2012, 7))

#ETS
onestepets <- holt(pcets4train)
onestepetsfit <- holt(pcets4, model=onestepets)
onestepforecastets <- window(fitted(onestepetsfit), start=c(2012, 7))

#--------------Accuracy Check--------------
accuracy(onestepforecastarima, pcets4test)
accuracy(onestepforecastdrift, pcets4test)
accuracy(onestepforecastets, pcets4test)

#-------Plot one-step model in one graph-------

autoplot(pcets4) +
  autolayer(onestepforecastarima, series = "One Step ARIMA forecast") + 
  autolayer(onestepforecastets, series = "One Step ETS forecast") + 
  autolayer(onestepforecastdrift, series = "One Step DRIFT forecast") + 
  guides(colour = guide_legend("Model"))


#######------Part 2 Topic Modelling----------####

rm(list=ls())

library(tidyverse)
library(tidyr)
library(stringr)
library(tokenizers)
library(textstem)
library(dplyr)
library(jsonlite)
library(tm) # package for text mining package
library(wordcloud) 
library(topicmodels) 
library(ggplot2) 
library(LDAvis)


df <- fromJSON("http://query.data.world/s/4ria2tfww73wmhfzke5z2w4zlez2re")

#--------Setting seed at 928-------
set.seed(928)
amazon <- sample_n(df, 5000)

#------Selecting review rating and text columns-----
amazonreviews <- amazon %>%
  select(review_rating, review_text)


#------Segregation of data into two data sets - positive and negative reviews ------
amazonreviews$review_scale <- gsub("([A-Za-z]+).*", "\\2", amazonreviews$review_rating)
amazonreviews$review_scale <- trimws(amazonreviews$review_scale, which = c("right"))
amazonreviews$review_scale <- as.factor(amazonreviews$review_scale)

review_pos <- amazonreviews %>%
  filter(review_scale== "4.0" | review_scale== "5.0")

review_neg <- amazonreviews %>%
  filter(review_scale== "1.0" | review_scale== "2.0")

review_pos1 <- stringr::str_conv(review_pos$review_text, "UTF-8")

review_neg1 <- stringr::str_conv(review_neg$review_text, "UTF-8")

#-----------Removing Emojis-------------

posrev1 <- gsub("[^\x01-\x7F]", "", review_pos1)

negrev1 <- gsub("[^\x01-\x7F]", "", review_neg1)

#--converting into Corpus---------------

posreview <- Corpus(VectorSource(posrev1))

negreview <- Corpus(VectorSource(negrev1))

#---Topic Modelling on positive reviews------

#-----Tokenization------

#positive
review_postoken <- tokenize_words(posreview$content)
print(review_postoken[3])

#negative
review_negtoken <- tokenize_words(negreview$content)
print(review_negtoken[16])

#----Lemmatization-----
#positive
postext_lemma <- tm_map(posreview, lemmatize_strings) 
poswords_lemma <- tokenize_words(postext_lemma$content) 
print(poswords_lemma[15])

#negative
negtext_lemma <- tm_map(negreview, lemmatize_strings) 
negwords_lemma <- tokenize_words(negtext_lemma$content) 
print(negwords_lemma[7])

#---Document Term Matrix----
#positive
posreviewDTM <- DocumentTermMatrix(posreview,
                                   control = list(lemma=TRUE,
                                                  removePunctuation=TRUE,
                                                  removeNumbers=TRUE,
                                                  stopwords=TRUE,
                                                  tolower=TRUE,
                                                  stripWhitespace=TRUE))

posreviewDTM

#negative
negreviewDTM <- DocumentTermMatrix(negreview,
                                   control = list(lemma=TRUE,
                                                  removePunctuation=TRUE,
                                                  removeNumbers=TRUE,
                                                  stopwords=TRUE,
                                                  tolower=TRUE,
                                                  stripWhitespace=TRUE))

negreviewDTMs 


#--removing rows with zero term count (no dimensions)----
#positive
raw.sum=apply(posreviewDTM, 1, FUN = sum)
posreviewDTM=posreviewDTM[raw.sum!=0,]

posreviewDTM

#negative
raw.sum=apply(negreviewDTM, 1, FUN = sum)
negreviewDTM=negreviewDTM[raw.sum!=0,]

negreviewDTM

#-----Frequency of terms-----
#positive
posfrequency <- as.matrix(posreviewDTM)
posfrequency1 <- colSums(posfrequency)
posfrequency1 <- sort(posfrequency1, decreasing = TRUE)
poslength <- rowSums(posfrequency)

posfrequency1[1:10]  #check

findFreqTerms(posreviewDTM, 50) #check

#negative
negfrequency <- as.matrix(negreviewDTM)
negfrequency1 <- colSums(negfrequency)
negfrequency1 <- sort(negfrequency1, decreasing = TRUE)
neglength <- rowSums(negfrequency)

negfrequency1[1:10]  #check

findFreqTerms(negreviewDTM, 50) #check

#-----------------Word Cloud---------------
#positive
words <- names(posfrequency1)

wordcloud(words[1:100], posfrequency1[1:100], rot.per = 0.15,
          random.order = FALSE, scale = c(5, 0.8),
          random.color = FALSE, colors = brewer.pal(8, "Dark2"))

#negative
words1 <- names(negfrequency1)

wordcloud(words1[1:100], negfrequency1[1:100], rot.per = 0.15,
          random.order = FALSE, scale = c(5, 0.8),
          random.color = FALSE, colors = brewer.pal(8, "Dark2"))

#----------------Determining no of topics----
#positive------------------------------------
iter <- 2000

#check number of topics for range 5-20
poscoherence <- c()

for( i in (5:20)){
  posLDA <- LDA(posreviewDTM, i, method = "Gibbs",
                control = list(iter=iter, seed = 1000))
  posphi <- posterior(posLDA)$terms %>% as.matrix
  postheta <- posterior(posLDA)$topics %>% as.matrix
  coherence_one <- mean(textmineR::CalcProbCoherence(phi = posphi,
                                                     dtm = posfrequency))
  poscoherence <- append(poscoherence, coherence_one)
}

posk <- c(5:20)[which.max(poscoherence)]
print(posk)

#check number of topics for range 5-15
pos1coherence <- c()
for( i in (5:15)){
  pos1LDA <- LDA(posreviewDTM, i, method = "Gibbs",
                 control = list(iter=iter, seed = 1000))
  pos1phi <- posterior(pos1LDA)$terms %>% as.matrix
  pos1theta <- posterior(pos1LDA)$topics %>% as.matrix
  coherence1_one <- mean(textmineR::CalcProbCoherence(phi = pos1phi,
                                                      dtm = posfrequency))
  pos1coherence <- append(pos1coherence, coherence1_one)
}

pos1k <- c(5:15)[which.max(pos1coherence)]
print(pos1k)

#check number of topics for range 5-25
pos2coherence <- c()
for( i in (5:25)){
  pos2LDA <- LDA(posreviewDTM, i, method = "Gibbs",
                 control = list(iter=iter, seed = 1000))
  pos2phi <- posterior(pos2LDA)$terms %>% as.matrix
  pos2theta <- posterior(pos2LDA)$topics %>% as.matrix
  coherence2_one <- mean(textmineR::CalcProbCoherence(phi = pos2phi,
                                                      dtm = posfrequency))
  pos2coherence <- append(pos2coherence, coherence2_one)
}

pos2k <- c(5:25)[which.max(pos2coherence)]
print(pos2k)



#negative----------------------------
iter <- 2000

#check no of topics for range 5-20
negcoherence <- c()

for( i in (5:20)){
  negLDA <- LDA(negreviewDTM, i, method = "Gibbs",
                control = list(iter=iter, seed = 1000))
  negphi <- posterior(negLDA)$terms %>% as.matrix
  negtheta <- posterior(negLDA)$topics %>% as.matrix
  coherence_two <- mean(textmineR::CalcProbCoherence(phi = negphi,
                                                     dtm = negfrequency))
  negcoherence <- append(negcoherence, coherence_two)
}

negk <- c(5:20)[which.max(negcoherence)]
print(negk)


#check no of topics for range 5-15
neg1coherence <- c()

for( i in (5:15)){
  neg1LDA <- LDA(negreviewDTM, i, method = "Gibbs",
                 control = list(iter=iter, seed = 1000))
  neg1phi <- posterior(neg1LDA)$terms %>% as.matrix
  neg1theta <- posterior(neg1LDA)$topics %>% as.matrix
  coherence1_two <- mean(textmineR::CalcProbCoherence(phi = neg1phi,
                                                      dtm = negfrequency))
  neg1coherence <- append(neg1coherence, coherence1_two)
}

neg1k <- c(5:15)[which.max(neg1coherence)]
print(neg1k)


#check no of topics for range 5-25
neg2coherence <- c()

for( i in (5:25)){
  neg2LDA <- LDA(negreviewDTM, i, method = "Gibbs",
                 control = list(iter=iter, seed = 1000))
  neg2phi <- posterior(neg2LDA)$terms %>% as.matrix
  neg2theta <- posterior(neg2LDA)$topics %>% as.matrix
  coherence2_two <- mean(textmineR::CalcProbCoherence(phi = neg2phi,
                                                      dtm = negfrequency))
  neg2coherence <- append(neg2coherence, coherence2_two)
}

neg2k <- c(5:25)[which.max(neg2coherence)]
print(neg2k)




#--------------Coherence Plot--------------------
#positive
poscoherence_mat <- data.frame(posk = c(5:20), coherence = poscoherence,
                               stringsAsFactors = FALSE)

ggplot(poscoherence_mat, aes(x = posk, y = poscoherence)) + geom_point() +
  geom_line(group = 1)+
  ggtitle("Optimal Count of Topic by Coherence Score - Positive Reviews") + 
  theme_minimal() + scale_x_continuous(breaks = seq(1,31,1)) + ylab("Coherence")

#negative
negcoherence_mat <- data.frame(negk = c(5:20), coherence = negcoherence,
                               stringsAsFactors = FALSE)

ggplot(negcoherence_mat, aes(x = negk, y = negcoherence)) + geom_point() +
  geom_line(group = 1)+
  ggtitle("Optimal Count of Topic by Coherence Score - Negative Reviews") + 
  theme_minimal() + scale_x_continuous(breaks = seq(1,31,1)) + ylab("Coherence")



#-------------------LDA-----------------------
#positive
posLDA <-LDA(posreviewDTM,posk, method="Gibbs", 
             control=list(iter=iter,seed=1000))
posphi <- posterior(posLDA)$terms %>% as.matrix
postheta <- posterior(posLDA)$topics %>% as.matrix

#negative
negLDA <-LDA(negreviewDTM,negk, method="Gibbs", 
             control=list(iter=iter,seed=1000))
negphi <- posterior(negLDA)$terms %>% as.matrix
negtheta <- posterior(negLDA)$topics %>% as.matrix


#------------Highest alpha terms in positive reviews-------
#positive
posLDA.terms <- as.matrix(terms(posLDA, 10))

#negative
negLDA.terms <- as.matrix(terms(negLDA, 10))

#alternate approach
#--------Checking topic and their frequent words (manual examination)-------
#----keeping optimal count for positive topics to be 7------
pos7LDA <-LDA(posreviewDTM,7, method="Gibbs", 
              control=list(iter=iter,seed=1000))
posphi7 <- posterior(pos7LDA)$terms %>% as.matrix
postheta7 <- posterior(pos7LDA)$topics %>% as.matrix

pos7LDA.terms <- as.matrix(terms(pos7LDA, 10))


#----keeping optimal count for negative topics to be 6------
neg6LDA <-LDA(negreviewDTM,6, method="Gibbs", 
              control=list(iter=iter,seed=1000))
negphi6 <- posterior(neg6LDA)$terms %>% as.matrix
negtheta6 <- posterior(neg6LDA)$topics %>% as.matrix

neg6LDA.terms <- as.matrix(terms(neg6LDA, 10))

#-------------Grouping documents by topics------
#positive
posLDA.topics <- data.frame(topics(posLDA))
posLDA.topics$index <- as.numeric(row.names(posLDA.topics)) 
review_pos$index <- as.numeric(row.names(review_pos))
posrevwithtopic <- merge(review_pos, posLDA.topics, by='index',all.x=TRUE) 
posrevwithtopic <- posrevwithtopic[order(posrevwithtopic$index), ]

posrevwithtopic[0:10,]

write.csv(posrevwithtopic, "2")

posrevwithtopic %>%
  group_by(topics.posLDA.) %>%
  summarise(count= n())

?group_by

#negative
negLDA.topics <- data.frame(topics(negLDA))
negLDA.topics$index <- as.numeric(row.names(negLDA.topics)) 
review_neg$index <- as.numeric(row.names(review_neg))
negrevwithtopic <- merge(review_neg, negLDA.topics, by='index',all.x=TRUE) 
negrevwithtopic <- negrevwithtopic[order(negrevwithtopic$index), ]

negrevwithtopic[0:10,]

write.csv(negrevwithtopic, "1")

negrevwithtopic %>%
  group_by(topics.negLDA.) %>%
  summarise(count= n())

#---------------How closely review relates with topics--------
#positive
posprob <- as.data.frame(posLDA@gamma)
posprob[0:10, 1:5]

write.csv(posprob, file = "xlss1")

#negative
negprob <- as.data.frame(negLDA@gamma)
negprob[0:10, 1:5]

write.csv(negprob, file = "xlss")

#------------LDA Visualization--------
#positive
posvocab <- colnames(posphi)

pos_lda <- createJSON(phi = posphi, theta = postheta,
                      vocab = posvocab, doc.length = poslength,
                      term.frequency = posfrequency1)

serVis(pos_lda, out.dir = "vis", open.browser = TRUE)


#negative
negvocab <- colnames(negphi)

neg_lda <- createJSON(phi = negphi, theta = negtheta,
                      vocab = negvocab, doc.length = neglength,
                      term.frequency = negfrequency1)

serVis(neg_lda, out.dir = "vis", open.browser = TRUE)


