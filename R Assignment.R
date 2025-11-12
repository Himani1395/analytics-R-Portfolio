rm(list=ls())
ICO <- read.csv("data_LUBS5990M_202122.csv", header = TRUE, stringsAsFactors = FALSE,
                encoding = "UTF-8")
str(ICO)
country <- read.csv("country.csv", header = TRUE)

library(dplyr)
library(tidyverse)
library(Boruta)
library(caret)
library(randomForest)
library(varImp)
library(kernlab)
library(e1071)
library(gmodels)
library(C50)
library(class)
library(neuralnet)
library(ROCR)
library(pROC)
library(ggplot2)
library(treemap)
library(corrgram)

#---------Data Understanding-----------------------------
summary(ICO$coinNum)

summary(ICO$teamSize)

unique(ICO$teamSize, incomparables = FALSE)

boxplot(ICO$teamSize)

hist(ICO$teamSize, main = "Team Size Distribution", xlab = "Team Size", ylim = c(0, 400))

treemap(country, index = "Country", vSize = "ICO", title = "Region-wise distribution of ICO",
        type = "index")

hist(ICO$overallrating, main = "Overall Rating of ICO Project", xlab = "Overall Rating", ylim = c(0, 300))

hist(ICO$ratingTeam, main = "Rating of ICO Project Teams", xlab = "Team Rating", ylim = c(0, 400))

hist(ICO$ratingProduct, main = "Rating of Start-up Product/Service", xlab = "product/Service Rating", ylim = c(0, 400))

hist(ICO$acceptingCurrencyNum, main = "Count of Accepting Currency", xlab = "Accepting Currency", ylim = c(0, 600))

ggplot(data.frame(ICO$acceptingCurrencyNum), aes(x=ICO$acceptingCurrencyNum)) +
  geom_bar() + ggtitle("Count of Accepting Currency") + labs(y = "Frequency", x = "Accepting Currency") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))

hist(ICO$whitepaper, main = "White Paper (0 - No, 1 - Yes)", xlab = "White Paper", ylim = c(0, 1200))

barplot(table(ICO$whitepaper), ylim = c(0, 1200)) + title(main = "White Paper (1 - Yes, 0 - No)", xlab = "White Paper", ylab = "Frequency")

barplot(table(ICO$video), ylim = c(0, 900)) + title(main = "Video (1 - Yes, 0 - No)", xlab = "Video", ylab = "Frequency")

barplot(table(ICO$GitHub), ylim = c(0, 600)) + title(main = "Github (1- Yes, 0- No)", xlab = "Github", ylab = "Frequency")

hist(ICOdata$teamLinkedIn, main = "Presence of team members on LinkedIn", xlab = "Team LinkedIn", ylim = c(0, 500))

hist(ICOdata$teamPhotos, main = "Availability of Photos of Team Members", xlab = "Team Photos", ylim = c(0, 1200))

barplot(table(ICO$CEOPhoto), ylim = c(0, 800)) + title(main = "CEO Photo (1- Yes, 0- No)", xlab = "CEO Photo", ylab = "Frequency")

#---------Data Pre-Processing-----------------------------


#Converting date columns into date format
ICO$startdate <- as.Date(ICO$startdate, format = "%d/%m/%Y")
ICO$enddate <- as.Date(ICO$enddate, format = "%d/%m/%Y")

#Calculating a new variable - campaign period
ICO <- ICO %>%
  mutate(Campaign_Period = enddate - startdate + 1)

#Checking if there are any negative period of campaigns for data cleaning
ICO %>%
  filter(ICO$Campaign_Period <0)

#Spelling errors in country_region
ICO <- ICO %>% 
  mutate(country_region = case_when(
    country_region %in% c("Cayman Islands", "Cayman Island") ~ "Cayman Islands"
    ,TRUE ~ country_region
  )
  )

ICO <- ICO %>% 
  mutate(country_region = case_when(
    country_region %in% c("Czech Republic", "Czechia") ~ "Czech Republic",
    TRUE ~ country_region
  ))

ICO <- ICO %>% 
  mutate(country_region = case_when(
    country_region %in% c("Netherlands", "Netherland") ~ "Netherlands",
    TRUE ~ country_region
  ))

ICO <- ICO %>% 
  mutate(country_region = case_when(
    country_region %in% c("Perú", "Peru") ~ "Peru",
    TRUE ~ country_region
  ))

ICO <- ICO %>% 
  mutate(country_region = case_when(
    country_region %in% c("Russia", "Russian") ~ "Russia",
    TRUE ~ country_region
  ))

ICO <- ICO %>% 
  mutate(country_region = case_when(
    country_region %in% c("United Arab Emirates", "UAE", "Dubai", "Ras al-Khaimah") ~ "United Arab Emirates",
    TRUE ~ country_region
  ))

ICO <- ICO %>% 
  mutate(country_region = case_when(
    country_region %in% c("United Kingdom", "UK", "England") ~ "UK",
    TRUE ~ country_region
  ))

ICO <- ICO %>% 
  mutate(country_region = case_when(
    country_region %in% c("india", "India") ~ "India",
    TRUE ~ country_region
  ))

ICO <- ICO %>% 
  mutate(country_region = case_when(
    country_region %in% c("United States", "USA", "United States of America") ~ "USA",
    TRUE ~ country_region
  ))

ICO <- ICO %>% 
  mutate(country_region = case_when(
    country_region %in% c("South Korea", "Korea") ~ "South Korea",
    TRUE ~ country_region
  ))

ICO <- ICO %>% 
  mutate(country_region = case_when(
    country_region %in% c("Saint Kitts and Nevis") ~ "St. Kitts and Nevis", TRUE ~ country_region
  ))

ICO <- ICO %>% 
  mutate(country_region = case_when(
    country_region %in% c("Saint Lucia") ~ "St. Lucia", TRUE ~ country_region
  ))

ICO <- ICO %>% 
  mutate(country_region = case_when(
    country_region %in% c("Virgin Islands") ~ "Virgin Islands (U.S.)", TRUE ~ country_region
  ))

#Correcting spelling Ethereum platform column
ICO <- ICO %>% 
  mutate(platform = case_when(
    platform %in% c("ETH", "Ehtereum") ~ "Ethereum", TRUE ~ platform
  ))


#adding a binary column if platform is Ethereum = 1, otherwise = 0
ICO$ethereum_platform <- ifelse(ICO$platform %in% c("Ethereum","Ethereum and Waves","Ethereum, Bitcoin") ,1, 0)

sum(ICO$ethereum_platform)

#adding a start year column
ICO <- ICO %>%
  mutate(start_year = as.numeric(format(startdate, "%Y")))

ICO <- ICO %>%
  mutate(country_startyear = paste(country_region, start_year, sep = " "))

#merging GDP - economical variable in ICO by unique column - start year & country
GDP <- read.csv("GDP.csv", header = TRUE)

ICOGDP <- merge(ICO, GDP, by.x = "country_startyear", by.y = "country_startyear", all.x = TRUE)
ICOGDP$Per.Capita.GDP <- as.numeric(ICOGDP$Per.Capita.GDP)

sum(ICOGDP$Per.Capita.GDP, na.rm = TRUE)

new_NAGDP <- ICOGDP[is.na(ICOGDP$Per.Capita.GDP),]

#merging bitcoin prices
BitcoinUSD <- read.csv("bitcoinpriceUSD.csv", header = TRUE)
bitcoin <- BitcoinUSD[,c('Date', 'Open')]
str(bitcoin)
bitcoin$Date <- as.Date(bitcoin$Date, format = "%Y-%m-%d")
FinalICO <- merge(ICOGDP, bitcoin, by.x = "startdate", by.y = "Date", all.x = TRUE)


#new data frame
ICOdata <- FinalICO[, -c(1:3, 5, 8, 9, 42, 53)]

overallrating <- ICOdata %>%
  filter(ICOdata$overallrating < 1)

#delete unwanted rows (6 rows)
ICOdata <- ICOdata[ICOdata$Campaign_Period > 0, ] #deleting rows with negative campaign period
ICOdata <- ICOdata[!is.na(ICOdata$Per.Capita.GDP), ] #deleting two rows with two countries mentioned
ICOdata <- ICOdata[-186, ] #deleting overall rating below 1

str(ICOdata)

#converting characters into numbers
ICOdata$acceptingCurrencyNum <- as.numeric(ICOdata$acceptingCurrencyNum)
ICOdata$Campaign_Period<- as.numeric(ICOdata$Campaign_Period)
ICOdata$teamLinkedIn = as.numeric(substr(ICOdata$teamLinkedIn,0,
                                           nchar(ICOdata$teamLinkedIn)-1))
ICOdata$teamPhotos = as.numeric(substr(ICOdata$teamPhotos,0,
                                         nchar(ICOdata$teamPhotos)-1))

colnum <- c(1, 4:32, 37:40, 43, 45)
ICOdata[,colnum] <- lapply(ICOdata[,colnum] ,factor )

str(ICOdata)

#Handling missing values for accepting currency using simple imputation (median)
ICOdata$acceptingCurrencyNum[is.na(ICOdata$acceptingCurrencyNum)] <- median(ICOdata$acceptingCurrencyNum, na.rm = TRUE)
sum(ICOdata$acceptingCurrencyNum)
corrgram(ICOdata)

#----------------Feature Engineering-------------------

#Boruta Principal
set.seed(928)
ICOdata.boruta <- Boruta(goal ~ ., data = ICOdata, doTrace = 2, maxruns = 300)
ICOdata.boruta

plot(ICOdata.boruta) + title("Mean Importance of Attributes (Boruta)")

getConfirmedFormula(ICOdata.boruta)
importanceboruta <- attStats(ICOdata.boruta)
importanceboruta1 <- importanceboruta[importanceboruta$decision !="Rejected", c("meanImp", "decision")]
write.csv(importanceboruta1, "boruta")

#Random Forest
ICOdata.RF <- randomForest(goal ~ ., data = ICOdata, maxruns = 500)
importanceRF <- importance(ICOdata.RF)
varImp(ICOdata.RF)
varImpPlot(ICOdata.RF)
write.csv(importanceRF, "RF")


#creating a new data set with chosen variables
ICOfinal <- ICOdata[, c(1:3, 8, 12, 20, 25, 33, 36, 38:44, 46, 47)]
str(ICOfinal)

#-----------Classification using different models-----
#creating train and test sets
smp_size1 <- floor(0.8 * nrow(ICOfinal))
# Set Seed so that same sample can be reproduced in future
set.seed(100)
trainset <- sample(nrow(ICOfinal), smp_size1)
trainingset <- ICOfinal[trainset, ]
testset <- ICOfinal[-trainset, ]




#----------SVM
SVMclassifier <- ksvm(goal ~ ., data = trainingset, kernel = "vanilladot")
SVMclassifier

SVMgoalprediction <- predict(SVMclassifier, select(testset, -goal))
head(SVMgoalprediction)

accuracy <- SVMgoalprediction == testset$goal
table(accuracy)
prop.table(table(accuracy)) #74.8% accuracy
CrossTable(x = testset$goal, y = SVMgoalprediction, prop.chisq=FALSE)

#ROC
SVMclassifierprob <- ksvm(goal ~ ., data = trainingset, kernel = "vanilladot", prob.model = TRUE)
SVMpredprob <- predict(SVMclassifierprob, testset, type = "prob")
SVMpred_object <- prediction(SVMpredprob[,2], testset$goal)
SVM_ROC <- performance(SVMpred_object, measure = "tpr", x.measure = "fpr")
plot(SVM_ROC, main = "SVM ROC Curve for ICO Success Rate", col = "green", lwd = 2)
auc_object_SVM <- performance(SVMpred_object, measure = "auc")
auc_SVM <- auc_object_SVM@y.values[[1]]
auc_SVM

#RBF
set.seed(928)
SVMclassifierrbf <- ksvm(goal ~ ., data = trainingset, kernel = "rbfdot")

SVMgoalpredictionrbf <- predict(SVMclassifierrbf, select(testset, -goal))

accuracyrbf <- SVMgoalpredictionrbf == testset$goal
table(accuracyrbf)
prop.table(table(accuracyrbf)) #77.4% accuracy

#ROC
SVMclassifierrbfprob <- ksvm(goal ~ ., data = trainingset, kernel = "rbfdot", prob.model = TRUE)
SVMpredprobrbf <- predict(SVMclassifierrbfprob, testset, type = "prob")
SVMpred_objectrbf <- prediction(SVMpredprobrbf[,2], testset$goal)
SVM_ROCrbf <- performance(SVMpred_objectrbf, measure = "tpr", x.measure = "fpr")
plot(SVM_ROCrbf, col = "orange", lwd = 2, add = TRUE)
legend("right", c("SVM Vanilla","SVM RBF"), lwd=1, 
       col = c("green", "orange"))
abline(a = 0, b = 1, lwd = 2, lty = 2)
auc_object_SVMrbf <- performance(SVMpred_objectrbf, measure = "auc")
auc_SVMrbf <- auc_object_SVMrbf@y.values[[1]]
auc_SVMrbf

#------decision tree
dtclassifier <- C5.0(goal ~ ., data = trainingset)
dtclassifier

dtgoalprediction <- predict(dtclassifier, testset)
head(dtgoalprediction)

accuracydt <- dtgoalprediction == testset$goal
table(accuracydt)
prop.table(table(accuracydt))
confusionMatrix(dtgoalprediction, testset$goal, positive = "Y")
CrossTable(x = testset$goal, y = dtgoalprediction, prop.chisq=FALSE) #72.7%

#ROC
dtpredprob <- predict(dtclassifier, testset, type = "prob")
DTpred_object <- prediction(dtpredprob[,2], testset$goal)
DT_ROC <- performance(DTpred_object, measure = "tpr", x.measure = "fpr")
plot(DT_ROC, main = "Decision Tree ROC curve for ICO Success Rate", col = "blue", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
auc_object_DT <- performance(DTpred_object, measure = "auc")
auc_DT <- auc_object_DT@y.values[[1]]
auc_DT

#adaptive boosting
dtboosting <- C5.0(goal ~ ., data = trainingset, trials = 10)
dtboosting
summary(dtboosting)
boostprediction <- predict(dtboosting, testset)

CrossTable(boostprediction, testset$goal, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default')) 
confusionMatrix(boostprediction, testset$goal, positive = "Y") #74.47%

#ROC
dtABpredprob <- predict(dtboosting, testset, type = "prob")
DTpred_objectAB <- prediction(dtABpredprob[,2], testset$goal)
ABDT_ROC <- performance(DTpred_objectAB, measure = "tpr", x.measure = "fpr")
plot(ABDT_ROC, col = "red", lwd = 2, add = TRUE)
legend("right", c("DT (c5.0)","DT (c5.0 Trials=10)"), lwd=1, 
       col = c("blue", "red"))
auc_object_DTAB <- performance(DTpred_objectAB, measure = "auc")
auc_DTAB <- auc_object_DTAB@y.values[[1]]
auc_DTAB



#-------KNN
ICOknn <- ICOfinal
ICOknn <- as.data.frame(lapply(ICOknn, as.numeric))
ICOknn$goal <- factor(ICOknn$goal)
str(ICOknn)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
traindatanormaliseKNN <- as.data.frame(lapply(ICOknn[2:18], normalize))

set.seed(100)
trainingKNN <- sample(nrow(traindatanormaliseKNN), smp_size1)
trainingsetKNN <- traindatanormaliseKNN[trainingKNN, ]
testsetKNN <- traindatanormaliseKNN[-trainingKNN, ]
trainlabelsKNN <- ICOknn[trainingKNN, 1]
testlabelsKNN <- ICOknn[-trainingKNN, 1]
K = 30
knnpred <- knn(train = trainingsetKNN, test = testsetKNN, cl = trainlabelsKNN, k=K)

CrossTable(x = testlabelsKNN, y = knnpred, prop.chisq=FALSE)
confusionMatrix(knnpred,testlabelsKNN, positive = "2") #68.09%

#ROC
KNNclassifierprob <- predict(caret:: knn3(trainingsetKNN,trainlabelsKNN, k=21), testsetKNN)
KNNpred_object <- prediction(KNNclassifierprob[,2], testset$goal)
KNN_ROC <- performance(KNNpred_object, measure = "tpr", x.measure = "fpr")
plot(KNN_ROC, main = "KNN ROC Curve for ICO Success Rate", col = "purple", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)
legend("right", c("KNN (K=30)"), lwd=1, 
       col = c("purple"))
auc_object_KNN <- performance(KNNpred_object, measure = "auc")
auc_KNN <- auc_object_KNN@y.values[[1]]
auc_KNN

#----------Evaluation of three models---------------
#-----SVM
#vanilla dot
CrossTable(x = testset$goal, y = SVMgoalprediction, prop.chisq=FALSE)
confusionMatrix(testset$goal, SVMgoalprediction, positive = "Y")

#10 fold loop
K = 10
set.seed(928)
folds <- createFolds(ICOfinal$goal, k = K)
str(folds)

accuracy_listSVM <-as.numeric()
sensitivity_listSVM <-as.numeric() 
specificity_listSVM <-as.numeric()
precision_listSVM <-as.numeric()

for(i in 1:K){
  SVMfold_test <- ICOfinal[folds[[i]],]
  SVMfold_train <- ICOfinal[-folds[[i]],] 
  SVM_fold_model <- ksvm(goal ~ ., data = SVMfold_train, kernel = "vanilladot")
  SVMfold_predict <- predict(SVM_fold_model, SVMfold_test)
  SVMfoldcm <- confusionMatrix(SVMfold_predict, SVMfold_test$goal, positive = "Y")
  SVMfold_accuracy <- SVMfoldcm$overall['Accuracy']
  SVMfold_sensitivity <- SVMfoldcm$byClass['Sensitivity']
  SVMfold_specificity <- SVMfoldcm$byClass['Specificity']
  SVMfold_precision <- SVMfoldcm$byClass['Precision']
  SVMfold_F <- SVMfoldcm$byClass['F1']
  accuracy_listSVM <- append(accuracy_listSVM,SVMfold_accuracy)
  sensitivity_listSVM <- append(sensitivity_listSVM, SVMfold_sensitivity)
  specificity_listSVM <- append(specificity_listSVM, SVMfold_specificity)
  precision_listSVM <- append(precision_listSVM, SVMfold_precision)
}

# compute the average over 10 folds
# accuracy
accuracy_average_SVM <- mean(accuracy_listSVM)
accuracy_average_SVM
sd(accuracy_listSVM)
#sensitivity
sensitivity_average_SVM <- mean(sensitivity_listSVM)
mean(sensitivity_listSVM)
sd(sensitivity_listSVM)
#specificity
specificity_average_SVM <- mean(specificity_listSVM)
mean(specificity_listSVM)
sd(specificity_listSVM)
#precision
precision_average_SVM <- mean(precision_listSVM)
mean(precision_listSVM)
sd(precision_listSVM)


#RBF
CrossTable(x = testset$goal, y = SVMgoalpredictionrbf, prop.chisq=FALSE)
confusionMatrix(SVMgoalpredictionrbf, testset$goal, positive = "Y") #confusion matrix

#10 fold loop
accuracy_listSVMRBF <-as.numeric()
sensitivity_listSVMRBF <-as.numeric() 
specificity_listSVMRBF <-as.numeric()
precision_listSVMRBF <-as.numeric()

for(i in 1:K){
  SVMfold_testRBF <- ICOfinal[folds[[i]],]
  SVMfold_trainRBF <- ICOfinal[-folds[[i]],] 
  SVM_fold_modelRBF <- ksvm(goal ~ ., data = SVMfold_trainRBF, kernel = "rbfdot")
  SVMfold_predictRBF <- predict(SVM_fold_modelRBF, SVMfold_testRBF)
  SVMfoldcmRBF <- confusionMatrix(SVMfold_predictRBF, SVMfold_testRBF$goal, positive = "Y")
  SVMfold_accuracyRBF <- SVMfoldcmRBF$overall['Accuracy']
  SVMfold_sensitivityRBF <- SVMfoldcmRBF$byClass['Sensitivity']
  SVMfold_specificityRBF <- SVMfoldcmRBF$byClass['Specificity']
  SVMfold_precisionRBF <- SVMfoldcmRBF$byClass['Precision']
  SVMfold_FRBF <- SVMfoldcmRBF$byClass['F1']
  accuracy_listSVMRBF <- append(accuracy_listSVMRBF,SVMfold_accuracyRBF)
  sensitivity_listSVMRBF <- append(sensitivity_listSVMRBF, SVMfold_sensitivityRBF)
  specificity_listSVMRBF <- append(specificity_listSVMRBF, SVMfold_specificityRBF)
  precision_listSVMRBF <- append(precision_listSVMRBF, SVMfold_precisionRBF)
}
accuracy_listSVMRBF
# compute the average over 10 folds
# accuracy
accuracy_average_SVMRBF <- mean(accuracy_listSVMRBF)
accuracy_average_SVMRBF
sd(accuracy_listSVMRBF)
#sensitivity
sensitivity_average_SVMRBF <- mean(sensitivity_listSVMRBF)
mean(sensitivity_listSVMRBF)
sd(sensitivity_listSVMRBF)
#specificity
specificity_average_SVMRBF <- mean(specificity_listSVMRBF)
mean(specificity_listSVMRBF)
sd(specificity_listSVMRBF)
#precision
precision_average_SVMRBF <- mean(precision_listSVMRBF)
mean(precision_listSVMRBF)
sd(precision_listSVMRBF)


#-----Decision Tree
confusionMatrix(table(dtgoalprediction,testset$goal))
CrossTable(x = testset$goal, y = dtgoalprediction, prop.chisq=FALSE) #64%
accuracy_listDT <-as.numeric()
sensitivity_listDT <-as.numeric() 
specificity_listDT <-as.numeric()
precision_listDT <-as.numeric()

for(i in 1:K){
  DTfold_test <- ICOfinal[folds[[i]],]
  DTfold_train <- ICOfinal[-folds[[i]],] 
  DT_fold_model <- C5.0(select(DTfold_train, - goal), DTfold_train$goal)
  DTfold_predict <- predict(DT_fold_model, DTfold_test)
  DTfoldcm <- confusionMatrix(DTfold_predict, DTfold_test$goal, positive = "Y")
  DTfold_accuracy <- DTfoldcm$overall['Accuracy']
  DTfold_sensitivity <- DTfoldcm$byClass['Sensitivity']
  DTfold_specificity <- DTfoldcm$byClass['Specificity']
  DTfold_precision <- DTfoldcm$byClass['Precision']
  DTfold_F <- DTfoldcm$byClass['F1']
  accuracy_listDT <- append(accuracy_listDT,DTfold_accuracy)
  sensitivity_listDT <- append(sensitivity_listDT, DTfold_sensitivity)
  specificity_listDT <- append(specificity_listDT, DTfold_specificity)
  precision_listDT <- append(precision_listDT, DTfold_precision)
  }

accuracy_listDT
# compute the average over 10 folds
# accuracy
accuracy_average_DT <- mean(accuracy_listDT)
accuracy_average_DT
sd(accuracy_listDT)
#sensitivity
sensitivity_average_DT <- mean(sensitivity_listDT)
mean(sensitivity_listDT)
sd(sensitivity_listDT)
#specificity
specificity_average_DT <- mean(specificity_listDT)
mean(specificity_listDT)
sd(specificity_listDT)
#precision
precision_average_DT <- mean(precision_listDT)
mean(precision_listDT)
sd(precision_listDT)

#adaptive boosting decision tree
CrossTable(boostprediction, testset$goal, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default')) 
confusionMatrix(boostprediction, testset$goal, positive = "Y") #confusion matrix

accuracy_listDTAB <-as.numeric()
sensitivity_listDTAB <-as.numeric() 
specificity_listDTAB <-as.numeric()
precision_listDTAB <-as.numeric()

for(i in 1:K){
  DTfold_testAB <- ICOfinal[folds[[i]],]
  DTfold_trainAB <- ICOfinal[-folds[[i]],] 
  DT_fold_modelAB <- C5.0(goal ~ ., data = DTfold_trainAB, trials = 10)
  DTfold_predictAB <- predict(DT_fold_modelAB, DTfold_testAB)
  DTfoldcmAB <- confusionMatrix(DTfold_predictAB, DTfold_testAB$goal, positive = "Y")
  DTfold_accuracyAB <- DTfoldcmAB$overall['Accuracy']
  DTfold_sensitivityAB <- DTfoldcmAB$byClass['Sensitivity']
  DTfold_specificityAB <- DTfoldcmAB$byClass['Specificity']
  DTfold_precisionAB <- DTfoldcmAB$byClass['Precision']
  DTfold_FAB <- DTfoldcmAB$byClass['F1']
  accuracy_listDTAB <- append(accuracy_listDTAB,DTfold_accuracyAB)
  sensitivity_listDTAB <- append(sensitivity_listDTAB, DTfold_sensitivityAB)
  specificity_listDTAB <- append(specificity_listDTAB, DTfold_specificityAB)
  precision_listDTAB <- append(precision_listDTAB, DTfold_precisionAB)
}

accuracy_listDTAB
# compute the average over 10 folds
# accuracy
accuracy_average_DTAB <- mean(accuracy_listDTAB)
accuracy_average_DTAB
sd(accuracy_listDTAB)
#sensitivity
sensitivity_average_DTAB <- mean(sensitivity_listDTAB)
mean(sensitivity_listDTAB)
sd(sensitivity_listDTAB)
#specificity
specificity_average_DTAB <- mean(specificity_listDTAB)
mean(specificity_listDTAB)
sd(specificity_listDTAB)
#precision
precision_average_DTAB <- mean(precision_listDTAB)
mean(precision_listDTAB)
sd(precision_listDTAB)



#---------KNN 
CrossTable(x = testlabelsKNN, y = knnpred, prop.chisq=FALSE)
confusionMatrix(knnpred,testlabelsKNN, positive = "2") #confusion matrix
accuracy_listKNN <-as.numeric()
sensitivity_listKNN <-as.numeric() 
specificity_listKNN <-as.numeric()
precision_listKNN <-as.numeric()

for(i in 1:K){
  KNNfold_test <- traindatanormaliseKNN[folds[[i]],]
  KNNfold_train <- traindatanormaliseKNN[-folds[[i]],]
  trainlabelsKNNfold <- ICOknn[-folds[[i]], 1]
  testlabelsKNNfold <- ICOknn[folds[[i]], 1]
  KNN_fold_model <- knn(train = KNNfold_train, test = KNNfold_test, cl = trainlabelsKNNfold, k = 21)
  KNNfoldcm <- confusionMatrix(KNN_fold_model, testlabelsKNNfold, positive = "2")
  KNNfold_accuracy <- KNNfoldcm$overall['Accuracy']
  KNNfold_sensitivity <- KNNfoldcm$byClass['Sensitivity']
  KNNfold_specificity <- KNNfoldcm$byClass['Specificity']
  KNNfold_precision <- KNNfoldcm$byClass['Precision']
  KNNfold_F <- KNNfoldcm$byClass['F1']
  accuracy_listKNN <- append(accuracy_listKNN,KNNfold_accuracy)
  sensitivity_listKNN <- append(sensitivity_listKNN, KNNfold_sensitivity)
  specificity_listKNN <- append(specificity_listKNN, KNNfold_specificity)
  precision_listKNN <- append(precision_listKNN, KNNfold_precision)
}
accuracy_listKNN
# compute the average over 10 folds
# accuracy
accuracy_average_KNN <- mean(accuracy_listKNN)
accuracy_average_KNN
sd(accuracy_listKNN)
#sensitivity
sensitivity_average_KNN <- mean(sensitivity_listKNN)
mean(sensitivity_listKNN)
sd(sensitivity_listKNN)
#specificity
specificity_average_KNN <- mean(specificity_listKNN)
mean(specificity_listKNN)
sd(specificity_listKNN)
#precision
precision_average_KNN <- mean(precision_listKNN)
mean(precision_listKNN)
sd(precision_listKNN)


