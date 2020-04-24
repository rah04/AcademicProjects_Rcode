

setwd("C:/Users/Murtaza/Downloads")

set.seed(1337)

##Reading Excel Sheet
library(readxl)
Mumbai_Sion_and_Bandra <- read_excel("Mumbai - Sion and Bandra.xlsx", 
                                     sheet = "Sion", skip = 15)

###Removing Blank Rows and Columns

keep <- c("Date","SO2","NOx","RSPM","AQI")
Sion_Data <- Mumbai_Sion_and_Bandra[keep]
Sion_Data <- Sion_Data[-c(1,2,3),]


## Every row with an AQI at NA value has all other rows as NAs as well. 
## This signifies that inputs were not taken that day.Hence, we are removing them all.

Sion_Data <- Sion_Data[complete.cases(Sion_Data[,5]),]

##Similarly all rows with AQI=0, also have no readings, Hence we remove those rows.

Sion_Data <- Sion_Data[!(Sion_Data$AQI==0),]


###Conversion of rows with BDL values to "NA"

Sion_Data$SO2 <- gsub("BDL - NA","NA",Sion_Data$SO2)
Sion_Data$SO2 <- gsub("BDL - 1","NA",Sion_Data$SO2)
Sion_Data$SO2 <- gsub("BDL - 2","NA",Sion_Data$SO2)
Sion_Data$SO2 <- gsub("BDL - 3","NA",Sion_Data$SO2)

Sion_Data$NOx <- gsub("BDL - NA","NA",Sion_Data$NOx)
Sion_Data$NOx <- gsub("BDL - 2","NA",Sion_Data$NOx)
Sion_Data$NOx <- gsub("BDL - 3","NA",Sion_Data$NOx)
Sion_Data$NOx <- gsub("BDL - 4","NA",Sion_Data$NOx)
Sion_Data$NOx <- gsub("BDL - 5","NA",Sion_Data$NOx)
Sion_Data$NOx <- gsub("BDL - 6","NA",Sion_Data$NOx)
Sion_Data$NOx <- gsub("BDL - 7","NA",Sion_Data$NOx)

##Conversion to numeric Class
Sion_Data$SO2 <- as.numeric(Sion_Data$SO2)
Sion_Data$NOx <- as.numeric(Sion_Data$NOx)
Sion_Data$RSPM <- as.numeric(Sion_Data$RSPM)

str(Sion_Data)

##As all three rows of Pollutants have NAs present, we need to impute these values
##Imputation via Machine Learning

library(mice)
miceMod <- mice(Sion_Data[, !names(Sion_Data) %in% c("Date","AQI")], method="rf")  # perform mice imputation, based on random forests.
miceOutput <- complete(miceMod)  # generate the completed data.
anyNA(miceOutput)

##Replacing imputed output into main dataframe
Sion_Data$SO2 <- miceOutput$SO2
Sion_Data$NOx <- miceOutput$NOx
Sion_Data$RSPM <- miceOutput$RSPM

anyNA(Sion_Data)   ##FALSE

###Exporting Ready File
write.csv(Sion_Data,"Sion_Individual.csv",row.names = FALSE)



#####################Sheet 2######################################


##Reading Excel Sheet
library(readxl)
Mumbai_Bandra <- read_excel("Mumbai - Sion and Bandra.xlsx", 
                                     sheet = "Bandra", skip = 15)

###Removing Blank Rows and Columns

keep <- c("Date","SO2","NOx","RSPM","AQI")
Bandra_Data <- Mumbai_Bandra[keep]
Bandra_Data <- Bandra_Data[-c(1,2,3),]

## Every row showing NA value at both AQI and RSPMhas all other rows with NAs as well. 
## This signifies that inputs were not taken that day.Hence, we are removing them all.

Bandra_Data <- Bandra_Data[complete.cases(Bandra_Data[,4:5]),]

##Also all rows with AQI=0, also have no readings, Hence we remove those rows.

Bandra_Data <- Bandra_Data[!(Bandra_Data$AQI==0),]

##Conversion of BDL values to NA

Bandra_Data$SO2 <- gsub("BDL - 1","NA",Bandra_Data$SO2)
Bandra_Data$SO2 <- gsub("BDL - 2","NA",Bandra_Data$SO2)
Bandra_Data$SO2 <- gsub("BDL - 3","NA",Bandra_Data$SO2)


Bandra_Data$NOx <- gsub("BDL - 1","NA",Bandra_Data$NOx)
Bandra_Data$NOx <- gsub("BDL - 2","NA",Bandra_Data$NOx)
Bandra_Data$NOx <- gsub("BDL - 3","NA",Bandra_Data$NOx)
Bandra_Data$NOx <- gsub("BDL - 5","NA",Bandra_Data$NOx)
Bandra_Data$NOx <- gsub("BDL - 6","NA",Bandra_Data$NOx)
Bandra_Data$NOx <- gsub("BDL - 7","NA",Bandra_Data$NOx)
Bandra_Data$NOx <- gsub("BDL - 8","NA",Bandra_Data$NOx)

##Conversion to numeric Class
Bandra_Data$SO2 <- as.numeric(Bandra_Data$SO2)
Bandra_Data$NOx <- as.numeric(Bandra_Data$NOx)
Bandra_Data$RSPM <- as.numeric(Bandra_Data$RSPM)

str(Bandra_Data)

##As all three rows of Pollutants have NAs present, we need to impute these values
##Imputation via Machine Learning

library(mice)
miceMod <- mice(Bandra_Data[, !names(Bandra_Data) %in% "AQI"], method="rf")  # perform mice imputation, based on random forests.
miceOutput <- complete(miceMod)  # generate the completed data.
anyNA(miceOutput)

Bandra_Data$SO2 <- miceOutput$SO2
Bandra_Data$NOx <- miceOutput$NOx
Bandra_Data$RSPM <- miceOutput$RSPM

anyNA(Bandra_Data)   ##FALSE

###Exporting Ready File
write.csv(Bandra_Data,"BandraIndividual.csv",row.names = FALSE)

#########Data Transformation ##################################


###We need to normalize the data


library(class)

normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
a<-read.csv("BandraIndividual.csv")
b <- read.csv("Sion_Individual.csv")
a1 <- subset(a,select = c(4,5))
a1 <- as.data.frame(lapply(a1, normalize))
b1 <- subset(b,select = c(4,5))
b1 <- as.data.frame(lapply(b1, normalize))
a$NormRSPM<-a1$RSPM
a$NormAQI<-a1$AQI
b$NormRSPM<-b1$RSPM
b$NormAQI<-b1$AQI

##Here a denotes Bandra 
##and  b is for Sion

par(mfrow=c(2,2))
plot(x=a$SO2,y=a$AQI)
plot(x=a$NOx,y=a$AQI)
plot(x=a$RSPM,y=a$AQI)

plot(x=b$SO2,y=b$AQI)
plot(x=b$NOx,y=b$AQI)
plot(x=b$RSPM,y=b$AQI)


################## Multiple Linear Regression ##########################

model_Bandra <- lm(a$AQI ~ a$SO2 + a$NOx + a$RSPM)
summary(model_Bandra)  ####The Summary gives us 92.7 adjusted R square value

model_Sion <- lm(b$AQI ~ b$SO2 + b$NOx + b$RSPM)
summary(model_Sion)    ####The Summary gives us 88.9 adjusted R square value

##Creating a New Column of Predicted AQI as per Multiple Linear Regression Model Equation

a$Predicted_Bandra_AQI <- 16.176292 - (0.148823*a$SO2) + (0.367373*a$NOx) + (0.668166*a$RSPM)
b$Predicted_Sion_AQI <- 9.785840 - (0.185355*b$SO2) + (0.456110*b$NOx) + (0.638684*b$RSPM)

###Evaluation of Linear Regression

###RMSE 
RMSE_Bandra = (sum((a$AQI - a$Predicted_Bandra_AQI)^2) / nrow(a)) ^ 0.5
RMSE_Sion = (sum((b$AQI - b$Predicted_Sion_AQI)^2) / nrow(b)) ^ 0.5

#MAD
MAD_Bandra <- sum(abs(a$Predicted_Bandra_AQI-mean(a$AQI)))/length(a$Predicted_Bandra_AQI)
MAD_Sion <- sum(abs(b$Predicted_Sion_AQI-mean(b$AQI)))/length(b$Predicted_Sion_AQI)

library(ie2misc)
#MAPE
MAPE_Bandra <- mape(a$Predicted_Bandra_AQI,a$AQI)
MAPE_Sion <- mape(b$Predicted_Sion_AQI,b$AQI)

###The Evaluation suggests that Multiple Linear Regression is not a good option

################################    End of Multiple Linear Regression    ####################################

##Running by Kanak has been done by changing file name and repeating code for changing location
##U can avoid reading csv if you have run the steps above


################################    Random Forest    ####################################

library(randomForest)
library(ie2misc)

data <- read.csv("BandraIndividual1.csv", header = T)
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )
# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]
daz <- subset(data, select = c(2:5))
aqi.rf <- randomForest(AQI ~ ., data=daz, mtry=3,
                       importance=TRUE, na.action=na.omit)
print(aqi.rf)

## Show "importance" of variables: higher value mean more important:
round(importance(aqi.rf), 2)
varImpPlot(aqi.rf)

forestPrediction <- predict(aqi.rf, datatest[,-1], type = "class")
(forestAcc <- 1- mean(forestPrediction != datatest$AQI))


###Evaluation with RMSE, MAD and MAPE
RMSE <- sqrt(mean((datatest$AQI-forestPrediction)^2))
MAD.NN <- sum(abs(forestPrediction-mean(datatest$AQI)))/length(forestPrediction)
MAPE.NN <- mape(forestPrediction,datatest$AQI)

###Plotting the Random Forest
plot(aqi.rf)


################################    End of Random Forest    ####################################


################################    SVR    ####################################

library(anfis)
library(e1071)

data <- read.csv("SionIndividual1.csv", header = T)
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )
# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]
daz <- subset(data, select = c(2:5))
#Scatter Plot
daz <- subset(data, select = c(2:5))
plot(daz)

#Regression with SVM
modelsvm <- svm(AQI ~.,daz)

#Predict using SVM regression
predYsvm <- predict(modelsvm, daz)

#Overlay SVM Predictions on Scatter Plot
points(daz$AQI, predYsvm,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model

#Find value of W
W = t(modelsvm$coefs) %*% modelsvm$SV

#Find value of b
b = modelsvm$rho

## RMSE for SVR Model

#Evaluation using RMSE,MAD and MAPE 
RMSEsvm=rmse(predYsvm,daz$AQI)
MADsvm <- sum(abs(predYsvm-mean(daz$AQI)))/length(predYsvm)
MAPEsvm <- mape(predYsvm,daz$AQI)

## Tuning SVR model by varying values of maximum allowable error and cost parameter

#Tune the SVM model
OptModelsvm=tune(svm, AQI ~., data=daz,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))

#Print optimum value of parameters
print(OptModelsvm)

#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)

################################    End of SVR   ####################################


################################    ANN    ####################################

# Read the Data
data = read.csv("BandraIndividual1.csv", header=T)

# Random sampling
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )

# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]

## Scale data for neural network

max = apply(subset(data, select = c(2:7)) , 2 , max)
min = apply(subset(data, select = c(2:7)), 2 , min)
scaled <- as.data.frame(scale(subset(data, select = c(2:7)), center = min, scale = max - min))

## Fit neural network 

# install library
install.packages("neuralnet ")

# load library
library(neuralnet)

# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]

# fit neural network
set.seed(2)
NN = neuralnet(AQI ~ SO2 + NOx + RSPM, trainNN, hidden = 3 , linear.output = T )

# plot neural network
plot(NN)


## Prediction using neural network

predict_testNN = compute(NN, testNN[,c(1:5)])
predict_testNN = (predict_testNN$net.result * (max(data$AQI) - min(data$AQI))) + min(data$AQI)

plot(datatest$AQI, predict_testNN, col='blue', pch=16, ylab = "predicted rating NN", xlab = "real rating", title(main = "AQI"))

abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$AQI - predict_testNN)^2) / nrow(datatest)) ^ 0.5

#MAD
MAD.NN <- sum(abs(predict_testNN-mean(datatest$AQI)))/length(predict_testNN)

#MAPE
MAPE.NN <- mape(predict_testNN,datatest$AQI)



################################    End of ANN    ####################################