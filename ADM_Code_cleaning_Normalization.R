

setwd("C:/Users/Murtaza/Downloads")
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

library(corrplot)
library(devtools)
library(factoextra)
library(normalr)

#correlation plot of sion area for features selection
siondata <- read.csv("Sion.csv", na.strings = "")
sionplot <- cor(subset(siondata, select=c(2:5)))
corrplot(sionplot, method = "number")

#correlation plot of Bandra area for features selection
bandradata <- read.csv("Bandra.csv", na.strings = "")
bandraplot <- cor(subset(bandradata, select=c(2:5)))
corrplot(bandraplot, method = "number")

#normalizing data for sion area for features scaling
library(class)
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
sion_n<-read.csv("Sion_Individual.csv")
siondata_n <- subset(sion_n,select = c(4,5))
siondata_n <- as.data.frame(lapply(siondata_n, normalize))
sion_n$NormRSPM<-siondata_n$RSPM
sion_n$NormAQI<-siondata_n$AQI
sion_n <- sion_n[-c(4,5)]
write.csv(sion_n,file = "SionIndividual1.csv",row.names = F)

#normalizing data for Bandra area for features scaling
bandra_n <- read.csv("BandraIndividual.csv")
bandradata_n <- subset(bandra_n,select = c(4,5))
bandradata_n <- as.data.frame(lapply(bandradata_n, normalize))
bandra_n$NormRSPM<-bandradata_n$RSPM
bandra_n$NormAQI<-bandradata_n$AQI
bandra_n <- bandra_n[-c(4,5)]
write.csv(bandra_n,file = "BandraIndividual1.csv",row.names = F)

#finding the correlation after normalization
#for sion area
siondata_r <- read.csv("H:/RIC/Arizona_Diamonbacks/History.csv", na.strings = "")
sionplot_r <- cor(subset(siondata_r, select=c(3:16)))
corrplot(sionplot_r, method = "number")

#for bandra area
bandradata_r <- read.csv("BandraIndividual1.csv", na.strings = "")
bandraplot_r <- cor(subset(bandradata_r, select=c(2:5)))
corrplot(bandraplot_r, method = "number")
