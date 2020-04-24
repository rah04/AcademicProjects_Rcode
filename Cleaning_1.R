library(tidyr)
library(dplyr)
library(stringr)

#import the ] seperated value file
read_data <- read.csv("H:\\Mimimum_Wages.csv")
gsub(read_data, "[]", " ")

#converting the contents into character
cellvalue <- as.character(read_data$clean_vecf)

#splitting the contents by "]" sign
splitvalue <- strsplit(cellvalue,"]")

#separating teh contents of each row into multiple rows
data <- NULL
for(i in 1:length(splitvalue)){
  rowdata <- unlist(splitvalue[i])
  data <- append(data,rowdata)
}
#deleting the redundant data in rows
data <- data[-c(5,7,10,13,16,17,19,22,27,31,33,35,39,42,46,48,54,56,64,67,69)]

#spltting the contents by ":" sign
alldata <- str_split_fixed(data,":",2)

#writing the contents in csv file
write.csv(alldata,"Mimimum_Wages.csv")
