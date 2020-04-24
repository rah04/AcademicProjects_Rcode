library(mlbench)
library(caret)
library(Metrics)

dbk <- read.csv("H:/RIC/Arizona_Diamondbacks/Final.csv", header = TRUE)

##Checking for linearity##
plot(x=dbk$W, y=dbk$Attendance.G)
plot(x=dbk$PA, y=dbk$Attendance.G)
plot(x=dbk$AB, y=dbk$Attendance.G)

library(caTools) 
set.seed(123) 
training_set = dbk [2:21, 5:37]
test_set = dbk [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
dbk_model = lm(formula = Attendance.G ~ ., 
               data = training_set) 
summary(dbk_model)
# Predicting the Test set results 
dbk_prediction = predict(dbk_model, newdata = test_set) 

#Evaluating the model
dbk_rmse <- (rmse(dbk_prediction,test_set$Attendance.G))/test_set$Attendance.G
dbk_mad <- (mad(dbk_prediction,test_set$Attendance.G))/test_set$Attendance.G
dbk_mape <- (mape(dbk_prediction,test_set$Attendance.G))/100
Team = "Arizona Diamondbacks"
dbk_evaluation <- cbind(Team, dbk_rmse, dbk_mad, dbk_mape)
colnames(dbk_evaluation)[2] <- "RMSE"
colnames(dbk_evaluation)[3] <- "MAD"
colnames(dbk_evaluation)[4] <- "MAPE"
dbk_evaluation
dbk_mlr_output <- cbind(Team, test_set$Attendance.G,dbk_prediction)
colnames(dbk_mlr_output)[2] <- "Actual"
colnames(dbk_evaluation)[3] <- "Predicted"
dbk_mlr_output
__________________________________________________________________________________________________________________

brv <- read.csv("H:/RIC/Atlanta Braves/Final.csv", header = TRUE)

set.seed(132) 
training_set_brv = brv [2:21, 5:37]
test_set_brv = brv [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
brv_model = lm(formula = Attendance.G ~ ., 
               data = training_set_brv) 
summary(brv_mode)

# Predicting the Test set results 
brv_prediction = predict(brv_model, newdata = test_set_brv) 

#Evaluating the model
brv_rmse <- (rmse(brv_prediction,test_set_brv$Attendance.G))/test_set_brv$Attendance.G
brv_mad <- (mad(brv_prediction,test_set_brv$Attendance.G))/test_set_brv$Attendance.G
brv_mape <- mape(brv_prediction,test_set_brv$Attendance.G)/100
Team = "Atlanta Braves"
brv_evaluation <- cbind(Team, brv_rmse, brv_mad, brv_mape)
colnames(brv_evaluation)[2] <- "RMSE"
colnames(brv_evaluation)[3] <- "MAD"
colnames(brv_evaluation)[4] <- "MAPE"
brv_evaluation
brv_mlr_output <- cbind(Team, test_set_brv$Attendance.G,brv_prediction)
colnames(brv_mlr_output)[2] <- "Actual"
colnames(brv_evaluation)[3] <- "Predicted"
brv_mlr_output
___________________________________________________________________________________________________________________

ori <- read.csv("H:/RIC/baltimore Orioles/Final.csv", header = TRUE)

set.seed(72) 
test_set_ori_ori = ori [2:21, 5:37]
test_set = ori [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
ori_model = lm(formula = Attendance.G ~ ., 
               data = test_set_ori_ori) 
summary(ori_model)

# Predicting the Test set results 
ori_prediction = predict(ori_model, newdata = test_set) 

#Evaluating the model
ori_rmse <- (rmse(test_set$Attendance.G,ori_prediction))/test_set$Attendance.G
ori_mad <- (mad(test_set$Attendance.G,ori_prediction))/test_set$Attendance.G
ori_mape <- (mape(test_set$Attendance.G,ori_prediction))/100
Team = "Baltimore Orioles"
ori_evaluation <- cbind(Team, ori_rmse, ori_mad, ori_mape)
colnames(ori_evaluation)[2] <- "RMSE"
colnames(ori_evaluation)[3] <- "MAD"
colnames(ori_evaluation)[4] <- "MAPE"
ori_evaluation
ori_mlr_output <- cbind(Team, test_set$Attendance.G,ori_prediction)
colnames(ori_mlr_output)[2] <- "Actual"
colnames(ori_evaluation)[3] <- "Predicted"
ori_mlr_output
__________________________________________________________________________________________________________________

redsox <- read.csv("H:/RIC/Boston Red Sox/Final.csv", header = TRUE)

set.seed(60) 
training_set_redsox = redsox [2:21, 5:37]
test_set_redsox = redsox [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
redsox_model = lm(formula = Attendance.G ~ ., 
                  data = training_set_redsox) 
summary(redsox_model)

# Predicting the Test set results 
redsox_prediction = predict(redsox_model, newdata = test_set_redsox) 

#Evaluating the model
redsox_rmse <- (rmse(redsox_prediction,test_set_redsox$Attendance.G))/test_set_redsox$Attendance.G
redsox_mad <- (mad(redsox_prediction,test_set_redsox$Attendance.G))/test_set_redsox$Attendance.G
redsox_mape <- (mape(redsox_prediction,test_set_redsox$Attendance.G))/100
Team = "Boston Red Sox"
redsox_evaluation <- cbind(Team, redsox_rmse, redsox_mad, redsox_mape)
colnames(redsox_evaluation)[2] <- "RMSE"
colnames(redsox_evaluation)[3] <- "MAD"
colnames(redsox_evaluation)[4] <- "MAPE"
redsox_evaluation
redsox_mlr_output <- cbind(Team, test_set_redsox$Attendance.G,redsox_prediction)
colnames(redsox_mlr_output)[2] <- "Actual"
colnames(redsox_evaluation)[3] <- "Predicted"
redsox_mlr_output
_________________________________________________________________________________________________________________

cubs <- read.csv("H:/RIC/Chicago Cubs/Final.csv", header = TRUE)

set.seed(50) 
training_set_cubs = cubs [2:119, 5:37]
test_set_cubs = cubs [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
cubs_model = lm(formula = Attendance.G ~ ., 
                data = training_set_cubs) 
summary(cubs_model)

# Predicting the Test set results 
cubs_prediction = predict(cubs_model, newdata = test_set_cubs) 

#Evaluating the model
cubs_rmse <- (rmse(cubs_prediction,test_set_cubs$Attendance.G))/test_set_cubs$Attendance.G
cubs_mad <- (mad(cubs_prediction,test_set_cubs$Attendance.G))/test_set_cubs$Attendance.G
cubs_mape <- (mape(cubs_prediction,test_set_cubs$Attendance.G))/100
Team = "Chicago Cubs"
cubs_evaluation <- cbind(Team, cubs_rmse, cubs_mad, cubs_mape)
colnames(cubs_evaluation)[2] <- "RMSE"
colnames(cubs_evaluation)[3] <- "MAD"
colnames(cubs_evaluation)[4] <- "MAPE"
cubs_evaluation
cubs_mlr_output <- cbind(Team, test_set_cubs$Attendance.G,cubs_prediction)
colnames(cubs_mlr_output)[2] <- "Actual"
colnames(cubs_evaluation)[3] <- "Predicted"
cubs_mlr_output
_________________________________________________________________________________________________________________

whitesox <- read.csv("H:/RIC/Chicago White Sox/Final.csv", header = TRUE)

set.seed(600) 
training_set_whitesox = whitesox [2:119, 5:37]
test_set_whitesox = whitesox [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
whitesox_model = lm(formula = Attendance.G ~ ., 
                    data = training_set_whitesox) 
summary(whitesox_model)

# Predicting the Test set results 
whitesox_prediction = predict(whitesox_model, newdata = test_set_whitesox) 

#Evaluating the model
whitesox_rmse <- (((whitesox_prediction - test_set_whitesox$Attendance.G)^2/nrow(whitesox))^0.5)/100
whitesox_mad <- ((whitesox_prediction - test_set_whitesox$Attendance.G)/nrow(whitesox))/10000
whitesox_mape <- (((test_set_whitesox$Attendance.G - whitesox_prediction)/(test_set_whitesox$Attendance.G))/nrow(whitesox))*100
Team = "Chicago White Sox"
whitesox_evaluation <- cbind(Team, whitesox_rmse, whitesox_mad, whitesox_mape)
colnames(whitesox_evaluation)[2] <- "RMSE"
colnames(whitesox_evaluation)[3] <- "MAD"
colnames(whitesox_evaluation)[4] <- "MAPE"
whitesox_evaluation
whitesox_mlr_output <- cbind(Team, test_set_whitesox$Attendance.G,whitesox_prediction)
colnames(whitesox_mlr_output)[2] <- "Actual"
colnames(whitesox_evaluation)[3] <- "Predicted"
whitesox_mlr_output
_________________________________________________________________________________________________________________

reds <- read.csv("H:/RIC/Cincinnati Reds/Final.csv", header = TRUE)

set.seed(80) 
training_set_reds = reds [2:138, 5:37]
test_set_reds = reds [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
reds_model = lm(formula = Attendance.G ~ ., 
                data = training_set_reds) 
summary(reds_model)

# Predicting the Test set results 
reds_prediction = predict(reds_model, newdata = test_set_reds) 

#Evaluating the model
reds_rmse <- (((reds_prediction - test_set_reds$Attendance.G)^2/nrow(reds))^0.5)/100
reds_mad <- ((test_set_reds$Attendance.G - reds_prediction)/nrow(reds))/10000
reds_mape <- (((test_set_reds$Attendance.G - reds_prediction)/(test_set_reds$Attendance.G))/nrow(reds))*100
Team = "Cincinnati Reds"
reds_evaluation <- cbind(Team, reds_rmse, reds_mad, reds_mape)
colnames(reds_evaluation)[2] <- "RMSE"
colnames(reds_evaluation)[3] <- "MAD"
colnames(reds_evaluation)[4] <- "MAPE"
reds_evaluation
reds_mlr_output <- cbind(Team, test_set_reds$Attendance.G,reds_prediction)
colnames(reds_mlr_output)[2] <- "Actual"
colnames(reds_evaluation)[3] <- "Predicted"
reds_mlr_output
_________________________________________________________________________________________________________________

indians <- read.csv("H:/RIC/Cleveland Indians/Final.csv", header = TRUE)

set.seed(98) 
training_set_indians = indians [2:119, 5:37]
test_set_indians = indians [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
indians_model = lm(formula = Attendance.G ~ ., 
                   data = training_set_indians) 
summary(indians_model)

# Predicting the Test set results 
indians_prediction <- predict(indians_model, newdata = test_set_indians) 
indians_prediction <- as.numeric(indians_prediction)
#Evaluating the model
indians_rmse <- (rmse(indians_prediction,test_set_indians$Attendance.G))/test_set_indians$Attendance.G
indians_mad <- (mad(indians_prediction,test_set_indians$Attendance.G))/test_set_indians$Attendance.G
indians_mape <- (mape(indians_prediction,test_set_indians$Attendance.G))/100
Team = "Cleveland Indians"
indians_evaluation <- cbind(Team,indians_rmse, indians_mad, indians_mape)
colnames(indians_evaluation)[2] <- "RMSE"
colnames(indians_evaluation)[3] <- "MAD"
colnames(indians_evaluation)[4] <- "MAPE"
indians_evaluation
indians_mlr_output <- cbind(Team, test_set_indians$Attendance.G,indians_prediction)
colnames(indians_mlr_output)[2] <- "Actual"
colnames(indians_mlr_output)[3] <- "Predicted"
indians_mlr_output
______________________________________________________________________________________________________________

rockies <- read.csv("H:/RIC/Colorado Rockies/Final.csv", header = TRUE)

set.seed(900) 
training_set_rockies = rockies [2:27, 5:37]
test_set_rockies = rockies [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
rockies_model = lm(formula = Attendance.G ~ ., 
                   data = training_set_rockies) 
summary(rockies_model)

# Predicting the Test set results 
rockies_prediction = predict(rockies_model, newdata = test_set_rockies) 

#Evaluating the model
rockies_rmse <- (rmse(rockies_prediction,test_set_rockies$Attendance.G))/test_set_rockies$Attendance.G
rockies_mad <- (mad(rockies_prediction,test_set_rockies$Attendance.G))/test_set_rockies$Attendance.G
rockies_mape <- (mape(rockies_prediction,test_set_rockies$Attendance.G))/100
Team = "Colorado Rockies"
rockies_evaluation <- cbind(Team, rockies_rmse, rockies_mad, rockies_mape)
colnames(rockies_evaluation)[2] <- "RMSE"
colnames(rockies_evaluation)[3] <- "MAD"
colnames(rockies_evaluation)[4] <- "MAPE"
rockies_evaluation
rockies_mlr_output <- cbind(Team, test_set_rockies$Attendance.G,rockies_prediction)
colnames(rockies_mlr_output)[2] <- "Actual"
colnames(rockies_evaluation)[3] <- "Predicted"
rockies_mlr_output
________________________________________________________________________________________________________________

tigers <- read.csv("H:/RIC/Detroit Tigers/Final.csv", header = TRUE)

set.seed(909) 
training_set_tigers = tigers [2:119, 5:37]
test_set_tigers = tigers [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
tigers_model = lm(formula = Attendance.G ~ ., 
                  data = training_set_tigers) 
summary(tigers_model)

# Predicting the Test set results 
tigers_prediction = predict(tigers_model, newdata = test_set_tigers) 

#Evaluating the model
tigers_rmse <- (rmse(tigers_prediction,test_set_tigers$Attendance.G))/test_set_tigers$Attendance.G
tigers_mad <- (mad(tigers_prediction,test_set_tigers$Attendance.G))/test_set_tigers$Attendance.G
tigers_mape <- (mape(tigers_prediction,test_set_tigers$Attendance.G))/100
Team = "Detroit Tigers"
tigers_evaluation <- cbind(Team, tigers_rmse, tigers_mad, tigers_mape)
colnames(tigers_evaluation)[2] <- "RMSE"
colnames(tigers_evaluation)[3] <- "MAD"
colnames(tigers_evaluation)[4] <- "MAPE"
tigers_evaluation
tigers_mlr_output <- cbind(Team, test_set_tigers$Attendance.G,tigers_prediction)
colnames(tigers_mlr_output)[2] <- "Actual"
colnames(tigers_evaluation)[3] <- "Predicted"
tigers_mlr_output
_________________________________________________________________________________________________________________

astros <- read.csv("H:/RIC/Houston Astros/Final.csv", header = TRUE)

set.seed(999) 
training_set_astros = astros [2:58, 5:37]
test_set_astros = astros [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
astros_model = lm(formula = Attendance.G ~ ., 
                  data = training_set_astros) 
summary(astros_model)

# Predicting the Test set results 
astros_prediction = predict(astros_model, newdata = test_set_astros) 

#Evaluating the model
astros_rmse <- (rmse(astros_prediction,test_set_astros$Attendance.G))/test_set_astros$Attendance.G
astros_mad <- (mad(astros_prediction,test_set_astros$Attendance.G))/test_set_astros$Attendance.G
astros_mape <- (mape(astros_prediction - test_set_astros$Attendance.G))/100
Team = "Houston Astros"
astros_evaluation <- cbind(Team, astros_rmse, astros_mad, astros_mape)
colnames(astros_evaluation)[2] <- "RMSE"
colnames(astros_evaluation)[3] <- "MAD"
colnames(astros_evaluation)[4] <- "MAPE"
astros_evaluation
astros_mlr_output <- cbind(Team, test_set_astros$Attendance.G,astros_prediction)
colnames(astros_mlr_output)[2] <- "Actual"
colnames(astros_evaluation)[3] <- "Predicted"
astros_mlr_output
_________________________________________________________________________________________________________________

royals <- read.csv("H:/RIC/Kansas City Royals/Final.csv", header = TRUE)

set.seed(899) 
training_set_royals = royals [2:51, 5:37]
test_set_royals = royals [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
royals_model = lm(formula = Attendance.G ~ ., 
                  data = training_set_royals) 
summary(royals_model)

# Predicting the Test set results 
royals_prediction = predict(royals_model, newdata = test_set_royals) 

#Evaluating the model
royals_rmse <- (rmse(royals_prediction,test_set_royals$Attendance.G))/test_set_royals$Attendance.G
royals_mad <- (mad(royals_prediction,test_set_royals$Attendance.G))/test_set_royals$Attendance.G
royals_mape <- (mape(royals_prediction,test_set_royals$Attendance.G))/100
Team = "Kansas City Royals"
royals_evaluation <- cbind(Team, royals_rmse, royals_mad, royals_mape)
colnames(royals_evaluation)[2] <- "RMSE"
colnames(royals_evaluation)[3] <- "MAD"
colnames(royals_evaluation)[4] <- "MAPE"
royals_evaluation
royals_mlr_output <- cbind(Team, test_set_royals$Attendance.G,royals_prediction)
colnames(royals_mlr_output)[2] <- "Actual"
colnames(royals_evaluation)[3] <- "Predicted"
royals_mlr_output
_________________________________________________________________________________________________________________

angels <- read.csv("H:/RIC/Los Angeles Angels/Final.csv", header = TRUE)

set.seed(699) 
training_set_angels = angels [2:51, 5:37]
test_set_angels = angels [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
angels_model = lm(formula = Attendance.G ~ ., 
                  data = training_set_angels) 
summary(angels_model)

# Predicting the Test set results 
angels_prediction = predict(angels_model, newdata = test_set_angels) 

#Evaluating the model
angels_rmse <- (rmse(angels_prediction,test_set_angels$Attendance.G))/test_set_angels$Attendance.G
angels_mad <- (mad(angels_prediction,test_set_angels$Attendance.G))/test_set_angels$Attendance.G
angels_mape <- (mape(angels_prediction,test_set_angels$Attendance.G))/100
Team = "Los Angeles Angels"
angels_evaluation <- cbind(Team, angels_rmse, angels_mad, angels_mape)
colnames(angels_evaluation)[2] <- "RMSE"
colnames(angels_evaluation)[3] <- "MAD"
colnames(angels_evaluation)[4] <- "MAPE"
angels_evaluation
angels_mlr_output <- cbind(Team, test_set_angels$Attendance.G,angels_prediction)
colnames(angels_mlr_output)[2] <- "Actual"
colnames(angels_evaluation)[3] <- "Predicted"
angels_mlr_output
_________________________________________________________________________________________________________________

dodgers <- read.csv("H:/RIC/Los Angeles Dodgers/Final.csv", header = TRUE)

set.seed(790) 
training_set_dodgers = dodgers [2:130, 5:37]
test_set_dodgers = dodgers [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
dodgers_model = lm(formula = Attendance.G ~ ., 
                   data = training_set_dodgers) 
summary(dodgers_model)

# Predicting the Test set results 
dodgers_prediction = predict(dodgers_model, newdata = test_set_dodgers) 

#Evaluating the model
dodgers_rmse <- (rmse(dodgers_prediction,test_set_dodgers$Attendance.G))/test_set_dodgers$Attendance.G
dodgers_mad <- (mad(dodgers_prediction,test_set_dodgers$Attendance.G))/test_set_dodgers$Attendance.G
dodgers_mape <- (mape(dodgers_prediction,test_set_dodgers$Attendance.G))/100
Team = "Los Angeles Dodgers"
dodgers_evaluation <- cbind(Team, dodgers_rmse, dodgers_mad, dodgers_mape)
colnames(dodgers_evaluation)[2] <- "RMSE"
colnames(dodgers_evaluation)[3] <- "MAD"
colnames(dodgers_evaluation)[4] <- "MAPE"
dodgers_evaluation
dodgers_mlr_output <- cbind(Team, test_set_dodgers$Attendance.G,dodgers_prediction)
colnames(dodgers_mlr_output)[2] <- "Actual"
colnames(dodgers_evaluation)[3] <- "Predicted"
dodgers_mlr_output
_________________________________________________________________________________________________________________

marlins <- read.csv("H:/RIC/Miami Marlins/Final.csv", header = TRUE)

set.seed(599) 
training_set_marlins = marlins [2:27, 5:37]
test_set_marlins = marlins [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
marlins_model = lm(formula = Attendance.G ~ ., 
                   data = training_set_marlins) 
summary(marlins_model)

# Predicting the Test set results 
marlins_prediction = predict(marlins_model, newdata = test_set_marlins) 

#Evaluating the model
marlins_rmse <- (rmse(marlins_prediction,test_set_marlins$Attendance.G))/test_set_marlins$Attendance.G
marlins_mad <- (mad(marlins_prediction,test_set_marlins$Attendance.G))/test_set_marlins$Attendance.G
marlins_mape <- (mape(marlins_prediction,test_set_marlins$Attendance.G))/100
Team = "Miami Marlins"
marlins_evaluation <- cbind(Team, marlins_rmse, marlins_mad, marlins_mape)
colnames(marlins_evaluation)[2] <- "RMSE"
colnames(marlins_evaluation)[3] <- "MAD"
colnames(marlins_evaluation)[4] <- "MAPE"
marlins_evaluation
marlins_mlr_output <- cbind(Team, test_set_marlins$Attendance.G,marlins_prediction)
colnames(marlins_mlr_output)[2] <- "Actual"
colnames(marlins_evaluation)[3] <- "Predicted"
marlins_mlr_output
_________________________________________________________________________________________________________________

brewers <- read.csv("H:/RIC/Milwaukee Brewers/Final.csv", header = TRUE)

set.seed(499) 
training_set_brewers = brewers [2:51, 5:37]
test_set_brewers = brewers [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
brewers_model = lm(formula = Attendance.G ~ ., 
                   data = training_set_brewers) 
summary(brewers_model)

# Predicting the Test set results 
brewers_prediction = predict(brewers_model, newdata = test_set_brewers) 

#Evaluating the model
brewers_rmse <- (rmse(brewers_prediction,test_set_brewers$Attendance.G))/test_set_brewers$Attendance.G
brewers_mad <- (mad(brewers_prediction,test_set_brewers$Attendance.G))/test_set_brewers$Attendance.G
brewers_mape <- (mape(brewers_prediction,test_set_brewers$Attendance.G))/100
Team = "Milwaukee Brewers"
brewers_evaluation <- cbind(Team, brewers_rmse, brewers_mad, brewers_mape)
colnames(brewers_evaluation)[2] <- "RMSE"
colnames(brewers_evaluation)[3] <- "MAD"
colnames(brewers_evaluation)[4] <- "MAPE"
brewers_evaluation
brewers_mlr_output <- cbind(Team, test_set_brewers$Attendance.G,brewers_prediction)
colnames(brewers_mlr_output)[2] <- "Actual"
colnames(brewers_evaluation)[3] <- "Predicted"
brewers_mlr_output
_________________________________________________________________________________________________________________

twins <- read.csv("H:/RIC/Minnesota Twins/Final.csv", header = TRUE)

set.seed(495) 
training_set_twins = twins [2:119, 5:37]
test_set_twins = twins [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
twins_model = lm(formula = Attendance.G ~ ., 
                 data = training_set_twins) 
summary(twins_model)

# Predicting the Test set results 
twins_prediction = predict(twins_model, newdata = test_set_twins) 

#Evaluating the model
twins_rmse <- (rmse(twins_prediction,test_set_twins$Attendance.G))/test_set_twins$Attendance.G
twins_mad <- (mad(test_set_twins$Attendance.G,twins_prediction))/test_set_twins$Attendance.G
twins_mape <- (mape(test_set_twins$Attendance.G,twins_prediction))/100
Team = "Minnesota Twins"
twins_evaluation <- cbind(Team, twins_rmse, twins_mad, twins_mape)
colnames(twins_evaluation)[2] <- "RMSE"
colnames(twins_evaluation)[3] <- "MAD"
colnames(twins_evaluation)[4] <- "MAPE"
twins_evaluation
twins_mlr_output <- cbind(Team, test_set_twins$Attendance.G,twins_prediction)
colnames(twins_mlr_output)[2] <- "Actual"
colnames(twins_evaluation)[3] <- "Predicted"
twins_mlr_output
_________________________________________________________________________________________________________________

mets <- read.csv("H:/RIC/New York Mets/Final.csv", header = TRUE)

set.seed(455) 
training_set_mets = mets [2:58, 5:37]
test_set_mets = mets [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
mets_model = lm(formula = Attendance.G ~ ., 
                data = training_set_mets) 
summary(mets_model)

# Predicting the Test set results 
mets_prediction = predict(mets_model, newdata = test_set_mets) 

#Evaluating the model
mets_rmse <- (rmse(mets_prediction,test_set_mets$Attendance.G))/test_set_mets$Attendance.G
mets_mad <- (mad(test_set_mets$Attendance.G,mets_prediction))/test_set_mets$Attendance.G
mets_mape <- (mape(test_set_mets$Attendance.G,mets_prediction))/100
Team = "New York Mets"
mets_evaluation <- cbind(Team, mets_rmse, mets_mad, mets_mape)
colnames(mets_evaluation)[2] <- "RMSE"
colnames(mets_evaluation)[3] <- "MAD"
colnames(mets_evaluation)[4] <- "MAPE"
mets_evaluation
mets_mlr_output <- cbind(Team, test_set_mets$Attendance.G,mets_prediction)
colnames(mets_mlr_output)[2] <- "Actual"
colnames(mets_evaluation)[3] <- "Predicted"
mets_mlr_output
_________________________________________________________________________________________________________________

yankees <- read.csv("H:/RIC/New York Yankees/Final.csv", header = TRUE)

set.seed(405) 
training_set_yankees = yankees [2:117, 5:37]
test_set_yankees = yankees [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
yankees_model = lm(formula = Attendance.G ~ ., 
                   data = training_set_yankees) 
summary(yankees_model)

# Predicting the Test set results 
yankees_prediction = predict(yankees_model, newdata = test_set_yankees) 

#Evaluating the model
yankees_rmse <- (rmse(test_set_yankees$Attendance.G,yankees_prediction))/test_set_yankees$Attendance.G
yankees_mad <- (mad(test_set_yankees$Attendance.G,yankees_prediction))/test_set_yankees$Attendance.G
yankees_mape <- (mape(test_set_yankees$Attendance.G,yankees_prediction))/100
Team = "New York Yankees"
yankees_evaluation <- cbind(Team, yankees_rmse, yankees_mad, yankees_mape)
colnames(yankees_evaluation)[2] <- "RMSE"
colnames(yankees_evaluation)[3] <- "MAD"
colnames(yankees_evaluation)[4] <- "MAPE"
yankees_evaluation
yankees_mlr_output <- cbind(Team, test_set_yankees$Attendance.G,yankees_prediction)
colnames(yankees_mlr_output)[2] <- "Actual"
colnames(yankees_evaluation)[3] <- "Predicted"
yankees_mlr_output
_________________________________________________________________________________________________________________

athletics <- read.csv("H:/RIC/Oakland Athletics/Final.csv", header = TRUE)

set.seed(355) 
training_set_athletics = athletics [2:119, 5:37]
test_set_athletics = athletics [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
athletics_model = lm(formula = Attendance.G ~ ., 
                     data = training_set_athletics) 
summary(athletics_model)

# Predicting the Test set results 
athletics_prediction = predict(athletics_model, newdata = test_set_athletics) 

#Evaluating the model
athletics_rmse <- (rmse(athletics_prediction,test_set_athletics$Attendance.G))/test_set_athletics$Attendance.G
athletics_mad <- (mad(test_set_athletics$Attendance.G,athletics_prediction))/test_set_athletics$Attendance.G
athletics_mape <- (mape(test_set_athletics$Attendance.G,athletics_prediction))/100
Team = "Oakland Athletics"
athletics_evaluation <- cbind(Team, athletics_rmse, athletics_mad, athletics_mape)
colnames(athletics_evaluation)[2] <- "RMSE"
colnames(athletics_evaluation)[3] <- "MAD"
colnames(athletics_evaluation)[4] <- "MAPE"
athletics_evaluation
athletics_mlr_output <- cbind(Team, test_set_athletics$Attendance.G,athletics_prediction)
colnames(athletics_mlr_output)[2] <- "Actual"
colnames(athletics_evaluation)[3] <- "Predicted"
athletics_mlr_output
__________________________________________________________________________________________________________________

phillies <- read.csv("H:/RIC/Philadelphia Phillies/Final.csv", header = TRUE)

set.seed(256) 
training_set_phillies = phillies [2:119, 5:37]
test_set_phillies = phillies [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
phillies_model = lm(formula = Attendance.G ~ ., 
                    data = training_set_phillies) 
summary(phillies_model)

# Predicting the Test set results 
phillies_prediction = predict(phillies_model, newdata = test_set_phillies) 

#Evaluating the model
phillies_rmse <- (rmse(phillies_prediction,test_set_phillies$Attendance.G))/test_set_phillies$Attendance.G
phillies_mad <- (mad(test_set_phillies$Attendance.G,phillies_prediction))/test_set_phillies$Attendance.G
phillies_mape <- (mape(test_set_phillies$Attendance.G,phillies_prediction))/100
Team = "Philadelphia Phillies"
phillies_evaluation <- cbind(Team, phillies_rmse, phillies_mad, phillies_mape)
colnames(phillies_evaluation)[2] <- "RMSE"
colnames(phillies_evaluation)[3] <- "MAD"
colnames(phillies_evaluation)[4] <- "MAPE"
phillies_evaluation
phillies_mlr_output <- cbind(Team, test_set_phillies$Attendance.G,phillies_prediction)
colnames(phillies_mlr_output)[2] <- "Actual"
colnames(phillies_evaluation)[3] <- "Predicted"
phillies_mlr_output
_________________________________________________________________________________________________________________
pirates <- read.csv("H:/RIC/Pittsburgh Pirates/Final.csv", header = TRUE)

set.seed(357) 
training_set_pirates = pirates [2:130, 5:37]
test_set_pirates = pirates [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
pirates_model = lm(formula = Attendance.G ~ ., 
                   data = training_set_pirates) 
summary(pirates_model)

# Predicting the Test set results 
pirates_prediction = predict(pirates_model, newdata = test_set_pirates) 

#Evaluating the model
pirates_rmse <- (rmse(pirates_prediction,test_set_pirates$Attendance.G))/test_set_pirates$Attendance.G
pirates_mad <- (mad(pirates_prediction,test_set_pirates$Attendance.G))/test_set_pirates$Attendance.G
pirates_mape <- (mape(pirates_prediction,test_set_pirates$Attendance.G))/100
Team = "Pittsburgh Pirates"
pirates_evaluation <- cbind(Team, pirates_rmse, pirates_mad, pirates_mape)
colnames(pirates_evaluation)[2] <- "RMSE"
colnames(pirates_evaluation)[3] <- "MAD"
colnames(pirates_evaluation)[4] <- "MAPE"
pirates_evaluation
pirates_mlr_output <- cbind(Team, test_set_pirates$Attendance.G,pirates_prediction)
colnames(pirates_mlr_output)[2] <- "Actual"
colnames(pirates_evaluation)[3] <- "Predicted"
pirates_mlr_output
__________________________________________________________________________________________________________________

padres <- read.csv("H:/RIC/San Diego Padres/Final.csv", header = TRUE)

set.seed(328) 
training_set_padres = padres [2:51, 5:37]
test_set_padres = padres [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
padres_model = lm(formula = Attendance.G ~ ., 
                  data = training_set_padres) 
summary(padres_model)

# Predicting the Test set results 
padres_prediction = predict(padres_model, newdata = test_set_padres) 

#Evaluating the model
padres_rmse <- (rmse(padres_prediction,test_set_padres$Attendance.G))/test_set_padres$Attendance.G
padres_mad <- (mad(test_set_padres$Attendance.G,padres_prediction))/test_set_padres$Attendance.G
padres_mape <- (mape(test_set_padres$Attendance.G,padres_prediction))/100
Team = "San Diego Padres"
padres_evaluation <- cbind(Team, padres_rmse, padres_mad, padres_mape)
colnames(padres_evaluation)[2] <- "RMSE"
colnames(padres_evaluation)[3] <- "MAD"
colnames(padres_evaluation)[4] <- "MAPE"
padres_evaluation
padres_mlr_output <- cbind(Team, test_set_padres$Attendance.G,padres_prediction)
colnames(padres_mlr_output)[2] <- "Actual"
colnames(padres_evaluation)[3] <- "Predicted"
padres_mlr_output
_________________________________________________________________________________________________________________

giants <- read.csv("H:/RIC/San Fransisco Giants/Final.csv", header = TRUE)

set.seed(117) 
training_set_giants = giants [2:119, 5:37]
test_set_giants = giants [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
giants_model = lm(formula = Attendance.G ~ ., 
                  data = training_set_giants) 
summary(giants_model)

# Predicting the Test set results 
giants_prediction = predict(giants_model, newdata = test_set_giants) 

#Evaluating the model
giants_rmse <- (rmse(giants_prediction,test_set_giants$Attendance.G))/test_set_giants$Attendance.G
giants_mad <- (mad(giants_prediction,test_set_giants$Attendance.G))/test_set_giants$Attendance.G
giants_mape <- (mape(giants_prediction,test_set_giants$Attendance.G))/100
Team = "San Fransisco Giants"
giants_evaluation <- cbind(Team, giants_rmse, giants_mad, giants_mape)
colnames(giants_evaluation)[2] <- "RMSE"
colnames(giants_evaluation)[3] <- "MAD"
colnames(giants_evaluation)[4] <- "MAPE"
giants_evaluation
giants_mlr_output <- cbind(Team, test_set_giants$Attendance.G,giants_prediction)
colnames(giants_mlr_output)[2] <- "Actual"
colnames(giants_evaluation)[3] <- "Predicted"
giants_mlr_output
_________________________________________________________________________________________________________________

mariners <- read.csv("H:/RIC/Seattle Mariners/Final.csv", header = TRUE)

set.seed(144) 
training_set_mariners = mariners [2:43, 5:37]
test_set_mariners = mariners [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
mariners_model = lm(formula = Attendance.G ~ ., 
                    data = training_set_mariners) 
summary(mariners_model)

# Predicting the Test set results 
mariners_prediction = predict(mariners_model, newdata = test_set_mariners) 

#Evaluating the model
mariners_rmse <- (rmse(mariners_prediction,test_set_mariners$Attendance.G))/test_set_mariners$Attendance.G
mariners_mad <- (mad(test_set_mariners$Attendance.G,mariners_prediction))/test_set_mariners$Attendance.G
mariners_mape <- (mape(test_set_mariners$Attendance.G,mariners_prediction))/100
Team = "Seattle Mariners"
mariners_evaluation <- cbind(Team, mariners_rmse, mariners_mad, mariners_mape)
colnames(mariners_evaluation)[2] <- "RMSE"
colnames(mariners_evaluation)[3] <- "MAD"
colnames(mariners_evaluation)[4] <- "MAPE"
mariners_evaluation
mariners_mlr_output <- cbind(Team, test_set_mariners$Attendance.G,mariners_prediction)
colnames(mariners_mlr_output)[2] <- "Actual"
colnames(mariners_evaluation)[3] <- "Predicted"
mariners_mlr_output
_________________________________________________________________________________________________________________

cardinals <- read.csv("H:/RIC/St. Louis Cardinals/Final.csv", header = TRUE)

set.seed(124) 
training_set_cardinals = cardinals [2:119, 5:37]
test_set_cardinals = cardinals [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
cardinals_model = lm(formula = Attendance.G ~ ., 
                     data = training_set_cardinals) 
summary(cardinals_model)

# Predicting the Test set results 
cardinals_prediction = predict(cardinals_model, newdata = test_set_cardinals) 

#Evaluating the model
cardinals_rmse <- (rmse(cardinals_prediction,test_set_cardinals$Attendance.G))/test_set_cardinals$Attendance.G
cardinals_mad <- (mad(cardinals_prediction,test_set_cardinals$Attendance.G))/test_set_cardinals$Attendance.G
cardinals_mape <- (mape(cardinals_prediction,test_set_cardinals$Attendance.G))/100
Team = "St. Louis Cardinals"
cardinals_evaluation <- cbind(Team, cardinals_rmse, cardinals_mad, cardinals_mape)
colnames(cardinals_evaluation)[2] <- "RMSE"
colnames(cardinals_evaluation)[3] <- "MAD"
colnames(cardinals_evaluation)[4] <- "MAPE"
cardinals_evaluation
cardinals_mlr_output <- cbind(Team, test_set_cardinals$Attendance.G,cardinals_prediction)
colnames(cardinals_mlr_output)[2] <- "Actual"
colnames(cardinals_evaluation)[3] <- "Predicted"
cardinals_mlr_output
_________________________________________________________________________________________________________________

rays <- read.csv("H:/RIC/Tampa Bay Rays/Final.csv", header = TRUE)

set.seed(129) 
training_set_rays = rays [2:119, 5:37]
test_set_rays = rays [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
rays_model = lm(formula = Attendance.G ~ ., 
                data = training_set_rays) 
summary(rays_model)

# Predicting the Test set results 
rays_prediction = predict(rays_model, newdata = test_set_rays) 

#Evaluating the model
rays_rmse <- (rmse(rays_prediction,test_set_rays$Attendance.G))/test_set_rays$Attendance.G
rays_mad <- (mad(rays_prediction,test_set_rays$Attendance.G))/test_set_rays$Attendance.G
rays_mape <- (mape(rays_prediction,test_set_rays$Attendance.G))/100
Team = "Tampa Bay Rays"
rays_evaluation <- cbind(Team, rays_rmse, rays_mad, rays_mape)
colnames(rays_evaluation)[2] <- "RMSE"
colnames(rays_evaluation)[3] <- "MAD"
colnames(rays_evaluation)[4] <- "MAPE"
rays_evaluation
rays_mlr_output <- cbind(Team, test_set_rays$Attendance.G,rays_prediction)
colnames(rays_mlr_output)[2] <- "Actual"
colnames(rays_evaluation)[3] <- "Predicted"
rays_mlr_output
_________________________________________________________________________________________________________________

rangers <- read.csv("H:/RIC/Texas Rangers/Final.csv", header = TRUE)

set.seed(196) 
training_set_rangers = rangers [2:119, 5:37]
test_set_rangers = rangers [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
rangers_model = lm(formula = Attendance.G ~ ., 
                   data = training_set_rangers) 
summary(rangers_model)

# Predicting the Test set results 
rangers_prediction = predict(rangers_model, newdata = test_set_rangers) 

#Evaluating the model
rangers_rmse <- (rmse(rangers_prediction,test_set_rangers$Attendance.G))/test_set_rangers$Attendance.G
rangers_mad <- (mad(rangers_prediction,test_set_rangers$Attendance.G))/test_set_rangers$Attendance.G
rangers_mape <- (mape(rangers_prediction,test_set_rangers$Attendance.G))/100
Team = "Texas Rangers"
rangers_evaluation <- cbind(Team, rangers_rmse, rangers_mad, rangers_mape)
colnames(rangers_evaluation)[2] <- "RMSE"
colnames(rangers_evaluation)[3] <- "MAD"
colnames(rangers_evaluation)[4] <- "MAPE"
rangers_evaluation
rangers_mlr_output <- cbind(Team, test_set_rangers$Attendance.G,rangers_prediction)
colnames(rangers_mlr_output)[2] <- "Actual"
colnames(rangers_evaluation)[3] <- "Predicted"
rangers_mlr_output
_________________________________________________________________________________________________________________

bluejays <- read.csv("H:/RIC/Toronto Blue Jays/Final.csv", header = TRUE)

set.seed(796) 
training_set_bluejays = bluejays [2:119, 5:37]
test_set_bluejays = bluejays [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
bluejays_model = lm(formula = Attendance.G ~ ., 
                    data = training_set_bluejays) 
summary(bluejays_model)

# Predicting the Test set results 
bluejays_prediction = predict(bluejays_model, newdata = test_set_bluejays) 

#Evaluating the model
bluejays_rmse <- (((bluejays_prediction - test_set_bluejays$Attendance.G)^2/nrow(bluejays))^0.5)/100
bluejays_mad <- ((bluejays_prediction - test_set_bluejays$Attendance.G)/nrow(bluejays))/10000
bluejays_mape <- (((bluejays_prediction - test_set_bluejays$Attendance.G)/(test_set_bluejays$Attendance.G))/nrow(bluejays))*100
Team = "Toronto Blue Jays"
bluejays_evaluation <- cbind(Team, bluejays_rmse, bluejays_mad, bluejays_mape)
colnames(bluejays_evaluation)[2] <- "RMSE"
colnames(bluejays_evaluation)[3] <- "MAD"
colnames(bluejays_evaluation)[4] <- "MAPE"
bluejays_evaluation
bluejays_mlr_output <- cbind(Team, test_set_bluejays$Attendance.G,bluejays_prediction)
colnames(bluejays_mlr_output)[2] <- "Actual"
colnames(bluejays_evaluation)[3] <- "Predicted"
bluejays_mlr_output
_________________________________________________________________________________________________________________

nationals <- read.csv("H:/RIC/Washington Nationals/Final.csv", header = TRUE)

set.seed(705) 
training_set_nationals = nationals [2:119, 5:37]
test_set_nationals = nationals [1, 5:37 ]

# Fitting Multiple Linear Regression to the Training set 
nationals_model = lm(formula = Attendance.G ~ ., 
                     data = training_set_nationals) 
summary(nationals_model)

# Predicting the Test set results 
nationals_prediction = predict(nationals_model, newdata = test_set_nationals) 

#Evaluating the model
nationals_rmse <- (rmse(nationals_prediction,test_set_nationals$Attendance.G))/test_set_nationals$Attendance.G
nationals_mad <- (mad(nationals_prediction,test_set_nationals$Attendance.G))/test_set_nationals$Attendance.G
nationals_mape <- (mape(nationals_prediction,test_set_nationals$Attendance.G))/100
Team = "Washington Nationals"
nationals_evaluation <- cbind(Team, nationals_rmse, nationals_mad, nationals_mape)
colnames(nationals_evaluation)[2] <- "RMSE"
colnames(nationals_evaluation)[3] <- "MAD"
colnames(nationals_evaluation)[4] <- "MAPE"
nationals_evaluation
nationals_mlr_output <- cbind(Team, test_set_nationals$Attendance.G,nationals_prediction)
colnames(nationals_mlr_output)[2] <- "Actual"
colnames(nationals_evaluation)[3] <- "Predicted"
nationals_mlr_output
_________________________________________________________________________________________________________________

write.csv(model_evaluation <- rbind(dbk_evaluation, brv_evaluation, ori_evaluation, redsox_evaluation, cubs_evaluation,
                          whitesox_evaluation, reds_evaluation, indians_evaluation, rockies_evaluation, tigers_evaluation, 
                          astros_evaluation, royals_evaluation, angels_evaluation, dodgers_evaluation, marlins_evaluation,
                          brewers_evaluation, twins_evaluation, mets_evaluation, yankees_evaluation, athletics_evaluation, 
                          phillies_evaluation, pirates_evaluation, padres_evaluation, giants_evaluation, mariners_evaluation, 
                          cardinals_evaluation, rays_evaluation, rangers_evaluation, bluejays_evaluation, nationals_evaluation), 
                          "H:/RIC/Evaluation/MLR_Evaluation.csv")
___________________________________________________________________________________________________________________

write.csv(model_mlr_output <- rbind(dbk_mlr_output, brv_mlr_output, ori_mlr_output, redsox_mlr_output, cubs_mlr_output,
                                    whitesox_mlr_output, reds_mlr_output, indians_mlr_output, rockies_mlr_output, tigers_mlr_output, 
                                    astros_mlr_output, royals_mlr_output, angels_mlr_output, dodgers_mlr_output, marlins_mlr_output,
                                    brewers_mlr_output, twins_mlr_output, mets_mlr_output, yankees_mlr_output, athletics_mlr_output, 
                                    phillies_mlr_output, pirates_mlr_output, padres_mlr_output, giants_mlr_output, mariners_mlr_output, 
                                    cardinals_mlr_output, rays_mlr_output, rangers_mlr_output, bluejays_mlr_output, nationals_mlr_output), 
                                    "H:/RIC/Evaluation/MLR_Output.csv")
