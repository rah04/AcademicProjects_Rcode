library(randomForest)
library(ie2misc)
library(caTools)
library(Metrics)
library(stats)

dbk_rf <- read.csv("H:/RIC/Arizona_Diamondbacks/Final.csv", header = TRUE)
training_dbk_rf <- dbk_rf [2:21, c(6,8:9,14:16,19,22,26,29,31:36)]
test_dbk_rf <- dbk_rf [1, c(6,8:9,14:16,19,22,26,29,31:36)]

dbk_rf_model <- randomForest(Attendance.G ~ ., data = training_dbk_rf, ntree = 400, mtry = 12, importance = TRUE)
importance(dbk_rf_model)
varImpPlot(dbk_rf_model)
plot(dbk_rf_model) 
dbk_rf_pred <- predict(dbk_rf_model, newdata = test_dbk_rf)
dbk_rmse <- (rmse(dbk_rf_pred,test_dbk_rf$Attendance.G))/test_dbk_rf$Attendance.G
dbk_mad <- (mae(dbk_rf_pred, test_dbk_rf$Attendance.G))/test_dbk_rf$Attendance.G
dbk_mape <- mape(dbk_rf_pred, test_dbk_rf$Attendance.G)
Team = "Arizona Diamondbacks"
dbk_rf_evaluation <- cbind(Team, dbk_rmse, dbk_mad, dbk_mape)
colnames(dbk_rf_evaluation)[2] <- "RMSE"
colnames(dbk_rf_evaluation)[3] <- "MAD"
colnames(dbk_rf_evaluation)[4] <- "MAPE"
dbk_rf_evaluation
dbk_rf_output <- cbind(Team, test_dbk_rf$Attendance.G,dbk_rf_pred)
colnames(dbk_rf_output)[2] <- "Actual"
colnames(dbk_rf_output)[3] <- "Predicted"
dbk_rf_output

_____________________________________________________________________________________________________________________

brv_rf <- read.csv("H:/RIC/Atlanta Braves/Final.csv", header = TRUE)
training_brv_rf <- brv_rf [2:21, c(5:10,14:16,19:20,22:23,25:26,29:36)]
test_brv_rf <- brv_rf [1, c(5:10,14:16,19:20,22:23,25:26,29:36)]

brv_rf_model <- randomForest(Attendance.G ~ ., data = training_brv_rf, ntree = 450, mtry = 13, importance = TRUE)
brv_rf_model
importance(brv_rf_model)
varImpPlot(brv_rf_model)
plot(brv_rf_model) 
brv_rf_pred <- predict(brv_rf_model, newdata = test_brv_rf)
brv_rmse <- (rmse(brv_rf_pred,test_brv_rf$Attendance.G))/test_brv_rf$Attendance.G
brv_mad <- (mae(brv_rf_pred, test_brv_rf$Attendance.G))/test_brv_rf$Attendance.G
brv_mape <- mape(brv_rf_pred, test_brv_rf$Attendance.G)
Team = "Atlanta Braves"
brv_rf_evaluation <- cbind(Team, brv_rmse, brv_mad, brv_mape)
colnames(brv_evaluation)[2] <- "RMSE"
colnames(brv_evaluation)[3] <- "MAD"
colnames(brv_evaluation)[4] <- "MAPE"
brv_rf_evaluation
brv_rf_output <- cbind(Team, test_brv_rf$Attendance.G,brv_rf_pred)
colnames(brv_rf_output)[2] <- "Actual"
colnames(brv_rf_output)[3] <- "Predicted"
brv_rf_output
__________________________________________________________________________________________________________________

ori_rf <- read.csv("H:/RIC/Baltimore Orioles/Final.csv", header = TRUE)
training_ori_rf <- ori_rf [2:119, 5:37]
test_ori_rf <- ori_rf [1, 5:37]

ori_rf_model <- randomForest(Attendance.G ~ ., data = training_ori_rf, ntree = 400, mtry = 12, importance = TRUE)
importance(ori_rf_model)
varImpPlot(ori_rf_model)
plot(ori_rf_model) 
ori_rf_pred <- predict(ori_rf_model, newdata = test_ori_rf)
ori_rmse <- (rmse(ori_rf_pred,test_ori_rf$Attendance.G))/test_ori_rf$Attendance.G
ori_mad <- (mae(ori_rf_pred, test_ori_rf$Attendance.G))/test_ori_rf$Attendance.G
ori_mape <- (mape(ori_rf_pred, test_ori_rf$Attendance.G))
Team = "Baltimore Orioles"
ori_rf_evaluation <- cbind(Team, ori_rmse, ori_mad, ori_mape)
colnames(ori_rf_evaluation)[2] <- "RMSE"
colnames(ori_rf_evaluation)[3] <- "MAD"
colnames(ori_rf_evaluation)[4] <- "MAPE"
ori_rf_evaluation
ori_rf_output <- cbind(Team, test_ori_rf$Attendance.G,ori_rf_pred)
colnames(ori_rf_output)[2] <- "Actual"
colnames(ori_rf_output)[3] <- "Predicted"
ori_rf_output
__________________________________________________________________________________________________________________

redsox_rf <- read.csv("H:/RIC/Boston Red Sox/Final.csv", header = TRUE)
training_redsox_rf <- redsox_rf [2:119, 5:37]
test_redsox_rf <- redsox_rf [1, 5:37]

redsox_rf_model <- randomForest(Attendance.G ~ ., data = training_redsox_rf, ntree = 400, mtry = 12, importance = TRUE)
importance(redsox_rf_model)
varImpPlot(redsox_rf_model)
plot(redsox_rf_model) 
redsox_rf_pred <- predict(redsox_rf_model, newdata = test_redsox_rf)
redsox_rmse <- (rmse(redsox_rf_pred,test_redsox_rf$Attendance.G))/test_redsox_rf$Attendance.G
redsox_mad <- (mae(redsox_rf_pred, test_redsox_rf$Attendance.G))/test_redsox_rf$Attendance.G
redsox_mape <- (mape(redsox_rf_pred, test_redsox_rf$Attendance.G))
Team = "Boston Red Sox"
redsox_rf_evaluation <- cbind(Team, redsox_rmse, redsox_mad, redsox_mape)
colnames(redsox_rf_evaluation)[2] <- "RMSE"
colnames(redsox_rf_evaluation)[3] <- "MAD"
colnames(redsox_rf_evaluation)[4] <- "MAPE"
redsox_rf_evaluation
redsox_rf_output <- cbind(Team, test_redsox_rf$Attendance.G,redsox_rf_pred)
colnames(redsox_rf_output)[2] <- "Actual"
colnames(redsox_rf_output)[3] <- "Predicted"
redsox_rf_output
___________________________________________________________________________________________________________________

cubs_rf <- read.csv("H:/RIC/Chicago Cubs/Final.csv", header = TRUE)
training_cubs_rf <- cubs_rf [2:119, 5:37]
test_cubs_rf <- cubs_rf [1, 5:37]

cubs_rf_model <- randomForest(Attendance.G ~ ., data = training_cubs_rf, ntree = 400, mtry = 12, importance = TRUE)
importance(cubs_rf_model)
varImpPlot(cubs_rf_model)
plot(cubs_rf_model) 
cubs_rf_pred <- predict(cubs_rf_model, newdata = test_cubs_rf)
cubs_rmse <- (rmse(cubs_rf_pred,test_cubs_rf$Attendance.G))/test_cubs_rf$Attendance.G
cubs_mad <- (mae(cubs_rf_pred, test_cubs_rf$Attendance.G))/test_cubs_rf$Attendance.G
cubs_mape <- (mape(cubs_rf_pred, test_cubs_rf$Attendance.G))
Team = "Chicago Cubs"
cubs_rf_evaluation <- cbind(Team, cubs_rmse, cubs_mad, cubs_mape)
colnames(cubs_rf_evaluation)[2] <- "RMSE"
colnames(cubs_rf_evaluation)[3] <- "MAD"
colnames(cubs_rf_evaluation)[4] <- "MAPE"
cubs_rf_evaluation
cubs_rf_output <- cbind(Team, test_cubs_rf$Attendance.G,cubs_rf_pred)
colnames(cubs_rf_output)[2] <- "Actual"
colnames(cubs_rf_output)[3] <- "Predicted"
cubs_rf_output
__________________________________________________________________________________________________________________

whitesox_rf <- read.csv("H:/RIC/Chicago White Sox/Final.csv", header = TRUE)
training_whitesox_rf <- whitesox_rf [2:119, 5:37]
test_whitesox_rf <- whitesox_rf [1, 5:37]

whitesox_rf_model <- randomForest(Attendance.G ~ ., data = training_whitesox_rf, ntree = 400, mtry = 12, importance = TRUE)
importance(whitesox_rf_model)
varImpPlot(whitesox_rf_model)
plot(whitesox_rf_model) 
whitesox_rf_pred <- predict(whitesox_rf_model, newdata = test_whitesox_rf)
whitesox_rmse <- (rmse(whitesox_rf_pred,test_whitesox_rf$Attendance.G))/test_whitesox_rf$Attendance.G
whitesox_mad <- (mae(whitesox_rf_pred, test_whitesox_rf$Attendance.G))/test_whitesox_rf$Attendance.G
whitesox_mape <- (mape(whitesox_rf_pred, test_whitesox_rf$Attendance.G))
Team = "Chicago White Sox"
whitesox_rf_evaluation <- cbind(Team, whitesox_rmse, whitesox_mad, whitesox_mape)
colnames(whitesox_rf_evaluation)[2] <- "RMSE"
colnames(whitesox_rf_evaluation)[3] <- "MAD"
colnames(whitesox_rf_evaluation)[4] <- "MAPE"
whitesox_rf_evaluation
whitesox_rf_output <- cbind(Team, test_whitesox_rf$Attendance.G,whitesox_rf_pred)
colnames(whitesox_rf_output)[2] <- "Actual"
colnames(whitesox_rf_output)[3] <- "Predicted"
whitesox_rf_output
_________________________________________________________________________________________________________________

reds_rf <- read.csv("H:/RIC/Cincinnati Reds/Final.csv", header = TRUE)
training_reds_rf <- reds_rf [2:130, 5:37]
test_reds_rf <- reds_rf [1, 5:37]

reds_rf_model <- randomForest(Attendance.G ~ ., data = training_reds_rf, ntree = 400, mtry = 12, importance = TRUE)
importance(reds_rf_model)
varImpPlot(reds_rf_model)
plot(reds_rf_model) 
reds_rf_pred <- predict(reds_rf_model, newdata = test_reds_rf)
reds_rmse <- (rmse(reds_rf_pred,test_reds_rf$Attendance.G))/test_reds_rf$Attendance.G
reds_mad <- (mae(reds_rf_pred, test_reds_rf$Attendance.G))/test_reds_rf$Attendance.G
reds_mape <- (mape(reds_rf_pred, test_reds_rf$Attendance.G))
Team = "Cincinnati Reds"
reds_rf_evaluation <- cbind(Team, reds_rmse, reds_mad, reds_mape)
colnames(reds_rf_evaluation)[2] <- "RMSE"
colnames(reds_rf_evaluation)[3] <- "MAD"
colnames(reds_rf_evaluation)[4] <- "MAPE"
reds_rf_evaluation
reds_rf_output <- cbind(Team, test_reds_rf$Attendance.G,reds_rf_pred)
colnames(reds_rf_output)[2] <- "Actual"
colnames(reds_rf_output)[3] <- "Predicted"
reds_rf_output
_________________________________________________________________________________________________________________

indians_rf <- read.csv("H:/RIC/Cleveland Indians/Final.csv", header = TRUE)
training_indians_rf <- indians_rf [3:119, 5:37]
test_indians_rf <- indians_rf [1:2, 5:37]

indians_rf_model <- randomForest(Attendance.G ~ ., data = training_indians_rf, ntree = 400, mtry = 12, importance = TRUE)
importance(indians_rf_model)
varImpPlot(indians_rf_model)
plot(indians_rf_model) 
indians_rf_pred <- predict(indians_rf_model, newdata = test_indians_rf)
indians_rmse <- (rmse(indians_rf_pred,test_indians_rf$Attendance.G))/test_indians_rf$Attendance.G
indians_mad <- (mae(indians_rf_pred, test_indians_rf$Attendance.G))/test_indians_rf$Attendance.G
indians_mape <- (mape(indians_rf_pred, test_indians_rf$Attendance.G))
Team = "Cleveland Indians"
indians_rf_evaluation <- cbind(Team, indians_rmse, indians_mad, indians_mape)
colnames(indians_rf_evaluation)[2] <- "RMSE"
colnames(indians_rf_evaluation)[3] <- "MAD"
colnames(indians_rf_evaluation)[4] <- "MAPE"
indians_rf_evaluation
indians_rf_output <- cbind(Team, test_indians_rf$Attendance.G,indians_rf_pred)
colnames(indians_rf_output)[2] <- "Actual"
colnames(indians_rf_output)[3] <- "Predicted"
indians_rf_output
__________________________________________________________________________________________________________________

rockies_rf <- read.csv("H:/RIC/Colorado Rockies/Final.csv", header = TRUE)
training_rockies_rf <- rockies_rf [2:27, 5:37]
test_rockies_rf <- rockies_rf [1, 5:37]

rockies_rf_model <- randomForest(Attendance.G ~ ., data = training_rockies_rf, ntree = 70, mtry = 6, importance = TRUE)
importance(rockies_rf_model)
varImpPlot(rockies_rf_model)
plot(rockies_rf_model) 
rockies_rf_pred <- predict(rockies_rf_model, newdata = test_rockies_rf)
rockies_rmse <- (rmse(rockies_rf_pred,test_rockies_rf$Attendance.G))/test_rockies_rf$Attendance.G
rockies_mad <- (mae(rockies_rf_pred, test_rockies_rf$Attendance.G))/test_rockies_rf$Attendance.G
rockies_mape <- (mape(rockies_rf_pred, test_rockies_rf$Attendance.G))
Team = "Colorado Rockies"
rockies_rf_evaluation <- cbind(Team, rockies_rmse, rockies_mad, rockies_mape)
colnames(rockies_rf_evaluation)[2] <- "RMSE"
colnames(rockies_rf_evaluation)[3] <- "MAD"
colnames(rockies_rf_evaluation)[4] <- "MAPE"
rockies_rf_evaluation
rockies_rf_output <- cbind(Team, test_rockies_rf$Attendance.G,rockies_rf_pred)
colnames(rockies_rf_output)[2] <- "Actual"
colnames(rockies_rf_output)[3] <- "Predicted"
rockies_rf_output
_________________________________________________________________________________________________________________

tigers_rf <- read.csv("H:/RIC/Detroit Tigers/Final.csv", header = TRUE)
training_tigers_rf <- tigers_rf [2:119, 5:37]
test_tigers_rf <- tigers_rf [1, 5:37]

tigers_rf_model <- randomForest(Attendance.G ~ ., data = training_tigers_rf, ntree = 500, mtry = 12, importance = TRUE)
importance(tigers_rf_model)
varImpPlot(tigers_rf_model)
plot(tigers_rf_model) 
tigers_rf_pred <- predict(tigers_rf_model, newdata = test_tigers_rf)
tigers_rmse <- (rmse(tigers_rf_pred,test_tigers_rf$Attendance.G))/test_tigers_rf$Attendance.G
tigers_mad <- (mae(tigers_rf_pred, test_tigers_rf$Attendance.G))/test_tigers_rf$Attendance.G
tigers_mape <- (mape(tigers_rf_pred, test_tigers_rf$Attendance.G))
Team = "Detroit Tigers"
tigers_rf_evaluation <- cbind(Team, tigers_rmse, tigers_mad, tigers_mape)
colnames(tigers_rf_evaluation)[2] <- "RMSE"
colnames(tigers_rf_evaluation)[3] <- "MAD"
colnames(tigers_rf_evaluation)[4] <- "MAPE"
tigers_rf_evaluation
tigers_rf_output <- cbind(Team, test_tigers_rf$Attendance.G,tigers_rf_pred)
colnames(tigers_rf_output)[2] <- "Actual"
colnames(tigers_rf_output)[3] <- "Predicted"
tigers_rf_output
__________________________________________________________________________________________________________________

astros_rf <- read.csv("H:/RIC/Houston Astros/Final.csv", header = TRUE)
training_astros_rf <- astros_rf [2:58, 5:37]
test_astros_rf <- astros_rf [1, 5:37]

astros_rf_model <- randomForest(Attendance.G ~ ., data = training_astros_rf, ntree = 450, mtry = 12, importance = TRUE)
importance(astros_rf_model)
varImpPlot(astros_rf_model)
plot(astros_rf_model) 
astros_rf_pred <- predict(astros_rf_model, newdata = test_astros_rf)
astros_rmse <- (rmse(astros_rf_pred,test_astros_rf$Attendance.G))/test_astros_rf$Attendance.G
astros_mad <- (mae(astros_rf_pred, test_astros_rf$Attendance.G))/test_astros_rf$Attendance.G
astros_mape <- (mape(astros_rf_pred, test_astros_rf$Attendance.G))
Team = "Houston Astros"
astros_rf_evaluation <- cbind(Team, astros_rmse, astros_mad, astros_mape)
colnames(astros_rf_evaluation)[2] <- "RMSE"
colnames(astros_rf_evaluation)[3] <- "MAD"
colnames(astros_rf_evaluation)[4] <- "MAPE"
astros_rf_evaluation
astros_rf_output <- cbind(Team, test_astros_rf$Attendance.G,astros_rf_pred)
colnames(astros_rf_output)[2] <- "Actual"
colnames(astros_rf_output)[3] <- "Predicted"
astros_rf_output
__________________________________________________________________________________________________________________

royals_rf <- read.csv("H:/RIC/Kansas City Royals/Final.csv", header = TRUE)
training_royals_rf <- royals_rf [2:51, 5:37]
test_royals_rf <- royals_rf [1, 5:37]

royals_rf_model <- randomForest(Attendance.G ~ ., data = training_royals_rf, ntree = 45, mtry = 4, importance = TRUE)
importance(royals_rf_model)
varImpPlot(royals_rf_model)
plot(royals_rf_model) 
royals_rf_pred <- predict(royals_rf_model, newdata = test_royals_rf)
royals_rmse <- (rmse(royals_rf_pred,test_royals_rf$Attendance.G))/test_royals_rf$Attendance.G
royals_mad <- (mae(royals_rf_pred, test_royals_rf$Attendance.G))/test_royals_rf$Attendance.G
royals_mape <- (mape(royals_rf_pred, test_royals_rf$Attendance.G))
Team = "Kansas City Royals"
royals_rf_evaluation <- cbind(Team, royals_rmse, royals_mad, royals_mape)
colnames(royals_rf_evaluation)[2] <- "RMSE"
colnames(royals_rf_evaluation)[3] <- "MAD"
colnames(royals_rf_evaluation)[4] <- "MAPE"
royals_rf_evaluation
royals_rf_output <- cbind(Team, test_royals_rf$Attendance.G,royals_rf_pred)
colnames(royals_rf_output)[2] <- "Actual"
colnames(royals_rf_output)[3] <- "Predicted"
royals_rf_output
_________________________________________________________________________________________________________________

angels_rf <- read.csv("H:/RIC/Los Angeles Angels/Final.csv", header = TRUE)
training_angels_rf <- angels_rf [2:51, 5:37]
test_angels_rf <- angels_rf [1, 5:37]

angels_rf_model <- randomForest(Attendance.G ~ ., data = training_angels_rf, ntree = 100, mtry = 6, importance = TRUE)
importance(angels_rf_model)
varImpPlot(angels_rf_model)
plot(angels_rf_model) 
angels_rf_pred <- predict(angels_rf_model, newdata = test_angels_rf)
angels_rmse <- (rmse(angels_rf_pred,test_angels_rf$Attendance.G))/test_angels_rf$Attendance.G
angels_mad <- (mae(angels_rf_pred, test_angels_rf$Attendance.G))/test_angels_rf$Attendance.G
angels_mape <- (mape(angels_rf_pred, test_angels_rf$Attendance.G))
Team = "Los Angeles Angels"
angels_rf_evaluation <- cbind(Team, angels_rmse, angels_mad, angels_mape)
colnames(angels_rf_evaluation)[2] <- "RMSE"
colnames(angels_rf_evaluation)[3] <- "MAD"
colnames(angels_rf_evaluation)[4] <- "MAPE"
angels_rf_evaluation
angels_rf_output <- cbind(Team, test_angels_rf$Attendance.G,angels_rf_pred)
colnames(angels_rf_output)[2] <- "Actual"
colnames(angels_rf_output)[3] <- "Predicted"
angels_rf_output
__________________________________________________________________________________________________________________

dodgers_rf <- read.csv("H:/RIC/Los Angeles Dodgers/Final.csv", header = TRUE)
training_dodgers_rf <- dodgers_rf [2:130, 5:37]
test_dodgers_rf <- dodgers_rf [1, 5:37]

dodgers_rf_model <- randomForest(Attendance.G ~ ., data = training_dodgers_rf, ntree = 350, mtry = 12, importance = TRUE)
importance(dodgers_rf_model)
varImpPlot(dodgers_rf_model)
plot(dodgers_rf_model) 
dodgers_rf_pred <- predict(dodgers_rf_model, newdata = test_dodgers_rf)
dodgers_rmse <- (rmse(dodgers_rf_pred,test_dodgers_rf$Attendance.G))/test_dodgers_rf$Attendance.G
dodgers_mad <- (mae(dodgers_rf_pred, test_dodgers_rf$Attendance.G))/test_dodgers_rf$Attendance.G
dodgers_mape <- (mape(dodgers_rf_pred, test_dodgers_rf$Attendance.G))
Team = "Los Angeles Dodgers"
dodgers_rf_evaluation <- cbind(Team, dodgers_rmse, dodgers_mad, dodgers_mape)
colnames(dodgers_rf_evaluation)[2] <- "RMSE"
colnames(dodgers_rf_evaluation)[3] <- "MAD"
colnames(dodgers_rf_evaluation)[4] <- "MAPE"
dodgers_rf_evaluation
dodgers_rf_output <- cbind(Team, test_dodgers_rf$Attendance.G,dodgers_rf_pred)
colnames(dodgers_rf_output)[2] <- "Actual"
colnames(dodgers_rf_output)[3] <- "Predicted"
dodgers_rf_output
__________________________________________________________________________________________________________________

marlins_rf <- read.csv("H:/RIC/Miami Marlins/Final.csv", header = TRUE)
training_marlins_rf <- marlins_rf [2:27, 5:37]
test_marlins_rf <- marlins_rf [1, 5:37]

marlins_rf_model <- randomForest(Attendance.G ~ ., data = training_marlins_rf, ntree = 400, mtry = 10, importance = TRUE)
importance(marlins_rf_model)
varImpPlot(marlins_rf_model)
plot(marlins_rf_model) 
marlins_rf_pred <- predict(marlins_rf_model, newdata = test_marlins_rf)
marlins_rmse <- (rmse(marlins_rf_pred,test_marlins_rf$Attendance.G))/test_marlins_rf$Attendance.G
marlins_mad <- (mae(marlins_rf_pred, test_marlins_rf$Attendance.G))/test_marlins_rf$Attendance.G
marlins_mape <- (mape(marlins_rf_pred, test_marlins_rf$Attendance.G))
Team = "Miami Marlins"
marlins_rf_evaluation <- cbind(Team, marlins_rmse, marlins_mad, marlins_mape)
colnames(marlins_rf_evaluation)[2] <- "RMSE"
colnames(marlins_rf_evaluation)[3] <- "MAD"
colnames(marlins_rf_evaluation)[4] <- "MAPE"
marlins_rf_evaluation
marlins_rf_output <- cbind(Team, test_marlins_rf$Attendance.G,marlins_rf_pred)
colnames(marlins_rf_output)[2] <- "Actual"
colnames(marlins_rf_output)[3] <- "Predicted"
marlins_rf_output
__________________________________________________________________________________________________________________

brewers_rf <- read.csv("H:/RIC/Milwaukee Brewers/Final.csv", header = TRUE)
training_brewers_rf <- brewers_rf [2:51, 5:37]
test_brewers_rf <- brewers_rf [1, 5:37]

brewers_rf_model <- randomForest(Attendance.G ~ ., data = training_brewers_rf, ntree = 400, mtry = 12, importance = TRUE)
importance(brewers_rf_model)
varImpPlot(brewers_rf_model)
plot(brewers_rf_model) 
brewers_rf_pred <- predict(brewers_rf_model, newdata = test_brewers_rf)
brewers_rmse <- (rmse(brewers_rf_pred - test_brewers_rf$Attendance.G))/test_brewers_rf$Attendance.G
brewers_mad <- (mae(brewers_rf_pred, test_brewers_rf$Attendance.G))/test_brewers_rf$Attendance.G
brewers_mape <- (mape(brewers_rf_pred, test_brewers_rf$Attendance.G))
Team = "Milwaukee Brewers"
brewers_rf_evaluation <- cbind(Team, brewers_rmse, brewers_mad, brewers_mape)
colnames(brewers_rf_evaluation)[2] <- "RMSE"
colnames(brewers_rf_evaluation)[3] <- "MAD"
colnames(brewers_rf_evaluation)[4] <- "MAPE"
brewers_rf_evaluation
brewers_rf_output <- cbind(Team, test_brewers_rf$Attendance.G,brewers_rf_pred)
colnames(brewers_rf_output)[2] <- "Actual"
colnames(brewers_rf_output)[3] <- "Predicted"
brewers_rf_output
__________________________________________________________________________________________________________________

twins_rf <- read.csv("H:/RIC/Minnesota Twins/Final.csv", header = TRUE)
training_twins_rf <- twins_rf [2:119, 5:37]
test_twins_rf <- twins_rf [1, 5:37]

twins_rf_model <- randomForest(Attendance.G ~ ., data = training_twins_rf, ntree = 400, mtry = 12, importance = TRUE)
importance(twins_rf_model)
varImpPlot(twins_rf_model)
plot(twins_rf_model) 
twins_rf_pred <- predict(twins_rf_model, newdata = test_twins_rf)
twins_rmse <- (rmse(twins_rf_pred,test_twins_rf$Attendance.G))/test_twins_rf$Attendance.G
twins_mad <- (mae(twins_rf_pred, test_twins_rf$Attendance.G))/test_twins_rf$Attendance.G
twins_mape <- (mape(twins_rf_pred, test_twins_rf$Attendance.G))/100
Team = "Minnesota Twins"
twins_rf_evaluation <- cbind(Team, twins_rmse, twins_mad, twins_mape)
colnames(twins_rf_evaluation)[2] <- "RMSE"
colnames(twins_rf_evaluation)[3] <- "MAD"
colnames(twins_rf_evaluation)[4] <- "MAPE"
twins_rf_evaluation
twins_rf_output <- cbind(Team, test_twins_rf$Attendance.G,twins_rf_pred)
colnames(twins_rf_output)[2] <- "Actual"
colnames(twins_rf_output)[3] <- "Predicted"
twins_rf_output
___________________________________________________________________________________________________________________

mets_rf <- read.csv("H:/RIC/New York Mets/Final.csv", header = TRUE)
training_mets_rf <- mets_rf [2:58, 5:37]
test_mets_rf <- mets_rf [1, 5:37]

mets_rf_model <- randomForest(Attendance.G ~ ., data = training_mets_rf, ntree = 55, mtry = 6, importance = TRUE)
importance(mets_rf_model)
varImpPlot(mets_rf_model)
plot(mets_rf_model) 
mets_rf_pred <- predict(mets_rf_model, newdata = test_mets_rf)
mets_rmse <- (rmse(mets_rf_pred,test_mets_rf$Attendance.G))/test_mets_rf$Attendance.G
mets_mad <- (mae(mets_rf_pred, test_mets_rf$Attendance.G))/test_mets_rf$Attendance.G
mets_mape <- (mape(mets_rf_pred, test_mets_rf$Attendance.G))
Team = "New York Mets"
mets_rf_evaluation <- cbind(Team, mets_rmse, mets_mad, mets_mape)
colnames(mets_rf_evaluation)[2] <- "RMSE"
colnames(mets_rf_evaluation)[3] <- "MAD"
colnames(mets_rf_evaluation)[4] <- "MAPE"
mets_rf_evaluation
mets_rf_output <- cbind(Team, test_mets_rf$Attendance.G,mets_rf_pred)
colnames(mets_rf_output)[2] <- "Actual"
colnames(mets_rf_output)[3] <- "Predicted"
mets_rf_output
__________________________________________________________________________________________________________________

yankees_rf <- read.csv("H:/RIC/New York Yankees/Final.csv", header = TRUE)
training_yankees_rf <- yankees_rf [2:117, 5:37]
test_yankees_rf <- yankees_rf [1, 5:37]

yankees_rf_model <- randomForest(Attendance.G ~ ., data = training_yankees_rf, ntree = 400, mtry = 12, importance = TRUE)
importance(yankees_rf_model)
varImpPlot(yankees_rf_model)
plot(yankees_rf_model) 
yankees_rf_pred <- predict(yankees_rf_model, newdata = test_yankees_rf)
yankees_rmse <- (rmse(yankees_rf_pred,test_yankees_rf$Attendance.G))/test_yankees_rf$Attendance.G
yankees_mad <- (mae(yankees_rf_pred, test_yankees_rf$Attendance.G))/test_yankees_rf$Attendance.G
yankees_mape <- (mape(yankees_rf_pred, test_yankees_rf$Attendance.G))
Team = "New York Yankees"
yankees_rf_evaluation <- cbind(Team, yankees_rmse, yankees_mad, yankees_mape)
colnames(yankees_rf_evaluation)[2] <- "RMSE"
colnames(yankees_rf_evaluation)[3] <- "MAD"
colnames(yankees_rf_evaluation)[4] <- "MAPE"
yankees_rf_evaluation
yankees_rf_output <- cbind(Team, test_yankees_rf$Attendance.G,yankees_rf_pred)
colnames(yankees_rf_output)[2] <- "Actual"
colnames(yankees_rf_output)[3] <- "Predicted"
yankees_rf_output
_________________________________________________________________________________________________________________

athletics_rf <- read.csv("H:/RIC/Oakland Athletics/Final.csv", header = TRUE)
training_athletics_rf <- athletics_rf [2:119, 5:37]
test_athletics_rf <- athletics_rf [1, 5:37]

athletics_rf_model <- randomForest(Attendance.G ~ ., data = training_athletics_rf, ntree = 400, mtry = 12, importance = TRUE)
importance(athletics_rf_model)
varImpPlot(athletics_rf_model)
plot(athletics_rf_model) 
athletics_rf_pred <- predict(athletics_rf_model, newdata = test_athletics_rf)
athletics_rmse <- (rmse(athletics_rf_pred,test_athletics_rf$Attendance.G))/test_athletics_rf$Attendance.G
athletics_mad <- (mae(athletics_rf_pred, test_athletics_rf$Attendance.G))/test_athletics_rf$Attendance.G
athletics_mape <- (mape(athletics_rf_pred, test_athletics_rf$Attendance.G))
Team = "Oakland Athletics"
athletics_rf_evaluation <- cbind(Team, athletics_rmse, athletics_mad, athletics_mape)
colnames(athletics_rf_evaluation)[2] <- "RMSE"
colnames(athletics_rf_evaluation)[3] <- "MAD"
colnames(athletics_rf_evaluation)[4] <- "MAPE"
athletics_rf_evaluation
athletics_rf_output <- cbind(Team, test_athletics_rf$Attendance.G,athletics_rf_pred)
colnames(athletics_rf_output)[2] <- "Actual"
colnames(athletics_rf_output)[3] <- "Predicted"
athletics_rf_output
__________________________________________________________________________________________________________________

phillies_rf <- read.csv("H:/RIC/Philadelphia Phillies/Final.csv", header = TRUE)
training_phillies_rf <- phillies_rf [2:119, 5:37]
test_phillies_rf <- phillies_rf [1, 5:37]

phillies_rf_model <- randomForest(Attendance.G ~ ., data = training_phillies_rf, ntree = 70, mtry = 12, importance = TRUE)
importance(phillies_rf_model)
varImpPlot(phillies_rf_model)
plot(phillies_rf_model) 
phillies_rf_pred <- predict(phillies_rf_model, newdata = test_phillies_rf)
phillies_rmse <- (rmse(phillies_rf_pred,test_phillies_rf$Attendance.G))/test_phillies_rf$Attendance.G
phillies_mad <- (mae(phillies_rf_pred, test_phillies_rf$Attendance.G))/test_phillies_rf$Attendance.G
phillies_mape <- (mape(phillies_rf_pred, test_phillies_rf$Attendance.G))
Team = "Philadelphia Phillies"
phillies_rf_evaluation <- cbind(Team, phillies_rmse, phillies_mad, phillies_mape)
colnames(phillies_rf_evaluation)[2] <- "RMSE"
colnames(phillies_rf_evaluation)[3] <- "MAD"
colnames(phillies_rf_evaluation)[4] <- "MAPE"
phillies_rf_evaluation
phillies_rf_output <- cbind(Team, test_phillies_rf$Attendance.G,phillies_rf_pred)
colnames(phillies_rf_output)[2] <- "Actual"
colnames(phillies_rf_output)[3] <- "Predicted"
phillies_rf_output
__________________________________________________________________________________________________________________

pirates_rf <- read.csv("H:/RIC/Pittsburgh Pirates/Final.csv", header = TRUE)
training_pirates_rf <- pirates_rf [2:130, 5:37]
test_pirates_rf <- pirates_rf [1, 5:37]

pirates_rf_model <- randomForest(Attendance.G ~ ., data = training_pirates_rf, ntree = 450, mtry = 12, importance = TRUE)
importance(pirates_rf_model)
varImpPlot(pirates_rf_model)
plot(pirates_rf_model) 
pirates_rf_pred <- predict(pirates_rf_model, newdata = test_pirates_rf)
pirates_rmse <- (rmse(pirates_rf_pred,test_pirates_rf$Attendance.G))/test_pirates_rf$Attendance.G
pirates_mad <- (mae(pirates_rf_pred, test_pirates_rf$Attendance.G))/test_pirates_rf$Attendance.G
pirates_mape <- (mape(pirates_rf_pred, test_pirates_rf$Attendance.G))
Team = "Pittsburgh Pirates"
pirates_rf_evaluation <- cbind(Team, pirates_rmse, pirates_mad, pirates_mape)
colnames(pirates_rf_evaluation)[2] <- "RMSE"
colnames(pirates_rf_evaluation)[3] <- "MAD"
colnames(pirates_rf_evaluation)[4] <- "MAPE"
pirates_rf_evaluation
pirates_rf_output <- cbind(Team, test_pirates_rf$Attendance.G,pirates_rf_pred)
colnames(pirates_rf_output)[2] <- "Actual"
colnames(pirates_rf_output)[3] <- "Predicted"
pirates_rf_output
__________________________________________________________________________________________________________________

padres_rf <- read.csv("H:/RIC/San Diego Padres/Final.csv", header = TRUE)
training_padres_rf <- padres_rf [2:51, 5:37]
test_padres_rf <- padres_rf [1, 5:37]

padres_rf_model <- randomForest(Attendance.G ~ ., data = training_padres_rf, ntree = 400, mtry = 12, importance = TRUE)
importance(padres_rf_model)
varImpPlot(padres_rf_model)
plot(padres_rf_model) 
padres_rf_pred <- predict(padres_rf_model, newdata = test_padres_rf)
padres_rmse <- (rmse(padres_rf_pred,test_padres_rf$Attendance.G))/test_padres_rf$Attendance.G
padres_mad <- (mae(padres_rf_pred, test_padres_rf$Attendance.G))/test_padres_rf$Attendance.G
padres_mape <- (mape(padres_rf_pred, test_padres_rf$Attendance.G))
Team = "San Diego Padres"
padres_rf_evaluation <- cbind(Team, padres_rmse, padres_mad, padres_mape)
colnames(padres_rf_evaluation)[2] <- "RMSE"
colnames(padres_rf_evaluation)[3] <- "MAD"
colnames(padres_rf_evaluation)[4] <- "MAPE"
padres_rf_evaluation
padres_rf_output <- cbind(Team, test_padres_rf$Attendance.G,padres_rf_pred)
colnames(padres_rf_output)[2] <- "Actual"
colnames(padres_rf_output)[3] <- "Predicted"
padres_rf_output
__________________________________________________________________________________________________________________

giants_rf <- read.csv("H:/RIC/San Fransisco Giants/Final.csv", header = TRUE)
training_giants_rf <- giants_rf [2:130, 5:37]
test_giants_rf <- giants_rf [1, 5:37]

giants_rf_model <- randomForest(Attendance.G ~ ., data = training_giants_rf, ntree = 400, mtry = 12, importance = TRUE)
importance(giants_rf_model)
varImpPlot(giants_rf_model)
plot(giants_rf_model) 
giants_rf_pred <- predict(giants_rf_model, newdata = test_giants_rf)
giants_rmse <- (rmse(giants_rf_pred,test_giants_rf$Attendance.G))/test_giants_rf$Attendance.G
giants_mad <- (mad(giants_rf_pred, test_giants_rf$Attendance.G))/test_giants_rf$Attendance.G
giants_mape <- (mape(giants_rf_pred, test_giants_rf$Attendance.G))
Team = "San Fransisco Giants"
giants_rf_evaluation <- cbind(Team, giants_rmse, giants_mad, giants_mape)
colnames(giants_rf_evaluation)[2] <- "RMSE"
colnames(giants_rf_evaluation)[3] <- "MAD"
colnames(giants_rf_evaluation)[4] <- "MAPE"
giants_rf_evaluation
giants_rf_output <- cbind(Team, test_giants_rf$Attendance.G,giants_rf_pred)
colnames(giants_rf_output)[2] <- "Actual"
colnames(giants_rf_output)[3] <- "Predicted"
giants_rf_output
__________________________________________________________________________________________________________________

mariners_rf <- read.csv("H:/RIC/Seattle Mariners/Final.csv", header = TRUE)
training_mariners_rf <- mariners_rf [2:43, 5:37]
test_mariners_rf <- mariners_rf [1, 5:37]

mariners_rf_model <- randomForest(Attendance.G ~ ., data = training_mariners_rf, ntree = 400, mtry = 12, importance = TRUE)
importance(mariners_rf_model)
varImpPlot(mariners_rf_model)
plot(mariners_rf_model) 
mariners_rf_pred <- predict(mariners_rf_model, newdata = test_mariners_rf)
mariners_rmse <- (rmse(mariners_rf_pred,test_mariners_rf$Attendance.G))/test_mariners_rf$Attendance.G
mariners_mad <- (mae(mariners_rf_pred, test_mariners_rf$Attendance.G))/test_mariners_rf$Attendance.G
mariners_mape <- (mape(mariners_rf_pred, test_mariners_rf$Attendance.G))/100
Team = "Seattle Mariners"
mariners_rf_evaluation <- cbind(Team, mariners_rmse, mariners_mad, mariners_mape)
colnames(mariners_rf_evaluation)[2] <- "RMSE"
colnames(mariners_rf_evaluation)[3] <- "MAD"
colnames(mariners_rf_evaluation)[4] <- "MAPE"
mariners_rf_evaluation
mariners_rf_output <- cbind(Team, test_mariners_rf$Attendance.G,mariners_rf_pred)
colnames(mariners_rf_output)[2] <- "Actual"
colnames(mariners_rf_output)[3] <- "Predicted"
mariners_rf_output
__________________________________________________________________________________________________________________

cardinals_rf <- read.csv("H:/RIC/St. Louis Cardinals/Final.csv", header = TRUE)
training_cardinals_rf <- cardinals_rf [2:128, 5:37]
test_cardinals_rf <- cardinals_rf [1, 5:37]

cardinals_rf_model <- randomForest(Attendance.G ~ ., data = training_cardinals_rf, ntree = 400, mtry = 12, importance = TRUE)
importance(cardinals_rf_model)
varImpPlot(cardinals_rf_model)
plot(cardinals_rf_model) 
cardinals_rf_pred <- predict(cardinals_rf_model, newdata = test_cardinals_rf)
cardinals_rmse <- (rmse(cardinals_rf_pred,test_cardinals_rf$Attendance.G))/test_cardinals_rf$Attendance.G
cardinals_mad <- (mad(cardinals_rf_pred, test_cardinals_rf$Attendance.G))/test_cardinals_rf$Attendance.G
cardinals_mape <- (mape(cardinals_rf_pred, test_cardinals_rf$Attendance.G))
Team = "St. Louis Cardinals"
cardinals_rf_evaluation <- cbind(Team, cardinals_rmse, cardinals_mad, cardinals_mape)
colnames(cardinals_rf_evaluation)[2] <- "RMSE"
colnames(cardinals_rf_evaluation)[3] <- "MAD"
colnames(cardinals_rf_evaluation)[4] <- "MAPE"
cardinals_rf_evaluation
cardinals_rf_output <- cbind(Team, test_cardinals_rf$Attendance.G,cardinals_rf_pred)
colnames(cardinals_rf_output)[2] <- "Actual"
colnames(cardinals_rf_output)[3] <- "Predicted"
cardinals_rf_output
_________________________________________________________________________________________________________________

rays_rf <- read.csv("H:/RIC/Tampa Bay Rays/Final.csv", header = TRUE)
training_rays_rf <- rays_rf [2:22, 5:38]
test_rays_rf <- rays_rf [1, 5:38]

rays_rf_model <- randomForest(Attendance.G ~ ., data = training_rays_rf, ntree = 70, mtry = 6, importance = TRUE)
importance(rays_rf_model)
varImpPlot(rays_rf_model)
plot(rays_rf_model) 
rays_rf_pred <- predict(rays_rf_model, newdata = test_rays_rf)
rays_rmse <- (rmse(rays_rf_pred,test_rays_rf$Attendance.G))/test_rays_rf$Attendance.G
rays_mad <- (mad(rays_rf_pred, test_rays_rf$Attendance.G))/test_rays_rf$Attendance.G
rays_mape <- (mape(rays_rf_pred, test_rays_rf$Attendance.G))
Team = "Tampa Bay Rays"
rays_rf_evaluation <- cbind(Team, rays_rmse, rays_mad, rays_mape)
colnames(rays_rf_evaluation)[2] <- "RMSE"
colnames(rays_rf_evaluation)[3] <- "MAD"
colnames(rays_rf_evaluation)[4] <- "MAPE"
rays_rf_evaluation
rays_rf_output <- cbind(Team, test_rays_rf$Attendance.G,rays_rf_pred)
colnames(rays_rf_output)[2] <- "Actual"
colnames(rays_rf_output)[3] <- "Predicted"
rays_rf_output
____________________________________________________________________________________________________________

rangers_rf <- read.csv("H:/RIC/Texas Rangers/Final.csv", header = TRUE)
training_rangers_rf <- rangers_rf [2:59, 5:38]
test_rangers_rf <- rangers_rf [1, 5:38]

rangers_rf_model <- randomForest(Attendance.G ~ ., data = training_rangers_rf, ntree = 400, mtry = 12, importance = TRUE)
importance(rangers_rf_model)
varImpPlot(rangers_rf_model)
plot(rangers_rf_model) 
rangers_rf_pred <- predict(rangers_rf_model, newdata = test_rangers_rf)
rangers_rmse <- (rmse(rangers_rf_pred,test_rangers_rf$Attendance.G))/test_rangers_rf$Attendance.G
rangers_mad <- (mad(rangers_rf_pred, test_rangers_rf$Attendance.G))/test_rangers_rf$Attendance.G
rangers_mape <- (mape(rangers_rf_pred, test_rangers_rf$Attendance.G))
Team = "Texas Rangers"
rangers_rf_evaluation <- cbind(Team, rangers_rmse, rangers_mad, rangers_mape)
colnames(rangers_rf_evaluation)[2] <- "RMSE"
colnames(rangers_rf_evaluation)[3] <- "MAD"
colnames(rangers_rf_evaluation)[4] <- "MAPE"
rangers_rf_evaluation
rangers_rf_output <- cbind(Team, test_rangers_rf$Attendance.G,rangers_rf_pred)
colnames(rangers_rf_output)[2] <- "Actual"
colnames(rangers_rf_output)[3] <- "Predicted"
rangers_rf_output
____________________________________________________________________________________________________________

jays_rf <- read.csv("H:/RIC/Toronto Blue Jays/Final.csv", header = TRUE)
training_jays_rf <- jays_rf [2:43, 5:38]
test_jays_rf <- jays_rf [1, 5:38]

jays_rf_model <- randomForest(Attendance.G ~ ., data = training_jays_rf, ntree = 70, mtry = 6, importance = TRUE)
importance(jays_rf_model)
varImpPlot(jays_rf_model)
plot(jays_rf_model) 
jays_rf_pred <- predict(jays_rf_model, newdata = test_jays_rf)
jays_rmse <- (rmse(jays_rf_pred,test_jays_rf$Attendance.G))/test_jays_rf$Attendance.G
jays_mad <- (mad(jays_rf_pred, test_jays_rf$Attendance.G))/test_jays_rf$Attendance.G
jays_mape <- (mape(jays_rf_pred, test_jays_rf$Attendance.G))
Team = "Toronto Blue Jays"
jays_rf_evaluation <- cbind(Team, jays_rmse, jays_mad, jays_mape)
colnames(jays_rf_evaluation)[2] <- "RMSE"
colnames(jays_rf_evaluation)[3] <- "MAD"
colnames(jays_rf_evaluation)[4] <- "MAPE"
jays_rf_evaluation
jays_rf_output <- cbind(Team, test_jays_rf$Attendance.G,jays_rf_pred)
colnames(jays_rf_output)[2] <- "Actual"
colnames(jays_rf_output)[3] <- "Predicted"
jays_rf_output
____________________________________________________________________________________________________________

nationals_rf <- read.csv("H:/RIC/Washington Nationals/Final.csv", header = TRUE)
training_nationals_rf <- nationals_rf [2:51, 5:38]
test_nationals_rf <- nationals_rf [1, 5:38]

nationals_rf_model <- randomForest(Attendance.G ~ ., data = training_nationals_rf, ntree = 70, mtry = 16, importance = TRUE)
importance(nationals_rf_model)
varImpPlot(nationals_rf_model)
plot(nationals_rf_model) 
nationals_rf_pred <- predict(nationals_rf_model, newdata = test_nationals_rf)
nationals_rmse <- (rmse(nationals_rf_pred,test_nationals_rf$Attendance.G))/test_nationals_rf$Attendance.G
nationals_mad <- (mad(nationals_rf_pred, test_nationals_rf$Attendance.G))/test_nationals_rf$Attendance.G
nationals_mape <- (mape(nationals_rf_pred, test_nationals_rf$Attendance.G))
Team = "Washington Nationals"
nationals_rf_evaluation <- cbind(Team, nationals_rmse, nationals_mad, nationals_mape)
colnames(nationals_rf_evaluation)[2] <- "RMSE"
colnames(nationals_rf_evaluation)[3] <- "MAD"
colnames(nationals_rf_evaluation)[4] <- "MAPE"
nationals_rf_evaluation
nationals_rf_output <- cbind(Team, test_nationals_rf$Attendance.G,nationals_rf_pred)
colnames(nationals_rf_output)[2] <- "Actual"
colnames(nationals_rf_output)[3] <- "Predicted"
nationals_rf_output
____________________________________________________________________________________________________________

write.csv(model_rf_evaluation <- rbind(dbk_rf_evaluation, brv_rf_evaluation, ori_rf_evaluation, redsox_rf_evaluation, cubs_rf_evaluation,
                                       whitesox_rf_evaluation, reds_rf_evaluation, indians_rf_evaluation, rockies_rf_evaluation, tigers_rf_evaluation, 
                                       astros_rf_evaluation, royals_rf_evaluation, angels_rf_evaluation, dodgers_rf_evaluation, marlins_rf_evaluation,
                                       brewers_rf_evaluation, twins_rf_evaluation, mets_rf_evaluation, yankees_rf_evaluation, athletics_rf_evaluation, 
                                       phillies_rf_evaluation, pirates_rf_evaluation, padres_rf_evaluation, giants_rf_evaluation, mariners_rf_evaluation, 
                                       cardinals_rf_evaluation, rays_rf_evaluation, rangers_rf_evaluation, jays_rf_evaluation, nationals_rf_evaluation), 
                                       "H:/RIC/Evaluation/RandomForest_Evaluation.csv")
______________________________________________________________________________________________________________________________________________________________

write.csv(model_rf_output <- rbind(dbk_rf_output, brv_rf_output, ori_rf_output, redsox_rf_output, cubs_rf_output,
                                   whitesox_rf_output, reds_rf_output, indians_rf_output, rockies_rf_output, tigers_rf_output, 
                                   astros_rf_output, royals_rf_output, angels_rf_output, dodgers_rf_output, marlins_rf_output,
                                   brewers_rf_output, twins_rf_output, mets_rf_output, yankees_rf_output, athletics_rf_output, 
                                   phillies_rf_output, pirates_rf_output, padres_rf_output, giants_rf_output, mariners_rf_output, 
                                   cardinals_rf_output, rays_rf_output, rangers_rf_output, jays_rf_output, nationals_rf_output), 
                                   "H:/RIC/Evaluation/RF_Output.csv")
