library(rpart)
library(e1071)
library(Metrics)
library(penalizedSVM)

dbk_svm <- read.csv("H:/RIC/Arizona_Diamonbacks/Final.csv", header = TRUE)

# Create training and test set
train_dbk_svm = dbk_svm[ 2:21, 5:38 ]
test_dbk_svm = dbk_svm[ 1, 5:38 ]

#Scatter Plot
daz <- subset(dbk_svm, select = c(36:38))
plot(daz)

#Regression with SVM
dbk_svm_model <- svm(Attendance.G ~., data = train_dbk_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(dbk_svm_model, train_dbk_svm)

#Predict using SVM regression
dbk_svm_pred <- predict(dbk_svm_model, test_dbk_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_dbk_svm[1], dbk_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_dbk_svm=tune(svm, Attendance.G ~., data=train_dbk_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_dbk_svm)

#finding the bestmodel
bstmodel_dbk_svm <- OptModel_dbk_svm$best.model

#Predict outcome using best model
best_dbk_svm_pred <- predict(bstmodel_dbk_svm, train_dbk_svm)
final_dbk_svm_pred <- mean(best_dbk_svm_pred)
#Find value of W
W = t(dbk_svm_model$coefs) %*% dbk_svm_model$SV
W
#Find value of b
b = dbk_svm_model$rho
b
dbk_rmse <- (rmse(final_dbk_svm_pred,test_dbk_svm$Attendance.G))/test_dbk_svm$Attendance.G
dbk_mad <- (mae(final_dbk_svm_pred, test_dbk_svm$Attendance.G))/test_dbk_svm$Attendance.G
dbk_mape <- mape(final_dbk_svm_pred, test_dbk_svm$Attendance.G)
Team = "Arizona Diamondbacks"
dbk_svm_evaluation <- cbind(Team, dbk_rmse, dbk_mad, dbk_mape)
colnames(dbk_svm_evaluation)[2] <- "RMSE"
colnames(dbk_svm_evaluation)[3] <- "MAD"
colnames(dbk_svm_evaluation)[4] <- "MAPE"
dbk_svm_evaluation
dbk_svm_output <- cbind(Team,test_dbk_svm$Attendance.G,final_dbk_svm_pred)
colnames(dbk_svm_output)[2] <- "Actual"
colnames(dbk_svm_output)[3] <- "Predicted"
dbk_svm_output

____________________________________________________________________________________________________________

brv_svm <- read.csv("H:/RIC/Atlanta Braves/Final.csv", header = TRUE)

# Create training and test set
train_brv_svm = brv_svm[ 2:21, 5:38 ]
test_brv_svm = brv_svm[ 1, 5:38 ]

#Scatter Plot
daz <- subset(brv_svm, select = c(36:38))
plot(daz)

#Regression with SVM
brv_svm_model <- svm(Attendance.G ~., data = train_brv_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(brv_svm_model, train_brv_svm)

#Predict using SVM regression
brv_svm_pred <- predict(brv_svm_model, test_brv_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_brv_svm[1], brv_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_brv_svm=tune(svm, Attendance.G ~., data=train_brv_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_brv_svm)

#finding the bestmodel
bstmodel_brv_svm <- OptModel_brv_svm$best.model

#Predict outcome using best model
best_brv_svm_pred <- predict(bstmodel_brv_svm, train_brv_svm)
final_brv_svm_pred <- mean(best_brv_svm_pred)
#Find value of W
W = t(brv_svm_model$coefs) %*% brv_svm_model$SV
W
#Find value of b
b = brv_svm_model$rho
b
brv_rmse <- (rmse(final_brv_svm_pred,test_brv_svm$Attendance.G))/test_brv_svm$Attendance.G
brv_mad <- (mae(final_brv_svm_pred, test_brv_svm$Attendance.G))/test_brv_svm$Attendance.G
brv_mape <- mape(final_brv_svm_pred, test_brv_svm$Attendance.G)
Team = "Atlanta Braves"
brv_svm_evaluation <- cbind(Team, brv_rmse, brv_mad, brv_mape)
colnames(brv_svm_evaluation)[2] <- "RMSE"
colnames(brv_svm_evaluation)[3] <- "MAD"
colnames(brv_svm_evaluation)[4] <- "MAPE"
brv_svm_evaluation
brv_svm_output <- cbind(Team,test_brv_svm$Attendance.G,final_brv_svm_pred)
colnames(brv_svm_output)[2] <- "Actual"
colnames(brv_svm_output)[3] <- "Predicted"
brv_svm_output
______________________________________________________________________________________________________________

ori_svm <- read.csv("H:/RIC/Baltimore Orioles/Final.csv", header = TRUE)

# Create training and test set
train_ori_svm = ori_svm[ 2:21, 5:37 ]
test_ori_svm = ori_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(ori_svm, select = c(36:38))
plot(daz)

#Regression with SVM
ori_svm_model <- svm(Attendance.G ~., data = train_ori_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(ori_svm_model, train_ori_svm)

#Predict using SVM regression
ori_svm_pred <- predict(ori_svm_model, test_ori_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_ori_svm[1], ori_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_ori_svm=tune(svm, Attendance.G ~., data=train_ori_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_ori_svm)

#finding the bestmodel
bstmodel_ori_svm <- OptModel_ori_svm$best.model

#Predict outcome using best model
best_ori_svm_pred <- predict(bstmodel_ori_svm, train_ori_svm)
final_ori_svm_pred <- mean(best_ori_svm_pred)
#Find value of W
W = t(ori_svm_model$coefs) %*% ori_svm_model$SV
W
#Find value of b
b = ori_svm_model$rho
b
ori_rmse <- (rmse(final_ori_svm_pred,test_ori_svm$Attendance.G))/test_ori_svm$Attendance.G
ori_mad <- (mae(final_ori_svm_pred, test_ori_svm$Attendance.G))/test_ori_svm$Attendance.G
ori_mape <- mape(final_ori_svm_pred, test_ori_svm$Attendance.G)
Team = "Baltimore Orioles"
ori_svm_evaluation <- cbind(Team, ori_rmse, ori_mad, ori_mape)
colnames(ori_svm_evaluation)[2] <- "RMSE"
colnames(ori_svm_evaluation)[3] <- "MAD"
colnames(ori_svm_evaluation)[4] <- "MAPE"
ori_svm_evaluation
ori_svm_output <- cbind(Team,test_ori_svm$Attendance.G,final_ori_svm_pred)
colnames(ori_svm_output)[2] <- "Actual"
colnames(ori_svm_output)[3] <- "Predicted"
ori_svm_output
____________________________________________________________________________________________________________

redsox_svm <- read.csv("H:/RIC/Boston Red Sox/Final.csv", header = TRUE)

# Create training and test set
train_redsox_svm = redsox_svm[ 2:119, 5:37 ]
test_redsox_svm = redsox_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(redsox_svm, select = c(36:38))
plot(daz)

#Regression with SVM
redsox_svm_model <- svm(Attendance.G ~., data = train_redsox_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(redsox_svm_model, train_redsox_svm)

#Predict using SVM regression
redsox_svm_pred <- predict(redsox_svm_model, test_redsox_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_redsox_svm[1], redsox_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_redsox_svm=tune(svm, Attendance.G ~., data=train_redsox_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_redsox_svm)

#finding the bestmodel
bstmodel_redsox_svm <- OptModel_redsox_svm$best.model

#Predict outcome using best model
best_redsox_svm_pred <- predict(bstmodel_redsox_svm, train_redsox_svm)
final_redsox_svm_pred <- mean(best_redsox_svm_pred)
#Find value of W
W = t(redsox_svm_model$coefs) %*% redsox_svm_model$SV
W
#Find value of b
b = redsox_svm_model$rho
b
redsox_rmse <- (rmse(final_redsox_svm_pred,test_redsox_svm$Attendance.G))/test_redsox_svm$Attendance.G
redsox_mad <- (mae(final_redsox_svm_pred, test_redsox_svm$Attendance.G))/test_redsox_svm$Attendance.G
redsox_mape <- mape(final_redsox_svm_pred, test_redsox_svm$Attendance.G)
Team = "Boston Red Sox"
redsox_svm_evaluation <- cbind(Team, redsox_rmse, redsox_mad, redsox_mape)
colnames(redsox_svm_evaluation)[2] <- "RMSE"
colnames(redsox_svm_evaluation)[3] <- "MAD"
colnames(redsox_svm_evaluation)[4] <- "MAPE"
redsox_svm_evaluation
redsox_svm_output <- cbind(Team,test_redsox_svm$Attendance.G,final_redsox_svm_pred)
colnames(redsox_svm_output)[2] <- "Actual"
colnames(redsox_svm_output)[3] <- "Predicted"
redsox_svm_output
_____________________________________________________________________________________________________________

cubs_svm <- read.csv("H:/RIC/Chicago Cubs/Final.csv", header = TRUE)

# Create training and test set
train_cubs_svm = cubs_svm[ 2:119, 5:37 ]
test_cubs_svm = cubs_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(cubs_svm, select = c(36:38))
plot(daz)

#Regression with SVM
cubs_svm_model <- svm(Attendance.G ~., data = train_cubs_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(cubs_svm_model, train_cubs_svm)

#Predict using SVM regression
cubs_svm_pred <- predict(cubs_svm_model, test_cubs_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_cubs_svm[1], cubs_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_cubs_svm=tune(svm, Attendance.G ~., data=train_cubs_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_cubs_svm)

#finding the bestmodel
bstmodel_cubs_svm <- OptModel_cubs_svm$best.model

#Predict outcome using best model
best_cubs_svm_pred <- predict(bstmodel_cubs_svm, train_cubs_svm)
final_cubs_svm_pred <- mean(best_cubs_svm_pred)
#Find value of W
W = t(cubs_svm_model$coefs) %*% cubs_svm_model$SV
W
#Find value of b
b = cubs_svm_model$rho
b
cubs_rmse <- (rmse(final_cubs_svm_pred,test_cubs_svm$Attendance.G))/test_cubs_svm$Attendance.G
cubs_mad <- (mae(final_cubs_svm_pred, test_cubs_svm$Attendance.G))/test_cubs_svm$Attendance.G
cubs_mape <- mape(final_cubs_svm_pred, test_cubs_svm$Attendance.G)
Team = "Chicago Cubs"
cubs_svm_evaluation <- cbind(Team, cubs_rmse, cubs_mad, cubs_mape)
colnames(cubs_svm_evaluation)[2] <- "RMSE"
colnames(cubs_svm_evaluation)[3] <- "MAD"
colnames(cubs_svm_evaluation)[4] <- "MAPE"
cubs_svm_evaluation
cubs_svm_output <- cbind(Team,test_cubs_svm$Attendance.G,final_cubs_svm_pred)
colnames(cubs_svm_output)[2] <- "Actual"
colnames(cubs_svm_output)[3] <- "Predicted"
cubs_svm_output
____________________________________________________________________________________________________________

whitesox_svm <- read.csv("H:/RIC/Chicago White Sox/Final.csv", header = TRUE)

# Create training and test set
train_whitesox_svm = whitesox_svm[ 2:119, 5:37 ]
test_whitesox_svm = whitesox_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(whitesox_svm, select = c(36:38))
plot(daz)

#Regression with SVM
whitesox_svm_model <- svm(Attendance.G ~., data = train_whitesox_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(whitesox_svm_model, train_whitesox_svm)

#Predict using SVM regression
whitesox_svm_pred <- predict(whitesox_svm_model, test_whitesox_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_whitesox_svm[1], whitesox_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_whitesox_svm=tune(svm, Attendance.G ~., data=train_whitesox_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_whitesox_svm)

#finding the bestmodel
bstmodel_whitesox_svm <- OptModel_whitesox_svm$best.model

#Predict outcome using best model
best_whitesox_svm_pred <- predict(bstmodel_whitesox_svm, train_whitesox_svm)
final_whitesox_svm_pred <- mean(best_whitesox_svm_pred)
#Find value of W
W = t(whitesox_svm_model$coefs) %*% whitesox_svm_model$SV
W
#Find value of b
b = whitesox_svm_model$rho
b
whitesox_rmse <- (rmse(final_whitesox_svm_pred,test_whitesox_svm$Attendance.G))/test_whitesox_svm$Attendance.G
whitesox_mad <- (mae(final_whitesox_svm_pred, test_whitesox_svm$Attendance.G))/test_whitesox_svm$Attendance.G
whitesox_mape <- mape(final_whitesox_svm_pred, test_whitesox_svm$Attendance.G)
Team = "Chicago White Sox"
whitesox_svm_evaluation <- cbind(Team, whitesox_rmse, whitesox_mad, whitesox_mape)
colnames(whitesox_svm_evaluation)[2] <- "RMSE"
colnames(whitesox_svm_evaluation)[3] <- "MAD"
colnames(whitesox_svm_evaluation)[4] <- "MAPE"
whitesox_svm_evaluation
whitesox_svm_output <- cbind(Team,test_whitesox_svm$Attendance.G,final_whitesox_svm_pred)
colnames(whitesox_svm_output)[2] <- "Actual"
colnames(whitesox_svm_output)[3] <- "Predicted"
whitesox_svm_output
_____________________________________________________________________________________________________________

reds_svm <- read.csv("H:/RIC/Cincinnati Reds/Final.csv", header = TRUE)

# Create training and test set
train_reds_svm = reds_svm[ 2:119, 5:37 ]
test_reds_svm = reds_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(reds_svm, select = c(36:38))
plot(daz)

#Regression with SVM
reds_svm_model <- svm(Attendance.G ~., data = train_reds_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(reds_svm_model, train_reds_svm)

#Predict using SVM regression
reds_svm_pred <- predict(reds_svm_model, test_reds_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_reds_svm[1], reds_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_reds_svm=tune(svm, Attendance.G ~., data=train_reds_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_reds_svm)

#finding the bestmodel
bstmodel_reds_svm <- OptModel_reds_svm$best.model

#Predict outcome using best model
best_reds_svm_pred <- predict(bstmodel_reds_svm, train_reds_svm)
final_reds_svm_pred <- mean(best_reds_svm_pred)
#Find value of W
W = t(reds_svm_model$coefs) %*% reds_svm_model$SV
W
#Find value of b
b = reds_svm_model$rho
b
reds_rmse <- (rmse(final_reds_svm_pred,test_reds_svm$Attendance.G))/test_reds_svm$Attendance.G
reds_mad <- (mae(final_reds_svm_pred, test_reds_svm$Attendance.G))/test_reds_svm$Attendance.G
reds_mape <- mape(final_reds_svm_pred, test_reds_svm$Attendance.G)
Team = "Cincinnati Reds"
reds_svm_evaluation <- cbind(Team, reds_rmse, reds_mad, reds_mape)
colnames(reds_svm_evaluation)[2] <- "RMSE"
colnames(reds_svm_evaluation)[3] <- "MAD"
colnames(reds_svm_evaluation)[4] <- "MAPE"
reds_svm_evaluation
reds_svm_output <- cbind(Team,test_reds_svm$Attendance.G,final_reds_svm_pred)
colnames(reds_svm_output)[2] <- "Actual"
colnames(reds_svm_output)[3] <- "Predicted"
reds_svm_output
_____________________________________________________________________________________________________________

indians_svm <- read.csv("H:/RIC/Cleveland Indians/Final.csv", header = TRUE)

# Create training and test set
train_indians_svm = indians_svm[ 2:119, 5:37 ]
test_indians_svm = indians_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(indians_svm, select = c(36:38))
plot(daz)

#Regression with SVM
indians_svm_model <- svm(Attendance.G ~., data = train_indians_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(indians_svm_model, train_indians_svm)

#Predict using SVM regression
indians_svm_pred <- predict(indians_svm_model, test_indians_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_indians_svm[1], indians_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_indians_svm=tune(svm, Attendance.G ~., data=train_indians_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_indians_svm)

#finding the bestmodel
bstmodel_indians_svm <- OptModel_indians_svm$best.model

#Predict outcome using best model
best_indians_svm_pred <- predict(bstmodel_indians_svm, train_indians_svm)
final_indians_svm_pred <- mean(best_indians_svm_pred)
#Find value of W
W = t(indians_svm_model$coefs) %*% indians_svm_model$SV
W
#Find value of b
b = indians_svm_model$rho
b
indians_rmse <- (rmse(final_indians_svm_pred,test_indians_svm$Attendance.G))/test_indians_svm$Attendance.G
indians_mad <- (mae(final_indians_svm_pred, test_indians_svm$Attendance.G))/test_indians_svm$Attendance.G
indians_mape <- mape(final_indians_svm_pred, test_indians_svm$Attendance.G)
Team = "Cleveland Indians"
indians_svm_evaluation <- cbind(Team, indians_rmse, indians_mad, indians_mape)
colnames(indians_svm_evaluation)[2] <- "RMSE"
colnames(indians_svm_evaluation)[3] <- "MAD"
colnames(indians_svm_evaluation)[4] <- "MAPE"
indians_svm_evaluation
indians_svm_output <- cbind(Team,test_indians_svm$Attendance.G,final_indians_svm_pred)
colnames(indians_svm_output)[2] <- "Actual"
colnames(indians_svm_output)[3] <- "Predicted"
indians_svm_output
____________________________________________________________________________________________________________

rockies_svm <- read.csv("H:/RIC/Colorado Rockies/Final.csv", header = TRUE)

# Create training and test set
train_rockies_svm = rockies_svm[ 2:27, 5:37 ]
test_rockies_svm = rockies_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(rockies_svm, select = c(36:38))
plot(daz)

#Regression with SVM
rockies_svm_model <- svm(Attendance.G ~., data = train_rockies_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(rockies_svm_model, train_rockies_svm)

#Predict using SVM regression
rockies_svm_pred <- predict(rockies_svm_model, test_rockies_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_rockies_svm[1], rockies_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_rockies_svm=tune(svm, Attendance.G ~., data=train_rockies_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_rockies_svm)

#finding the bestmodel
bstmodel_rockies_svm <- OptModel_rockies_svm$best.model

#Predict outcome using best model
best_rockies_svm_pred <- predict(bstmodel_rockies_svm, train_rockies_svm)
final_rockies_svm_pred <- mean(best_rockies_svm_pred)
#Find value of W
W = t(rockies_svm_model$coefs) %*% rockies_svm_model$SV
W
#Find value of b
b = rockies_svm_model$rho
b
rockies_rmse <- (rmse(final_rockies_svm_pred,test_rockies_svm$Attendance.G))/test_rockies_svm$Attendance.G
rockies_mad <- (mae(final_rockies_svm_pred, test_rockies_svm$Attendance.G))/test_rockies_svm$Attendance.G
rockies_mape <- mape(final_rockies_svm_pred, test_rockies_svm$Attendance.G)
Team = "Colorado Rockies"
rockies_svm_evaluation <- cbind(Team, rockies_rmse, rockies_mad, rockies_mape)
colnames(rockies_svm_evaluation)[2] <- "RMSE"
colnames(rockies_svm_evaluation)[3] <- "MAD"
colnames(rockies_svm_evaluation)[4] <- "MAPE"
rockies_svm_evaluation
rockies_svm_output <- cbind(Team,test_rockies_svm$Attendance.G,final_rockies_svm_pred)
colnames(rockies_svm_output)[2] <- "Actual"
colnames(rockies_svm_output)[3] <- "Predicted"
rockies_svm_output
____________________________________________________________________________________________________________

tigers_svm <- read.csv("H:/RIC/Detroit Tigers/Final.csv", header = TRUE)

# Create training and test set
train_tigers_svm = tigers_svm[ 2:119, 5:37 ]
test_tigers_svm = tigers_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(tigers_svm, select = c(36:38))
plot(daz)

#Regression with SVM
tigers_svm_model <- svm(Attendance.G ~., data = train_tigers_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(tigers_svm_model, train_tigers_svm)

#Predict using SVM regression
tigers_svm_pred <- predict(tigers_svm_model, test_tigers_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_tigers_svm[1], tigers_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_tigers_svm=tune(svm, Attendance.G ~., data=train_tigers_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_tigers_svm)

#finding the bestmodel
bstmodel_tigers_svm <- OptModel_tigers_svm$best.model

#Predict outcome using best model
best_tigers_svm_pred <- predict(bstmodel_tigers_svm, train_tigers_svm)
final_tigers_svm_pred <- mean(best_tigers_svm_pred)
#Find value of W
W = t(tigers_svm_model$coefs) %*% tigers_svm_model$SV
W
#Find value of b
b = tigers_svm_model$rho
b
tigers_rmse <- (rmse(final_tigers_svm_pred,test_tigers_svm$Attendance.G))/test_tigers_svm$Attendance.G
tigers_mad <- (mae(final_tigers_svm_pred, test_tigers_svm$Attendance.G))/test_tigers_svm$Attendance.G
tigers_mape <- mape(final_tigers_svm_pred, test_tigers_svm$Attendance.G)
Team = "Detroit Tigers"
tigers_svm_evaluation <- cbind(Team, tigers_rmse, tigers_mad, tigers_mape)
colnames(tigers_svm_evaluation)[2] <- "RMSE"
colnames(tigers_svm_evaluation)[3] <- "MAD"
colnames(tigers_svm_evaluation)[4] <- "MAPE"
tigers_svm_evaluation
tigers_svm_output <- cbind(Team,test_tigers_svm$Attendance.G,final_tigers_svm_pred)
colnames(tigers_svm_output)[2] <- "Actual"
colnames(tigers_svm_output)[3] <- "Predicted"
tigers_svm_output
____________________________________________________________________________________________________________

astros_svm <- read.csv("H:/RIC/Houston Astros/Final.csv", header = TRUE)

# Create training and test set
train_astros_svm = astros_svm[ 2:58, 5:37 ]
test_astros_svm = astros_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(astros_svm, select = c(36:38))
plot(daz)

#Regression with SVM
astros_svm_model <- svm(Attendance.G ~., data = train_astros_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(astros_svm_model, train_astros_svm)

#Predict using SVM regression
astros_svm_pred <- predict(astros_svm_model, test_astros_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_astros_svm[1], astros_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_astros_svm=tune(svm, Attendance.G ~., data=train_astros_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_astros_svm)

#finding the bestmodel
bstmodel_astros_svm <- OptModel_astros_svm$best.model

#Predict outcome using best model
best_astros_svm_pred <- predict(bstmodel_astros_svm, train_astros_svm)
final_astros_svm_pred <- mean(best_astros_svm_pred)
#Find value of W
W = t(astros_svm_model$coefs) %*% astros_svm_model$SV
W
#Find value of b
b = astros_svm_model$rho
b
astros_rmse <- (rmse(final_astros_svm_pred,test_astros_svm$Attendance.G))/test_astros_svm$Attendance.G
astros_mad <- (mae(final_astros_svm_pred, test_astros_svm$Attendance.G))/test_astros_svm$Attendance.G
astros_mape <- mape(final_astros_svm_pred, test_astros_svm$Attendance.G)
Team = "Houston Astros"
astros_svm_evaluation <- cbind(Team, astros_rmse, astros_mad, astros_mape)
colnames(astros_svm_evaluation)[2] <- "RMSE"
colnames(astros_svm_evaluation)[3] <- "MAD"
colnames(astros_svm_evaluation)[4] <- "MAPE"
astros_svm_evaluation
astros_svm_output <- cbind(Team,test_astros_svm$Attendance.G,final_astros_svm_pred)
colnames(astros_svm_output)[2] <- "Actual"
colnames(astros_svm_output)[3] <- "Predicted"
astros_svm_output
____________________________________________________________________________________________________________

royals_svm <- read.csv("H:/RIC/Kansas City Royals/Final.csv", header = TRUE)

# Create training and test set
train_royals_svm = royals_svm[ 2:51, 5:37 ]
test_royals_svm = royals_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(royals_svm, select = c(36:38))
plot(daz)

#Regression with SVM
royals_svm_model <- svm(Attendance.G ~., data = train_royals_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(royals_svm_model, train_royals_svm)

#Predict using SVM regression
royals_svm_pred <- predict(royals_svm_model, test_royals_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_royals_svm[1], royals_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_royals_svm=tune(svm, Attendance.G ~., data=train_royals_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_royals_svm)

#finding the bestmodel
bstmodel_royals_svm <- OptModel_royals_svm$best.model

#Predict outcome using best model
best_royals_svm_pred <- predict(bstmodel_royals_svm, train_royals_svm)
final_royals_svm_pred <- mean(best_royals_svm_pred)
#Find value of W
W = t(royals_svm_model$coefs) %*% royals_svm_model$SV
W
#Find value of b
b = royals_svm_model$rho
b
royals_rmse <- (rmse(final_royals_svm_pred,test_royals_svm$Attendance.G))/test_royals_svm$Attendance.G
royals_mad <- (mae(final_royals_svm_pred, test_royals_svm$Attendance.G))/test_royals_svm$Attendance.G
royals_mape <- mape(final_royals_svm_pred, test_royals_svm$Attendance.G)
Team = "Kansas City Royals"
royals_svm_evaluation <- cbind(Team, royals_rmse, royals_mad, royals_mape)
colnames(royals_svm_evaluation)[2] <- "RMSE"
colnames(royals_svm_evaluation)[3] <- "MAD"
colnames(royals_svm_evaluation)[4] <- "MAPE"
royals_svm_evaluation
royals_svm_output <- cbind(Team,test_royals_svm$Attendance.G,final_royals_svm_pred)
colnames(royals_svm_output)[2] <- "Actual"
colnames(royals_svm_output)[3] <- "Predicted"
royals_svm_output
_____________________________________________________________________________________________________________

angels_svm <- read.csv("H:/RIC/Los Angeles Angels/Final.csv", header = TRUE)

# Create training and test set
train_angels_svm = angels_svm[ 2:51, 5:37 ]
test_angels_svm = angels_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(angels_svm, select = c(36:38))
plot(daz)

#Regression with SVM
angels_svm_model <- svm(Attendance.G ~., data = train_angels_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(angels_svm_model, train_angels_svm)

#Predict using SVM regression
angels_svm_pred <- predict(angels_svm_model, test_angels_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_angels_svm[1], angels_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_angels_svm=tune(svm, Attendance.G ~., data=train_angels_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_angels_svm)

#finding the bestmodel
bstmodel_angels_svm <- OptModel_angels_svm$best.model

#Predict outcome using best model
best_angels_svm_pred <- predict(bstmodel_angels_svm, train_angels_svm)
final_angels_svm_pred <- mean(best_angels_svm_pred)
#Find value of W
W = t(angels_svm_model$coefs) %*% angels_svm_model$SV
W
#Find value of b
b = angels_svm_model$rho
b
angels_rmse <- (rmse(final_angels_svm_pred,test_angels_svm$Attendance.G))/test_angels_svm$Attendance.G
angels_mad <- (mae(final_angels_svm_pred, test_angels_svm$Attendance.G))/test_angels_svm$Attendance.G
angels_mape <- mape(final_angels_svm_pred, test_angels_svm$Attendance.G)
Team = "Los Angeles Angels"
angels_svm_evaluation <- cbind(Team, angels_rmse, angels_mad, angels_mape)
colnames(angels_svm_evaluation)[2] <- "RMSE"
colnames(angels_svm_evaluation)[3] <- "MAD"
colnames(angels_svm_evaluation)[4] <- "MAPE"
angels_svm_evaluation
angels_svm_output <- cbind(Team,test_angels_svm$Attendance.G,final_angels_svm_pred)
colnames(angels_svm_output)[2] <- "Actual"
colnames(angels_svm_output)[3] <- "Predicted"
angels_svm_output
____________________________________________________________________________________________________________

dodgers_svm <- read.csv("H:/RIC/Los Angeles Dodgers/Final.csv", header = TRUE)

# Create training and test set
train_dodgers_svm = dodgers_svm[ 2:130, 5:37 ]
test_dodgers_svm = dodgers_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(dodgers_svm, select = c(36:38))
plot(daz)

#Regression with SVM
dodgers_svm_model <- svm(Attendance.G ~., data = train_dodgers_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(dodgers_svm_model, train_dodgers_svm)

#Predict using SVM regression
dodgers_svm_pred <- predict(dodgers_svm_model, test_dodgers_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_dodgers_svm[1], dodgers_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_dodgers_svm=tune(svm, Attendance.G ~., data=train_dodgers_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_dodgers_svm)

#finding the bestmodel
bstmodel_dodgers_svm <- OptModel_dodgers_svm$best.model

#Predict outcome using best model
best_dodgers_svm_pred <- predict(bstmodel_dodgers_svm, train_dodgers_svm)
final_dodgers_svm_pred <- mean(best_dodgers_svm_pred)
#Find value of W
W = t(dodgers_svm_model$coefs) %*% dodgers_svm_model$SV
W
#Find value of b
b = dodgers_svm_model$rho
b
dodgers_rmse <- (rmse(final_dodgers_svm_pred,test_dodgers_svm$Attendance.G))/test_dodgers_svm$Attendance.G
dodgers_mad <- (mae(final_dodgers_svm_pred, test_dodgers_svm$Attendance.G))/test_dodgers_svm$Attendance.G
dodgers_mape <- mape(final_dodgers_svm_pred, test_dodgers_svm$Attendance.G)
Team = "Los Angeles Dodgers"
dodgers_svm_evaluation <- cbind(Team, dodgers_rmse, dodgers_mad, dodgers_mape)
colnames(dodgers_svm_evaluation)[2] <- "RMSE"
colnames(dodgers_svm_evaluation)[3] <- "MAD"
colnames(dodgers_svm_evaluation)[4] <- "MAPE"
dodgers_svm_evaluation
dodgers_svm_output <- cbind(Team,test_dodgers_svm$Attendance.G,final_dodgers_svm_pred)
colnames(dodgers_svm_output)[2] <- "Actual"
colnames(dodgers_svm_output)[3] <- "Predicted"
dodgers_svm_output
_____________________________________________________________________________________________________________

marlins_svm <- read.csv("H:/RIC/Miami Marlins/Final.csv", header = TRUE)

# Create training and test set
train_marlins_svm = marlins_svm[ 2:27, 5:37 ]
test_marlins_svm = marlins_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(marlins_svm, select = c(36:38))
plot(daz)

#Regression with SVM
marlins_svm_model <- svm(Attendance.G ~., data = train_marlins_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(marlins_svm_model, train_marlins_svm)

#Predict using SVM regression
marlins_svm_pred <- predict(marlins_svm_model, test_marlins_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_marlins_svm[1], marlins_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_marlins_svm=tune(svm, Attendance.G ~., data=train_marlins_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_marlins_svm)

#finding the bestmodel
bstmodel_marlins_svm <- OptModel_marlins_svm$best.model

#Predict outcome using best model
best_marlins_svm_pred <- predict(bstmodel_marlins_svm, train_marlins_svm)
final_marlins_svm_pred <- mean(best_marlins_svm_pred)
#Find value of W
W = t(marlins_svm_model$coefs) %*% marlins_svm_model$SV
W
#Find value of b
b = marlins_svm_model$rho
b
marlins_rmse <- (rmse(final_marlins_svm_pred,test_marlins_svm$Attendance.G))/test_marlins_svm$Attendance.G
marlins_mad <- (mae(final_marlins_svm_pred, test_marlins_svm$Attendance.G))/test_marlins_svm$Attendance.G
marlins_mape <- mape(final_marlins_svm_pred, test_marlins_svm$Attendance.G)
Team = "Miami Marlins"
marlins_svm_evaluation <- cbind(Team, marlins_rmse, marlins_mad, marlins_mape)
colnames(marlins_svm_evaluation)[2] <- "RMSE"
colnames(marlins_svm_evaluation)[3] <- "MAD"
colnames(marlins_svm_evaluation)[4] <- "MAPE"
marlins_svm_evaluation
marlins_svm_output <- cbind(Team,test_marlins_svm$Attendance.G,final_marlins_svm_pred)
colnames(marlins_svm_output)[2] <- "Actual"
colnames(marlins_svm_output)[3] <- "Predicted"
marlins_svm_output
_____________________________________________________________________________________________________________

brewers_svm <- read.csv("H:/RIC/Milwaukee Brewers/Final.csv", header = TRUE)

# Create training and test set
train_brewers_svm = brewers_svm[ 2:51, 5:37 ]
test_brewers_svm = brewers_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(brewers_svm, select = c(36:38))
plot(daz)

#Regression with SVM
brewers_svm_model <- svm(Attendance.G ~., data = train_brewers_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(brewers_svm_model, train_brewers_svm)

#Predict using SVM regression
brewers_svm_pred <- predict(brewers_svm_model, test_brewers_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_brewers_svm[1], brewers_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_brewers_svm=tune(svm, Attendance.G ~., data=train_brewers_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_brewers_svm)

#finding the bestmodel
bstmodel_brewers_svm <- OptModel_brewers_svm$best.model

#Predict outcome using best model
best_brewers_svm_pred <- predict(bstmodel_brewers_svm, train_brewers_svm)
final_brewers_svm_pred <- mean(best_brewers_svm_pred)
#Find value of W
W = t(brewers_svm_model$coefs) %*% brewers_svm_model$SV
W
#Find value of b
b = brewers_svm_model$rho
b
brewers_rmse <- (rmse(final_brewers_svm_pred,test_brewers_svm$Attendance.G))/test_brewers_svm$Attendance.G
brewers_mad <- (mae(final_brewers_svm_pred, test_brewers_svm$Attendance.G))/test_brewers_svm$Attendance.G
brewers_mape <- mape(final_brewers_svm_pred, test_brewers_svm$Attendance.G)
Team = "Milwaukee Brewers"
brewers_svm_evaluation <- cbind(Team, brewers_rmse, brewers_mad, brewers_mape)
colnames(brewers_svm_evaluation)[2] <- "RMSE"
colnames(brewers_svm_evaluation)[3] <- "MAD"
colnames(brewers_svm_evaluation)[4] <- "MAPE"
brewers_svm_evaluation
brewers_svm_output <- cbind(Team,test_brewers_svm$Attendance.G,final_brewers_svm_pred)
colnames(brewers_svm_output)[2] <- "Actual"
colnames(brewers_svm_output)[3] <- "Predicted"
brewers_svm_output
____________________________________________________________________________________________________________

twins_svm <- read.csv("H:/RIC/Minnesota Twins/Final.csv", header = TRUE)

# Create training and test set
train_twins_svm = twins_svm[ 2:51, 5:37 ]
test_twins_svm = twins_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(twins_svm, select = c(36:38))
plot(daz)

#Regression with SVM
twins_svm_model <- svm(Attendance.G ~., data = train_twins_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(twins_svm_model, train_twins_svm)

#Predict using SVM regression
twins_svm_pred <- predict(twins_svm_model, test_twins_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_twins_svm[1], twins_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_twins_svm=tune(svm, Attendance.G ~., data=train_twins_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_twins_svm)

#finding the bestmodel
bstmodel_twins_svm <- OptModel_twins_svm$best.model

#Predict outcome using best model
best_twins_svm_pred <- predict(bstmodel_twins_svm, train_twins_svm)
final_twins_svm_pred <- mean(best_twins_svm_pred)
#Find value of W
W = t(twins_svm_model$coefs) %*% twins_svm_model$SV
W
#Find value of b
b = twins_svm_model$rho
b
twins_rmse <- (rmse(final_twins_svm_pred,test_twins_svm$Attendance.G))/test_twins_svm$Attendance.G
twins_mad <- (mae(final_twins_svm_pred, test_twins_svm$Attendance.G))/test_twins_svm$Attendance.G
twins_mape <- mape(final_twins_svm_pred, test_twins_svm$Attendance.G)
Team = "Minnesota Twins"
twins_svm_evaluation <- cbind(Team, twins_rmse, twins_mad, twins_mape)
colnames(twins_svm_evaluation)[2] <- "RMSE"
colnames(twins_svm_evaluation)[3] <- "MAD"
colnames(twins_svm_evaluation)[4] <- "MAPE"
twins_svm_evaluation
twins_svm_output <- cbind(Team,test_twins_svm$Attendance.G,final_twins_svm_pred)
colnames(twins_svm_output)[2] <- "Actual"
colnames(twins_svm_output)[3] <- "Predicted"
twins_svm_output
____________________________________________________________________________________________________________

mets_svm <- read.csv("H:/RIC/New York Mets/Final.csv", header = TRUE)

# Create training and test set
train_mets_svm = mets_svm[ 2:58, 5:37 ]
test_mets_svm = mets_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(mets_svm, select = c(36:38))
plot(daz)

#Regression with SVM
mets_svm_model <- svm(Attendance.G ~., data = train_mets_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(mets_svm_model, train_mets_svm)

#Predict using SVM regression
mets_svm_pred <- predict(mets_svm_model, test_mets_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_mets_svm[1], mets_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_mets_svm=tune(svm, Attendance.G ~., data=train_mets_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_mets_svm)

#finding the bestmodel
bstmodel_mets_svm <- OptModel_mets_svm$best.model

#Predict outcome using best model
best_mets_svm_pred <- predict(bstmodel_mets_svm, train_mets_svm)
final_mets_svm_pred <- mean(best_mets_svm_pred)
#Find value of W
W = t(mets_svm_model$coefs) %*% mets_svm_model$SV
W
#Find value of b
b = mets_svm_model$rho
b
mets_rmse <- (rmse(final_mets_svm_pred,test_mets_svm$Attendance.G))/test_mets_svm$Attendance.G
mets_mad <- (mae(final_mets_svm_pred, test_mets_svm$Attendance.G))/test_mets_svm$Attendance.G
mets_mape <- mape(final_mets_svm_pred, test_mets_svm$Attendance.G)
Team = "New York Mets"
mets_svm_evaluation <- cbind(Team, mets_rmse, mets_mad, mets_mape)
colnames(mets_svm_evaluation)[2] <- "RMSE"
colnames(mets_svm_evaluation)[3] <- "MAD"
colnames(mets_svm_evaluation)[4] <- "MAPE"
mets_svm_evaluation
mets_svm_output <- cbind(Team,test_mets_svm$Attendance.G,final_mets_svm_pred)
colnames(mets_svm_output)[2] <- "Actual"
colnames(mets_svm_output)[3] <- "Predicted"
mets_svm_output
____________________________________________________________________________________________________________

yankees_svm <- read.csv("H:/RIC/New York Yankees/Final.csv", header = TRUE)

# Create training and test set
train_yankees_svm = yankees_svm[ 2:117, 5:37 ]
test_yankees_svm = yankees_svm[ 1, 5:37 ]

#Scatter Plot
daz <- subset(yankees_svm, select = c(36:38))
plot(daz)

#Regression with SVM
yankees_svm_model <- svm(Attendance.G ~., data = train_yankees_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(yankees_svm_model, train_yankees_svm)

#Predict using SVM regression
yankees_svm_pred <- predict(yankees_svm_model, test_yankees_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_yankees_svm[1], yankees_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_yankees_svm=tune(svm, Attendance.G ~., data=train_yankees_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_yankees_svm)

#finding the bestmodel
bstmodel_yankees_svm <- OptModel_yankees_svm$best.model

#Predict outcome using best model
best_yankees_svm_pred <- predict(bstmodel_yankees_svm, train_yankees_svm)
final_yankees_svm_pred <- mean(best_yankees_svm_pred)
#Find value of W
W = t(yankees_svm_model$coefs) %*% yankees_svm_model$SV
W
#Find value of b
b = yankees_svm_model$rho
b
yankees_rmse <- (rmse(final_yankees_svm_pred,test_yankees_svm$Attendance.G))/test_yankees_svm$Attendance.G
yankees_mad <- (mae(final_yankees_svm_pred, test_yankees_svm$Attendance.G))/test_yankees_svm$Attendance.G
yankees_mape <- mape(final_yankees_svm_pred, test_yankees_svm$Attendance.G)
Team = "New York Yankees"
yankees_svm_evaluation <- cbind(Team, yankees_rmse, yankees_mad, yankees_mape)
colnames(yankees_svm_evaluation)[2] <- "RMSE"
colnames(yankees_svm_evaluation)[3] <- "MAD"
colnames(yankees_svm_evaluation)[4] <- "MAPE"
yankees_svm_evaluation
yankees_svm_output <- cbind(Team,test_yankees_svm$Attendance.G,final_yankees_svm_pred)
colnames(yankees_svm_output)[2] <- "Actual"
colnames(yankees_svm_output)[3] <- "Predicted"
yankees_svm_output
____________________________________________________________________________________________________________

athletics_svm <- read.csv("H:/RIC/Oakland Athletics/Final.csv", header = TRUE)

# Create training and test set
train_athletics_svm = athletics_svm[ 2:119, 5:38 ]
test_athletics_svm = athletics_svm[ 1, 5:38 ]

#Scatter Plot
daz <- subset(athletics_svm, select = c(36:38))
plot(daz)

#Regression with SVM
athletics_svm_model <- svm(Attendance.G ~., data = train_athletics_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(athletics_svm_model, train_athletics_svm)

#Predict using SVM regression
athletics_svm_pred <- predict(athletics_svm_model, test_athletics_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_athletics_svm[1], athletics_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_athletics_svm=tune(svm, Attendance.G ~., data=train_athletics_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_athletics_svm)

#finding the bestmodel
bstmodel_athletics_svm <- OptModel_athletics_svm$best.model

#Predict outcome using best model
best_athletics_svm_pred <- predict(bstmodel_athletics_svm, train_athletics_svm)
final_athletics_svm_pred <- mean(best_athletics_svm_pred)
#Find value of W
W = t(athletics_svm_model$coefs) %*% athletics_svm_model$SV
W
#Find value of b
b = athletics_svm_model$rho
b
athletics_rmse <- (rmse(final_athletics_svm_pred,test_athletics_svm$Attendance.G))/test_athletics_svm$Attendance.G
athletics_mad <- (mae(final_athletics_svm_pred, test_athletics_svm$Attendance.G))/test_athletics_svm$Attendance.G
athletics_mape <- mape(final_athletics_svm_pred, test_athletics_svm$Attendance.G)
Team = "Oakland Athletics"
athletics_svm_evaluation <- cbind(Team, athletics_rmse, athletics_mad, athletics_mape)
colnames(athletics_svm_evaluation)[2] <- "RMSE"
colnames(athletics_svm_evaluation)[3] <- "MAD"
colnames(athletics_svm_evaluation)[4] <- "MAPE"
athletics_svm_evaluation
athletics_svm_output <- cbind(Team,test_athletics_svm$Attendance.G,final_athletics_svm_pred)
colnames(athletics_svm_output)[2] <- "Actual"
colnames(athletics_svm_output)[3] <- "Predicted"
athletics_svm_output
___________________________________________________________________________________________________________

phillies_svm <- read.csv("H:/RIC/Philadelphia Phillies/Final.csv", header = TRUE)

# Create training and test set
train_phillies_svm = phillies_svm[ 2:119, 5:38 ]
test_phillies_svm = phillies_svm[ 1, 5:38 ]

#Scatter Plot
daz <- subset(phillies_svm, select = c(36:38))
plot(daz)

#Regression with SVM
phillies_svm_model <- svm(Attendance.G ~., data = train_phillies_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(phillies_svm_model, train_phillies_svm)

#Predict using SVM regression
phillies_svm_pred <- predict(phillies_svm_model, test_phillies_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_phillies_svm[1], phillies_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_phillies_svm=tune(svm, Attendance.G ~., data=train_phillies_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_phillies_svm)

#finding the bestmodel
bstmodel_phillies_svm <- OptModel_phillies_svm$best.model

#Predict outcome using best model
best_phillies_svm_pred <- predict(bstmodel_phillies_svm, train_phillies_svm)
final_phillies_svm_pred <- mean(best_phillies_svm_pred)
#Find value of W
W = t(phillies_svm_model$coefs) %*% phillies_svm_model$SV
W
#Find value of b
b = phillies_svm_model$rho
b
phillies_rmse <- (rmse(final_phillies_svm_pred,test_phillies_svm$Attendance.G))/test_phillies_svm$Attendance.G
phillies_mad <- (mae(final_phillies_svm_pred, test_phillies_svm$Attendance.G))/test_phillies_svm$Attendance.G
phillies_mape <- mape(final_phillies_svm_pred, test_phillies_svm$Attendance.G)
Team = "Philadelphia Phillies"
phillies_svm_evaluation <- cbind(Team, phillies_rmse, phillies_mad, phillies_mape)
colnames(phillies_svm_evaluation)[2] <- "RMSE"
colnames(phillies_svm_evaluation)[3] <- "MAD"
colnames(phillies_svm_evaluation)[4] <- "MAPE"
phillies_svm_evaluation
phillies_svm_output <- cbind(Team,test_phillies_svm$Attendance.G,final_phillies_svm_pred)
colnames(phillies_svm_output)[2] <- "Actual"
colnames(phillies_svm_output)[3] <- "Predicted"
phillies_svm_output
____________________________________________________________________________________________________________

pirates_svm <- read.csv("H:/RIC/Pittsburgh Pirates/Final.csv", header = TRUE)

# Create training and test set
train_pirates_svm = pirates_svm[ 2:130, 5:38 ]
test_pirates_svm = pirates_svm[ 1, 5:38 ]

#Scatter Plot
daz <- subset(pirates_svm, select = c(36:38))
plot(daz)

#Regression with SVM
pirates_svm_model <- svm(Attendance.G ~., data = train_pirates_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(pirates_svm_model, train_pirates_svm)

#Predict using SVM regression
pirates_svm_pred <- predict(pirates_svm_model, test_pirates_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_pirates_svm[1], pirates_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_pirates_svm=tune(svm, Attendance.G ~., data=train_pirates_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_pirates_svm)

#finding the bestmodel
bstmodel_pirates_svm <- OptModel_pirates_svm$best.model

#Predict outcome using best model
best_pirates_svm_pred <- predict(bstmodel_pirates_svm, train_pirates_svm)
final_pirates_svm_pred <- mean(best_pirates_svm_pred)
#Find value of W
W = t(pirates_svm_model$coefs) %*% pirates_svm_model$SV
W
#Find value of b
b = pirates_svm_model$rho
b
pirates_rmse <- (rmse(final_pirates_svm_pred,test_pirates_svm$Attendance.G))/test_pirates_svm$Attendance.G
pirates_mad <- (mae(final_pirates_svm_pred, test_pirates_svm$Attendance.G))/test_pirates_svm$Attendance.G
pirates_mape <- mape(final_pirates_svm_pred, test_pirates_svm$Attendance.G)
Team = "Pittsburgh Pirates"
pirates_svm_evaluation <- cbind(Team, pirates_rmse, pirates_mad, pirates_mape)
colnames(pirates_svm_evaluation)[2] <- "RMSE"
colnames(pirates_svm_evaluation)[3] <- "MAD"
colnames(pirates_svm_evaluation)[4] <- "MAPE"
pirates_svm_evaluation
pirates_svm_output <- cbind(Team,test_pirates_svm$Attendance.G,final_pirates_svm_pred)
colnames(pirates_svm_output)[2] <- "Actual"
colnames(pirates_svm_output)[3] <- "Predicted"
pirates_svm_output
____________________________________________________________________________________________________________

padres_svm <- read.csv("H:/RIC/San Diego Padres/Final.csv", header = TRUE)

# Create training and test set
train_padres_svm = padres_svm[ 2:51, 5:38 ]
test_padres_svm = padres_svm[ 1, 5:38 ]

#Scatter Plot
daz <- subset(padres_svm, select = c(36:38))
plot(daz)

#Regression with SVM
padres_svm_model <- svm(Attendance.G ~., data = train_padres_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(padres_svm_model, train_padres_svm)

#Predict using SVM regression
padres_svm_pred <- predict(padres_svm_model, test_padres_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_padres_svm[1], padres_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_padres_svm=tune(svm, Attendance.G ~., data=train_padres_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_padres_svm)

#finding the bestmodel
bstmodel_padres_svm <- OptModel_padres_svm$best.model

#Predict outcome using best model
best_padres_svm_pred <- predict(bstmodel_padres_svm, train_padres_svm)
final_padres_svm_pred <- mean(best_padres_svm_pred)
#Find value of W
W = t(padres_svm_model$coefs) %*% padres_svm_model$SV
W
#Find value of b
b = padres_svm_model$rho
b
padres_rmse <- (rmse(final_padres_svm_pred,test_padres_svm$Attendance.G))/test_padres_svm$Attendance.G
padres_mad <- (mae(final_padres_svm_pred, test_padres_svm$Attendance.G))/test_padres_svm$Attendance.G
padres_mape <- mape(final_padres_svm_pred, test_padres_svm$Attendance.G)
Team = "San Diego Padres"
padres_svm_evaluation <- cbind(Team, padres_rmse, padres_mad, padres_mape)
colnames(padres_svm_evaluation)[2] <- "RMSE"
colnames(padres_svm_evaluation)[3] <- "MAD"
colnames(padres_svm_evaluation)[4] <- "MAPE"
padres_svm_evaluation
padres_svm_output <- cbind(Team,test_padres_svm$Attendance.G,final_padres_svm_pred)
colnames(padres_svm_output)[2] <- "Actual"
colnames(padres_svm_output)[3] <- "Predicted"
padres_svm_output
____________________________________________________________________________________________________________

giants_svm <- read.csv("H:/RIC/San Fransisco Giants/Final.csv", header = TRUE)

# Create training and test set
train_giants_svm = giants_svm[ 2:130, 5:38 ]
test_giants_svm = giants_svm[ 1, 5:38 ]

#Scatter Plot
daz <- subset(giants_svm, select = c(36:38))
plot(daz)

#Regression with SVM
giants_svm_model <- svm(Attendance.G ~., data = train_giants_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(giants_svm_model, train_giants_svm)

#Predict using SVM regression
giants_svm_pred <- predict(giants_svm_model, test_giants_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_giants_svm[1], giants_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_giants_svm=tune(svm, Attendance.G ~., data=train_giants_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_giants_svm)

#finding the bestmodel
bstmodel_giants_svm <- OptModel_giants_svm$best.model

#Predict outcome using best model
best_giants_svm_pred <- predict(bstmodel_giants_svm, train_giants_svm)
final_giants_svm_pred <- mean(best_giants_svm_pred)
#Find value of W
W = t(giants_svm_model$coefs) %*% giants_svm_model$SV
W
#Find value of b
b = giants_svm_model$rho
b
giants_rmse <- (rmse(final_giants_svm_pred,test_giants_svm$Attendance.G))/test_giants_svm$Attendance.G
giants_mad <- (mae(final_giants_svm_pred, test_giants_svm$Attendance.G))/test_giants_svm$Attendance.G
giants_mape <- mape(final_giants_svm_pred, test_giants_svm$Attendance.G)
Team = "San Fransisco Giants"
giants_svm_evaluation <- cbind(Team, giants_rmse, giants_mad, giants_mape)
colnames(giants_svm_evaluation)[2] <- "RMSE"
colnames(giants_svm_evaluation)[3] <- "MAD"
colnames(giants_svm_evaluation)[4] <- "MAPE"
giants_svm_evaluation
giants_svm_output <- cbind(Team,test_giants_svm$Attendance.G,final_giants_svm_pred)
colnames(giants_svm_output)[2] <- "Actual"
colnames(giants_svm_output)[3] <- "Predicted"
giants_svm_output
___________________________________________________________________________________________________________

mariners_svm <- read.csv("H:/RIC/Seattle Mariners/Final.csv", header = TRUE)

# Create training and test set
train_mariners_svm = mariners_svm[ 2:42, 5:38 ]
test_mariners_svm = mariners_svm[ 1, 5:38 ]

#Scatter Plot
daz <- subset(mariners_svm, select = c(36:38))
plot(daz)

#Regression with SVM
mariners_svm_model <- svm(Attendance.G ~., data = train_mariners_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(mariners_svm_model, train_mariners_svm)

#Predict using SVM regression
mariners_svm_pred <- predict(mariners_svm_model, test_mariners_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_mariners_svm[1], mariners_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_mariners_svm=tune(svm, Attendance.G ~., data=train_mariners_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_mariners_svm)

#finding the bestmodel
bstmodel_mariners_svm <- OptModel_mariners_svm$best.model

#Predict outcome using best model
best_mariners_svm_pred <- predict(bstmodel_mariners_svm, train_mariners_svm)
final_mariners_svm_pred <- mean(best_mariners_svm_pred)
#Find value of W
W = t(mariners_svm_model$coefs) %*% mariners_svm_model$SV
W
#Find value of b
b = mariners_svm_model$rho
b
mariners_rmse <- (rmse(final_mariners_svm_pred,test_mariners_svm$Attendance.G))/test_mariners_svm$Attendance.G
mariners_mad <- (mae(final_mariners_svm_pred, test_mariners_svm$Attendance.G))/test_mariners_svm$Attendance.G
mariners_mape <- mape(final_mariners_svm_pred, test_mariners_svm$Attendance.G)
Team = "Seattle Mariners"
mariners_svm_evaluation <- cbind(Team, mariners_rmse, mariners_mad, mariners_mape)
colnames(mariners_svm_evaluation)[2] <- "RMSE"
colnames(mariners_svm_evaluation)[3] <- "MAD"
colnames(mariners_svm_evaluation)[4] <- "MAPE"
mariners_svm_evaluation
mariners_svm_output <- cbind(Team,test_mariners_svm$Attendance.G,final_mariners_svm_pred)
colnames(mariners_svm_output)[2] <- "Actual"
colnames(mariners_svm_output)[3] <- "Predicted"
mariners_svm_output
___________________________________________________________________________________________________________

cardinals_svm <- read.csv("H:/RIC/St. Louis Cardinals/Final.csv", header = TRUE)

# Create training and test set
train_cardinals_svm = cardinals_svm[ 2:130, 5:38 ]
test_cardinals_svm = cardinals_svm[ 1, 5:38 ]

#Scatter Plot
daz <- subset(cardinals_svm, select = c(36:38))
plot(daz)

#Regression with SVM
cardinals_svm_model <- svm(Attendance.G ~., data = train_cardinals_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(cardinals_svm_model, train_cardinals_svm)

#Predict using SVM regression
cardinals_svm_pred <- predict(cardinals_svm_model, test_cardinals_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_cardinals_svm[1], cardinals_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_cardinals_svm=tune(svm, Attendance.G ~., data=train_cardinals_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_cardinals_svm)

#finding the bestmodel
bstmodel_cardinals_svm <- OptModel_cardinals_svm$best.model

#Predict outcome using best model
best_cardinals_svm_pred <- predict(bstmodel_cardinals_svm, train_cardinals_svm)
final_cardinals_svm_pred <- mean(best_cardinals_svm_pred)
#Find value of W
W = t(cardinals_svm_model$coefs) %*% cardinals_svm_model$SV
W
#Find value of b
b = cardinals_svm_model$rho
b
cardinals_rmse <- (rmse(final_cardinals_svm_pred,test_cardinals_svm$Attendance.G))/test_cardinals_svm$Attendance.G
cardinals_mad <- (mae(final_cardinals_svm_pred, test_cardinals_svm$Attendance.G))/test_cardinals_svm$Attendance.G
cardinals_mape <- mape(final_cardinals_svm_pred, test_cardinals_svm$Attendance.G)
Team = "St. Louis Cardinals"
cardinals_svm_evaluation <- cbind(Team, cardinals_rmse, cardinals_mad, cardinals_mape)
colnames(cardinals_svm_evaluation)[2] <- "RMSE"
colnames(cardinals_svm_evaluation)[3] <- "MAD"
colnames(cardinals_svm_evaluation)[4] <- "MAPE"
cardinals_svm_evaluation
cardinals_svm_output <- cbind(Team,test_cardinals_svm$Attendance.G,final_cardinals_svm_pred)
colnames(cardinals_svm_output)[2] <- "Actual"
colnames(cardinals_svm_output)[3] <- "Predicted"
cardinals_svm_output
___________________________________________________________________________________________________________
rays_svm <- read.csv("H:/RIC/Tampa Bay Rays/Final.csv", header = TRUE)

# Create training and test set
train_rays_svm = rays_svm[ 2:21, 5:38 ]
test_rays_svm = rays_svm[ 1, 5:38 ]

#Scatter Plot
daz <- subset(rays_svm, select = c(36:38))
plot(daz)

#Regression with SVM
rays_svm_model <- svm(Attendance.G ~., data = train_rays_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(rays_svm_model, train_rays_svm)

#Predict using SVM regression
rays_svm_pred <- predict(rays_svm_model, test_rays_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_rays_svm[1], rays_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_rays_svm=tune(svm, Attendance.G ~., data=train_rays_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_rays_svm)

#finding the bestmodel
bstmodel_rays_svm <- OptModel_rays_svm$best.model

#Predict outcome using best model
best_rays_svm_pred <- predict(bstmodel_rays_svm, train_rays_svm)
final_rays_svm_pred <- mean(best_rays_svm_pred)
#Find value of W
W = t(rays_svm_model$coefs) %*% rays_svm_model$SV
W
#Find value of b
b = rays_svm_model$rho
b
rays_rmse <- (rmse(final_rays_svm_pred,test_rays_svm$Attendance.G))/test_rays_svm$Attendance.G
rays_mad <- (mae(final_rays_svm_pred, test_rays_svm$Attendance.G))/test_rays_svm$Attendance.G
rays_mape <- mape(final_rays_svm_pred, test_rays_svm$Attendance.G)
Team = "Tampa Bay Rays"
rays_svm_evaluation <- cbind(Team, rays_rmse, rays_mad, rays_mape)
colnames(rays_svm_evaluation)[2] <- "RMSE"
colnames(rays_svm_evaluation)[3] <- "MAD"
colnames(rays_svm_evaluation)[4] <- "MAPE"
rays_svm_evaluation
rays_svm_output <- cbind(Team,test_rays_svm$Attendance.G,final_rays_svm_pred)
colnames(rays_svm_output)[2] <- "Actual"
colnames(rays_svm_output)[3] <- "Predicted"
rays_svm_output
___________________________________________________________________________________________________________

rangers_svm <- read.csv("H:/RIC/Texas Rangers/Final.csv", header = TRUE)

# Create training and test set
train_rangers_svm = rangers_svm[ 2:59, 5:38 ]
test_rangers_svm = rangers_svm[ 1, 5:38 ]

#Scatter Plot
daz <- subset(rangers_svm, select = c(36:38))
plot(daz)

#Regression with SVM
rangers_svm_model <- svm(Attendance.G ~., data = train_rangers_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(rangers_svm_model, train_rangers_svm)

#Predict using SVM regression
rangers_svm_pred <- predict(rangers_svm_model, test_rangers_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_rangers_svm[1], rangers_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_rangers_svm=tune(svm, Attendance.G ~., data=train_rangers_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_rangers_svm)

#finding the bestmodel
bstmodel_rangers_svm <- OptModel_rangers_svm$best.model

#Predict outcome using best model
best_rangers_svm_pred <- predict(bstmodel_rangers_svm, train_rangers_svm)
final_rangers_svm_pred <- mean(best_rangers_svm_pred)
#Find value of W
W = t(rangers_svm_model$coefs) %*% rangers_svm_model$SV
W
#Find value of b
b = rangers_svm_model$rho
b
rangers_rmse <- (rmse(final_rangers_svm_pred,test_rangers_svm$Attendance.G))/test_rangers_svm$Attendance.G
rangers_mad <- (mae(final_rangers_svm_pred, test_rangers_svm$Attendance.G))/test_rangers_svm$Attendance.G
rangers_mape <- mape(final_rangers_svm_pred, test_rangers_svm$Attendance.G)
Team = "Texas Rangers"
rangers_svm_evaluation <- cbind(Team, rangers_rmse, rangers_mad, rangers_mape)
colnames(rangers_svm_evaluation)[2] <- "RMSE"
colnames(rangers_svm_evaluation)[3] <- "MAD"
colnames(rangers_svm_evaluation)[4] <- "MAPE"
rangers_svm_evaluation
rangers_svm_output <- cbind(Team,test_rangers_svm$Attendance.G,final_rangers_svm_pred)
colnames(rangers_svm_output)[2] <- "Actual"
colnames(rangers_svm_output)[3] <- "Predicted"
rangers_svm_output
___________________________________________________________________________________________________________

bluejays_svm <- read.csv("H:/RIC/Toronto Blue Jays/Final.csv", header = TRUE)

# Create training and test set
train_bluejays_svm = bluejays_svm[ 2:43, 5:38 ]
test_bluejays_svm = bluejays_svm[ 1, 5:38 ]

#Scatter Plot
daz <- subset(bluejays_svm, select = c(36:38))
plot(daz)

#Regression with SVM
bluejays_svm_model <- svm(Attendance.G ~., data = train_bluejays_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(bluejays_svm_model, train_bluejays_svm)

#Predict using SVM regression
bluejays_svm_pred <- predict(bluejays_svm_model, test_bluejays_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_bluejays_svm[1], bluejays_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_bluejays_svm=tune(svm, Attendance.G ~., data=train_bluejays_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_bluejays_svm)

#finding the bestmodel
bstmodel_bluejays_svm <- OptModel_bluejays_svm$best.model

#Predict outcome using best model
best_bluejays_svm_pred <- predict(bstmodel_bluejays_svm, train_bluejays_svm)
final_bluejays_svm_pred <- mean(best_bluejays_svm_pred)
#Find value of W
W = t(bluejays_svm_model$coefs) %*% bluejays_svm_model$SV
W
#Find value of b
b = bluejays_svm_model$rho
b
bluejays_rmse <- (rmse(final_bluejays_svm_pred,test_bluejays_svm$Attendance.G))/test_bluejays_svm$Attendance.G
bluejays_mad <- (mae(final_bluejays_svm_pred, test_bluejays_svm$Attendance.G))/test_bluejays_svm$Attendance.G
bluejays_mape <- mape(final_bluejays_svm_pred, test_bluejays_svm$Attendance.G)
Team = "Toronto Blue Jays"
bluejays_svm_evaluation <- cbind(Team, bluejays_rmse, bluejays_mad, bluejays_mape)
colnames(bluejays_svm_evaluation)[2] <- "RMSE"
colnames(bluejays_svm_evaluation)[3] <- "MAD"
colnames(bluejays_svm_evaluation)[4] <- "MAPE"
bluejays_svm_evaluation
bluejays_svm_output <- cbind(Team,test_bluejays_svm$Attendance.G,final_bluejays_svm_pred)
colnames(bluejays_svm_output)[2] <- "Actual"
colnames(bluejays_svm_output)[3] <- "Predicted"
bluejays_svm_output
____________________________________________________________________________________________________________

nationals_svm <- read.csv("H:/RIC/Washington Nationals/Final.csv", header = TRUE)

# Create training and test set
train_nationals_svm = nationals_svm[ 2:51, 5:38 ]
test_nationals_svm = nationals_svm[ 1, 5:38 ]

#Scatter Plot
daz <- subset(nationals_svm, select = c(36:38))
plot(daz)

#Regression with SVM
nationals_svm_model <- svm(Attendance.G ~., data = train_nationals_svm, kernel = "linear", cost = 10, scale = FALSE)
plot(nationals_svm_model, train_nationals_svm)

#Predict using SVM regression
nationals_svm_pred <- predict(nationals_svm_model, test_nationals_svm)

#Overlay SVM Predictions on Scatter Plot
points(test_nationals_svm[1], nationals_svm_pred,col=c("red","black"),pch=16)

##Calculate parameters of the SVR model
OptModel_nationals_svm=tune(svm, Attendance.G ~., data=train_nationals_svm,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))
plot(OptModel_nationals_svm)

#finding the bestmodel
bstmodel_nationals_svm <- OptModel_nationals_svm$best.model

#Predict outcome using best model
best_nationals_svm_pred <- predict(bstmodel_nationals_svm, train_nationals_svm)
final_nationals_svm_pred <- mean(best_nationals_svm_pred)
#Find value of W
W = t(nationals_svm_model$coefs) %*% nationals_svm_model$SV
W
#Find value of b
b = nationals_svm_model$rho
b
nationals_rmse <- (rmse(final_nationals_svm_pred,test_nationals_svm$Attendance.G))/test_nationals_svm$Attendance.G
nationals_mad <- (mae(final_nationals_svm_pred, test_nationals_svm$Attendance.G))/test_nationals_svm$Attendance.G
nationals_mape <- mape(final_nationals_svm_pred, test_nationals_svm$Attendance.G)
Team = "Washington Nationals"
nationals_svm_evaluation <- cbind(Team, nationals_rmse, nationals_mad, nationals_mape)
colnames(nationals_svm_evaluation)[2] <- "RMSE"
colnames(nationals_svm_evaluation)[3] <- "MAD"
colnames(nationals_svm_evaluation)[4] <- "MAPE"
nationals_svm_evaluation
nationals_svm_output <- cbind(Team,test_nationals_svm$Attendance.G,final_nationals_svm_pred)
colnames(nationals_svm_output)[2] <- "Actual"
colnames(nationals_svm_output)[3] <- "Predicted"
nationals_svm_output
____________________________________________________________________________________________________________

write.csv(model_svm_evaluation <- rbind(dbk_svm_evaluation, brv_svm_evaluation, ori_svm_evaluation, redsox_svm_evaluation, cubs_svm_evaluation,
                                        whitesox_svm_evaluation, reds_svm_evaluation, indians_svm_evaluation, rockies_svm_evaluation, tigers_svm_evaluation, 
                                        astros_svm_evaluation, royals_svm_evaluation, angels_svm_evaluation, dodgers_svm_evaluation, marlins_svm_evaluation,
                                        brewers_svm_evaluation, twins_svm_evaluation, mets_svm_evaluation, yankees_svm_evaluation, athletics_svm_evaluation, 
                                        phillies_svm_evaluation, pirates_svm_evaluation, padres_svm_evaluation, giants_svm_evaluation, mariners_svm_evaluation, 
                                        cardinals_svm_evaluation, rays_svm_evaluation, rangers_svm_evaluation, bluejays_svm_evaluation, nationals_svm_evaluation), 
                                        "H:/RIC/Evaluation/SVM_Evaluation.csv")

write.csv(model_svm_output <- rbind(dbk_svm_output, brv_svm_output, ori_svm_output, redsox_svm_output, cubs_svm_output,
                                    whitesox_svm_output, reds_svm_output, indians_svm_output, rockies_svm_output, tigers_svm_output, 
                                    astros_svm_output, royals_svm_output, angels_svm_output, dodgers_svm_output, marlins_svm_output,
                                    brewers_svm_output, twins_svm_output, mets_svm_output, yankees_svm_output, athletics_svm_output, 
                                    phillies_svm_output, pirates_svm_output, padres_svm_output, giants_svm_output, mariners_svm_output, 
                                    cardinals_svm_output, rays_svm_output, rangers_svm_output, bluejays_svm_output, nationals_svm_output), 
                                    "H:/RIC/Evaluation/SVM_Output.csv")
