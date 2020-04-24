mlr_evaluation <- read.csv("H:/RIC/Evaluation/MLR_Evaluation.csv")
ann_evaluation <- read.csv("H:/RIC/Evaluation/ANN_Evaluation.csv")
rf_evaluation <- read.csv("H:/RIC/Evaluation/RandomForest_Evaluation_1.csv")
svm_evaluation <- read.csv("H:/RIC/Evaluation/SVM_Evaluation.csv")

ann_avg_rmse <- mean(ann_evaluation$RMSE)
ann_avg_mad <- mean(ann_evaluation$MAD)
ann_avg_mape <- mean(ann_evaluation$MAPE)
model="Artificail Neural Network"
avg_ann <- cbind(model,ann_avg_rmse,ann_avg_mad,ann_avg_mape)

rf_avg_rmse <- mean(rf_evaluation$RMSE)
rf_avg_mad <- mean(rf_evaluation$MAD)
rf_avg_mape <- mean(rf_evaluation$MAPE)
model1="Random Forest"
avg_rf <- cbind(model1,rf_avg_rmse,rf_avg_mad,rf_avg_mape)

svm_avg_rmse <- mean(svm_evaluation$RMSE)
svm_avg_mad <- mean(svm_evaluation$MAD)
svm_avg_mape <- mean(svm_evaluation$MAPE)
model2="Support vector Machines"
avg_svm <- cbind(model2,svm_avg_rmse,svm_avg_mad,svm_avg_mape)

avg <- rbind(avg_ann,avg_rf,avg_svm)
colnames(avg)[2] <- "RMSE"
colnames(avg)[3] <- "MAD"
colnames(avg)[4] <- "MAPE"

write.csv(avg,"H:/RIC/Evaluation/Average.csv")

