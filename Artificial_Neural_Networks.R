library(neuralnet)
library(Metrics)
library(FLR)

dbk_ann = read.csv("H:/RIC/Arizona_Diamonbacks/Final.csv", header=T)

#dividing the data into training and testing dataset
train_dbk_ann = dbk_ann[ 2:21, 5:38 ]
test_dbk_ann = dbk_ann[ 1, 5:38 ]

d <- density(train_dbk_ann$Attendance.G)
plot(d, main="Dennsity plot for attendance per Game")
polygon(d, col="blue")

m <- mean(train_dbk_ann$Attendance.G)
std <- sqrt(var(train_dbk_ann$Attendance.G))
hist(train_dbk_ann$Attendance.G, density = 20, breaks=10, prob= TRUE,
     xlab="x-variable", main="Normal Distribution for Average Attendance per Game")
curve(dnorm(x, mean=m, sd=std), 
            col="darkblue", lwd=2, add=TRUE, yaxt="n")

#performing Principal Component analysis
princ_comp_dbk <- prcomp(train_dbk_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_dbk$rotation

#plotting the resultant Principal Components
biplot(princ_comp_dbk, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_dbk$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_dbk_pca <- data.frame(Attendance.G = train_dbk_ann$Attendance.G, princ_comp_dbk$x)

#creating the training data with PCs
train_dbk_pca <- train_dbk_pca[ ,1:20 ]

# fitting the neural network model now
set.seed(122)
dbk_ann_model = neuralnet(Attendance.G ~ ., train_dbk_pca, hidden = 3 , linear.output = T )

# plot neural network
plot(dbk_ann_model)

#transforming initial test data into PCA
test_dbk_pca <- predict(princ_comp_dbk,newdata = test_dbk_ann)
test_dbk_pca <- as.data.frame(test_dbk_pca)

#selecting all the required components
test_dbk_pca <- test_dbk_pca[,1:20]

## Prediction using neural network
dbk_ann_pred <- predict(dbk_ann_model,test_dbk_pca)
dbk_ann_pred <- as.data.frame(dbk_ann_pred)

dbk_rmse <- (rmse(dbk_ann_pred$V1, test_dbk_ann$Attendance.G))/test_dbk_ann$Attendance.G
dbk_mad <- (mae(dbk_ann_pred$V1, test_dbk_ann$Attendance.G))/test_dbk_ann$Attendance.G
dbk_mape <- mape(dbk_ann_pred$V1, test_dbk_ann$Attendance.G)
Team = "Arizona Diamondbacks"
dbk_ann_evaluation <- cbind(Team, dbk_rmse, dbk_mad, dbk_mape)
colnames(dbk_ann_evaluation)[2] <- "RMSE"
colnames(dbk_ann_evaluation)[3] <- "MAD"
colnames(dbk_ann_evaluation)[4] <- "MAPE"
dbk_ann_evaluation
dbk_ann_output <- cbind(Team, test_dbk_ann$Attendance.G,dbk_ann_pred)
colnames(dbk_ann_output)[2] <- "Actual"
colnames(dbk_ann_output)[3] <- "Predicted"
dbk_ann_output

______________________________________________________________________________________________

brv_ann = read.csv("H:/RIC/ATlanta Braves/Final.csv", header=T)

#dividing the data into training and testing dataset
train_brv_ann = brv_ann[ 2:21, 5:38 ]
test_brv_ann = brv_ann[ 1, 5:38 ]

#performing Principal Component analysis
princ_comp_brv <- prcomp(train_brv_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_brv$rotation

#plotting the resultant Principal Components
biplot(princ_comp_brv, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_brv$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_brv_pca <- data.frame(Attendance.G = train_brv_ann$Attendance.G, princ_comp_brv$x)

#creating the training data with PCs
train_brv_pca <- train_brv_pca[ ,1:20 ]

# fitting the neural network model now
set.seed(999)
brv_ann_model = neuralnet(Attendance.G ~ ., train_brv_pca, hidden = 4 , linear.output = T )

# plot neural network
plot(brv_ann_model)

#transforming initial test data into PCA
test_brv_pca <- predict(princ_comp_brv,newdata = test_brv_ann)
test_brv_pca <- as.data.frame(test_brv_pca)

#selecting all the required components
test_brv_pca <- test_brv_pca[,1:20]

## Prediction using neural network
brv_ann_pred <- predict(brv_ann_model,test_brv_pca)
brv_ann_pred <- as.data.frame(brv_ann_pred)

brv_rmse <- (rmse(brv_ann_pred$V1, test_brv_ann$Attendance.G))/test_brv_ann$Attendance.G
brv_mad <- (mae(brv_ann_pred$V1, test_brv_ann$Attendance.G))/test_brv_ann$Attendance.G
brv_mape <- mape(brv_ann_pred$V1, test_brv_ann$Attendance.G)
Team = "Atlanta Braves"
brv_ann_evaluation <- cbind(Team, brv_rmse, brv_mad, brv_mape)
colnames(brv_ann_evaluation)[2] <- "RMSE"
colnames(brv_ann_evaluation)[3] <- "MAD"
colnames(brv_ann_evaluation)[4] <- "MAPE"
brv_ann_evaluation
brv_ann_output <- cbind(Team, test_brv_ann$Attendance.G,brv_ann_pred)
colnames(brv_ann_output)[2] <- "Actual"
colnames(brv_ann_output)[3] <- "Predicted"
brv_ann_output

_____________________________________________________________________________________________

ori_ann = read.csv("H:/RIC/Baltimore Orioles/Final.csv", header=T)

#dividing the data into training and testing dataset
train_ori_ann = ori_ann[ 2:119, 5:37 ]
test_ori_ann = ori_ann[ 1, 5:37 ]

#performing Principal Component analysis
princ_comp_ori <- prcomp(train_ori_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_ori$rotation

#plotting the resultant Principal Components
biplot(princ_comp_ori, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_ori$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_ori_pca <- data.frame(Attendance.G = train_ori_ann$Attendance.G, princ_comp_ori$x)

#creating the training data with PCs
train_ori_pca <- train_ori_pca[ ,1:33 ]

# fitting the neural network model now
set.seed(34)
ori_ann_model = neuralnet(Attendance.G ~ ., train_ori_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(ori_ann_model)

#transforming initial test data into PCA
test_ori_pca <- predict(princ_comp_ori,newdata = test_ori_ann)
test_ori_pca <- as.data.frame(test_ori_pca)

#selecting all the required components
test_ori_pca <- test_ori_pca[,1:33]

## Prediction using neural network
ori_ann_pred <- predict(ori_ann_model,test_ori_pca)
ori_ann_pred <- as.data.frame(ori_ann_pred)

ori_rmse <- (rmse(ori_ann_pred$V1, test_ori_ann$Attendance.G))/test_ori_ann$Attendance.G
ori_mad <- (mae(ori_ann_pred$V1, test_ori_ann$Attendance.G))/test_ori_ann$Attendance.G
ori_mape <- mape(ori_ann_pred$V1, test_ori_ann$Attendance.G)
Team = "Baltimore Orioles"
ori_ann_evaluation <- cbind(Team, ori_rmse, ori_mad, ori_mape)
colnames(ori_ann_evaluation)[2] <- "RMSE"
colnames(ori_ann_evaluation)[3] <- "MAD"
colnames(ori_ann_evaluation)[4] <- "MAPE"
ori_ann_evaluation
ori_ann_output <- cbind(Team, test_ori_ann$Attendance.G,ori_ann_pred)
colnames(ori_ann_output)[2] <- "Actual"
colnames(ori_ann_output)[3] <- "Predicted"
ori_ann_output

_____________________________________________________________________________________________

redsox_ann = read.csv("H:/RIC/Boston Red Sox/Final.csv", header=T)

#dividing the data into training and testing dataset
train_redsox_ann = redsox_ann[ 2:119, 5:37 ]
test_redsox_ann = redsox_ann[ 1, 5:37 ]

# fitting the neural network model now
set.seed(705)
redsox_ann_model = neuralnet(Attendance.G ~ ., train_redsox_ann, hidden = 2 , linear.output = T )

# plot neural network
plot(redsox_ann_model)

## Prediction using neural network
redsox_ann_pred <- predict(redsox_ann_model,test_redsox_ann)
redsox_ann_pred <- as.data.frame(redsox_ann_pred)

redsox_rmse <- (rmse(redsox_ann_pred$V1, test_redsox_ann$Attendance.G))/test_redsox_ann$Attendance.G
redsox_mad <- (mae(redsox_ann_pred$V1, test_redsox_ann$Attendance.G))/test_redsox_ann$Attendance.G
redsox_mape <- mape(redsox_ann_pred$V1, test_redsox_ann$Attendance.G)
Team = "Boston Red Sox"
redsox_ann_evaluation <- cbind(Team, redsox_rmse, redsox_mad, redsox_mape)
colnames(redsox_ann_evaluation)[2] <- "RMSE"
colnames(redsox_ann_evaluation)[3] <- "MAD"
colnames(redsox_ann_evaluation)[4] <- "MAPE"
redsox_ann_evaluation
redsox_ann_output <- cbind(Team, test_redsox_ann$Attendance.G,redsox_ann_pred)
colnames(redsox_ann_output)[2] <- "Actual"
colnames(redsox_ann_output)[3] <- "Predicted"
redsox_ann_output

_____________________________________________________________________________________________________________

cubs_ann = read.csv("H:/RIC/Chicago Cubs/Final.csv", header=T)

#dividing the data into training and testing dataset
train_cubs_ann = cubs_ann[ 2:119, 5:37 ]
test_cubs_ann = cubs_ann[ 1, 5:37 ]

#performing Principal Component analysis
princ_comp_cubs <- prcomp(train_cubs_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_cubs$rotation

#plotting the resultant Principal Components
biplot(princ_comp_cubs, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_cubs$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_cubs_pca <- data.frame(Attendance.G = train_cubs_ann$Attendance.G, princ_comp_cubs$x)

#creating the training data with PCs
train_cubs_pca <- train_cubs_pca[ ,1:34 ]

# fitting the neural network model now
set.seed(853)
cubs_ann_model = neuralnet(Attendance.G ~ ., train_cubs_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(cubs_ann_model)

#transforming initial test data into PCA
test_cubs_pca <- predict(princ_comp_cubs,newdata = test_cubs_ann)
test_cubs_pca <- as.data.frame(test_cubs_pca)

#selecting all the required components
test_cubs_pca <- test_cubs_pca[,1:33]

## Prediction using neural network
cubs_ann_pred <- predict(cubs_ann_model,test_cubs_pca)
cubs_ann_pred <- as.data.frame(cubs_ann_pred)

cubs_rmse <- (rmse(cubs_ann_pred$V1, test_cubs_ann$Attendance.G))/test_cubs_ann$Attendance.G
cubs_mad <- (mae(cubs_ann_pred$V1, test_cubs_ann$Attendance.G))/test_cubs_ann$Attendance.G
cubs_mape <- mape(cubs_ann_pred$V1, test_cubs_ann$Attendance.G)
Team = "Chicago Cubs"
cubs_ann_evaluation <- cbind(Team, cubs_rmse, cubs_mad, cubs_mape)
colnames(cubs_ann_evaluation)[2] <- "RMSE"
colnames(cubs_ann_evaluation)[3] <- "MAD"
colnames(cubs_ann_evaluation)[4] <- "MAPE"
cubs_ann_evaluation
cubs_ann_output <- cbind(Team, test_cubs_ann$Attendance.G,cubs_ann_pred)
colnames(cubs_ann_output)[2] <- "Actual"
colnames(cubs_ann_output)[3] <- "Predicted"
cubs_ann_output

____________________________________________________________________________________________________________

whitesox_ann = read.csv("H:/RIC/Chicago White Sox/Final.csv", header=T)

#dividing the data into training and testing dataset
train_whitesox_ann = whitesox_ann[ 2:119, 5:37 ]
test_whitesox_ann = whitesox_ann[ 1, 5:37 ]

#performing Principal Component analysis
princ_comp_whitesox <- prcomp(train_whitesox_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_whitesox$rotation

#plotting the resultant Principal Components
biplot(princ_comp_whitesox, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_whitesox$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_whitesox_pca <- data.frame(Attendance.G = train_whitesox_ann$Attendance.G, princ_comp_whitesox$x)

#creating the training data with PCs
train_whitesox_pca <- train_whitesox_pca[ ,1:20 ]

# fitting the neural network model now
set.seed(150)
whitesox_ann_model = neuralnet(Attendance.G ~ ., train_whitesox_pca, hidden = 3 , linear.output = T )

# plot neural network
plot(whitesox_ann_model)

#transforming initial test data into PCA
test_whitesox_pca <- predict(princ_comp_whitesox,newdata = test_whitesox_ann)
test_whitesox_pca <- as.data.frame(test_whitesox_pca)

#selecting all the required components
test_whitesox_pca <- test_whitesox_pca[,1:20]

## Prediction using neural network
whitesox_ann_pred <- predict(whitesox_ann_model,test_whitesox_pca)
whitesox_ann_pred <- as.data.frame(whitesox_ann_pred)

whitesox_rmse <- (rmse(whitesox_ann_pred$V1, test_whitesox_ann$Attendance.G))/test_whitesox_ann$Attendance.G
whitesox_mad <- (mae(whitesox_ann_pred$V1, test_whitesox_ann$Attendance.G))/test_whitesox_ann$Attendance.G
whitesox_mape <- mape(whitesox_ann_pred$V1, test_whitesox_ann$Attendance.G)
Team = "Chicago White Sox"
whitesox_ann_evaluation <- cbind(Team, whitesox_rmse, whitesox_mad, whitesox_mape)
colnames(whitesox_ann_evaluation)[2] <- "RMSE"
colnames(whitesox_ann_evaluation)[3] <- "MAD"
colnames(whitesox_ann_evaluation)[4] <- "MAPE"
whitesox_ann_evaluation
whitesox_ann_output <- cbind(Team, test_whitesox_ann$Attendance.G,whitesox_ann_pred)
colnames(whitesox_ann_output)[2] <- "Actual"
colnames(whitesox_ann_output)[3] <- "Predicted"
whitesox_ann_output

_____________________________________________________________________________________________________________

reds_ann = read.csv("H:/RIC/Cincinnati Reds/Final.csv", header=T)

#dividing the data into training and testing dataset
train_reds_ann = reds_ann[ 2:130, 5:37 ]
test_reds_ann = reds_ann[ 1, 5:37 ]

#performing Principal Component analysis
princ_comp_reds <- prcomp(train_reds_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_reds$rotation

#plotting the resultant Principal Components
biplot(princ_comp_reds, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_reds$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_reds_pca <- data.frame(Attendance.G = train_reds_ann$Attendance.G, princ_comp_reds$x)

#creating the training data with PCs
train_reds_pca <- train_reds_pca[ ,1:34 ]

# fitting the neural network model now
set.seed(882)
reds_ann_model = neuralnet(Attendance.G ~ ., train_reds_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(reds_ann_model)

#transforming initial test data into PCA
test_reds_pca <- predict(princ_comp_reds,newdata = test_reds_ann)
test_reds_pca <- as.data.frame(test_reds_pca)

#selecting all the required components
test_reds_pca <- test_reds_pca[,1:33]

## Prediction using neural network
reds_ann_pred <- predict(reds_ann_model,test_reds_pca)
reds_ann_pred <- as.data.frame(reds_ann_pred)

reds_rmse <- (rmse(reds_ann_pred$V1, test_reds_ann$Attendance.G))/test_reds_ann$Attendance.G
reds_mad <- (mae(reds_ann_pred$V1, test_reds_ann$Attendance.G))/test_reds_ann$Attendance.G
reds_mape <- mape(reds_ann_pred$V1, test_reds_ann$Attendance.G)
Team = "Cincinnati Reds"
reds_ann_evaluation <- cbind(Team, reds_rmse, reds_mad, reds_mape)
colnames(reds_ann_evaluation)[2] <- "RMSE"
colnames(reds_ann_evaluation)[3] <- "MAD"
colnames(reds_ann_evaluation)[4] <- "MAPE"
reds_ann_evaluation
reds_ann_output <- cbind(Team, test_reds_ann$Attendance.G,reds_ann_pred)
colnames(reds_ann_output)[2] <- "Actual"
colnames(reds_ann_output)[3] <- "Predicted"
reds_ann_output

_____________________________________________________________________________________________________________

indians_ann = read.csv("H:/RIC/Cleveland Indians/Final.csv", header=T)

#dividing the data into training and testing dataset
train_indians_ann = indians_ann[ 2:119, 5:37 ]
test_indians_ann = indians_ann[ 1, 5:37 ]

#performing Principal Component analysis
princ_comp_indians <- prcomp(train_indians_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_indians$rotation

#plotting the resultant Principal Components
biplot(princ_comp_indians, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_indians$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_indians_pca <- data.frame(Attendance.G = train_indians_ann$Attendance.G, princ_comp_indians$x)

#creating the training data with PCs
train_indians_pca <- train_indians_pca[ ,1:34 ]

# fitting the neural network model now
set.seed(678)
indians_ann_model = neuralnet(Attendance.G ~ ., train_indians_pca, hidden = 4 , linear.output = T )

# plot neural network
plot(indians_ann_model)

#transforming initial test data into PCA
test_indians_pca <- predict(princ_comp_indians,newdata = test_indians_ann)
test_indians_pca <- as.data.frame(test_indians_pca)

#selecting all the required components
test_indians_pca <- test_indians_pca[,1:33]

## Prediction using neural network
indians_ann_pred <- predict(indians_ann_model,test_indians_pca)
indians_ann_pred <- as.data.frame(indians_ann_pred)

indians_rmse <- (rmse(indians_ann_pred$V1, test_indians_ann$Attendance.G))/test_indians_ann$Attendance.G
indians_mad <- (mae(indians_ann_pred$V1, test_indians_ann$Attendance.G))/test_indians_ann$Attendance.G
indians_mape <- mape(indians_ann_pred$V1, test_indians_ann$Attendance.G)
Team = "Cleveland Indians"
indians_ann_evaluation <- cbind(Team, indians_rmse, indians_mad, indians_mape)
colnames(indians_ann_evaluation)[2] <- "RMSE"
colnames(indians_ann_evaluation)[3] <- "MAD"
colnames(indians_ann_evaluation)[4] <- "MAPE"
indians_ann_evaluation
indians_ann_output <- cbind(Team, test_indians_ann$Attendance.G,indians_ann_pred)
colnames(indians_ann_output)[2] <- "Actual"
colnames(indians_ann_output)[3] <- "Predicted"
indians_ann_output
____________________________________________________________________________________________________________

rockies_ann = read.csv("H:/RIC/Colorado Rockies/Final.csv", header=T)

#dividing the data into training and testing dataset
train_rockies_ann = rockies_ann[ 2:27, 5:37 ]
test_rockies_ann = rockies_ann[ 1, 5:37 ]

#performing Principal Component analysis
princ_comp_rockies <- prcomp(train_rockies_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_rockies$rotation

#plotting the resultant Principal Components
biplot(princ_comp_rockies, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_rockies$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_rockies_pca <- data.frame(Attendance.G = train_rockies_ann$Attendance.G, princ_comp_rockies$x)

#creating the training data with PCs
train_rockies_pca <- train_rockies_pca[ ,1:27 ]

# fitting the neural network model now
set.seed(786)
rockies_ann_model = neuralnet(Attendance.G ~ ., train_rockies_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(rockies_ann_model)

#transforming initial test data into PCA
test_rockies_pca <- predict(princ_comp_rockies,newdata = test_rockies_ann)
test_rockies_pca <- as.data.frame(test_rockies_pca)

#selecting all the required components
test_rockies_pca <- test_rockies_pca[,1:26]

## Prediction using neural network
rockies_ann_pred <- predict(rockies_ann_model,test_rockies_pca)
rockies_ann_pred <- as.data.frame(rockies_ann_pred)

rockies_rmse <- (rmse(rockies_ann_pred$V1, test_rockies_ann$Attendance.G))/test_rockies_ann$Attendance.G
rockies_mad <- (mae(rockies_ann_pred$V1, test_rockies_ann$Attendance.G))/test_rockies_ann$Attendance.G
rockies_mape <- mape(rockies_ann_pred$V1, test_rockies_ann$Attendance.G)
Team = "Colorado Rockies"
rockies_ann_evaluation <- cbind(Team, rockies_rmse, rockies_mad, rockies_mape)
colnames(rockies_ann_evaluation)[2] <- "RMSE"
colnames(rockies_ann_evaluation)[3] <- "MAD"
colnames(rockies_ann_evaluation)[4] <- "MAPE"
rockies_ann_evaluation
rockies_ann_output <- cbind(Team, test_rockies_ann$Attendance.G,rockies_ann_pred)
colnames(rockies_ann_output)[2] <- "Actual"
colnames(rockies_ann_output)[3] <- "Predicted"
rockies_ann_output
_______________________________________________________________________________________________________________

tigers_ann = read.csv("H:/RIC/Detroit Tigers/Final.csv", header=T)

#dividing the data into training and testing dataset
train_tigers_ann = tigers_ann[ 2:119, 5:37 ]
test_tigers_ann = tigers_ann[ 1, 5:37 ]

#performing Principal Component analysis
princ_comp_tigers <- prcomp(train_tigers_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_tigers$rotation

#plotting the resultant Principal Components
biplot(princ_comp_tigers, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_tigers$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_tigers_pca <- data.frame(Attendance.G = train_tigers_ann$Attendance.G, princ_comp_tigers$x)

#creating the training data with PCs
train_tigers_pca <- train_tigers_pca[ ,1:20 ]

# fitting the neural network model now
set.seed(775)
tigers_ann_model = neuralnet(Attendance.G ~ ., train_tigers_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(tigers_ann_model)

#transforming initial test data into PCA
test_tigers_pca <- predict(princ_comp_tigers,newdata = test_tigers_ann)
test_tigers_pca <- as.data.frame(test_tigers_pca)

#selecting all the required components
test_tigers_pca <- test_tigers_pca[,1:20]

## Prediction using neural network
tigers_ann_pred <- predict(tigers_ann_model,test_tigers_pca)
tigers_ann_pred <- as.data.frame(tigers_ann_pred)

tigers_rmse <- (rmse(tigers_ann_pred$V1, test_tigers_ann$Attendance.G))/test_tigers_ann$Attendance.G
tigers_mad <- (mae(tigers_ann_pred$V1, test_tigers_ann$Attendance.G))/test_tigers_ann$Attendance.G
tigers_mape <- mape(tigers_ann_pred$V1, test_tigers_ann$Attendance.G)
Team = "Detroit Tigers"
tigers_ann_evaluation <- cbind(Team, tigers_rmse, tigers_mad, tigers_mape)
colnames(tigers_ann_evaluation)[2] <- "RMSE"
colnames(tigers_ann_evaluation)[3] <- "MAD"
colnames(tigers_ann_evaluation)[4] <- "MAPE"
tigers_ann_evaluation
tigers_ann_output <- cbind(Team, test_tigers_ann$Attendance.G,tigers_ann_pred)
colnames(tigers_ann_output)[2] <- "Actual"
colnames(tigers_ann_output)[3] <- "Predicted"
tigers_ann_output
_____________________________________________________________________________________________________________

astros_ann = read.csv("H:/RIC/Houston Astros/Final.csv", header=T)

#dividing the data into training and testing dataset
train_astros_ann = astros_ann[ 2:58, 5:37 ]
test_astros_ann = astros_ann[ 1, 5:37 ]

#performing Principal Component analysis
princ_comp_astros <- prcomp(train_astros_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_astros$rotation

#plotting the resultant Principal Components
biplot(princ_comp_astros, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_astros$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_astros_pca <- data.frame(Attendance.G = train_astros_ann$Attendance.G, princ_comp_astros$x)

#creating the training data with PCs
train_astros_pca <- train_astros_pca[ ,1:34 ]

# fitting the neural network model now
set.seed(963)
astros_ann_model = neuralnet(Attendance.G ~ ., train_astros_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(astros_ann_model)

#transforming initial test data into PCA
test_astros_pca <- predict(princ_comp_astros,newdata = test_astros_ann)
test_astros_pca <- as.data.frame(test_astros_pca)

#selecting all the required components
test_astros_pca <- test_astros_pca[,1:33]

## Prediction using neural network
astros_ann_pred <- predict(astros_ann_model,test_astros_pca)
astros_ann_pred <- as.data.frame(astros_ann_pred)

astros_rmse <- (rmse(astros_ann_pred$V1, test_astros_ann$Attendance.G))/test_astros_ann$Attendance.G
astros_mad <- (mae(astros_ann_pred$V1, test_astros_ann$Attendance.G))/test_astros_ann$Attendance.G
astros_mape <- mape(astros_ann_pred$V1, test_astros_ann$Attendance.G)
Team = "Houston Astros"
astros_ann_evaluation <- cbind(Team, astros_rmse, astros_mad, astros_mape)
colnames(astros_ann_evaluation)[2] <- "RMSE"
colnames(astros_ann_evaluation)[3] <- "MAD"
colnames(astros_ann_evaluation)[4] <- "MAPE"
astros_ann_evaluation
astros_ann_output <- cbind(Team, test_astros_ann$Attendance.G,astros_ann_pred)
colnames(astros_ann_output)[2] <- "Actual"
colnames(astros_ann_output)[3] <- "Predicted"
astros_ann_output
____________________________________________________________________________________________________________

royals_ann = read.csv("H:/RIC/Kansas City Royals/Final.csv", header=T)

#dividing the data into training and testing dataset
train_royals_ann = royals_ann[ 2:51, 5:37 ]
test_royals_ann = royals_ann[ 1, 5:37 ]

#performing Principal Component analysis
princ_comp_royals <- prcomp(train_royals_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_royals$rotation

#plotting the resultant Principal Components
biplot(princ_comp_royals, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_royals$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_royals_pca <- data.frame(Attendance.G = train_royals_ann$Attendance.G, princ_comp_royals$x)

#creating the training data with PCs
train_royals_pca <- train_royals_pca[ ,1:34 ]

# fitting the neural network model now
set.seed(699)
royals_ann_model = neuralnet(Attendance.G ~ ., train_royals_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(royals_ann_model)

#transforming initial test data into PCA
test_royals_pca <- predict(princ_comp_royals,newdata = test_royals_ann)
test_royals_pca <- as.data.frame(test_royals_pca)

#selecting all the required components
test_royals_pca <- test_royals_pca[,1:33]

## Prediction using neural network
royals_ann_pred <- predict(royals_ann_model,test_royals_pca)
royals_ann_pred <- as.data.frame(royals_ann_pred)

royals_rmse <- (rmse(royals_ann_pred$V1, test_royals_ann$Attendance.G))/test_royals_ann$Attendance.G
royals_mad <- (mae(royals_ann_pred$V1, test_royals_ann$Attendance.G))/test_royals_ann$Attendance.G
royals_mape <- mape(royals_ann_pred$V1, test_royals_ann$Attendance.G)
Team = "Kansas City Royals"
royals_ann_evaluation <- cbind(Team, royals_rmse, royals_mad, royals_mape)
colnames(royals_ann_evaluation)[2] <- "RMSE"
colnames(royals_ann_evaluation)[3] <- "MAD"
colnames(royals_ann_evaluation)[4] <- "MAPE"
royals_ann_evaluation
royals_ann_output <- cbind(Team, test_royals_ann$Attendance.G,royals_ann_pred)
colnames(royals_ann_output)[2] <- "Actual"
colnames(royals_ann_output)[3] <- "Predicted"
royals_ann_output
____________________________________________________________________________________________________________

angels_ann = read.csv("H:/RIC/Los Angeles Angels/Final.csv", header=T)

#dividing the data into training and testing dataset
train_angels_ann = angels_ann[ 2:51, 5:37 ]
test_angels_ann = angels_ann[ 1, 5:37 ]

#performing Principal Component analysis
princ_comp_angels <- prcomp(train_angels_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_angels$rotation

#plotting the resultant Principal Components
biplot(princ_comp_angels, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_angels$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_angels_pca <- data.frame(Attendance.G = train_angels_ann$Attendance.G, princ_comp_angels$x)

#creating the training data with PCs
train_angels_pca <- train_angels_pca[ ,1:34 ]

# fitting the neural network model now
set.seed(606)
angels_ann_model = neuralnet(Attendance.G ~ ., train_angels_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(angels_ann_model)

#transforming initial test data into PCA
test_angels_pca <- predict(princ_comp_angels,newdata = test_angels_ann)
test_angels_pca <- as.data.frame(test_angels_pca)

#selecting all the required components
test_angels_pca <- test_angels_pca[,1:33]

## Prediction using neural network
angels_ann_pred <- predict(angels_ann_model,test_angels_pca)
angels_ann_pred <- as.data.frame(angels_ann_pred)

angels_rmse <- (rmse(angels_ann_pred$V1, test_angels_ann$Attendance.G))/test_angels_ann$Attendance.G
angels_mad <- (mae(angels_ann_pred$V1, test_angels_ann$Attendance.G))/test_angels_ann$Attendance.G
angels_mape <- mape(angels_ann_pred$V1, test_angels_ann$Attendance.G)
Team = "Los Angeles Angels"
angels_ann_evaluation <- cbind(Team, angels_rmse, angels_mad, angels_mape)
colnames(angels_ann_evaluation)[2] <- "RMSE"
colnames(angels_ann_evaluation)[3] <- "MAD"
colnames(angels_ann_evaluation)[4] <- "MAPE"
angels_ann_evaluation
angels_ann_output <- cbind(Team, test_angels_ann$Attendance.G,angels_ann_pred)
colnames(angels_ann_output)[2] <- "Actual"
colnames(angels_ann_output)[3] <- "Predicted"
angels_ann_output
____________________________________________________________________________________________________________

dodgers_ann = read.csv("H:/RIC/Los Angeles Dodgers/Final.csv", header=T)

#dividing the data into training and testing dataset
train_dodgers_ann = dodgers_ann[ 2:130, 5:38 ]
test_dodgers_ann = dodgers_ann[ 1, 5:38 ]

#performing Principal Component analysis
princ_comp_dodgers <- prcomp(train_dodgers_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_dodgers$rotation

#plotting the resultant Principal Components
biplot(princ_comp_dodgers, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_dodgers$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_dodgers_pca <- data.frame(Attendance.G = train_dodgers_ann$Attendance.G, princ_comp_dodgers$x)

#creating the training data with PCs
train_dodgers_pca <- train_dodgers_pca[ ,1:35 ]

# fitting the neural network model now
set.seed(453)
dodgers_ann_model = neuralnet(Attendance.G ~ ., train_dodgers_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(dodgers_ann_model)

#transforming initial test data into PCA
test_dodgers_pca <- predict(princ_comp_dodgers,newdata = test_dodgers_ann)
test_dodgers_pca <- as.data.frame(test_dodgers_pca)

#selecting all the required components
test_dodgers_pca <- test_dodgers_pca[,1:34]

## Prediction using neural network
dodgers_ann_pred <- predict(dodgers_ann_model,test_dodgers_pca)
dodgers_ann_pred <- as.data.frame(dodgers_ann_pred)

dodgers_rmse <- (rmse(dodgers_ann_pred$V1, test_dodgers_ann$Attendance.G))/test_dodgers_ann$Attendance.G
dodgers_mad <- (mae(dodgers_ann_pred$V1, test_dodgers_ann$Attendance.G))/test_dodgers_ann$Attendance.G
dodgers_mape <- mape(dodgers_ann_pred$V1, test_dodgers_ann$Attendance.G)
Team = "Los Angeles Dodgers"
dodgers_ann_evaluation <- cbind(Team, dodgers_rmse, dodgers_mad, dodgers_mape)
colnames(dodgers_ann_evaluation)[2] <- "RMSE"
colnames(dodgers_ann_evaluation)[3] <- "MAD"
colnames(dodgers_ann_evaluation)[4] <- "MAPE"
dodgers_ann_evaluation
dodgers_ann_output <- cbind(Team, test_dodgers_ann$Attendance.G,dodgers_ann_pred)
colnames(dodgers_ann_output)[2] <- "Actual"
colnames(dodgers_ann_output)[3] <- "Predicted"
dodgers_ann_output
____________________________________________________________________________________________________________

marlins_ann = read.csv("H:/RIC/Miami Marlins/Final.csv", header=T)

#dividing the data into training and testing dataset
train_marlins_ann = marlins_ann[ 2:27, 5:37 ]
test_marlins_ann = marlins_ann[ 1, 5:37 ]

#performing Principal Component analysis
princ_comp_marlins <- prcomp(train_marlins_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_marlins$rotation

#plotting the resultant Principal Components
biplot(princ_comp_marlins, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_marlins$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_marlins_pca <- data.frame(Attendance.G = train_marlins_ann$Attendance.G, princ_comp_marlins$x)

#creating the training data with PCs
train_marlins_pca <- train_marlins_pca[ ,1:27 ]

# fitting the neural network model now
set.seed(354)
marlins_ann_model = neuralnet(Attendance.G ~ ., train_marlins_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(marlins_ann_model)

#transforming initial test data into PCA
test_marlins_pca <- predict(princ_comp_marlins,newdata = test_marlins_ann)
test_marlins_pca <- as.data.frame(test_marlins_pca)

#selecting all the required components
test_marlins_pca <- test_marlins_pca[,1:26]

## Prediction using neural network
marlins_ann_pred <- predict(marlins_ann_model,test_marlins_pca)
marlins_ann_pred <- as.data.frame(marlins_ann_pred)

marlins_rmse <- (rmse(marlins_ann_pred$V1, test_marlins_ann$Attendance.G))/test_marlins_ann$Attendance.G
marlins_mad <- (mae(marlins_ann_pred$V1, test_marlins_ann$Attendance.G))/test_marlins_ann$Attendance.G
marlins_mape <- mape(marlins_ann_pred$V1, test_marlins_ann$Attendance.G)
Team = "Miami Marlins"
marlins_ann_evaluation <- cbind(Team, marlins_rmse, marlins_mad, marlins_mape)
colnames(marlins_ann_evaluation)[2] <- "RMSE"
colnames(marlins_ann_evaluation)[3] <- "MAD"
colnames(marlins_ann_evaluation)[4] <- "MAPE"
marlins_ann_evaluation
marlins_ann_output <- cbind(Team, test_marlins_ann$Attendance.G,marlins_ann_pred)
colnames(marlins_ann_output)[2] <- "Actual"
colnames(marlins_ann_output)[3] <- "Predicted"
marlins_ann_output
____________________________________________________________________________________________________________

brewers_ann = read.csv("H:/RIC/Milwaukee Brewers/Final.csv", header=T)

#dividing the data into training and testing dataset
train_brewers_ann = brewers_ann[ 2:51, 5:37 ]
test_brewers_ann = brewers_ann[ 1, 5:37 ]

#performing Principal Component analysis
princ_comp_brewers <- prcomp(train_brewers_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_brewers$rotation

#plotting the resultant Principal Components
biplot(princ_comp_brewers, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_brewers$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_brewers_pca <- data.frame(Attendance.G = train_brewers_ann$Attendance.G, princ_comp_brewers$x)

#creating the training data with PCs
train_brewers_pca <- train_brewers_pca[ ,1:34 ]

# fitting the neural network model now
set.seed(266)
brewers_ann_model = neuralnet(Attendance.G ~ ., train_brewers_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(brewers_ann_model)

#transforming initial test data into PCA
test_brewers_pca <- predict(princ_comp_brewers,newdata = test_brewers_ann)
test_brewers_pca <- as.data.frame(test_brewers_pca)

#selecting all the required components
test_brewers_pca <- test_brewers_pca[,1:33]

## Prediction using neural network
brewers_ann_pred <- predict(brewers_ann_model,test_brewers_pca)
brewers_ann_pred <- as.data.frame(brewers_ann_pred)

brewers_rmse <- (rmse(brewers_ann_pred$V1, test_brewers_ann$Attendance.G))/test_brewers_ann$Attendance.G
brewers_mad <- (mae(brewers_ann_pred$V1, test_brewers_ann$Attendance.G))/test_brewers_ann$Attendance.G
brewers_mape <- mape(brewers_ann_pred$V1, test_brewers_ann$Attendance.G)
Team = "Milwaukee Brewers"
brewers_ann_evaluation <- cbind(Team, brewers_rmse, brewers_mad, brewers_mape)
colnames(brewers_ann_evaluation)[2] <- "RMSE"
colnames(brewers_ann_evaluation)[3] <- "MAD"
colnames(brewers_ann_evaluation)[4] <- "MAPE"
brewers_ann_evaluation
brewers_ann_output <- cbind(Team, test_brewers_ann$Attendance.G,brewers_ann_pred)
colnames(brewers_ann_output)[2] <- "Actual"
colnames(brewers_ann_output)[3] <- "Predicted"
brewers_ann_output
_____________________________________________________________________________________________________________

twins_ann = read.csv("H:/RIC/Minnesota Twins/Final.csv", header=T)

#dividing the data into training and testing dataset
train_twins_ann = twins_ann[ 2:119, 5:38 ]
test_twins_ann = twins_ann[ 1, 5:38 ]

#performing Principal Component analysis
princ_comp_twins <- prcomp(train_twins_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_twins$rotation

#plotting the resultant Principal Components
biplot(princ_comp_twins, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_twins$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_twins_pca <- data.frame(Attendance.G = train_twins_ann$Attendance.G, princ_comp_twins$x)

#creating the training data with PCs
train_twins_pca <- train_twins_pca[ ,1:25 ]

# fitting the neural network model now
set.seed(945)
twins_ann_model = neuralnet(Attendance.G ~ ., train_twins_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(twins_ann_model)

#transforming initial test data into PCA
test_twins_pca <- predict(princ_comp_twins,newdata = test_twins_ann)
test_twins_pca <- as.data.frame(test_twins_pca)

#selecting all the required components
test_twins_pca <- test_twins_pca[,1:34]

## Prediction using neural network
twins_ann_pred <- predict(twins_ann_model,test_twins_pca)
twins_ann_pred <- as.data.frame(twins_ann_pred)

twins_rmse <- (rmse(twins_ann_pred$V1, test_twins_ann$Attendance.G))/test_twins_ann$Attendance.G
twins_mad <- (mae(twins_ann_pred$V1, test_twins_ann$Attendance.G))/test_twins_ann$Attendance.G
twins_mape <- mape(twins_ann_pred$V1, test_twins_ann$Attendance.G)
Team = "Minnesota Twins"
twins_ann_evaluation <- cbind(Team, twins_rmse, twins_mad, twins_mape)
colnames(twins_ann_evaluation)[2] <- "RMSE"
colnames(twins_ann_evaluation)[3] <- "MAD"
colnames(twins_ann_evaluation)[4] <- "MAPE"
twins_ann_evaluation
twins_ann_output <- cbind(Team, test_twins_ann$Attendance.G,twins_ann_pred)
colnames(twins_ann_output)[2] <- "Actual"
colnames(twins_ann_output)[3] <- "Predicted"
twins_ann_output
___________________________________________________________________________________________________________

mets_ann = read.csv("H:/RIC/New York Mets/Final.csv", header=T)

#dividing the data into training and testing dataset
train_mets_ann = mets_ann[ 2:58, 5:38 ]
test_mets_ann = mets_ann[ 1, 5:38 ]

#performing Principal Component analysis
princ_comp_mets <- prcomp(train_mets_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_mets$rotation

#plotting the resultant Principal Components
biplot(princ_comp_mets, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_mets$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_mets_pca <- data.frame(Attendance.G = train_mets_ann$Attendance.G, princ_comp_mets$x)

#creating the training data with PCs
train_mets_pca <- train_mets_pca[ ,1:35 ]

# fitting the neural network model now
set.seed(922)
mets_ann_model = neuralnet(Attendance.G ~ ., train_mets_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(mets_ann_model)

#transforming initial test data into PCA
test_mets_pca <- predict(princ_comp_mets,newdata = test_mets_ann)
test_mets_pca <- as.data.frame(test_mets_pca)

#selecting all the required components
test_mets_pca <- test_mets_pca[,1:34]

## Prediction using neural network
mets_ann_pred <- predict(mets_ann_model,test_mets_pca)
mets_ann_pred <- as.data.frame(mets_ann_pred)

mets_rmse <- (rmse(mets_ann_pred$V1, test_mets_ann$Attendance.G))/test_mets_ann$Attendance.G
mets_mad <- (mae(mets_ann_pred$V1, test_mets_ann$Attendance.G))/test_mets_ann$Attendance.G
mets_mape <- mape(mets_ann_pred$V1, test_mets_ann$Attendance.G)
Team = "New York Mets"
mets_ann_evaluation <- cbind(Team, mets_rmse, mets_mad, mets_mape)
colnames(mets_ann_evaluation)[2] <- "RMSE"
colnames(mets_ann_evaluation)[3] <- "MAD"
colnames(mets_ann_evaluation)[4] <- "MAPE"
mets_ann_evaluation
mets_ann_output <- cbind(Team, test_mets_ann$Attendance.G,mets_ann_pred)
colnames(mets_ann_output)[2] <- "Actual"
colnames(mets_ann_output)[3] <- "Predicted"
mets_ann_output
___________________________________________________________________________________________________________

yankees_ann = read.csv("H:/RIC/New York Yankees/Final.csv", header=T)

#dividing the data into training and testing dataset
train_yankees_ann = yankees_ann[ 2:117, 5:38 ]
test_yankees_ann = yankees_ann[ 1, 5:38 ]

#performing Principal Component analysis
princ_comp_yankees <- prcomp(train_yankees_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_yankees$rotation

#plotting the resultant Principal Components
biplot(princ_comp_yankees, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_yankees$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_yankees_pca <- data.frame(Attendance.G = train_yankees_ann$Attendance.G, princ_comp_yankees$x)

#creating the training data with PCs
train_yankees_pca <- train_yankees_pca[ ,1:35 ]

# fitting the neural network model now
set.seed(727)
yankees_ann_model = neuralnet(Attendance.G ~ ., train_yankees_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(yankees_ann_model)

#transforming initial test data into PCA
test_yankees_pca <- predict(princ_comp_yankees,newdata = test_yankees_ann)
test_yankees_pca <- as.data.frame(test_yankees_pca)

#selecting all the required components
test_yankees_pca <- test_yankees_pca[,1:34]

## Prediction using neural network
yankees_ann_pred <- predict(yankees_ann_model,test_yankees_pca)
yankees_ann_pred <- as.data.frame(yankees_ann_pred)

yankees_rmse <- (rmse(yankees_ann_pred$V1, test_yankees_ann$Attendance.G))/test_yankees_ann$Attendance.G
yankees_mad <- (mae(yankees_ann_pred$V1, test_yankees_ann$Attendance.G))/test_yankees_ann$Attendance.G
yankees_mape <- mape(yankees_ann_pred$V1, test_yankees_ann$Attendance.G)
Team = "New York Mets"
yankees_ann_evaluation <- cbind(Team, yankees_rmse, yankees_mad, yankees_mape)
colnames(yankees_ann_evaluation)[2] <- "RMSE"
colnames(yankees_ann_evaluation)[3] <- "MAD"
colnames(yankees_ann_evaluation)[4] <- "MAPE"
yankees_ann_evaluation
yankees_ann_output <- cbind(Team, test_yankees_ann$Attendance.G,yankees_ann_pred)
colnames(yankees_ann_output)[2] <- "Actual"
colnames(yankees_ann_output)[3] <- "Predicted"
yankees_ann_output
___________________________________________________________________________________________________________

athletics_ann = read.csv("H:/RIC/Oakland Athletics/Final.csv", header=T)

#dividing the data into training and testing dataset
train_athletics_ann = athletics_ann[ 2:119, 5:38 ]
test_athletics_ann = athletics_ann[ 1, 5:38 ]

#performing Principal Component analysis
princ_comp_athletics <- prcomp(train_athletics_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_athletics$rotation

#plotting the resultant Principal Components
biplot(princ_comp_athletics, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_athletics$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_athletics_pca <- data.frame(Attendance.G = train_athletics_ann$Attendance.G, princ_comp_athletics$x)

#creating the training data with PCs
train_athletics_pca <- train_athletics_pca[ ,1:34 ]

# fitting the neural network model now
set.seed(525)
athletics_ann_model = neuralnet(Attendance.G ~ ., train_athletics_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(athletics_ann_model)

#transforming initial test data into PCA
test_athletics_pca <- predict(princ_comp_athletics,newdata = test_athletics_ann)
test_athletics_pca <- as.data.frame(test_athletics_pca)

#selecting all the required components
test_athletics_pca <- test_athletics_pca[,1:33]

## Prediction using neural network
athletics_ann_pred <- predict(athletics_ann_model,test_athletics_pca)
athletics_ann_pred <- as.data.frame(athletics_ann_pred)

athletics_rmse <- (rmse(athletics_ann_pred$V1, test_athletics_ann$Attendance.G))/test_athletics_ann$Attendance.G
athletics_mad <- (mae(athletics_ann_pred$V1, test_athletics_ann$Attendance.G))/test_athletics_ann$Attendance.G
athletics_mape <- mape(athletics_ann_pred$V1, test_athletics_ann$Attendance.G)
Team = "Oakland Athletics"
athletics_ann_evaluation <- cbind(Team, athletics_rmse, athletics_mad, athletics_mape)
colnames(athletics_ann_evaluation)[2] <- "RMSE"
colnames(athletics_ann_evaluation)[3] <- "MAD"
colnames(athletics_ann_evaluation)[4] <- "MAPE"
athletics_ann_evaluation
athletics_ann_output <- cbind(Team, test_athletics_ann$Attendance.G,athletics_ann_pred)
colnames(athletics_ann_output)[2] <- "Actual"
colnames(athletics_ann_output)[3] <- "Predicted"
athletics_ann_output
___________________________________________________________________________________________________________

phillies_ann = read.csv("H:/RIC/Philadelphia Phillies/Final.csv", header=T)

#dividing the data into training and testing dataset
train_phillies_ann = phillies_ann[ 2:119, 5:38 ]
test_phillies_ann = phillies_ann[ 1, 5:38 ]

#performing Principal Component analysis
princ_comp_phillies <- prcomp(train_phillies_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_phillies$rotation

#plotting the resultant Principal Components
biplot(princ_comp_phillies, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_phillies$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_phillies_pca <- data.frame(Attendance.G = train_phillies_ann$Attendance.G, princ_comp_phillies$x)

#creating the training data with PCs
train_phillies_pca <- train_phillies_pca[ ,1:35 ]

# fitting the neural network model now
set.seed(777)
phillies_ann_model = neuralnet(Attendance.G ~ ., train_phillies_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(phillies_ann_model)

#transforming initial test data into PCA
test_phillies_pca <- predict(princ_comp_phillies,newdata = test_phillies_ann)
test_phillies_pca <- as.data.frame(test_phillies_pca)

#selecting all the required components
test_phillies_pca <- test_phillies_pca[,1:34]

## Prediction using neural network
phillies_ann_pred <- predict(phillies_ann_model,test_phillies_pca)
phillies_ann_pred <- as.data.frame(phillies_ann_pred)

phillies_rmse <- (rmse(phillies_ann_pred$V1, test_phillies_ann$Attendance.G))/test_phillies_ann$Attendance.G
phillies_mad <- (mae(phillies_ann_pred$V1, test_phillies_ann$Attendance.G))/test_phillies_ann$Attendance.G
phillies_mape <- mape(phillies_ann_pred$V1, test_phillies_ann$Attendance.G)
Team = "New York Mets"
phillies_ann_evaluation <- cbind(Team, phillies_rmse, phillies_mad, phillies_mape)
colnames(phillies_ann_evaluation)[2] <- "RMSE"
colnames(phillies_ann_evaluation)[3] <- "MAD"
colnames(phillies_ann_evaluation)[4] <- "MAPE"
phillies_ann_evaluation
phillies_ann_output <- cbind(Team, test_phillies_ann$Attendance.G,phillies_ann_pred)
colnames(phillies_ann_output)[2] <- "Actual"
colnames(phillies_ann_output)[3] <- "Predicted"
phillies_ann_output
___________________________________________________________________________________________________________

pirates_ann = read.csv("H:/RIC/Pittsburgh Pirates/Final.csv", header=T)

#dividing the data into training and testing dataset
train_pirates_ann = pirates_ann[ 2:130, 5:38 ]
test_pirates_ann = pirates_ann[ 1, 5:38 ]

#performing Principal Component analysis
princ_comp_pirates <- prcomp(train_pirates_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_pirates$rotation

#plotting the resultant Principal Components
biplot(princ_comp_pirates, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_pirates$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_pirates_pca <- data.frame(Attendance.G = train_pirates_ann$Attendance.G, princ_comp_pirates$x)

#creating the training data with PCs
train_pirates_pca <- train_pirates_pca[ ,1:35 ]

# fitting the neural network model now
set.seed(111)
pirates_ann_model = neuralnet(Attendance.G ~ ., train_pirates_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(pirates_ann_model)

#transforming initial test data into PCA
test_pirates_pca <- predict(princ_comp_pirates,newdata = test_pirates_ann)
test_pirates_pca <- as.data.frame(test_pirates_pca)

#selecting all the required components
test_pirates_pca <- test_pirates_pca[,1:34]

## Prediction using neural network
pirates_ann_pred <- predict(pirates_ann_model,test_pirates_pca)
pirates_ann_pred <- as.data.frame(pirates_ann_pred)

pirates_rmse <- (rmse(pirates_ann_pred$V1, test_pirates_ann$Attendance.G))/test_pirates_ann$Attendance.G
pirates_mad <- (mae(pirates_ann_pred$V1, test_pirates_ann$Attendance.G))/test_pirates_ann$Attendance.G
pirates_mape <- mape(pirates_ann_pred$V1, test_pirates_ann$Attendance.G)
Team = "Pittsburgh Pirates"
pirates_ann_evaluation <- cbind(Team, pirates_rmse, pirates_mad, pirates_mape)
colnames(pirates_ann_evaluation)[2] <- "RMSE"
colnames(pirates_ann_evaluation)[3] <- "MAD"
colnames(pirates_ann_evaluation)[4] <- "MAPE"
pirates_ann_evaluation
pirates_ann_output <- cbind(Team, test_pirates_ann$Attendance.G,pirates_ann_pred)
colnames(pirates_ann_output)[2] <- "Actual"
colnames(pirates_ann_output)[3] <- "Predicted"
pirates_ann_output
____________________________________________________________________________________________________________

padres_ann = read.csv("H:/RIC/San Diego Padres/Final.csv", header=T)

#dividing the data into training and testing dataset
train_padres_ann = padres_ann[ 2:51, 5:38 ]
test_padres_ann = padres_ann[ 1, 5:38 ]

#performing Principal Component analysis
princ_comp_padres <- prcomp(train_padres_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_padres$rotation

#plotting the resultant Principal Components
biplot(princ_comp_padres, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_padres$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_padres_pca <- data.frame(Attendance.G = train_padres_ann$Attendance.G, princ_comp_padres$x)

#creating the training data with PCs
train_padres_pca <- train_padres_pca[ ,1:35 ]

# fitting the neural network model now
set.seed(111)
padres_ann_model = neuralnet(Attendance.G ~ ., train_padres_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(padres_ann_model)

#transforming initial test data into PCA
test_padres_pca <- predict(princ_comp_padres,newdata = test_padres_ann)
test_padres_pca <- as.data.frame(test_padres_pca)

#selecting all the required components
test_padres_pca <- test_padres_pca[,1:34]

## Prediction using neural network
padres_ann_pred <- predict(padres_ann_model,test_padres_pca)
padres_ann_pred <- as.data.frame(padres_ann_pred)

padres_rmse <- (rmse(padres_ann_pred$V1, test_padres_ann$Attendance.G))/test_padres_ann$Attendance.G
padres_mad <- (mae(padres_ann_pred$V1, test_padres_ann$Attendance.G))/test_padres_ann$Attendance.G
padres_mape <- mape(padres_ann_pred$V1, test_padres_ann$Attendance.G)
Team = "San Diego Padres"
padres_ann_evaluation <- cbind(Team, padres_rmse, padres_mad, padres_mape)
colnames(padres_ann_evaluation)[2] <- "RMSE"
colnames(padres_ann_evaluation)[3] <- "MAD"
colnames(padres_ann_evaluation)[4] <- "MAPE"
padres_ann_evaluation
padres_ann_output <- cbind(Team, test_padres_ann$Attendance.G,padres_ann_pred)
colnames(padres_ann_output)[2] <- "Actual"
colnames(padres_ann_output)[3] <- "Predicted"
padres_ann_output
___________________________________________________________________________________________________________

giants_ann = read.csv("H:/RIC/San Fransisco Giants/Final.csv", header=T)

#dividing the data into training and testing dataset
train_giants_ann = giants_ann[ 2:130, 5:38 ]
test_giants_ann = giants_ann[ 1, 5:38 ]

#performing Principal Component analysis
princ_comp_giants <- prcomp(train_giants_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_giants$rotation

#plotting the resultant Principal Components
biplot(princ_comp_giants, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_giants$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_giants_pca <- data.frame(Attendance.G = train_giants_ann$Attendance.G, princ_comp_giants$x)

#creating the training data with PCs
train_giants_pca <- train_giants_pca[ ,1:35 ]

# fitting the neural network model now
set.seed(199)
giants_ann_model = neuralnet(Attendance.G ~ ., train_giants_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(giants_ann_model)

#transforming initial test data into PCA
test_giants_pca <- predict(princ_comp_giants,newdata = test_giants_ann)
test_giants_pca <- as.data.frame(test_giants_pca)

#selecting all the required components
test_giants_pca <- test_giants_pca[,1:34]

## Prediction using neural network
giants_ann_pred <- predict(giants_ann_model,test_giants_pca)
giants_ann_pred <- as.data.frame(giants_ann_pred)

giants_rmse <- (rmse(giants_ann_pred$V1, test_giants_ann$Attendance.G))/test_giants_ann$Attendance.G
giants_mad <- (mae(giants_ann_pred$V1, test_giants_ann$Attendance.G))/test_giants_ann$Attendance.G
giants_mape <- mape(giants_ann_pred$V1, test_giants_ann$Attendance.G)
Team = "San Fransisco Giants"
giants_ann_evaluation <- cbind(Team, giants_rmse, giants_mad, giants_mape)
colnames(giants_ann_evaluation)[2] <- "RMSE"
colnames(giants_ann_evaluation)[3] <- "MAD"
colnames(giants_ann_evaluation)[4] <- "MAPE"
giants_ann_evaluation
giants_ann_output <- cbind(Team, test_giants_ann$Attendance.G,giants_ann_pred)
colnames(giants_ann_output)[2] <- "Actual"
colnames(giants_ann_output)[3] <- "Predicted"
giants_ann_output
___________________________________________________________________________________________________________

mariners_ann = read.csv("H:/RIC/Seattle Mariners/Final.csv", header=T)

#dividing the data into training and testing dataset
train_mariners_ann = mariners_ann[ 2:43, 5:38 ]
test_mariners_ann = mariners_ann[ 1, 5:38 ]

#performing Principal Component analysis
princ_comp_mariners <- prcomp(train_mariners_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_mariners$rotation

#plotting the resultant Principal Components
biplot(princ_comp_mariners, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_mariners$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_mariners_pca <- data.frame(Attendance.G = train_mariners_ann$Attendance.G, princ_comp_mariners$x)

#creating the training data with PCs
train_mariners_pca <- train_mariners_pca[ ,1:35 ]

# fitting the neural network model now
set.seed(388)
mariners_ann_model = neuralnet(Attendance.G ~ ., train_mariners_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(mariners_ann_model)

#transforming initial test data into PCA
test_mariners_pca <- predict(princ_comp_mariners,newdata = test_mariners_ann)
test_mariners_pca <- as.data.frame(test_mariners_pca)

#selecting all the required components
test_mariners_pca <- test_mariners_pca[,1:34]

## Prediction using neural network
mariners_ann_pred <- predict(mariners_ann_model,test_mariners_pca)
mariners_ann_pred <- as.data.frame(mariners_ann_pred)

mariners_rmse <- (rmse(mariners_ann_pred$V1, test_mariners_ann$Attendance.G))/test_mariners_ann$Attendance.G
mariners_mad <- (mae(mariners_ann_pred$V1, test_mariners_ann$Attendance.G))/test_mariners_ann$Attendance.G
mariners_mape <- mape(mariners_ann_pred$V1, test_mariners_ann$Attendance.G)
Team = "Seattle Mariners"
mariners_ann_evaluation <- cbind(Team, mariners_rmse, mariners_mad, mariners_mape)
colnames(mariners_ann_evaluation)[2] <- "RMSE"
colnames(mariners_ann_evaluation)[3] <- "MAD"
colnames(mariners_ann_evaluation)[4] <- "MAPE"
mariners_ann_evaluation
mariners_ann_output <- cbind(Team, test_mariners_ann$Attendance.G,mariners_ann_pred)
colnames(mariners_ann_output)[2] <- "Actual"
colnames(mariners_ann_output)[3] <- "Predicted"
mariners_ann_output
___________________________________________________________________________________________________________

cardinals_ann = read.csv("H:/RIC/St. Louis Cardinals/Final.csv", header=T)

#dividing the data into training and testing dataset
train_cardinals_ann = cardinals_ann[ 2:128, 5:38 ]
test_cardinals_ann = cardinals_ann[ 1, 5:38 ]

#performing Principal Component analysis
princ_comp_cardinals <- prcomp(train_cardinals_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_cardinals$rotation

#plotting the resultant Principal Components
biplot(princ_comp_cardinals, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_cardinals$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_cardinals_pca <- data.frame(Attendance.G = train_cardinals_ann$Attendance.G, princ_comp_cardinals$x)

#creating the training data with PCs
train_cardinals_pca <- train_cardinals_pca[ ,1:35 ]

# fitting the neural network model now
set.seed(465)
cardinals_ann_model = neuralnet(Attendance.G ~ ., train_cardinals_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(cardinals_ann_model)

#transforming initial test data into PCA
test_cardinals_pca <- predict(princ_comp_cardinals,newdata = test_cardinals_ann)
test_cardinals_pca <- as.data.frame(test_cardinals_pca)

#selecting all the required components
test_cardinals_pca <- test_cardinals_pca[,1:34]

## Prediction using neural network
cardinals_ann_pred <- predict(cardinals_ann_model,test_cardinals_pca)
cardinals_ann_pred <- as.data.frame(cardinals_ann_pred)

cardinals_rmse <- (rmse(cardinals_ann_pred$V1, test_cardinals_ann$Attendance.G))/test_cardinals_ann$Attendance.G
cardinals_mad <- (mae(cardinals_ann_pred$V1, test_cardinals_ann$Attendance.G))/test_cardinals_ann$Attendance.G
cardinals_mape <- mape(cardinals_ann_pred$V1, test_cardinals_ann$Attendance.G)
Team = "St. Louis Cardinals"
cardinals_ann_evaluation <- cbind(Team, cardinals_rmse, cardinals_mad, cardinals_mape)
colnames(cardinals_ann_evaluation)[2] <- "RMSE"
colnames(cardinals_ann_evaluation)[3] <- "MAD"
colnames(cardinals_ann_evaluation)[4] <- "MAPE"
cardinals_ann_evaluation
cardinals_ann_output <- cbind(Team, test_cardinals_ann$Attendance.G,cardinals_ann_pred)
colnames(cardinals_ann_output)[2] <- "Actual"
colnames(cardinals_ann_output)[3] <- "Predicted"
cardinals_ann_output
____________________________________________________________________________________________________________

rays_ann = read.csv("H:/RIC/Tampa Bay Rays/Final.csv", header=T)

#dividing the data into training and testing dataset
train_rays_ann = rays_ann[ 2:22, 5:38 ]
test_rays_ann = rays_ann[ 1, 5:38 ]

#performing Principal Component analysis
princ_comp_rays <- prcomp(train_rays_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_rays$rotation

#plotting the resultant Principal Components
biplot(princ_comp_rays, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_rays$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_rays_pca <- data.frame(Attendance.G = train_rays_ann$Attendance.G, princ_comp_rays$x)

#creating the training data with PCs
train_rays_pca <- train_rays_pca[ ,1:22 ]

# fitting the neural network model now
set.seed(591)
rays_ann_model = neuralnet(Attendance.G ~ ., train_rays_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(rays_ann_model)

#transforming initial test data into PCA
test_rays_pca <- predict(princ_comp_rays,newdata = test_rays_ann)
test_rays_pca <- as.data.frame(test_rays_pca)

#selecting all the required components
test_rays_pca <- test_rays_pca[,1:21]

## Prediction using neural network
rays_ann_pred <- predict(rays_ann_model,test_rays_pca)
rays_ann_pred <- as.data.frame(rays_ann_pred)

rays_rmse <- (rmse(rays_ann_pred$V1, test_rays_ann$Attendance.G))/test_rays_ann$Attendance.G
rays_mad <- (mae(rays_ann_pred$V1, test_rays_ann$Attendance.G))/test_rays_ann$Attendance.G
rays_mape <- mape(rays_ann_pred$V1, test_rays_ann$Attendance.G)
Team = "Tampa Bay Rays"
rays_ann_evaluation <- cbind(Team, rays_rmse, rays_mad, rays_mape)
colnames(rays_ann_evaluation)[2] <- "RMSE"
colnames(rays_ann_evaluation)[3] <- "MAD"
colnames(rays_ann_evaluation)[4] <- "MAPE"
rays_ann_evaluation
rays_ann_output <- cbind(Team, test_rays_ann$Attendance.G,rays_ann_pred)
colnames(rays_ann_output)[2] <- "Actual"
colnames(rays_ann_output)[3] <- "Predicted"
rays_ann_output
____________________________________________________________________________________________________________

rangers_ann = read.csv("H:/RIC/Texas Rangers/Final.csv", header=T)

#dividing the data into training and testing dataset
train_rangers_ann = rangers_ann[ 2:59, 5:38 ]
test_rangers_ann = rangers_ann[ 1, 5:38 ]

#performing Principal Component analysis
princ_comp_rangers <- prcomp(train_rangers_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_rangers$rotation

#plotting the resultant Principal Components
biplot(princ_comp_rangers, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_rangers$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_rangers_pca <- data.frame(Attendance.G = train_rangers_ann$Attendance.G, princ_comp_rangers$x)

#creating the training data with PCs
train_rangers_pca <- train_rangers_pca[ ,1:35 ]

# fitting the neural network model now
set.seed(342)
rangers_ann_model = neuralnet(Attendance.G ~ ., train_rangers_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(rangers_ann_model)

#transforming initial test data into PCA
test_rangers_pca <- predict(princ_comp_rangers,newdata = test_rangers_ann)
test_rangers_pca <- as.data.frame(test_rangers_pca)

#selecting all the required components
test_rangers_pca <- test_rangers_pca[,1:34]

## Prediction using neural network
rangers_ann_pred <- predict(rangers_ann_model,test_rangers_pca)
rangers_ann_pred <- as.data.frame(rangers_ann_pred)

rangers_rmse <- (rmse(rangers_ann_pred$V1, test_rangers_ann$Attendance.G))/test_rangers_ann$Attendance.G
rangers_mad <- (mae(rangers_ann_pred$V1, test_rangers_ann$Attendance.G))/test_rangers_ann$Attendance.G
rangers_mape <- mape(rangers_ann_pred$V1, test_rangers_ann$Attendance.G)
Team = "Texas Rangers"
rangers_ann_evaluation <- cbind(Team, rangers_rmse, rangers_mad, rangers_mape)
colnames(rangers_ann_evaluation)[2] <- "RMSE"
colnames(rangers_ann_evaluation)[3] <- "MAD"
colnames(rangers_ann_evaluation)[4] <- "MAPE"
rangers_ann_evaluation
rangers_ann_output <- cbind(Team, test_rangers_ann$Attendance.G,rangers_ann_pred)
colnames(rangers_ann_output)[2] <- "Actual"
colnames(rangers_ann_output)[3] <- "Predicted"
rangers_ann_output
___________________________________________________________________________________________________________

bluejays_ann = read.csv("H:/RIC/Toronto Blue Jays/Final.csv", header=T)

#dividing the data into training and testing dataset
train_bluejays_ann = bluejays_ann[ 2:43, 5:38 ]
test_bluejays_ann = bluejays_ann[ 1, 5:38 ]

#performing Principal Component analysis
princ_comp_bluejays <- prcomp(train_bluejays_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_bluejays$rotation

#plotting the resultant Principal Components
biplot(princ_comp_bluejays, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_bluejays$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_bluejays_pca <- data.frame(Attendance.G = train_bluejays_ann$Attendance.G, princ_comp_bluejays$x)

#creating the training data with PCs
train_bluejays_pca <- train_bluejays_pca[ ,1:35 ]

# fitting the neural network model now
set.seed(259)
bluejays_ann_model = neuralnet(Attendance.G ~ ., train_bluejays_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(bluejays_ann_model)

#transforming initial test data into PCA
test_bluejays_pca <- predict(princ_comp_bluejays,newdata = test_bluejays_ann)
test_bluejays_pca <- as.data.frame(test_bluejays_pca)

#selecting all the required components
test_bluejays_pca <- test_bluejays_pca[,1:34]

## Prediction using neural network
bluejays_ann_pred <- predict(bluejays_ann_model,test_bluejays_pca)
bluejays_ann_pred <- as.data.frame(bluejays_ann_pred)

bluejays_rmse <- (rmse(bluejays_ann_pred$V1, test_bluejays_ann$Attendance.G))/test_bluejays_ann$Attendance.G
bluejays_mad <- (mae(bluejays_ann_pred$V1, test_bluejays_ann$Attendance.G))/test_bluejays_ann$Attendance.G
bluejays_mape <- mape(bluejays_ann_pred$V1, test_bluejays_ann$Attendance.G)
Team = "Toronto Blue Jays"
bluejays_ann_evaluation <- cbind(Team, bluejays_rmse, bluejays_mad, bluejays_mape)
colnames(bluejays_ann_evaluation)[2] <- "RMSE"
colnames(bluejays_ann_evaluation)[3] <- "MAD"
colnames(bluejays_ann_evaluation)[4] <- "MAPE"
bluejays_ann_evaluation
bluejays_ann_output <- cbind(Team, test_bluejays_ann$Attendance.G,bluejays_ann_pred)
colnames(bluejays_ann_output)[2] <- "Actual"
colnames(bluejays_ann_output)[3] <- "Predicted"
bluejays_ann_output
____________________________________________________________________________________________________________

nationals_ann = read.csv("H:/RIC/Washington Nationals/Final.csv", header=T)

#dividing the data into training and testing dataset
train_nationals_ann = nationals_ann[ 2:51, 5:38 ]
test_nationals_ann = nationals_ann[ 1, 5:38 ]

#performing Principal Component analysis
princ_comp_nationals <- prcomp(train_nationals_ann, scale. = TRUE)

#analysing the loading of Principal Components
princ_comp_nationals$rotation

#plotting the resultant Principal Components
biplot(princ_comp_nationals, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp <-  (princ_comp_nationals$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp <- variance_prcomp/sum(variance_prcomp)
plot(propvar_exp, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#adding principal components with training dataset
train_nationals_pca <- data.frame(Attendance.G = train_nationals_ann$Attendance.G, princ_comp_nationals$x)

#creating the training data with PCs
train_nationals_pca <- train_nationals_pca[ ,1:35 ]

# fitting the neural network model now
set.seed(800)
nationals_ann_model = neuralnet(Attendance.G ~ ., train_nationals_pca, hidden = 2 , linear.output = T )

# plot neural network
plot(nationals_ann_model)

#transforming initial test data into PCA
test_nationals_pca <- predict(princ_comp_nationals,newdata = test_nationals_ann)
test_nationals_pca <- as.data.frame(test_nationals_pca)

#selecting all the required components
test_nationals_pca <- test_nationals_pca[,1:34]

## Prediction using neural network
nationals_ann_pred <- predict(nationals_ann_model,test_nationals_pca)
nationals_ann_pred <- as.data.frame(nationals_ann_pred)

nationals_rmse <- (rmse(nationals_ann_pred$V1, test_nationals_ann$Attendance.G))/test_nationals_ann$Attendance.G
nationals_mad <- (mae(nationals_ann_pred$V1, test_nationals_ann$Attendance.G))/test_nationals_ann$Attendance.G
nationals_mape <- mape(nationals_ann_pred$V1, test_nationals_ann$Attendance.G)
Team = "Washington Nationals"
nationals_ann_evaluation <- cbind(Team, nationals_rmse, nationals_mad, nationals_mape)
colnames(nationals_ann_evaluation)[2] <- "RMSE"
colnames(nationals_ann_evaluation)[3] <- "MAD"
colnames(nationals_ann_evaluation)[4] <- "MAPE"
nationals_ann_evaluation
nationals_ann_output <- cbind(Team, test_nationals_ann$Attendance.G,nationals_ann_pred)
colnames(nationals_ann_output)[2] <- "Actual"
colnames(nationals_ann_output)[3] <- "Predicted"
nationals_ann_output
____________________________________________________________________________________________________________

write.csv(model_ann_evaluation <- rbind(dbk_ann_evaluation, brv_ann_evaluation, ori_ann_evaluation, redsox_ann_evaluation, cubs_ann_evaluation,
                                        whitesox_ann_evaluation, reds_ann_evaluation, indians_ann_evaluation, rockies_ann_evaluation, tigers_ann_evaluation, 
                                        astros_ann_evaluation, royals_ann_evaluation, angels_ann_evaluation, dodgers_ann_evaluation, marlins_ann_evaluation,
                                        brewers_ann_evaluation, twins_ann_evaluation, mets_ann_evaluation, yankees_ann_evaluation, athletics_ann_evaluation, 
                                        phillies_ann_evaluation, pirates_ann_evaluation, padres_ann_evaluation, giants_ann_evaluation, mariners_ann_evaluation, 
                                        cardinals_ann_evaluation, rays_ann_evaluation, rangers_ann_evaluation, bluejays_ann_evaluation, nationals_ann_evaluation), 
                                        "H:/RIC/Evaluation/ANN_Evaluation.csv")
__________________________________________________________________________________________________________________________________________________________________

write.csv(model_ann_output <- rbind(dbk_ann_output, brv_ann_output, ori_ann_output, redsox_ann_output, cubs_ann_output,
                                    whitesox_ann_output, reds_ann_output, indians_ann_output, rockies_ann_output, tigers_ann_output, 
                                    astros_ann_output, royals_ann_output, angels_ann_output, dodgers_ann_output, marlins_ann_output,
                                    brewers_ann_output, twins_ann_output, mets_ann_output, yankees_ann_output, athletics_ann_output, 
                                    phillies_ann_output, padres_ann_output, giants_ann_output, mariners_ann_output, 
                                    cardinals_ann_output, rays_ann_output, rangers_ann_output, bluejays_ann_output, nationals_ann_output), 
                                    "H:/RIC/Evaluation/ANN_Output.csv")
