library(corrplot)
library(mlbench)
library(caret)
library(factoextra)

#Pre Processing for Arizona diamondbacks team
arz_dbk <- read.csv("H:/RIC/Arizona_Diamonbacks/bat.csv")
arz_dbk_bat <- subset(arz_dbk, select=c(2:17,25:26))
str(arz_dbk_bat)

arz_dbk_1 <- read.csv("H:/RIC/Arizona_Diamonbacks/pitching.csv")
arz_dbk_pitch <- subset(arz_dbk_1, select=c(6:17,24))
str(arz_dbk_pitch)

arz_dbk_2 <- read.csv("H:/RIC/Arizona_Diamonbacks/history.csv")
arz_dbk_hist <- subset(arz_dbk_2, select=c(1,14:16))
str(arz_dbk_hist)

dbk_final <- cbind(arz_dbk_bat,arz_dbk_pitch,arz_dbk_hist)
dbk_final$Win_pctg <- (dbk_final$W/dbk_final$G)
dbk_final$Loss_pctg <- (dbk_final$L/dbk_final$G)
write.csv(dbk_final, "H:/RIC/Arizona_Diamonbacks/Final.csv")

dbk <- read.csv("H:/RIC/Arizona_Diamonbacks/Final.csv", header = TRUE)
dbk_plot <- cor(subset(dbk, select=c(3:38)))
round(dbk_plot, 2)
corrplot(dbk_plot, method = "color")

dbk_pca <- prcomp(dbk, scale. = TRUE)
dbk_pca$rotation

#plotting the resultant Principal Components
biplot(dbk_pca, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp_dbk <-  (dbk_pca$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp_dbk <- variance_prcomp_dbk/sum(variance_prcomp_dbk)
plot(propvar_exp_dbk, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp_dbk), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

dataset_dbk <- data.frame(Attendance.G=dbk$Attendance.G, dbk_pca$x)
write.csv(dataset_dbk, "H:/RIC/Arizona_Diamonbacks/Data_afterPCA.csv")
**********************************************************************************************************
  
#Pre-Processing for Atlanta Braves
atl_brv <- read.csv("H:/RIC/Atlanta Braves/bat.csv")
atl_brv_bat <- subset(atl_brv, select=c(2:17,25:26))
str(atl_brv_bat)

atl_brv_1 <- read.csv("H:/RIC/Atlanta Braves/pitching.csv")
atl_brv_pitch <- subset(atl_brv_1, select=c(6:17,24))
str(atl_brv_pitch)

atl_brv_2 <- read.csv("H:/RIC/Atlanta Braves/history.csv")
atl_brv_hist <- subset(atl_brv_2, select=c(14:16))
str(atl_brv_hist)

brv_final <- cbind(atl_brv_bat,atl_brv_pitch,atl_brv_hist)
brv_final$Win_pctg <- (brv_final$W/brv_final$G)
brv_final$Loss_pctg <- (brv_final$L/brv_final$G)
write.csv(dbk_final, "H:/RIC/Atlanta Braves/Final.csv")

brv <- read.csv("H:/RIC/Atlanta Braves/Final.csv", header = TRUE)
brv_plot <- cor(subset(brv, select=c(3:38)))
round(brv_plot, 2)
corrplot(brv_plot, method = "color")

brv_pca <- prcomp(brv, scale. = TRUE)
brv_pca$rotation

#plotting the resultant Principal Components
biplot(brv_pca, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp_brv <-  (brv_pca$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp_brv <- variance_prcomp_brv/sum(variance_prcomp_brv)
plot(propvar_exp_brv, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp_brv), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

dataset_brv <- data.frame(Attendance.G=brv$Attendance.G, brv_pca$x)
write.csv(dataset_brv, "H:/RIC/Atlanta Braves/Data_afterPCA.csv")

*************************************************************************************************

#preprocessing for Baltinore Orioles
bal_ori <- read.csv("H:/RIC/Baltimore Orioles/bat.csv")
bal_ori_bat <- subset(bal_ori, select=c(2:17,25:26))
bal_ori_bat[is.na(bal_ori_bat)] <- 0
str(bal_ori_bat)

bal_ori_1 <- read.csv("H:/RIC/Baltimore Orioles/pitching.csv")
bal_ori_pitch <- subset(bal_ori_1, select=c(6:17,24))
str(bal_ori_pitch)

bal_ori_2 <- read.csv("H:/RIC/Baltimore Orioles/history.csv")
bal_ori_hist <- subset(bal_ori_2, select=c(15:18))
str(bal_ori_hist)

ori_final <- cbind(bal_ori_bat,bal_ori_pitch,bal_ori_hist)
ori_final$Win_pctg <- (ori_final$W/ori_final$G)
ori_final$Loss_pctg <- (ori_final$L/ori_final$G)
write.csv(ori_final, "H:/RIC/Baltimore Orioles/Final.csv")

ori <- read.csv("H:/RIC/Baltimore Orioles/Final.csv", header = TRUE)
ori_plot <- cor(subset(ori, select=c(3:37)))
round(ori_plot, 2)
corrplot(ori_plot, method = "color")

ori_pca <- prcomp(ori, scale. = TRUE)
ori_pca$rotation

#plotting the resultant Principal Components
biplot(ori_pca, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp_ori <-  (ori_pca$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp_ori <- variance_prcomp_ori/sum(variance_prcomp_ori)
plot(propvar_exp_ori, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp_ori), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

dataset_ori <- data.frame(Attendance.G=ori$Attendance.G, ori_pca$x)
write.csv(dataset_ori, "H:/RIC/Baltimore Orioles/Data_afterPCA.csv")

*************************************************************************************************
#preprocessing for Boston Red Sox
red_sox <- read.csv("H:/RIC/Boston Red Sox/bat.csv")
red_sox$CS <- ifelse(is.na(red_sox$CS), mean(red_sox$CS, na.rm=TRUE), red_sox$CS)
red_sox_bat <- subset(red_sox, select=c(2:17,25:26))
str(red_sox_bat)

red_sox_1 <- read.csv("H:/RIC/Boston Red Sox/pitching.csv")
red_sox_pitch <- subset(red_sox_1, select=c(6:17,24))
str(red_sox_pitch)

red_sox_2 <- read.csv("H:/RIC/Boston Red Sox/history.csv")
red_sox_hist <- subset(red_sox_2, select=c(15:17))
str(red_sox_hist)

redsox_final <- cbind(red_sox_bat,red_sox_pitch,red_sox_hist)
redsox_final$Win_pctg <- (redsox_final$W/redsox_final$G)
redsox_final$Loss_pctg <- (redsox_final$L/redsox_final$G)
write.csv(redsox_final, "H:/RIC/Boston Red Sox/Final.csv")

redsox <- read.csv("H:/RIC/Boston Red Sox/Final.csv", header = TRUE)
redsox_plot <- cor(subset(redsox, select=c(3:37)))
round(redsox_plot, 2)
corrplot(redsox_plot, method = "color")

redsox_pca <- prcomp(redsox, scale. = TRUE)
redsox_pca$rotation

#plotting the resultant Principal Components
biplot(redsox_pca, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp_redsox <-  (redsox_pca$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp_redsox <- variance_prcomp_redsox/sum(variance_prcomp_redsox)
plot(propvar_exp_redsox, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp_redsox), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

dataset_redsox <- data.frame(Attendance.G=redsox$Attendance.G, redsox_pca$x)
write.csv(dataset_redsox, "H:/RIC/Boston Red Sox/Data_afterPCA.csv")

*************************************************************************************************
#preprocessing for Chicago Cubs
chi_cub <- read.csv("H:/RIC/Chicago Cubs/bat.csv")
chi_cub$CS <- ifelse(is.na(chi_cub$CS), mean(chi_cub$CS, na.rm=TRUE), chi_cub$CS)
chi_cub_bat <- subset(chi_cub, select=c(2:17,25:26))
str(chi_cub_bat)

chi_cub_1 <- read.csv("H:/RIC/Chicago Cubs/pitching.csv")
chi_cub_pitch <- subset(chi_cub_1, select=c(6:17,24))
str(chi_cub_pitch)

chi_cub_2 <- read.csv("H:/RIC/Chicago Cubs/history.csv")
chi_cub_hist <- subset(chi_cub_2, select=c(15:18))
str(chi_cub_hist)

cub_final <- cbind(red_sox_bat,red_sox_pitch,red_sox_hist)
cub_final$Win_pctg <- (cub_final$W/cub_final$G)
cub_final$Loss_pctg <- (cub_final$L/cub_final$G)
write.csv(cub_final, "H:/RIC/Chicago Cubs/Final.csv")

cubs <- read.csv("H:/RIC/Chicago Cubs/Final.csv", header = TRUE)
cubs_plot <- cor(subset(cubs, select=c(3:37)))
round(cubs_plot, 2)
corrplot(cubs_plot, method = "color")

cubs_pca <- prcomp(cubs, scale. = TRUE)
cubs_pca$rotation

#plotting the resultant Principal Components
biplot(cubs_pca, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp_cubs <-  (cubs_pca$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp_cubs <- variance_prcomp_cubs/sum(variance_prcomp_cubs)
plot(propvar_exp_cubs, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp_cubs), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

dataset_cubs <- data.frame(Attendance.G=cubs$Attendance.G, cubs_pca$x)
write.csv(dataset_cubs, "H:/RIC/Chicago Cubs/Data_afterPCA.csv")
_______________________________________________________________________________________________________________________

#preprocessing for Chicago White Sox
whi_sox <- read.csv("H:/RIC/Chicago White Sox/bat.csv")
whi_sox$CS <- ifelse(is.na(whi_sox$CS), mean(whi_sox$CS, na.rm=TRUE), whi_sox$CS)
whi_sox_bat <- subset(whi_sox, select=c(2:17,25:26))
str(whi_sox_bat)

whi_sox_1 <- read.csv("H:/RIC/Chicago White Sox/pitching.csv")
whi_sox_pitch <- subset(whi_sox_1, select=c(6:17,24))
str(whi_sox_pitch)

whi_sox_2 <- read.csv("H:/RIC/Chicago White Sox/history.csv")
whi_sox_hist <- subset(whi_sox_2, select=c(15:18))
str(whi_sox_hist)

whitesox_final <- cbind(whi_sox_bat,whi_sox_pitch,whi_sox_hist)
whitesox_final$Win_pctg <- (whitesox_final$W/whitesox_final$G)
whitesox_final$Loss_pctg <- (whitesox_final$L/whitesox_final$G)
write.csv(whitesox_final, "H:/RIC/Chicago White Sox/Final.csv")

whitesox <- read.csv("H:/RIC/Chicago White Sox/Final.csv", header = TRUE)
whitesox_plot <- cor(subset(whitesox, select=c(3:37)))
round(whitesox_plot, 2)
corrplot(whitesox_plot, method = "color")

whitesox_pca <- prcomp(whitesox, scale. = TRUE)
whitesox_pca$rotation

#plotting the resultant Principal Components
biplot(whitesox_pca, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp_whitesox <-  (whitesox_pca$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp_whitesox <- variance_prcomp_whitesox/sum(variance_prcomp_whitesox)
plot(propvar_exp_whitesox, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp_whitesox), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

dataset_whitesox <- data.frame(Attendance.G=whitesox$Attendance.G, whitesox_pca$x)
write.csv(dataset_whitesox, "H:/RIC/Chicago White Sox/Data_afterPCA.csv")

************************************************************************************************
  
#preprocessing for Cincinati Reds
cin_red <- read.csv("H:/RIC/Cincinnati Reds/bat.csv")
cin_red$CS <- ifelse(is.na(cin_red$CS), mean(cin_red$CS, na.rm=TRUE), cin_red$CS)
cin_red_bat <- subset(cin_red, select=c(2:17,25:26))
str(cin_red_bat)

cin_red_1 <- read.csv("H:/RIC/Cincinnati Reds/pitching.csv")
cin_red_pitch <- subset(cin_red_1, select=c(6:17,24))
str(cin_red_pitch)

cin_red_2 <- read.csv("H:/RIC/Cincinnati Reds/history.csv")
cin_red_hist <- subset(cin_red_2, select=c(15:18))
str(cin_red_hist)

cin_red_final <- cbind(cin_red_bat,cin_red_pitch,cin_red_hist)
cin_red_final$Win_pctg <- (cin_red_final$W/cin_red_final$G)
cin_red_final$Loss_pctg <- (cin_red_final$L/cin_red_final$G)
write.csv(cin_red_final, "H:/RIC/Cincinnati Reds/Final.csv")

reds <- read.csv("H:/RIC/Cincinnati Reds/Final.csv", header = TRUE)
reds_plot <- cor(subset(reds, select=c(3:37)))
round(reds_plot, 2)
corrplot(reds_plot, method = "color")

reds_pca <- prcomp(reds, scale. = TRUE)
reds_pca$rotation

#plotting the resultant Principal Components
biplot(reds_pca, scale = 0)

#computing the amount of variance explained by the PCs
variance_prcomp_reds <-  (reds_pca$sdev)^2

#computing the amount of variance explained by each PC
propvar_exp_reds <- variance_prcomp_reds/sum(variance_prcomp_reds)
plot(propvar_exp_reds, xlab="Prinicpal Component", 
     ylab="Proporation of Variance Explained", 
     type = "b")

#plotting the cumulative variance explained by all the PCs
plot(cumsum(propvar_exp_reds), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

dataset_reds <- data.frame(Attendance.G=reds$Attendance.G, reds_pca$x)
write.csv(dataset_reds, "H:/RIC/Cincinnati Reds/Data_afterPCA.csv")
************************************************************************************************
#preprocessing for Cleveland Indians
cle_ind <- read.csv("H:/RIC/Cleveland Indians/bat.csv")
cle_ind$CS <- ifelse(is.na(cle_ind$CS), mean(cle_ind$CS, na.rm=TRUE), cle_ind$CS)
cle_ind_bat <- subset(cle_ind, select=c(2:17,25:26))
str(cle_ind_bat)

cle_ind_1 <- read.csv("H:/RIC/Cleveland Indians/pitching.csv")
cle_ind_pitch <- subset(cle_ind_1, select=c(6:17,24))
str(cle_ind_pitch)

cle_ind_2 <- read.csv("H:/RIC/Cleveland Indians/history.csv")
cle_ind_hist <- subset(cle_ind_2, select=c(15:18))
str(cin_red_hist)

cle_ind_final <- cbind(cle_ind_bat,cle_ind_pitch,cle_ind_hist)
cle_ind_final$Win_pctg <- (cle_ind_final$W/cle_ind_final$G)
cle_ind_final$Loss_pctg <- (cle_ind_final$L/cle_ind_final$G)
write.csv(cle_ind_final, "H:/RIC/Cleveland Indians/Final.csv")

indians_final_1 <- read.csv("H:/RIC/Cleveland Indians/Final.csv", na.strings = "")
indians_final_1 <- as.data.frame(sapply(indians_final_1, as.numeric))
#drawing correlation plot
indians_final_plot <- cor(subset(indians_final_1, select=c(5:37)))
round(indians_final_plot, 2)
corrplot(indians_final_plot, method = "color")
#doing PCA
indians_final_1_pca <- prcomp(indians_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(indians_final_1_pca)
fviz_eig(indians_final_1_pca)
fviz_pca_ind(indians_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(indians_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(indians_final_1_pca, scale = 0)
biplot(indians_final_1_pca, expand=10, xlim=c(0.0, 0.1), ylim=c(-0.2, 0.0))

***********************************************************************************************************************

#preprocessing for Colorado Rockies
col_rock <- read.csv("H:/RIC/Colorado Rockies/bat.csv")
col_rock$CS <- ifelse(is.na(col_rock$CS), mean(col_rock$CS, na.rm=TRUE), col_rock$CS)
col_rock_bat <- subset(col_rock, select=c(2:17,25:26))
str(col_rock_bat)

col_rock_1 <- read.csv("H:/RIC/Colorado Rockies/pitching.csv")
col_rock_pitch <- subset(col_rock_1, select=c(6:17,24))
str(col_rock_pitch)

col_rock_2 <- read.csv("H:/RIC/Colorado Rockies/history.csv")
col_rock_hist <- subset(col_rock_2, select=c(15:18))
str(col_rock_hist)

rockies_final <- cbind(col_rock_bat,col_rock_pitch,col_rock_hist)
rockies_final$Win_pctg <- (rockies_final$W/rockies_final$G)
rockies_final$Loss_pctg <- (rockies_final$L/rockies_final$G)
write.csv(rockies_final, "H:/RIC/Colorado Rockies/Final.csv")

rockies_final_1 <- read.csv("H:/RIC/Colorado Rockies/Final.csv", na.strings = "")
rockies_final_1 <- as.data.frame(sapply(rockies_final_1, as.numeric))
#drawing correlation plot
rockies_final_plot <- cor(subset(rockies_final_1, select=c(5:37)))
round(rockies_final_plot, 2)
corrplot(rockies_final_plot, method = "color")
#doing PCA
rockies_final_1_pca <- prcomp(rockies_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(rockies_final_1_pca)
fviz_eig(rockies_final_1_pca)
fviz_pca_ind(rockies_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(rockies_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(rockies_final_1_pca, scale = 0)
biplot(rockies_final_1_pca, expand=10, xlim=c(-0.02, 0.07), ylim=c(-0.15, 0.0))

**********************************************************************************************************
#preprocessing for Detroit Tigers
det_tig <- read.csv("H:/RIC/Detroit Tigers/bat.csv")
det_tig$CS <- ifelse(is.na(det_tig$CS), mean(det_tig$CS, na.rm=TRUE), det_tig$CS)
det_tig_bat <- subset(det_tig, select=c(2:17,25:26))
str(det_tig_bat)

det_tig_1 <- read.csv("H:/RIC/Detroit Tigers/pitching.csv")
det_tig_pitch <- subset(det_tig_1, select=c(6:17,24))
str(det_tig_pitch)

det_tig_2 <- read.csv("H:/RIC/Detroit Tigers/history.csv")
det_tig_hist <- subset(det_tig_2, select=c(15:18))
str(det_tig_hist)

tigers_final <- cbind(det_tig_bat,det_tig_pitch,det_tig_hist)
tigers_final$Win_pctg <- (tigers_final$W/tigers_final$G)
tigers_final$Loss_pctg <- (tigers_final$L/tigers_final$G)
write.csv(tigers_final, "H:/RIC/Detroit Tigers/Final.csv")

tigers_final_1 <- read.csv("H:/RIC/Detroit Tigers/Final.csv", na.strings = "")
tigers_final_1 <- as.data.frame(sapply(tigers_final_1, as.numeric))
#drawing correlation plot
tigers_final_plot <- cor(subset(tigers_final_1, select=c(5:37)))
round(tigers_final_plot, 2)
corrplot(tigers_final_plot, method = "color")
#doing PCA
tigers_final_1_pca <- prcomp(tigers_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(tigers_final_1_pca)
fviz_eig(tigers_final_1_pca)
fviz_pca_ind(tigers_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(tigers_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(tigers_final_1_pca, scale = 0)
biplot(rockies_final_1_pca, expand=10, xlim=c(-0.01, 0.065), ylim=c(-0.2, 0.0))

**********************************************************************************************************

#preprocessing for Houston Astros
hou_ast <- read.csv("H:/RIC/Houston Astros/bat.csv")
hou_ast_bat <- subset(hou_ast, select=c(2:17,25:26))
str(hou_ast_bat)

hou_ast_1 <- read.csv("H:/RIC/Houston Astros/pitching.csv")
hou_ast_pitch <- subset(hou_ast_1, select=c(6:17,24))
str(hou_ast_pitch)

hou_ast_2 <- read.csv("H:/RIC/Houston Astros/history.csv")
hou_ast_hist <- subset(hou_ast_2, select=c(15:18))
str(hou_ast_hist)

astros_final <- cbind(hou_ast_bat,hou_ast_pitch,hou_ast_hist)
astros_final$Win_pctg <- (astros_final$W/astros_final$G)
astros_final$Loss_pctg <- (astros_final$L/astros_final$G)
write.csv(astros_final, "H:/RIC/Houston Astros/Final.csv")

astros_final_1 <- read.csv("H:/RIC/Houston Astros/Final.csv", na.strings = "")
astros_final_1 <- as.data.frame(sapply(astros_final_1, as.numeric))
#drawing correlation plot
astros_final_plot <- cor(subset(astros_final_1, select=c(5:37)))
round(astros_final_plot, 2)
corrplot(astros_final_plot, method = "color")
#doing PCA
astros_final_1_pca <- prcomp(astros_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(astros_final_1_pca)
fviz_eig(astros_final_1_pca)
fviz_pca_ind(astros_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(astros_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(astros_final_1_pca, scale = 0)
biplot(rockies_final_1_pca, expand=10, xlim=c(-0.02, 0.06), ylim=c(-0.10, 0.0))

**********************************************************************************************************
  
#preprocessingb for Kansas City Royals
kan_roy <- read.csv("H:/RIC/Kansas City Royals/bat.csv")
kan_roy_bat <- subset(kan_roy, select=c(2:17,25:26))
str(kan_roy_bat)

kan_roy_1 <- read.csv("H:/RIC/Kansas City Royals/pitching.csv")
kan_roy_pitch <- subset(kan_roy_1, select=c(6:17,24))
str(kan_roy_pitch)

kan_roy_2 <- read.csv("H:/RIC/Kansas City Royals/history.csv")
kan_roy_hist <- subset(kan_roy_2, select=c(15:18))
str(kan_roy_hist)

royals_final <- cbind(kan_roy_bat,kan_roy_pitch,kan_roy_hist)
royals_final$Win_pctg <- (royals_final$W/royals_final$G)
royals_final$Loss_pctg <- (royals_final$L/royals_final$G)
write.csv(royals_final, "H:/RIC/Kansas City Royals/Final.csv")

royals_final_1 <- read.csv("H:/RIC/Kansas City Royals/Final.csv", na.strings = "")
royals_final_1 <- as.data.frame(sapply(royals_final_1, as.numeric))
#drawing correlation plot
royals_final_plot <- cor(subset(royals_final_1, select=c(5:37)))
round(royals_final_plot, 2)
corrplot(royals_final_plot, method = "color")
#doing PCA
royals_final_1_pca <- prcomp(royals_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(royals_final_1_pca)
fviz_eig(royals_final_1_pca)
fviz_pca_ind(royals_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(royals_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(royals_final_1_pca, scale = 0)
biplot(royals_final_1_pca, expand=10, xlim=c(-0.02, 0.03), ylim=c(0.0, 0.1))

**********************************************************************************************************

#preprocessing for Los Angeles Angels
la_angel <- read.csv("H:/RIC/Los Angeles Angels/bat.csv")
la_angel_bat <- subset(la_angel, select=c(2:17,25:26))
str(la_angel_bat)

la_angel_1 <- read.csv("H:/RIC/Los Angeles Angels/pitching.csv")
la_angel_pitch <- subset(la_angel_1, select=c(6:17,24))
str(la_angel_pitch)

la_angel_2 <- read.csv("H:/RIC/Los Angeles Angels/history.csv")
la_angel_hist <- subset(la_angel_2, select=c(15:18))
str(la_angel_hist)

angels_final <- cbind(la_angel_bat,la_angel_pitch,la_angel_hist)
angels_final$Win_pctg <- (angels_final$W/angels_final$G)
angels_final$Loss_pctg <- (angels_final$L/angels_final$G)
write.csv(royals_final, "H:/RIC/Los Angeles Angels/Final.csv")

angels_final_1 <- read.csv("H:/RIC/Los Angeles Angels/Final.csv", na.strings = "")
angels_final_1 <- as.data.frame(sapply(angels_final_1, as.numeric))
#drawing correlation plot
angels_final_plot <- cor(subset(angels_final_1, select=c(5:37)))
round(angels_final_plot, 2)
corrplot(angels_final_plot, method = "color")
#doing PCA
angels_final_1_pca <- prcomp(angels_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(angels_final_1_pca)
fviz_eig(angels_final_1_pca)
fviz_pca_ind(angels_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(angels_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(angels_final_1_pca, scale = 0)
biplot(angels_final_1_pca, expand=10, xlim=c(-0.02, 0.03), ylim=c(0.0, 0.1))

***********************************************************************************************************
  
#preprocessing for Los Angeles Dodgers
la_dod <- read.csv("H:/RIC/Los Angeles Dodgers/bat.csv")
la_dod$CS <- ifelse(is.na(la_dod$CS), mean(la_dod$CS, na.rm=TRUE), la_dod$CS)
la_dod_bat <- subset(la_dod, select=c(2:17,25:26))
str(la_dod_bat)

la_dod_1 <- read.csv("H:/RIC/Los Angeles Dodgers/pitching.csv")
la_dod_pitch <- subset(la_dod_1, select=c(6:17,24))
str(la_dod_pitch)

la_dod_2 <- read.csv("H:/RIC/Los Angeles Dodgers/history.csv")
la_dod_hist <- subset(la_dod_2, select=c(15:18))
str(la_dod_hist)

dodgers_final <- cbind(la_dod_bat,la_dod_pitch,la_dod_hist)
dodgers_final$Win_pctg <- (dodgers_final$W/dodgers_final$G)
dodgers_final$Loss_pctg <- (dodgers_final$L/dodgers_final$G)
write.csv(dodgers_final, "H:/RIC/Los Angeles Dodgers/Final.csv")

dodgers_final_1 <- read.csv("H:/RIC/Los Angeles Dodgers/Final.csv", na.strings = "")
dodgers_final_1 <- as.data.frame(sapply(dodgers_final_1, as.numeric))
#drawing correlation plot
dodgers_final_plot <- cor(subset(dodgers_final_1, select=c(5:37)))
round(dodgers_final_plot, 2)
corrplot(dodgers_final_plot, method = "color")
#doing PCA
dodgers_final_1_pca <- prcomp(dodgers_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(dodgers_final_1_pca)
fviz_eig(dodgers_final_1_pca)
fviz_pca_ind(dodgers_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(dodgers_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(dodgers_final_1_pca, scale = 0)
biplot(royals_final_1_pca, expand=10, xlim=c(-0.03, 0.04), ylim=c(-0.01, 0.15))

**********************************************************************************************************
  
#preprocessing for Miami Marlins
mia_mar <- read.csv("H:/RIC/Miami Marlins/bat.csv")
mia_mar_bat <- subset(mia_mar, select=c(2:17,25:26))
str(mia_mar_bat)

mia_mar_1 <- read.csv("H:/RIC/Miami Marlins/pitching.csv")
mia_mar_pitch <- subset(mia_mar_1, select=c(6:17,24))
str(mia_mar_pitch)

mia_mar_2 <- read.csv("H:/RIC/Miami Marlins/history.csv")
mia_mar_hist <- subset(mia_mar_2, select=c(16:18))
str(mia_mar_hist)

marlins_final <- cbind(mia_mar_bat,mia_mar_pitch,mia_mar_hist)
marlins_final$Win_pctg <- (marlins_final$W/marlins_final$G)
marlins_final$Loss_pctg <- (marlins_final$L/marlins_final$G)
write.csv(marlins_final, "H:/RIC/Miami Marlins/Final.csv")

marlins_final_1 <- read.csv("H:/RIC/Miami Marlins/Final.csv", na.strings = "")
marlins_final_1 <- as.data.frame(sapply(marlins_final_1, as.numeric))
#drawing correlation plot
marlins_final_plot <- cor(subset(marlins_final_1, select=c(5:37)))
round(marlins_final_plot, 2)
corrplot(marlins_final_plot, method = "color")
#doing PCA
marlins_final_1_pca <- prcomp(marlins_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(marlins_final_1_pca)
fviz_eig(marlins_final_1_pca)
fviz_pca_ind(marlins_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(marlins_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(marlins_final_1_pca, scale = 0)
biplot(marlins_final_1_pca, expand=10, xlim=c(-0.35, 0.01), ylim=c(-0.15, 0.15))

**********************************************************************************************************

#preprocessing for Milwaukee Brewers
mil_brew <- read.csv("H:/RIC/Milwaukee Brewers/bat.csv")
mil_brew_bat <- subset(mil_brew, select=c(2:17,25:26))
str(mil_brew_bat)

mil_brew_1 <- read.csv("H:/RIC/Milwaukee Brewers/pitching.csv")
mil_brew_pitch <- subset(mil_brew_1, select=c(6:17,24))
str(mil_brew_pitch)

mil_brew_2 <- read.csv("H:/RIC/Milwaukee Brewers/history.csv")
mil_brew_hist <- subset(mil_brew_2, select=c(16:18))
str(mil_brew_hist)

brewers_final <- cbind(mil_brew_bat,mil_brew_pitch,mil_brew_hist)
brewers_final$Win_pctg <- (brewers_final$W/brewers_final$G)
brewers_final$Loss_pctg <- (brewers_final$L/brewers_final$G)
write.csv(brewers_final, "H:/RIC/Milwaukee Brewers/Final.csv")

brewers_final_1 <- read.csv("H:/RIC/Milwaukee Brewers/Final.csv", na.strings = "")
brewers_final_1 <- as.data.frame(sapply(brewers_final_1, as.numeric))
#drawing correlation plot
brewers_final_plot <- cor(subset(brewers_final_1, select=c(5:37)))
round(brewers_final_plot, 2)
corrplot(brewers_final_plot, method = "color")
#doing PCA
brewers_final_1_pca <- prcomp(brewers_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(brewers_final_1_pca)
fviz_eig(brewers_final_1_pca)
fviz_pca_ind(brewers_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(brewers_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(brewers_final_1_pca, scale = 0)
biplot(brewers_final_1_pca, expand=10, xlim=c(-0.01, 0.35), ylim=c(-0.05, 0.2))

**********************************************************************************************************
  
#preprocessing for Minnesota Twins
min_twin <- read.csv("H:/RIC/Minnesota Twins/bat.csv")
min_twin$CS <- ifelse(is.na(min_twin$CS), mean(min_twin$CS, na.rm=TRUE), min_twin$CS)
min_twin_bat <- subset(min_twin, select=c(2:17,25:26))
str(min_twin)

min_twin_1 <- read.csv("H:/RIC/Minnesota Twins/pitching.csv")
min_twin_pitch <- subset(min_twin_1, select=c(6:17,24))
str(min_twin_pitch)

min_twin_2 <- read.csv("H:/RIC/Minnesota Twins/history.csv")
min_twin_hist <- subset(min_twin_2, select=c(15:18))
str(min_twin_hist)

twins_final <- cbind(min_twin_bat,min_twin_pitch,min_twin_hist)
twins_final$Win_pctg <- (twins_final$W/twins_final$G)
twins_final$Loss_pctg <- (twins_final$L/twins_final$G)
write.csv(twins_final, "H:/RIC/Minnesota Twins/Final.csv")

twins_final_1 <- read.csv("H:/RIC/Minnesota Twins/Final.csv", na.strings = "")
twins_final_1 <- as.data.frame(sapply(twins_final_1, as.numeric))
#drawing correlation plot
twins_final_plot <- cor(subset(twins_final_1, select=c(5:37)))
round(twins_final_plot, 2)
corrplot(twins_final_plot, method = "color")
#doing PCA
twins_final_1_pca <- prcomp(twins_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(twins_final_1_pca)
fviz_eig(twins_final_1_pca)
fviz_pca_ind(twins_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(twins_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(twins_final_1_pca, scale = 0)
**********************************************************************************************************
  
#preprocessing for New York Mets
ny_mets <- read.csv("H:/RIC/New York Mets/bat.csv")
ny_mets_bat <- subset(ny_mets, select=c(2:17,25:26))
str(ny_mets_bat)

ny_mets_1 <- read.csv("H:/RIC/New York Mets/pitching.csv")
ny_mets_pitch <- subset(ny_mets_1, select=c(6:17,24))
str(ny_mets_pitch)

ny_mets_2 <- read.csv("H:/RIC/New York Mets/history.csv")
ny_mets_hist <- subset(ny_mets_2, select=c(15:18))
str(ny_mets_hist)

mets_final <- cbind(ny_mets_bat,ny_mets_pitch,ny_mets_hist)
mets_final$Win_pctg <- (mets_final$W/mets_final$G)
mets_final$Loss_pctg <- (mets_final$L/mets_final$G)
write.csv(mets_final, "H:/RIC/New York Mets/Final.csv")

mets_final_1 <- read.csv("H:/RIC/New York Mets/Final.csv", na.strings = "")
mets_final_1 <- as.data.frame(sapply(mets_final_1, as.numeric))
#drawing correlation plot
mets_final_plot <- cor(subset(mets_final_1, select=c(5:37)))
round(mets_final_plot, 2)
corrplot(mets_final_plot, method = "color")
#doing PCA
mets_final_1_pca <- prcomp(mets_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(mets_final_1_pca)
fviz_eig(mets_final_1_pca)
fviz_pca_ind(mets_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(mets_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(mets_final_1_pca, scale = 0)
**********************************************************************************************************

#preprocessing for New York Yankees
ny_yank <- read.csv("H:/RIC/New York Yankees/bat.csv")
ny_yank$CS <- ifelse(is.na(ny_yank$CS), mean(ny_yank$CS, na.rm=TRUE), ny_yank$CS)
ny_yank_bat <- subset(ny_yank, select=c(2:17,25:26))
str(ny_yank_bat)

ny_yank_1 <- read.csv("H:/RIC/New York Yankees/pitching.csv")
ny_yank_pitch <- subset(ny_yank_1, select=c(6:17,24))
str(ny_yank_pitch)

ny_yank_2 <- read.csv("H:/RIC/New York Yankees/history.csv")
ny_yank_hist <- subset(ny_yank_2, select=c(15:18))
str(ny_yank_hist)

yankees_final <- cbind(ny_yank_bat,ny_yank_pitch,ny_yank_hist)
yankees_final$Win_pctg <- (yankees_final$W/yankees_final$G)
yankees_final$Loss_pctg <- (yankees_final$L/yankees_final$G)
write.csv(yankees_final, "H:/RIC/New York Yankees/Final.csv")

yankees_final_1 <- read.csv("H:/RIC/New York Yankees/Final.csv", na.strings = "")
yankees_final_1 <- as.data.frame(sapply(yankees_final_1, as.numeric))
#drawing correlation plot
yankees_final_plot <- cor(subset(yankees_final_1, select=c(5:37)))
round(yankees_final_plot, 2)
corrplot(yankees_final_plot, method = "color")
#doing PCA
yankees_final_1_pca <- prcomp(yankees_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(yankees_final_1_pca)
fviz_eig(yankees_final_1_pca)
fviz_pca_ind(yankees_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(yankees_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(yankees_final_1_pca, scale = 0)
**********************************************************************************************************
  
#preprocessing for Oakland Athletics
oak_ath <- read.csv("H:/RIC/Oakland Athletics/bat.csv")
oak_ath$CS <- ifelse(is.na(oak_ath$CS), mean(oak_ath$CS, na.rm=TRUE), oak_ath$CS)
oak_ath_bat <- subset(oak_ath, select=c(2:17,25:26))
str(oak_ath_bat)

oak_ath_1 <- read.csv("H:/RIC/Oakland Athletics/pitching.csv")
oak_ath_pitch <- subset(oak_ath_1, select=c(6:17,24))
str(oak_ath_pitch)

oak_ath_2 <- read.csv("H:/RIC/Oakland Athletics/history.csv")
oak_ath_hist <- subset(oak_ath_2, select=c(15:18))
str(oak_ath_hist)

athletics_final <- cbind(oak_ath_bat,oak_ath_pitch,oak_ath_hist)
athletics_final$Win_pctg <- (athletics_final$W/athletics_final$G)
athletics_final$Loss_pctg <- (athletics_final$L/athletics_final$G)
write.csv(athletics_final, "H:/RIC/Oakland Athletics/Final.csv")

athletics_final_1 <- read.csv("H:/RIC/Oakland Athletics/Final.csv", na.strings = "")
athletics_final_1 <- as.data.frame(sapply(athletics_final_1, as.numeric))
#drawing correlation plot
athletics_final_plot <- cor(subset(athletics_final_1, select=c(5:37)))
round(athletics_final_plot, 2)
corrplot(athletics_final_plot, method = "color")
#doing PCA
athletics_final_1_pca <- prcomp(athletics_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(athletics_final_1_pca)
fviz_eig(athletics_final_1_pca)
fviz_pca_ind(athletics_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(athletics_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(athletics_final_1_pca, scale = 0)
**********************************************************************************************************
  
#preprocessing for Philadelphia Phillies
phi_phil <- read.csv("H:/RIC/Philadelphia Phillies/bat.csv")
phi_phil$CS <- ifelse(is.na(phi_phil$CS), mean(phi_phil$CS, na.rm=TRUE), phi_phil$CS)
phi_phil_bat <- subset(phi_phil, select=c(2:17,25:26))
str(phi_phil_bat)

phi_phil_1 <- read.csv("H:/RIC/Philadelphia Phillies/pitching.csv")
phi_phil_pitch <- subset(phi_phil_1, select=c(6:17,24))
str(phi_phil_pitch)

phi_phil_2 <- read.csv("H:/RIC/Philadelphia Phillies/history.csv")
phi_phil_hist <- subset(phi_phil_2, select=c(15:18))
str(phi_phil_hist)

phillies_final <- cbind(phi_phil_bat,phi_phil_pitch,phi_phil_hist)
phillies_final$Win_pctg <- (phillies_final$W/phillies_final$G)
phillies_final$Loss_pctg <- (phillies_final$L/phillies_final$G)
write.csv(athletics_final, "H:/RIC/Philadelphia Phillies/Final.csv")

phillies_final_1 <- read.csv("H:/RIC/Philadelphia Phillies/Final.csv", na.strings = "")
phillies_final_1 <- as.data.frame(sapply(phillies_final_1, as.numeric))
#drawing correlation plot
phillies_final_plot <- cor(subset(phillies_final_1, select=c(5:37)))
round(phillies_final_plot, 2)
corrplot(phillies_final_plot, method = "color")
#doing PCA
phillies_final_1_pca <- prcomp(phillies_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(phillies_final_1_pca)
fviz_eig(phillies_final_1_pca)
fviz_pca_ind(phillies_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(phillies_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(phillies_final_1_pca, scale = 0)
**********************************************************************************************************

# PreProcessing for Pittsburgh Pirates
pit_pir <- read.csv("H:/RIC/Pittsburgh Pirates/bat.csv")
pit_pir$CS <- ifelse(is.na(pit_pir$CS), mean(pit_pir$CS, na.rm=TRUE), pit_pir$CS)
pit_pir_bat <- subset(pit_pir, select=c(2:17,25:26))
str(pit_pir_bat)

pit_pir_1 <- read.csv("H:/RIC/Pittsburgh Pirates/pitching.csv")
pit_pir_pitch <- subset(pit_pir_1, select=c(6:17,24))
str(pit_pir_pitch)

pit_pir_2 <- read.csv("H:/RIC/Pittsburgh Pirates/history.csv")
pit_pir_hist <- subset(pit_pir_2, select=c(15:18))
str(pit_pir_hist)

pirates_final <- cbind(pit_pir_bat,pit_pir_pitch,pit_pir_hist)
pirates_final$Win_pctg <- (pirates_final$W/pirates_final$G)
pirates_final$Loss_pctg <- (pirates_final$L/pirates_final$G)
write.csv(pirates_final, "H:/RIC/Pittsburgh Pirates/Final.csv")

pirates_final_1 <- read.csv("H:/RIC/Pittsburgh Pirates/Final.csv", na.strings = "")
pirates_final_1 <- as.data.frame(sapply(pirates_final_1, as.numeric))
#drawing correlation plot
pirates_final_plot <- cor(subset(pirates_final_1, select=c(5:37)))
round(pirates_final_plot, 2)
corrplot(pirates_final_plot, method = "color")
#doing PCA
pirates_final_1_pca <- prcomp(pirates_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(pirates_final_1_pca)
fviz_eig(pitrates_final_1_pca)
fviz_pca_ind(pirates_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(pirates_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(pirates_final_1_pca, scale = 0)
**********************************************************************************************************
  
#Preprocessing for San Diego Padres
sd_pad <- read.csv("H:/RIC/San Diego Padres/bat.csv")
sd_pad_bat <- subset(sd_pad, select=c(2:17,25:26))
str(sd_pad_bat)

sd_pad_1 <- read.csv("H:/RIC/San Diego Padres/pitching.csv")
sd_pad_pitch <- subset(sd_pad_1, select=c(6:17,24))
str(sd_pad_pitch)

sd_pad_2 <- read.csv("H:/RIC/San Diego Padres/history.csv")
sd_pad_hist <- subset(sd_pad_2, select=c(15:18))
str(sd_pad_hist)

padres_final <- cbind(sd_pad_bat,sd_pad_pitch,sd_pad_hist)
padres_final$Win_pctg <- (padres_final$W/padres_final$G)
padres_final$Loss_pctg <- (padres_final$L/padres_final$G)
write.csv(padres_final, "H:/RIC/San Diego Padres/Final.csv")

padres_final_1 <- read.csv("H:/RIC/San Diego Padres/Final.csv", na.strings = "")
padres_final_1 <- as.data.frame(sapply(padres_final_1, as.numeric))
#drawing correlation plot
padres_final_plot <- cor(subset(padres_final_1, select=c(5:37)))
round(padres_final_plot, 2)
corrplot(padres_final_plot, method = "color")
#doing PCA
padres_final_1_pca <- prcomp(padres_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(padres_final_1_pca)
fviz_eig(padres_final_1_pca)
fviz_pca_ind(padres_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(padres_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(padres_final_1_pca, scale = 0)
**********************************************************************************************************
  
#Preprocessing for San Fransisco Giants
sf_gia <- read.csv("H:/RIC/San Fransisco Giants/bat.csv")
sf_gia$CS <- ifelse(is.na(sf_gia$CS), mean(sf_gia$CS, na.rm=TRUE), sf_gia$CS)
sf_gia_bat <- subset(sf_gia, select=c(2:17,25:26))
str(sf_gia_bat)

sf_gia_1 <- read.csv("H:/RIC/San Fransisco Giants/pitching.csv")
sf_gia_pitch <- subset(sf_gia_1, select=c(6:17,24))
str(sf_gia_pitch)

sf_gia_2 <- read.csv("H:/RIC/San Fransisco Giants/history.csv")
sf_gia$Attendance.G <- ifelse(is.na(sf_gia$Attendance.G), mean(sf_gia$Attendance.G, na.rm=TRUE), sf_gia$Attendance.G)
sf_gia_hist <- subset(sf_gia_2, select=c(15:18))
str(sf_gia_hist)

giants_final <- cbind(sf_gia_bat,sf_gia_pitch,sf_gia_hist)
giants_final$Win_pctg <- (giants_final$W/giants_final$G)
giants_final$Loss_pctg <- (giants_final$L/giants_final$G)
write.csv(giants_final, "H:/RIC/San Fransisco Giants/Final.csv")

giants_final_1 <- read.csv("H:/RIC/San Fransisco Giants/Final.csv", na.strings = "")
giants_final_1 <- as.data.frame(sapply(giants_final_1, as.numeric))
#drawing correlation plot
giants_final_plot <- cor(subset(giants_final_1, select=c(5:37)))
round(giants_final_plot, 2)
corrplot(giants_final_plot, method = "color")
#doing PCA
giants_final_1_pca <- prcomp(giants_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(giants_final_1_pca)
fviz_eig(giants_final_1_pca)
fviz_pca_ind(giants_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(giants_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(giants_final_1_pca, scale = 0)
**********************************************************************************************************

#Preprocessing for Seattle Mariners
sea_mar <- read.csv("H:/RIC/Seattle Mariners/bat.csv")
sea_mar_bat <- subset(sea_mar, select=c(2:17,25:26))
str(sea_mar_bat)

sea_mar_1 <- read.csv("H:/RIC/Seattle Mariners/pitching.csv")
sea_mar_pitch <- subset(sea_mar_1, select=c(6:17,24))
str(sea_mar_pitch)

sea_mar_2 <- read.csv("H:/RIC/Seattle Mariners/history.csv")
sea_mar_hist <- subset(sea_mar_2, select=c(15:18))
str(sea_mar_hist)

mariners_final <- cbind(sea_mar_bat,sea_mar_pitch,sea_mar_hist)
mariners_final$Win_pctg <- (mariners_final$W/mariners_final$G)
mariners_final$Loss_pctg <- (mariners_final$L/mariners_final$G)
write.csv(mariners_final, "H:/RIC/Seattle Mariners/Final.csv")

mariners_final_1 <- read.csv("H:/RIC/Seattle Mariners/Final.csv", na.strings = "")
mariners_final_1 <- as.data.frame(sapply(mariners_final_1, as.numeric))
#drawing correlation plot
mariners_final_plot <- cor(subset(mariners_final_1, select=c(5:37)))
round(mariners_final_plot, 2)
corrplot(mariners_final_plot, method = "color")
#doing PCA
mariners_final_1_pca <- prcomp(mariners_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(mariners_final_1_pca)
fviz_eig(mariners_final_1_pca)
fviz_pca_ind(mariners_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(mariners_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(mariners_final_1_pca, scale = 0)
******************************************************************************************************
  
#Preprocessing for St. Louis Cardinals
st_card <- read.csv("H:/RIC/St. Louis Cardinals/bat.csv")
st_card$CS <- ifelse(is.na(st_card$CS), mean(st_card$CS, na.rm=TRUE), st_card$CS)
st_card_bat <- subset(st_card, select=c(2:17,25:26))
st_card_bat <- as.data.frame(sapply(st_card_bat, as.numeric)) 
str(st_card_bat)

st_card_1 <- read.csv("H:/RIC/St. Louis Cardinals/pitching.csv")
st_card_pitch <- subset(st_card_1, select=c(6:17,24))
st_card_pitch <- as.data.frame(sapply(st_card_pitch, as.numeric)) 
str(st_card_pitch)

st_card_2 <- read.csv("H:/RIC/St. Louis Cardinals/history.csv")
st_card_2$Attendance.G <- ifelse(is.na(st_card_2$Attendance.G), mean(st_card_2$Attendance.G, na.rm=TRUE), st_card_2$Attendance.G)
st_card_hist <- subset(st_card_2, select=c(15:18))
st_card_hist <- as.data.frame(sapply(st_card_hist, as.numeric)) 
str(st_card_hist)

cardinals_final <- cbind(st_card_bat,st_card_pitch,st_card_hist)
cardinals_final$Win_pctg <- (cardinals_final$W/cardinals_final$G)
cardinals_final$Loss_pctg <- (cardinals_final$L/cardinals_final$G)
write.csv(cardinals_final, "H:/RIC/St. Louis Cardinals/Final.csv")

cardinals_final_1 <- read.csv("H:/RIC/St. Louis Cardinals/Final.csv", na.strings = "")
cardinals_final_1 <- as.data.frame(sapply(cardinals_final_1, as.numeric))
#drawing correlation plot
cardinals_final_plot <- cor(subset(cardinals_final_1, select=c(5:37)))
round(cardinals_final_plot, 2)
corrplot(cardinals_final_plot, method = "color")
#doing PCA
cardinals_final_1_pca <- prcomp(cardinals_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(cardinals_final_1_pca)
fviz_eig(cardinals_final_1_pca)
fviz_pca_ind(cardinals_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(cardinals_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(cardinals_final_1_pca, scale = 0)
******************************************************************************************************

#Preprocessing for Tampa Bay Rays
tb_rays <- read.csv("H:/RIC/Tampa Bay Rays/bat.csv")
tb_rays_bat <- subset(tb_rays, select=c(2:17,25:26))
str(tb_rays_bat)

tb_rays_1 <- read.csv("H:/RIC/Tampa Bay Rays/pitching.csv")
tb_rays_pitch <- subset(tb_rays_1, select=c(6:17,24))
str(tb_rays_pitch)

tb_rays_2 <- read.csv("H:/RIC/Tampa Bay Rays/history.csv")
tb_rays_hist <- subset(tb_rays_2, select=c(15:18))
str(tb_rays_hist)

rays_final <- cbind(tb_rays_bat,tb_rays_pitch,tb_rays_hist)
rays_final$Win_pctg <- (rays_final$W/rays_final$G)
rays_final$Loss_pctg <- (rays_final$L/rays_final$G)
write.csv(rays_final, "H:/RIC/Tampa Bay Rays/Final.csv")

rays_final_1 <- read.csv("H:/RIC/Tampa Bay Rays/Final.csv", na.strings = "")
rays_final_1 <- as.data.frame(sapply(rays_final_1, as.numeric))
#drawing correlation plot
rays_final_plot <- cor(subset(rays_final_1, select=c(5:37)))
round(rays_final_plot, 2)
corrplot(rays_final_plot, method = "color")
#doing PCA
rays_final_1_pca <- prcomp(rays_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(rays_final_1_pca)
fviz_eig(rays_final_1_pca)
fviz_pca_ind(rays_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(rays_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(rays_final_1_pca, scale = 0)
******************************************************************************************************

#Preprocessing for Texas Rangers
tex_ran <- read.csv("H:/RIC/Texas Rangers/bat.csv")
tex_ran_bat <- subset(tex_ran, select=c(2:17,25:26))
str(tex_ran_bat)

tex_ran_1 <- read.csv("H:/RIC/Texas Rangers/pitching.csv")
tex_ran_pitch <- subset(tex_ran_1, select=c(6:17,24))
str(tex_ran_pitch)

tex_ran_2 <- read.csv("H:/RIC/Texas Rangers/history.csv")
tex_ran_hist <- subset(tex_ran_2, select=c(15:18))
str(tex_ran_hist)

rangers_final <- cbind(tex_ran_bat,tex_ran_pitch,tex_ran_hist)
rangers_final$Win_pctg <- (rangers_final$W/rangers_final$G)
rangers_final$Loss_pctg <- (rangers_final$L/rangers_final$G)
write.csv(rangers_final, "H:/RIC/Texas Rangers/Final.csv")

rangers_final_1 <- read.csv("H:/RIC/Texas Rangers/Final.csv", na.strings = "")
rangers_final_1 <- as.data.frame(sapply(rangers_final_1, as.numeric))
#drawing correlation plot
rangers_final_plot <- cor(subset(rangers_final_1, select=c(5:37)))
round(rangers_final_plot, 2)
corrplot(rangers_final_plot, method = "color")
#doing PCA
rangers_final_1_pca <- prcomp(rangers_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(rangers_final_1_pca)
fviz_eig(rangers_final_1_pca)
fviz_pca_ind(rangers_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(rangers_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(rangers_final_1_pca, scale = 0)
******************************************************************************************************
  
#preprocessing for Toronto Blue Jays
tor_bj <- read.csv("H:/RIC/Toronto Blue Jays/bat.csv")
tor_bj_bat <- subset(tor_bj, select=c(2:17,25:26))
str(tor_bj_bat)

tor_bj_1 <- read.csv("H:/RIC/Toronto Blue Jays/pitching.csv")
tor_bj_pitch <- subset(tor_bj_1, select=c(6:17,24))
str(tor_bj_pitch)

tor_bj_2 <- read.csv("H:/RIC/Toronto Blue Jays/history.csv")
tor_bj_hist <- subset(tor_bj_2, select=c(15:18))
str(tor_bj_hist)

bluejays_final <- cbind(tor_bj_bat,tor_bj_pitch,tor_bj_hist)
bluejays_final$Win_pctg <- (bluejays_final$W/bluejays_final$G)
bluejays_final$Loss_pctg <- (bluejays_final$L/bluejays_final$G)
write.csv(bluejays_final, "H:/RIC/Toronto Blue Jays/Final.csv")

bluejays_final_1 <- read.csv("H:/RIC/Toronto Blue Jays/Final.csv", na.strings = "")
bluejays_final_1 <- as.data.frame(sapply(bluejays_final_1, as.numeric))
#drawing correlation plot
bluejays_final_plot <- cor(subset(bluejays_final_1, select=c(5:37)))
round(bluejays_final_plot, 2)
highlycorrelated <- findcorre
corrplot(bluejays_final_plot, method = "color")
#doing PCA
bluejays_final_1_pca <- prcomp(bluejays_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(bluejays_final_1_pca)
fviz_eig(bluejays_final_1_pca)
fviz_pca_ind(bluejays_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(bluejays_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(bluejays_final_1_pca, scale = 0)
******************************************************************************************************

#Preprocessing for Washington Nationals
wsh_nat <- read.csv("H:/RIC/Washington Nationals/bat.csv")
wsh_nat_bat <- subset(wsh_nat, select=c(2:17,25:26))
str(wsh_nat_bat)

wsh_nat_1 <- read.csv("H:/RIC/Washington Nationals/pitching.csv")
wsh_nat_pitch <- subset(wsh_nat_1, select=c(6:17,24))
str(wsh_nat_pitch)

wsh_nat_2 <- read.csv("H:/RIC/Washington Nationals/history.csv")
wsh_nat_hist <- subset(wsh_nat_2, select=c(15:18))
str(wsh_nat_hist)

nationals_final <- cbind(wsh_nat_bat,wsh_nat_pitch,wsh_nat_hist)
nationals_final$Win_pctg <- (nationals_final$W/nationals_final$G)
nationals_final$Loss_pctg <- (nationals_final$L/nationals_final$G)
write.csv(nationals_final, "H:/RIC/Washington Nationals/Final.csv")

nationals_final_1 <- read.csv("H:/RIC/Washington Nationals/Final.csv", na.strings = "")
nationals_final_1 <- as.data.frame(sapply(nationals_final_1, as.numeric))
#drawing correlation plot
nationals_final_plot <- cor(subset(nationals_final_1, select=c(5:37)))
round(bluejays_final_plot, 2)
corrplot(nationals_final_plot, method = "color")
#doing PCA
nationals_final_1_pca <- prcomp(nationals_final_1[,c(5:37)], center = TRUE, scale. = FALSE)
summary(nationals_final_1_pca)
fviz_eig(nationals_final_1_pca)
fviz_pca_ind(nationals_final_1_pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
fviz_pca_var(nationals_final_1_pca,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = "TRUE")
biplot(nationals_final_1_pca, scale = 0)
