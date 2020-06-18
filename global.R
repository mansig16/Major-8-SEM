rm(list = ls())
#call for packages#
library(mice)
library(ggplot2)
library(caret)
library(ROCR)
library(class)
library(C50)
library(rpart)
library(randomForest)
library(shiny)
library(tree)
library(e1071)

#Import 4 datasets#
Total_data<- read.csv("updated data.csv")
idx <- Total_data == "?"
is.na(Total_data) <- idx

#add attribute name#
colnames(Total_data)<-c("age","sex","cp","trestbps","chol","fbs","restecg","thalach","exang","oldpeak","slope","smoker","diabetes","famhist","target")
#change attribute to numerial"
Total_data$sex<-as.factor(Total_data$sex)
Total_data$cp<-as.factor(Total_data$cp)
Total_data$trestbps<-as.numeric(Total_data$trestbps)
Total_data$chol<-as.numeric(Total_data$chol)
Total_data$fbs<-as.factor(Total_data$fbs)
Total_data$restecg<-as.factor(Total_data$restecg)
Total_data$thalach<-as.numeric(Total_data$thalach)
Total_data$exang<-as.factor(Total_data$exang)
Total_data$oldpeak<-as.numeric(Total_data$oldpeak)
Total_data$slope<-as.factor(Total_data$slope)
Total_data$smoker<-as.factor(Total_data$smoker)
Total_data$famhist<-as.factor(Total_data$famhist)
Total_data$diabetes<-as.factor(Total_data$diabetes)
Total_data$target<-as.factor(Total_data$target)

#Turn the num into binary level0=N(egative),others=P(ositive)#
ds <- as.data.frame(apply(Total_data, 2, as.numeric))
ds$target[ds$target > 0] <- 1
ds$target <- factor(ds$target, levels = c(0,1), labels = c("negative", "positive"))

Total_data_clean<-ds[,-c(12,13)]
percentmiss=function(x){
  sum(is.na(x))/length(x)*100
}
replace=mice(Total_data_clean)
Final_data=complete(replace)

#Split the dataset
set.seed(323)
inTrain<-createDataPartition(y=Final_data$target,p=0.7,list=F)
training<-Final_data[inTrain,]
testing<-Final_data[-inTrain,]
dim(training);dim(testing)

#classifier#
model_rf<-train(target~.,data=training,method="rf",trControl=trainControl(method="cv"),number=10)
model_glm<-train(target~.,data=training,method="glm")
model_c50<-C5.0(target~.,data=training,rules=T,trControl=trainControl(method="cv"),number=10)

model_knn <- train(target ~., method = "knn", data = training,tuneLength = 10,  tuneGrid=data.frame(k=1:5),
                   trControl = trainControl(
                     method = "cv"))
# accuracy improved after increase the k and preProcess "pca" (signal extraction (11), centered (11), scaled (11) )
model_knn <- train(target ~., method = "knn", preProcess="pca",data = training,tuneLength = 10,  tuneGrid=data.frame(k=5:25),
                   trControl = trainControl(
                     method = "cv"))

#testing data set prediction

pred_rf_p<-predict(model_rf,newdata=testing,method="class",type="prob")
pred_rf_r<-predict(model_rf,newdata = testing,method="class",type="raw")
pred_glm_p<-predict(model_glm,newdata=testing,type="prob")
pred_glm_r<-predict(model_glm,newdata=testing,type="raw")
pred_c50_p<-predict(model_c50,newdata=testing,type="prob")
pred_c50_c<-predict(model_c50,newdata = testing,type="class")
pred_knn_p<-predict(model_knn,newdata=testing,type="prob")
pred_knn_r<-predict(model_knn,newdata=testing,type="raw")

#plot(ROC)
pred_roc_rf<-prediction(pred_rf_p$positive,testing$target)
perf_roc_rf<-performance(pred_roc_rf,"tpr", "fpr")
plot(perf_roc_rf,col=2,main="ROC curves comparing classification performance of four machine learning models")
auc_rf<- performance(pred_roc_rf,"auc")
auc_rf

pred_roc_glm<-prediction(pred_glm_p$positive,testing$target)
perf_roc_glm<-performance(pred_roc_glm,"tpr", "fpr")
plot(perf_roc_glm,add=T,col=3)
auc_glm<- performance(pred_roc_glm,"auc")
auc_glm

pred_roc_c50<-prediction(as.data.frame(pred_c50_p)$positive,testing$target)
perf_roc_c50<-performance(pred_roc_c50,"tpr", "fpr")
plot(perf_roc_c50,add=T,col=3)
#plot(perf_roc_c50,col=2,main="C5.0")
auc_c50<- performance(pred_roc_c50,"auc")
auc_c50


pred_roc_knn<-prediction(pred_knn_p$positive,testing$target)
perf_roc_knn<-performance(pred_roc_knn,"tpr", "fpr")
plot(perf_roc_knn,col=5,add=T)
#plot(perf_roc_knn,col=2,main="RF")
auc_knn<- performance(pred_roc_knn,"auc")
auc_knn

#legend of the ROC table
legend(0.1, 0.3, c('knn', 'c5.0','GLM','rf'), 2:5,seg.len = 0.01,text.width=0.05,horiz = T)

#Evaluation metrix(Recall...)
Evaluation_outuput<- function(x,y){
  TP  <- sum(x == "positive" & y== "positive")
  FP   <- sum(x == "negative" & y== "positive")
  TN  <- sum(x == "negative" & y == "negative")
  FN   <- sum(x == "positive" & y == "negative")
  Accuracy<-(TP+TN)/(TP+FP+TN+FN)
  Precision<-TP/(TP+FP)
  Recall_Sensitivity<-TP/(TP+FN)
  F1<-2*Precision*Recall_Sensitivity/(Precision+Recall_Sensitivity)
  row.names <- c("Prediction_T", "Prediction_F" )
  col.names <- c("Test_T", "Test_F")
  Outcome_Matrix<-cbind(Outcome = row.names, as.data.frame(matrix( 
    c(TP, FN, FP, TP) ,
    nrow = 2, ncol = 2,dimnames = list(row.names, col.names))))
  cat("Accuracy:",Accuracy)
  cat("Precision:",Precision)
  cat("Recall:",Recall_Sensitivity)
  cat("F:",F1)
  print(Outcome_Matrix)
}





