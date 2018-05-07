#comparing logistic regression with decision tree using ROC under R.
#v2 added random forest.
#clear memory
rm(list=ls())
library(tree);library(ROCR)
library(caret) #this is for varImp(model_fit) for standardize coefficient
library(stats);library(MASS)
library(randomForest)

data01 <- read.csv(paste("H:/01_self study_flash drive/AAAschulich",
                        "/aaa_ma/time series/R learning/r_reference/R_exercise/model compare",
                        "/develop.csv",sep=""))
#change col names to lower case
names(data01) <- tolower(names(data01))

#convert category var to factor for logistic regression
attach(data01)
data01$dda <- factor(dda)
data01$nsf <- factor(nsf)
data01$sav <- factor(sav)
data01$ira <- factor(ira)
data01$loc <- factor(loc)
data01$mtg <- factor(mtg)
#data01$ins <- factor(ins)

#check predictor variable type
str(data01) 
#check missing values
summary(data01)

#imputation missing values for age, "<<-" assign as a global var
imput <- function(data_try)
{
  temp <- is.na(data_try)
  temp2 <<- which(temp==T)
  value <<- median(data_try,na.rm=T)
}

imput(data01$age)
data01$age[temp2] <- value


data02 <- subset(data01,select=c("ins","dda","ddabal","depamt","cashbk","checks",
         "nsf","phone","sav","savbal","ira","loc","mtg","income","age","crscore"))
#select rows without missing values. 
data03<- data02[complete.cases(data02),]
detach(data01)
attach(data03)
set.seed(123)
train_vec <- sample(1:(0.7*length(data03$age)),replace=F)
data03_train <- data03[train_vec,]
data03_test <- data03[-train_vec,]
logi_all <- glm(ins~dda+ddabal+depamt+cashbk+checks+
                nsf+phone+sav+savbal+ira+loc+mtg+income
                +age+crscore, data=data03_train,family=binomial)

#logi_sas_book <- glm(ins~dda+ddabal+dep+depamt+cashbk+checks+
#                res, data=data03,  family=binomial)
summary(logi_all)

#feature selection: AIC criteria

#logi_all_back <- stepAIC(logi_all,direction="backward")
#logi_sas_book_forward <- stepAIC(logi_sas_book,direction="forward")
logi_all_stepwise <- stepAIC(logi_all,direction="both")

summary(logi_all_stepwise)
logi_all2 <- glm(ins~dda+ddabal+depamt+cashbk+checks+phone+sav+savbal+ira+mtg,
                 data=data03_train,family=binomial)
summary(logi_all2)
logi_all2_test <-predict(logi_all2,data03_test,type="response")
#check overall fitness: 1-pchisq(null deviance-residual deviance, DF_null-DF_residual)
#This is analogous to the global F test for the overall significance of the model that 
#comes automatically when we run the lm() command. This is testing the null hypothesis 
#that the model is no better (in terms of likelihood) than a model fit with only the 
#intercept term, i.e. that all beta terms are 0.
1-pchisq(logi_all2$null.deviance-logi_all2$deviance,
         logi_all2$df.null-logi_all2$df.residual)
#result is 0, reject the null hyp at 0.05 level
#means model is significant!

#confusion matrix
table(ActualValue=data03_test$ins,PredictedValue=logi_all2_test>0.5)
varImp(logi_all2)

#decision tree
tree_model <- tree(ins~dda+ddabal+depamt+cashbk+checks+phone+sav+savbal+ira
                   +mtg,data03_train)
plot(tree_model,col="blue",lwd=3)
text(tree_model,pretty=0,cex=1.9)

tree_pred <- predict(tree_model,data03_test)
mean((tree_pred-data03_test$ins)^2)

cv_tree <- cv.tree(tree_model) 
names(cv_tree)

plot(cv_tree$size,cv_tree$dev,type="o",lty=2,
     xlab="Tree Size",ylab="MSE",col="blue",lwd=2,cex=1.7,
     cex.axis=1.5,cex.lab=1.5,pch=19)
pruned_model <- prune.tree(tree_model,best=5)
plot(pruned_model)
text(pruned_model)
data03_test$score=NA
detach(data03)
attach(data03_test)
data03_test$score[savbal<1532 & dda==0]=0.503
data03_test$score[savbal<1532&dda==1&ddabal<1930]=0.161
data03_test$score[savbal<1532&dda==1&ddabal>=1930]=0.442
data03_test$score[savbal>=1532&savbal<8587]=0.555
data03_test$score[savbal>=1532&savbal>=8587]=0.768
detach(data03_test)

#random forest
model_rf <- randomForest(as.factor(ins)~dda+ddabal+depamt+cashbk+checks+
                           nsf+phone+sav+savbal+ira+loc+mtg+income
                         +age+crscore,data=data03_train,importance=T)
model_rf

#check how many var randomly chosen is optimal for min out of bag
#errors. This function runs very slowly (5min)!
list <- rep(0,15)
for (var_num in 1:10)
{
  rf_check <- randomForest(as.factor(ins)~dda+ddabal+depamt+cashbk+
              checks+nsf+phone+sav+savbal+ira+loc+mtg+income
              +age+crscore,mtry=var_num,data=data03_train,importance=T)
  list[var_num] <- mean(rf_check$err.rate[,1])
  
}
plot(x=c(1:10),y=list[1:10],lty=2,type="o",lwd=2,col="blue",
     xlab="Number of Predictors at Each Split",
     ylab="Out Of Bag Error Rate",cex=1.3,main="Optimal No. of Predictors",
     pch=19,cex.axis=1.2)

model_rf2 <- randomForest(as.factor(ins)~dda+ddabal+depamt+cashbk+checks+
                           nsf+phone+sav+savbal+ira+loc+mtg+income
                         +age+crscore,data=data03_train,importance=T,
                         mtry=3)
model_rf2

#var importance plot
varImpPlot(model_rf2,sort=T,main="Variable Importance",n.var=5,col="blue")
model_rf_test <- predict(model_rf2,data03_test,type="vote")

#ROC curve 
#ROC for tree
roc_pred_tree <- ROCR::prediction(data03_test$score,data03_test$ins)
roc_perf_tree <- ROCR::performance(roc_pred_tree,"tpr","fpr")

#ROC for logistic
ROCRpred=ROCR::prediction(logi_all2_test,data03_test$ins)
ROCRpref=ROCR::performance(ROCRpred,"tpr","fpr")

#ROC for randomforest
roc_pred_rf <- ROCR::prediction(data.frame(model_rf_test)[,2],
                     data03_test$ins)
roc_pred_rf_pef <- ROCR::performance(roc_pred_rf,"tpr","fpr")

#add colorize=T to plot function to add cut-off color
plot(ROCRpref,col="orange",lty=1,lwd=3,main="ROC Curve")

plot(roc_perf_tree,col="green3",lty=2,add=TRUE,lwd=3)

plot(roc_pred_rf_pef,col="tomato",lty=2,add=TRUE,lwd=3,
     print.cutoffs.at=seq(0.1,by=0.1))
abline(a=0,b=1,lwd=2,lty=3,col="dimgray")
auc1 <- performance(ROCRpred,"auc")
auc2 <- performance(roc_pred_tree,"auc")
auc3 <- performance(roc_pred_rf,"auc")
auc_tree <- slot(auc2,"y.values")
auc_logi <- slot(auc1,"y.values")
auc_rf <- slot(auc3,"y.values")
legend(x=-0.02,y=1.01,legend=c("logi (auc: 0.745)","tree (auc: 0.739)"
       ,"rdf (auc: 0.769)"),col=c("orange", "green3","tomato"), 
       lty=1:2,cex=0.8,text.font=4,lwd=3)
