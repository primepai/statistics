#comparing logistic regression with decision tree using ROC under R.
#clear memory
rm(list=ls())
library(tree);library(ROCR)
library(caret) #this is for varImp(model_fit) for standardize coefficient
library(stats);library(MASS)

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
#that the model is no better (in terms of likelihood) than a model fit with only the intercept 
#term, i.e. that all beta terms are 0.
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

plot(cv_tree$size,cv_tree$dev,type="b",
     xlab="Tree Size",ylab="MSE",col="blue",lwd=2,cex=1.7,
     cex.axis=1.5,cex.lab=1.6)
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

#ROC curve 
roc_pred_tree <- prediction(data03_test$score,data03_test$ins)
roc_perf_tree <- performance(roc_pred_tree,"tpr","fpr")

ROCRpred=prediction(logi_all2_test,data03_test$ins)
ROCRpref=performance(ROCRpred,"tpr","fpr")

#add colorize=T to plot function to add cut-off color
plot(ROCRpref,col="orange",lwd=9,main="ROC Curve",
     print.cutoffs.at=seq(0.1,by=0.1))

plot(roc_perf_tree,col="seagreen3",lty=2,add=TRUE,lwd=6,
     print.cutoffs.at=seq(0.1,by=0.1))
abline(a=0,b=1,lwd=2,lty=3,col="tomato")
auc1 <-performance(ROCRpred,"auc")
auc2 <- performance(roc_pred_tree,"auc")
slot(auc2,"y.values")
slot(auc1,"y.values")
legend(x=-0.02,y=1.01,legend=c("logi (auc: 0.75)","tree (auc: 0.74)"),
       col=c("orange", "seagreen3"), lty=1:2,cex=1.2,
       text.font=6,lwd=3)
