#install.packages("readcsv")
#install.packages("tidyverse")
#install.packages('ROCR')
#install.packages('BOOT')



german_credit = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")
colnames(german_credit)= c("chk_acct", "duration", "credit_his", "purpose", "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", "present_resid", "property", "age", "other_install", "housing", "n_credits", "job", "n_people", "telephone", "foreign", "response")
# orginal response coding 1= good, 2 = bad we need 0 = good, 1 = bad
german_credit$response = german_credit$response - 1
library(MASS)
library(dplyr)
library(ROCR)
library(boot)



colnames(german_credit)

#How many people defaulted in this sample
mean(german_credit$response)
#str allows us to see the variable time
str(german_credit)
summary(german_credit)

#We see all variables are int, but we know that SEX, EDUCATION, MARRIAGE are categorical, we
#convert them to factor.



german_credit$credit_his<- as.factor(german_credit$credit_his)
german_credit$purpose<- as.factor(german_credit$purpose)
german_credit$chk_acct<- as.factor(german_credit$chk_acct)
german_credit$saving_acct<- as.factor(german_credit$saving_acct)
german_credit$present_emp<- as.factor(german_credit$present_emp)
german_credit$sex<- as.factor(german_credit$sex)
german_credit$other_debtor<- as.factor(german_credit$other_debtor)
german_credit$property<- as.factor(german_credit$property)
german_credit$other_install<- as.factor(german_credit$other_install)
german_credit$housing<- as.factor(german_credit$housing)
german_credit$job<- as.factor(german_credit$job)
german_credit$telephone<- as.factor(german_credit$telephone)
german_credit$foreign<- as.factor(german_credit$foreign)


#Randomly split the data to training (80%) and testing (20%) datasets:
index <- sample(nrow(german_credit),nrow(german_credit)*0.80)
credit_train = german_credit[index,]
credit_test = german_credit[-index,]

#Checking for outliers through the boxplot per varoable by column
boxplot(german_credit[,1],notch=T)
boxplot(german_credit[,2],notch=T)
boxplot(german_credit[,3],notch=T)
boxplot(german_credit[,4],notch=T)
boxplot(german_credit[,5],notch=T)
boxplot(german_credit[,6],notch=T)
boxplot(german_credit[,7],notch=T)
boxplot(german_credit[,8],notch=T)
boxplot(german_credit[,9],notch=T)
boxplot(german_credit[,10],notch=T)
boxplot(german_credit[,13],notch=T)
boxplot(german_credit[,16],notch=T)
boxplot(german_credit[,19],notch=T)
boxplot(german_credit[,20],notch=T)


#Model Building for full data set and best variable selection
#regression with all indepoendt variables
credit_glm0<- glm(response~., family = binomial, data = credit_train)
summary(credit_glm0)
#Best model of the 80% of the data 
credit_glm_back <- step(credit_glm0) # backward selection (if you don't specify anything)
summary(credit_glm_back)
credit_glm_back$deviance
#MODEL 1 IS CREDIT_GLM0
#MODEL 2 IS CREDIT_GLM_BACK 
#testing the best model of 80% of the data
AIC(credit_glm_back)
BIC(credit_glm_back)
#Testing all of the training data set
AIC(credit_glm0)
BIC(credit_glm0)
#

#MODEL 2 AUC
pred_resp1 <- predict(credit_glm_back,type="response")
hist(pred_resp1)
#ROC CURVE
pred <- prediction(pred_resp1, credit_train$response)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
#AUC
unlist(slot(performance(pred, "auc"), "y.values"))

table(credit_train$response, (pred_resp1 > 0.5)*1, dnn=c("Truth","Predicted"))
table(credit_train$response, (pred_resp1 > 0.2)*1, dnn=c("Truth","Predicted"))
table(credit_train$response, (pred_resp1 > 0.0001)*1, dnn=c("Truth","Predicte
d"))

#MODEL 1 AUC
pred_resp2 <- predict(credit_glm0,type="response")
hist(pred_resp2)
#ROC CURVE
pred2 <- prediction(pred_resp2, credit_train$response)
perf2 <- performance(pred, "tpr", "fpr")
plot(perf2, colorize=TRUE)
#AUC
unlist(slot(performance(pred2, "auc"), "y.values"))
table(credit_train$response, (pred_resp2 > 0.5)*1, dnn=c("Truth","Predicted"))
table(credit_train$response, (pred_resp2 > 0.2)*1, dnn=c("Truth","Predicted"))
table(credit_train$response, (pred_resp2 > 0.0001)*1, dnn=c("Truth","Predicte
d"))

#cross validation
costfunc <- function(obs, pred.p){
  weight1 <- 5 # define the weight for "true=1 but pred=0" (FN)
  weight0 <- 1 # define the weight for "true=0 but pred=1" (FP)
  pcut <- 1/(1+weight1/weight0)
  c1 <- (obs==1)&(pred.p < pcut) # count for "true=1 but pred=0" (FN)
  c0 <- (obs==0)&(pred.p >= pcut) # count for "true=0 but pred=1" (FP)
  cost <- mean(weight1*c1 + weight0*c0) # misclassification with weight
  return(cost) # you have to return to a value when you write R functions
} # end


credit_glm0<- glm(response~., family=binomial, data=german_credit)
cv_result <- cv.glm(data = german_credit, glmfit=credit_glm0, cost=costfunc, K=5)
cv_result$delta[2]


#AUC TEST DATA
pred_glm0_test<- predict(credit_glm_back, newdata = credit_test, type="response")
#ROC CURVE
pred3 <- prediction(pred_glm0_test, credit_test$response)
perf3 <- performance(pred3, "tpr", "fpr")
plot(perf, colorize=TRUE)
#AUC
unlist(slot(performance(pred, "auc"), "y.values"))


table(credit_test$response, (pred_glm0_test > 0.5)*1, dnn=c("Truth","Predicted"))
table(credit_test$response, (pred_glm0_test > 0.2)*1, dnn=c("Truth","Predicted"))
table(credit_test$response, (pred_glm0_test > 0.0001)*1, dnn=c("Truth","Predicte
d"))

