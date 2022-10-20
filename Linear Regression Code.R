#install.packages("readcsv")
credit_data <- read.csv(file = "https://xiaoruizhu.github.io/Data-Mining-R/lecture/data/credit_default.csv", header=T)
colnames(credit_data)

mean(credit_data$default.payment.next.month)

#need to install the package before using the library
#install.packages("dplyr")
library(dplyr)

#rename the "default.payment.next.month" to "default" 
credit_data<- rename(credit_data, default=default.payment.next.month)

# structure - see variable type
str(credit_data)
# summary statistics
summary(credit_data) 

credit_data$SEX<- as.factor(credit_data$SEX)
credit_data$EDUCATION<- as.factor(credit_data$EDUCATION)
credit_data$MARRIAGE<- as.factor(credit_data$MARRIAGE)

#Randomly split the data to training and teasting data
index <- sample(nrow(credit_data),nrow(credit_data)*0.80) 
credit_train = credit_data[index,] 
credit_test = credit_data[-index,]

#family=binomial is important
credit_glm0<- glm(default~., family=binomial, data=credit_train) 
summary(credit_glm0)

#3.2 variable selection (should go before the ROC curve)
credit_glm_back <- step(credit_glm0) 
# backward selection (if you don't specify anything) 
summary(credit_glm_back) 
credit_glm_back$deviance 
AIC(credit_glm_back) 
BIC(credit_glm_back)

#using the credit_train
credit_glm_back_BIC <- step(credit_glm0, k=log(nrow(credit_train)))  
summary(credit_glm_back_BIC) 
credit_glm_back_BIC$deviance 
AIC(credit_glm_back_BIC) 
BIC(credit_glm_back_BIC)


index <- sample(nrow(credit_data),nrow(credit_data)*0.80)
credit_train = credit_data[index,]
credit_test = credit_data[-index,]

credit_glm0 <- glm(default~., family=binomial, data=credit_train)
summary(credit_glm0)

#2.2 Prediction
hist(predict(credit_glm0))
pred_resp <- predict(credit_glm0,type="response") 
hist(pred_resp)
#cut-off value is 0.5 to predict
table(credit_train$default, (pred_resp > 0.5)*1, dnn=c("Truth","Predicted"))
#cut-off value 0.2 to predict
table(credit_train$default, (pred_resp > 0.2)*1, dnn=c("Truth","Predicted"))
#this is not a good cut-off value(0.0001), we're losing a lot of good customers
table(credit_train$default, (pred_resp > 0.0001)*1, dnn=c("Truth","Predicted"))


#2.2.2 ROC curve
pred_glm0_train<- predict(credit_glm0, type="response")
#install the ROC curve package
install.packages('ROCR')
library(ROCR) 
pred <- prediction(pred_glm0_train, credit_train$default) 
perf <- performance(pred, "tpr", "fpr") 
plot(perf, colorize=TRUE)
#Get the AUC 
unlist(slot(performance(pred, "auc"), "y.values"))

#Binary Classifivation
pred_glm0_train <- predict(credit_glm0, type="response") 
#0.9, 0.5, 0.2, 0.0001 is the cut-off probability
table(credit_train$default, (pred_glm0_train > 0.9)*1, dnn=c("Truth","Predicted"))
table(credit_train$default, (pred_glm0_train > 0.5)*1, dnn=c("Truth","Predicted"))
table(credit_train$default, (pred_glm0_train > 0.2)*1, dnn=c("Truth","Predicted"))
table(credit_train$default, (pred_glm0_train > 0.0001)*1, dnn=c("Truth","Predicted"))
#credit_train$default -> the truth value
#pred_glm0_train -> the predicted value
#reading the result: truth 0, predicted 0-> right prediction 
#ex1: (truth 0 predicted value 0)you predicted this is the good customer, and this is actually a good customer
#ex2: (truth 1 predicted value 1)you predicted this is a bad customer, and this is actually a bad customer. 
#ex3: (truth 0 predicted value 1)you predicted this is a good customer, however this is a bad customer.
#ex4: (truth 1 predicted value 0)you predicted this is a bad customer, however, this is a good customer.

#Cross validation for logistic regression
pcut <- 0.5 
#Symmetric cost 
cost1 <- function(r, pi, pcut){ 
  mean(((r==0)&(pi>pcut)) | ((r==1)&(pi<pcut)))
} 
#Asymmetric cost 
cost2 <- function(r, pi, pcut){ 
  weight1 <- 2 
  weight0 <- 1 
  c1 <- (r==1)&(pi<pcut) #logical vector - true if actual 1 but predict 0 
  c0 <-(r==0)&(pi>pcut) #logical vector - true if actual 0 but predict 1 
  return(mean(weight1*c1+weight0*c0)) 
}

#same result with the different way 
costfunc  <- function(obs, pred.p){ 
  weight1 <- 5   # define the weight for "true=1 but pred=0" (FN) 
  weight0 <- 1    # define the weight for "true=0 but pred=1" (FP) 
  pcut <- 1/(1+weight1/weight0) 
  c1 <- (obs==1)&(pred.p < pcut)    # count for "true=1 but pred=0"   (FN) 
  c0 <- (obs==0)&(pred.p >= pcut)   # count for "true=0 but pred=1"   (FP) 
  cost <- mean(weight1*c1 + weight0*c0)  # misclassification with weight 
  return(cost) # you have to return to a value when you write R functions 
} # end 

#10 cross validation
credit_data <- read.csv(file = "https://xiaoruizhu.github.io/Data-Mining-R/l
ecture/data/credit_default.csv", header=T) 
library(dplyr) 
credit_data<- rename(credit_data, default=default.payment.next.month) 
credit_data$SEX<- as.factor(credit_data$SEX) 
credit_data$EDUCATION<- as.factor(credit_data$EDUCATION) 
credit_data$MARRIAGE<- as.factor(credit_data$MARRIAGE) 

library(boot) 
credit_glm1<- glm(default~. , family=binomial, data=credit_data);   
cv_result  <- cv.glm(data=credit_data, glmfit=credit_glm1, cost=costfunc, K=
                       10)  
cv_result$delta[2]
