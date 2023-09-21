data <- read.csv("SelectedVariables1.csv", header=TRUE)
str(data)

data$Y <- as.factor(data$Y)
data$tourney_name <- as.factor(data$tourney_name)
data$surface <- as.factor(data$surface)


install.packages("rpart")
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")
library(dplyr)
library(tidyr)
library(glmnet)

set.seed(1234)
dt <- sort (sample(nrow(data), nrow(data) *.7))
train <- data[dt,]
test <- data[-dt,]
rtree <- rpart(Y ~ ., data, method = "class")
rpart.plot(rtree)

rtree <- rpart(Y ~ ., train, method = "class")
rpart.plot(rtree)




##Finding training error for the gini model
tree_gini<- rpart(Y~.,data=train, parms=list(split="gini"))
plot(tree_gini,main="Gini Gain Tree")
text(tree_gini)

## predictions for the test set
train_pred_gini<-predict(rtree, train, type="class")

#misclassification matrix
train_misclass<-table(Actual=train$Y, Predicted=train_pred_gini)
train_misclass

#the error
train_err<-(train_misclass[1,2]+train_misclass[2,1])/sum(train_misclass)
train_err

printcp(tree_gini)

# plotting the complexity vs error
plotcp(tree_gini,upper="splits")

################################ CI for accuracy ##########################
test_pred_gini<-predict(rtree, test, type="class")

#misclassification matrix
test_misclass_gini<-table(Actual=test$Y, Predicted=test_pred_gini)
test_misclass_gini

#testing accuracy
test_acc<-(test_misclass_gini[1,1]+test_misclass_gini[2,2])/sum(test_misclass_gini)
test_acc

#predictions for the test set 
train_pred_pre<-predict(tree_preprune,data_train)

#get the misclassification matrix
train_misclass_pre<-table(Actual=data_train$Class,Predicted=train_pred_pre)
train_misclass_pre

#get the error
train_err_pre<-(train_misclass_pre[1,2]+train_misclass_pre[2,1])/sum(train_misclass_pre)
train_err_pre

tree_preprune<-rpart(Y~., data=train, parms=list(split="gini"),
                     control = rpart.control(minbucket = 20))






N<-nrow(test)
numerator_p1<-2*N*test_acc+1.96^2
numerator_p2<-1.96*sqrt(1.96^2+4*N*test_acc-4*N*test_acc^2)
denominator<-2*(N+1.96^2)
LB<-(numerator_p1-numerator_p2)/denominator
UB<-(numerator_p1+numerator_p2)/denominator
c(LB,UB)

tree_entropy<-rpart(Class~.,data=train[,-c(1,5,6,9,10,13,15:24)],
                    parms=list(split="information"))
plot(tree_entropy,main="Entropy Based Tree")
text(tree_entropy)

test_pred_ent<-predict(tree_entropy,test,type="class")

#misclassification matrix
test_misclass_ent<-table(Actual=test$Y,Predicted=test_pred_ent)
test_misclass_ent

#find the testing accuracy
test_acc_ent<-(test_misclass_ent[1,1]+test_misclass_ent[2,2])/sum(test_misclass_ent)
test_acc_ent


#95% CI for this accuracy based on the test set
N<-nrow(test)
numerator_p1<-2*N*test_acc_ent+1.96^2
numerator_p2<-1.96*sqrt(1.96^2+4*N*test_acc_ent-4*N*test_acc_ent^2)
denominator<-2*(N+1.96^2)
LB_ent<-(numerator_p1-numerator_p2)/denominator
UB_ent<-(numerator_p1+numerator_p2)/denominator
c(LB_ent,UB_ent)

















