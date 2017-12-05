#1.predict Total revenue generated on Given Customer dataset(knn regression)
# do necessary preprocessing
# try both standardized and un-standardized


#2.Bin target variable in to two classes based on revenue generated And aplly knn classification(knn classification)


#3.upload R file in GitHUb
rm(list=ls(all=TRUE))
setwd("G:/INSOFE/july 29")
customerdata=read.csv("CustomerData.csv")
library(class)

library(dummies)

library(vegan)



customerdata$CustomerID=NULL
sum(is.na(customerdata))
dummy1 = dummy(customerdata$FavoriteChannelOfTransaction)
dummy2=dummy(customerdata$FavoriteGame)

customerdata2=subset(customerdata,select=-c(FavoriteChannelOfTransaction,FavoriteGame))

customerdata=cbind(dummy1,dummy2,customerdata2)

set.seed(123)

trainrow=sample(1:nrow(customerdata),nrow(customerdata)*0.6)
train_data=customerdata[trainrow,]
test_data=customerdata[-trainrow,]



#table(customerdata$TotalRevenueGenerated)
#table(train_data$TotalRevenueGenerated)
#table(test_data$TotalRevenueGenerated)


#Get the train and test sets excluding the target variable

train_datawithouttarget=subset(train_data,select=-c(TotalRevenueGenerated))
test_datawithouttarget=subset(test_data,select=-c(TotalRevenueGenerated))
library(FNN)
pred=knn.reg(train_datawithouttarget,test_datawithouttarget,train_data$TotalRevenueGenerated,k=3)

library(DMwR)
pred<-data.frame(pred$pred)
result<-regr.eval(test_data$TotalRevenueGenerated, pred)
#k=5
pred2=knn.reg(train_datawithouttarget,test_datawithouttarget,train_data$TotalRevenueGenerated,k=5)
pred2<-data.frame(pred2$pred)
result1<-regr.eval(test_data$TotalRevenueGenerated, pred2)


pred3=knn.reg(train_datawithouttarget,test_datawithouttarget,train_data$TotalRevenueGenerated,k=7)
pred3<-data.frame(pred3$pred)
result2<-regr.eval(test_data$TotalRevenueGenerated, pred3)

#standardizing the dataset
customerdata3=decostand(customerdata,"range") # standardizing the data using 'Range' method


set.seed(123) # to get same data in each time
train = sample(1:nrow(customerdata3),nrow(customerdata3)*0.6)

train_data = customerdata3[train,] 
test_data= customerdata3[-train,] 

train_datawithouttarget = subset(train_data,select=-c(TotalRevenueGenerated))
test_datawithouttarget = subset(test_data,select=-c(TotalRevenueGenerated))



#Deciding the value of k
#** Experiment with various odd values of k; k={1,3,5,7,..}
#k = 3

library(FNN)

pred1<-knn.reg(train_datawithouttarget,test_datawithouttarget,train_data$TotalRevenueGenerated,k=3)

library(DMwR)
pred11<-data.frame(pred$pred)
result<-regr.eval(test_data$TotalRevenueGenerated, pred11)

#try with k=5

pred12<-knn.reg(train_datawithouttarget,test_datawithouttarget,train_data$TotalRevenueGenerated,k=5)
pred12<-data.frame(pred12$pred)
result1<-regr.eval(test_data$TotalRevenueGenerated, pred12)


keep = condense(train_datawithouttarget,train_data$TotalRevenueGenerated)
## [1] 2452


install.packages("gdata")
library(gdata)

pred33=FNN::knn.reg(train_datawithouttarget[keep,], test_datawithouttarget, 
                    train_data$TotalRevenueGenerated[keep],k=5, algorithm = 'kd_tree')

indices = knnx.index(train_datawithouttarget[keep, , drop=FALSE], test_datawithouttarget, k=5)


# If you want the indices of the 5 nearest neighbors to row 20 of test dataset :
print(indices[20, ])

