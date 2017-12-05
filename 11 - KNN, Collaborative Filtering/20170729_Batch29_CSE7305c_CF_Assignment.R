# Build a recommendation engine for movies
# Do necessary preprocessing if required and follow the steps given in class room activity


#upload R file in github#install.packages("recommenderlab")
rm(list=ls(all=TRUE))
#Loading Required Packages
library(recommenderlab) 
library(lsa)
setwd("G:/INSOFE/july 29")
movies=read.csv("Movies.csv")
#Simulating the data
set.seed(5643)
c <- matrix(sample(c(as.numeric(0:5), NA), 50,
                   replace=TRUE, prob=c(rep(.4/6,6),.6)), ncol=10,
            dimnames=list(user=paste("u", 1:5, sep=""),
                          item=paste("i", 1:10, sep="")))  
c
sum(is.na(c))  
#Convert User-item Matrix to realRatingMatrix matrix
g <- as(c, "realRatingMatrix")
getRatings(g)
#Normalize the realRating matrix
g_c <- normalize(g)
getRatings(g_c)
(as(g_c, "data.frame"))
#Visualize the ratings matrix
image(g, main = "Raw Ratings")
image(g_c, main = "Normalized Ratings")
g_b <- binarize(g, minRating=4)
image(g_b)
#Create Recommender systems using IBCF & UBCF
getRatings(g)
g1 <- Recommender(g, method = "UBCF") #input is un-normalized  data 
g1
getModel(g1)
#Split the data into train and evaluation sets
e <- evaluationScheme(g, method="split", train=0.7,
                      given=3)
getRatings(getData(e, "train"))
#UBCF
g2 <- Recommender(getData(e, "train"), "UBCF")
g2
#IBCF
g3 <- Recommender(getData(e, "train"), "IBCF")
g3
#Predict recommendations
getRatingMatrix(getData(e, "known"))
#Predict UBCF
p1 <- predict(g2, getData(e, "known"), type="ratings")
p1
as(p1, "list")
#as(p1, "matrix")
getRatingMatrix(g)
getRatingMatrix(getData(e, "known")) #retains 3 rankings from each record of test-set
getRatingMatrix(getData(e, "unknown"))#removes the 3 rankings from the test-set
#Predict IBCF
p2 <- predict(g3, getData(e, "known"), type="ratings")
p2
as(p2, "list")
#as(p2, "matrix")
getRatingMatrix(getData(e, "known"))
getRatingMatrix(getData(e, "unknown"))
#Compare the performance of IBCF and UBCF
error <- rbind(
  calcPredictionAccuracy(p1, getData(e, "unknown")),
  calcPredictionAccuracy(p2, getData(e, "unknown"))
)
rownames(error) <- c("UBCF","IBCF")
error
