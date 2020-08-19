library(dplyr)
library(tibble)
library(plyr)
library(party)
library(rpart)
library(tree)
library(ipred)
library(rpart)
library(randomForest)
setwd("~/Desktop/7995-final ")
data <- read.csv("~/Desktop/7995-final /data.csv", header=TRUE)
#====================Data Cleaning ===========
isOver95 <- function(x){
  thresh_hold = ceiling(length(x) * 0.95)
  c = 0
  for(i in x){
    if(i == 0) {
      c = c + 1
    }
    if(c == thresh_hold) {
      return(TRUE)
    }
  }
  return(FALSE)
}
abondonCols <- apply(data, 2, isOver95)
data <- data[ ,!abondonCols]
#================use====================
use <- read.csv("~/Desktop/7995-final /use.csv")
#=========breeder vs. non-breeder 
df_breeder<-use[,-c(1,3)]
ldf_breeder<-log(df_breeder[,-1])
breeder_name<-df_breeder$breeder
ldf_breeder<-cbind(breeder_name,ldf_breeder)
set.seed(1)
#*********Linear Model 
#*********Decision Tree
tree.breeder = tree(as.factor(ldf_breeder$breeder_name)~., data = ldf_breeder)
plot(tree.breeder)
text(tree.breeder)
#*********Random Forest****************
# random forests
rf.breeder = randomForest(ldf_breeder[,-1], as.factor(ldf_breeder$breeder_name), ntree = 500, mtry = 5, nodesize = 5, importance = TRUE)
varImpPlot(rf.breeder,type=2)

#=========female and male 
df_sex<-use[,-c(1,2)]
ldf_sex<-log(df_sex[,-1])
sex_name<-df_sex$female
ldf_sex<-cbind(sex_name,ldf_sex)
set.seed(1)
#*********Linear Model 
#*********Decision Tree
tree.sex= tree(as.factor(ldf_sex$sex_name)~., data = ldf_sex)
plot(tree.sex)
text(tree.sex)
#*********Random Forest****************
# random forests
rf.sex = randomForest(ldf_sex[,-1], as.factor(ldf_sex$sex_name), ntree = 500, mtry = 5, nodesize = 5, importance = TRUE)
varImpPlot(rf.sex,type=2)

