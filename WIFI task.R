#libraries####
install.packages("scatterplot3d")
library(caret)
library(dplyr)
library(RMySQL)
#import data####
wifi<- read.csv(file.choose(),header=TRUE)
summary(wifi)
View(wifi)
#sampling the data####
newifi<- wifi[wifi[["BUILDINGID"]]==0,]
newifi
#data exploration####
droplist<- c("USERID","PHONEID","TIMESTAMP")
newifi<- select(newifi,-droplist)

new1 <- nearZeroVar(newifi,
                   saveMetrics = TRUE)
head(new1)
x <- 0.0100316 
dim(new1[new1$percentUnique > x,])
colz <- c(rownames(new1[new1$percentUnique > x,]))
new_newifi <-
  as.data.frame(newifi[,colz])

remove(x)
remove(colz)
remove(new1)
dim(new_newifi)
#building 0####
set.seed(321)
intrain1<- createDataPartition(y=new_newifi$SPACEID,p=0.70,list=FALSE)
trctrl <- trainControl(method = "cv", number = 10)
train1<-newifi[intrain1,]
test1<-newifi[-intrain1,]
m1 <- train(SPACEID ~., data = train1,
            method = "knn",
            trControl=trctrl,
            preProcess = c("center", "scale"))
m1

#building 1####
newifi1<- wifi[wifi[["BUILDINGID"]]==1,]
newifi1
newifi1<- select(newifi1,-droplist)
newifi1
new2 <- nearZeroVar(newifi1,
                    saveMetrics = TRUE)
x <- 0.0100316 
dim(new2[new2$percentUnique > x,])
colz <- c(rownames(new2[new2$percentUnique > x,]))
new_newifi1 <-as.data.frame(newifi1[,colz])

remove(x)
remove(colz)
remove(new2)
dim(new_newifi1)
intrain2<- createDataPartition(y=new_newifi1$SPACEID,p=0.70,list=FALSE)
train2<-new_newifi1[intrain2,]
test2<-new_newifi1[-intrain2,]
m2 <- train(SPACEID ~., data = train2,
            method = "knn",
            trControl=trctrl,
            preProcess = c("center", "scale"))
summary(m2)

#building 2####
newifi2<- wifi[wifi[["BUILDINGID"]]==2,]
newifi2
newifi2<- select(newifi2,-droplist)
newifi2
new3 <- nearZeroVar(newifi2,
                    saveMetrics = TRUE)
x <- 0.0100316 
dim(new3[new3$percentUnique > x,])
colz <- c(rownames(new3[new3$percentUnique > x,]))
new_newifi2 <-as.data.frame(newifi2[,colz])
?function(buildNum)
remove(x)
remove(colz)
remove(new3)
dim(new_newifi2)
intrain3<- createDataPartition(y=new_newifi2$SPACEID,p=0.70,list=FALSE)
train3<-new_newifi2[intrain3,]
test3<-new_newifi2[-intrain3,]
m3 <- train(SPACEID ~., data = train3,
            method = "knn",
            trControl=trctrl,
            preProcess = c("center", "scale"))
m3

library(scatterplot3d)
scatterplot3d( x= newifi$LATITUDE,
               y= newifi$LONGITUDE,
               z = newifi$FLOOR,
               type = "p",
               color = newifi$SPACEID,
               pch = 5)
scatterplot3d( x= newifi1$LATITUDE,
               y= newifi1$LONGITUDE,
               z = newifi1$FLOOR,
               type = "p",
               color = newifi1$SPACEID,
               pch = 5)
scatterplot3d( x= newifi2$LATITUDE,
               y= newifi2$LONGITUDE,
               z = newifi2$FLOOR,
               type = "p",
               color = newifi2$SPACEID,
               pch = 5)


