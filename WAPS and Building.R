#libraries####
library(caret)
library(dplyr)
install.packages("crunch")
install.packages("qdap")
library(crunch)
library(qdap)
#import data####
wifi<- read.csv(file.choose(),header=TRUE)
validationwifi<- read.csv(file.choose(),header=TRUE)
summary(wifi)
View(wifi)

#sample data####
droplist<- c("SPACEID","RELATIVEPOSITION","USERID","PHONEID","TIMESTAMP")
new_newifi<- select(wifi,-droplist)
new_valid<- select(validationwifi,-droplist)
View(new_newifi)
WAPdrop<- c("WAP003","WAP004","WAP079","WAP092","WAP093","WAP094","WAP095","WAP152",
            "WAP153","WAP158","WAP159","WAP160","WAP190","WAP193","WAP197","WAP199",
            "WAP205","WAP206","WAP207","WAP208","WAP209","WAP210","WAP211","WAP212",
            "WAP213","WAP215","WAP217","WAP219","WAP220","WAP221","WAP226","WAP227",
            "WAP228","WAP231","WAP238","WAP239","WAP240","WAP241","WAP242","WAP243",
            "WAP244","WAP245","WAP246","WAP247","WAP251","WAP252","WAP254","WAP266",
            "WAP283","WAP293","WAP296","WAP297","WAP301","WAP303","WAP304","WAP307",
            "WAP321","WAP333","WAP339","WAP347","WAP349","WAP352","WAP353","WAP357",
            "WAP360","WAP361","WAP365","WAP373","WAP378","WAP388","WAP395","WAP406",
            "WAP407","WAP408","WAP411","WAP412","WAP414","WAP416","WAP417","WAP419",
            "WAP420","WAP421","WAP423","WAP424","WAP425","WAP426","WAP427","WAP429",
            "WAP430","WAP431","WAP433","WAP436","WAP437","WAP438","WAP440","WAP441",
            "WAP442","WAP444","WAP445","WAP446","WAP448","WAP450","WAP451","WAP453",
            "WAP454","WAP457","WAP458","WAP462","WAP463","WAP464","WAP466","WAP468",
            "WAP469","WAP470","WAP471","WAP472","WAP474","WAP475","WAP476","WAP477",
            "WAP482","WAP485","WAP487","WAP488","WAP490","WAP491","WAP497","WAP505",
            "WAP507","WAP509","WAP510","WAP512","WAP519","WAP520")
new_newifi<- select (new_newifi,-WAPdrop)
new_valid<- select (new_valid,-WAPdrop)
new_newifi$BUILDINGID<-as.factor(new_newifi$BUILDINGID)
new_valid$BUILDINGID<-as.factor(new_valid$BUILDINGID)
#remove rows with 100####
##builddrop<- c("BUILDINGID")
##WAP100<- select(new_newifi,-builddrop)
##WAP100$myid <- seq(1:nrow(WAP100))
##WAP100new<- which(!WAP100==100 )
##WAP100<- as.numeric(WAP100(1:ncol()))

##index100 <- WAP100 %>% 
##select(starts_with('WAP')) %>% 
##distinct


##str(WAP100)
##WAP100new<-WAP100[as.logical(rowSums(WAP100 != 100)), ]
##View(WAP100new)

new_newifi<- select(wifi,-droplist)
new_newifi<- select (new_newifi,-WAPdrop)
new_newifi$BUILDINGID<-as.factor(new_newifi$BUILDINGID)

WAP200<- new_newifi %>% select(starts_with('WAP'))
WAP201<- new_valid %>% select(starts_with('WAP'))
WAP200_2 <- as.logical(rowSums(WAP200 != 100))
WAP201_2 <- as.logical(rowSums(WAP201 != 100))
thisIsTheGoodOne <- new_newifi[WAP200_2,]
thisIsTheGoodOnevalid <- new_valid[WAP201_2,]
View(thisIsTheGoodOne)
#training the data with building as dependant####
set.seed(321)
intrain1<- createDataPartition(y=thisIsTheGoodOne$BUILDINGID,p=0.50,list=FALSE)
trctrl <- trainControl(method = "cv", number = 5)
train1<-thisIsTheGoodOne[intrain1,]
test1<-thisIsTheGoodOne[-intrain1,]
m1 <- train(BUILDINGID ~., data = train1,
            method = "knn",
            trControl=trctrl,
            preProcess = c("center", "scale"))
m1
#add predcited column to the validation set####
predictbuilding<- predict(m1,newdata = thisIsTheGoodOnevalid)

thisIsTheGoodOnevalid$predictedbuilding <- predictbuilding
View(thisIsTheGoodOnevalid)

#use new dataframe to test buidling wise####
thisIsTheGoodOne$FLOOR<-as.factor(as.character(thisIsTheGoodOne$FLOOR))
thisIsTheGoodOnevalid$FLOOR<-as.factor(as.character(thisIsTheGoodOnevalid$FLOOR))
dropbuilding<- c("BUILDINGID")
thisIsTheGoodOne<- select(thisIsTheGoodOne,-dropbuilding)
intrain2<- createDataPartition(y=thisIsTheGoodOne$FLOOR,p=0.50,list=FALSE)
train2<-thisIsTheGoodOne[intrain2,]
test2<-thisIsTheGoodOne[-intrain2,]

allbuildings <- train(FLOOR ~., data = train2,
            method = "knn",
            trControl=trctrl,
            preProcess = c("center", "scale"))
allbuildings

#predcit floor with the predicted building column####
predictfloor<- predict(allbuildings,newdata = thisIsTheGoodOnevalid)

thisIsTheGoodOnevalid$predictfloor <- predictfloor
View(thisIsTheGoodOnevalid)

#predict longitude####
dropfloor<- c("FLOOR")
thisIsTheGoodOne<- select(thisIsTheGoodOne,-dropfloor)
intrain3<- createDataPartition(y=thisIsTheGoodOne$LONGITUDE,p=0.50,list=FALSE)
train3<-thisIsTheGoodOne[intrain3,]
test3<-thisIsTheGoodOne[-intrain3,]
mlongitude <- train(LONGITUDE ~., data = train3,
            method = "knn",
            trControl=trctrl,
            preProcess = c("center", "scale"))
mlongitude
predictlongitude<- predict(mlongitude,newdata = thisIsTheGoodOnevalid)

thisIsTheGoodOnevalid$predictlongitude <- predictlongitude
View(thisIsTheGoodOnevalid)
#predict latitude####

droplongitude<- c("LONGITUDE")
thisIsTheGoodOne<- select(thisIsTheGoodOne,-droplongitude)
intrain4<- createDataPartition(y=thisIsTheGoodOne$LATITUDE,p=0.50,list=FALSE)
train4<-thisIsTheGoodOne[intrain4,]
test4<-thisIsTheGoodOne[-intrain4,]
mlatitude <- train(LATITUDE ~., data = train4,
                    method = "knn",
                    trControl=trctrl,
                    preProcess = c("center", "scale"))
mlatitude
predictlatitude<- predict(mlatitude,newdata = thisIsTheGoodOnevalid)

thisIsTheGoodOnevalid$predictlatitude <- predictlatitude
View(thisIsTheGoodOnevalid)

#find the error value####
thisIsTheGoodOnevalid$longituderror<- thisIsTheGoodOnevalid$LONGITUDE- thisIsTheGoodOnevalid$predictlongitude
thisIsTheGoodOnevalid$latituderror<- thisIsTheGoodOnevalid$LATITUDE- thisIsTheGoodOnevalid$predictlatitude
thisIsTheGoodOnevalid$c2<- (thisIsTheGoodOnevalid$longituderror^2) + (thisIsTheGoodOnevalid$latituderror^2)
thisIsTheGoodOnevalid$c<- sqrt(thisIsTheGoodOnevalid$c2)
View(thisIsTheGoodOnevalid)
caverage<- mean(thisIsTheGoodOnevalid[["c"]])
caverage
write.csv(thisIsTheGoodOnevalid, file = "List with error distance.csv")
