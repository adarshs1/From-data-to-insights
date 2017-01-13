

#Read the crash dataset
vcrash1<-read.csv("Vehicular_Crashes.csv")

#Read vision zero dataset
vision<-read.csv("Vision_Zero_Safety_Transportation.csv")
#5062 street seg

#Read street features
sdata<-read.csv("Street_Segments.csv")

#merge the two
freqdata<-merge(x=vcrash1,y=sdata,by="STREETSEGID",all.x=T)

#Calculate frequency of crashes
streetsinfo<-count(distinct(vcrash1), STREETSEGID)
streetsinfo<-as.data.frame(streetsinfo)

#Merge frequency
CrashFrequency<-merge(x=freqdata,y=streetsinfo,by="STREETSEGID",all.x=T)
write.csv(CrashFrequency,"CrashFrequency.csv")


#Divide into train and test
#Rmoved street seg id 0 in excel
CrashFrequency_Filtered<-read.csv("CrashFrequency.csv")
dtfreq<-sort(sample(nrow(CrashFrequency_Filtered), nrow(CrashFrequency_Filtered)*.7))
trainfreq<-CrashFrequency_Filtered[dtfreq,]
testfreq<-CrashFrequency_Filtered[-dtfreq,]

####################### PREDICTION #################
############### Multiple linear regression ##########

#Mentioning the categorical variables

trainfreq$DAYOFWEEK<-factor(trainfreq$DAYOFWEEK)
trainfreq$ONSTREET<-factor(trainfreq$ONSTREET)
trainfreq$COLLISIONTYPE<-factor(trainfreq$COLLISIONTYPE)
trainfreq$ROADWAYTYPE<-factor(trainfreq$ROADWAYTYPE)
trainfreq$CONSTRUCTION<-factor(trainfreq$CONSTRUCTION)
trainfreq$ROADSURFACE<-factor(trainfreq$ROADSURFACE)
trainfreq$ROADTYPE<-factor(trainfreq$ROADTYPE)
trainfreq$ROADCONDITION<-factor(trainfreq$ROADCONDITION)
trainfreq$STREETLIGHT<-factor(trainfreq$STREETLIGHT)
trainfreq$LIGHTCONDITION<-factor(trainfreq$LIGHTCONDITION)
trainfreq$WEATHER<-factor(trainfreq$WEATHER)
trainfreq$TRAFFICCONDITION<-factor(trainfreq$TRAFFICCONDITION)
trainfreq$STREETTYPE<-factor(trainfreq$STREETTYPE)
trainfreq$QUADRANT<-factor(trainfreq$QUADRANT)


LM1 <- lm(n ~ DAYOFWEEK+ONSTREET+COLLISIONTYPE+ROADWAYTYPE+
            CONSTRUCTION+ROADSURFACE+ROADTYPE+ROADCONDITION+STREETLIGHT+
            LIGHTCONDITION+WEATHER+TRAFFICCONDITION+STREETTYPE+QUADRANT
          +SHAPE_Length, data = trainfreq)


summary(LM1)
summary(LM1)$r.squared

testfreq$DAYOFWEEK<-factor(testfreq$DAYOFWEEK)
testfreq$ONSTREET<-factor(testfreq$ONSTREET)
testfreq$COLLISIONTYPE<-factor(testfreq$COLLISIONTYPE)
testfreq$ROADWAYTYPE<-factor(testfreq$ROADWAYTYPE)
testfreq$CONSTRUCTION<-factor(testfreq$CONSTRUCTION)
testfreq$ROADSURFACE<-factor(testfreq$ROADSURFACE)
testfreq$ROADTYPE<-factor(testfreq$ROADTYPE)
testfreq$ROADCONDITION<-factor(testfreq$ROADCONDITION)
testfreq$STREETLIGHT<-factor(testfreq$STREETLIGHT)
testfreq$LIGHTCONDITION<-factor(testfreq$LIGHTCONDITION)
testfreq$WEATHER<-factor(testfreq$WEATHER)
testfreq$TRAFFICCONDITION<-factor(testfreq$TRAFFICCONDITION)
testfreq$STREETTYPE<-factor(testfreq$STREETTYPE)
testfreq$QUADRANT<-factor(testfreq$QUADRANT)

#Prediction
ypredfreq1<-predict(LM1, newdata = testfreq)

#TestRMSE1
RMSE.log.reg.1 <- sqrt(mean((ypredfreq1-testfreq$n)^2,na.rm = TRUE))
RMSE.log.reg.1/(max(testfreq$n)-min(testfreq$n))

#TrainRMSE1
ypredtrain1<-predict(LM1, newdata = trainfreq)
RMSE.log.reg.train.1 <- sqrt(mean((ypredtrain1-trainfreq$n)^2,na.rm = TRUE))
RMSE.log.reg.train.1/(max(trainfreq$n)-min(trainfreq$n))

#Correlation
try1 <-cbind(testfreq,ypredfreq1)
cor(try1$ypredfreq1,try1$n,use="complete")

#######################randomForest######################


#trainfreq <-trainfreq[,colSums(is.na(trainfreq))==0]
modelFit <- rpart(n ~ DAYOFWEEK+ONSTREET+COLLISIONTYPE+ROADWAYTYPE+
                    CONSTRUCTION+ROADSURFACE+ROADTYPE+ROADCONDITION+STREETLIGHT+
                    LIGHTCONDITION+WEATHER+TRAFFICCONDITION+STREETTYPE+QUADRANT
                  +SHAPE_Length,method="anova", data = trainfreq)
prediction <- predict(modelFit, testfreq)

RMSE.rtree <- sqrt(mean((prediction-testfreq$n)^2))
42.48041
RMSE.rtree/(max(testfreq$n)-min(testfreq$n))
[1] 0.06334923

###RMSETRAIN
prediction.rtrain <- predict(modelFit, trainfreq)
RMSE.rtree.rtrain <- sqrt(mean((prediction.rtrain-trainfreq$n)^2,na.rm = TRUE))
RMSE.rtree.rtrain/(max(trainfreq$n)-min(trainfreq$n))

printcp(modelFit)
min.xerror <- modelFit$cptable[which.min(modelFit$cptable[,"xerror"]),"CP"]

min.xerror <- modelFit$cptable[which.min(modelFit$cptable[,"xerror"]),"CP"]
min.xerror
0.01
rt.pruned <- prune(modelFit,cp = min.xerror)
prediction.pruned <- predict(rt.pruned, testfreq)
RMSE.rtree.pruned <- sqrt(mean((prediction.pruned-testfreq$n)^2))
RMSE.rtree.pruned/(max(testfreq$n)-min(testfreq$n)) 
[1] 0.06127384

predtrain<-predict(modelFit, newdata = trainfreq)

t<-table(prediction, testfreq$n)
print(t)
##accuracy <- sum(prediction)/nrow(testfreq)
#accuracies <- c(accuracies,accuracy)
#print(accuracy)


rsq.rpart(modelFit)
plot(modelFit, uniform=TRUE, main="Regression Tree for Mileage ")
text(modelFit, use.n=TRUE, all=TRUE, cex=.8)


RMSE.rtree.1 <- sqrt(mean((predtrain-trainfreq$n)^2))
RMSE.rtree.1
41.92531



#####Tableau Visualization
vision<-read.csv("Vision_Zero_Safety_Transportation.csv")
sdata<-read.csv("Street_Segments.csv")

visionzero<-freqdata<-merge(x=sdata,y=vision,by="STREETSEGID",all.y=T)
write.csv(visionzero,"visionzero.csv")


#Calculate frequency of requests
streetsrequests<-count(distinct(vision), STREETSEGID)
streetsrequests<-as.data.frame(streetsrequests)

#Merge frequency
RequestFrequency<-merge(x=vision,y=streetsrequests,by="STREETSEGID",all.x=T)
write.csv(RequestFrequency,"RequestFrequency.csv")



