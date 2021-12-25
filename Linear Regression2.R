library(readxl)
AirPollution <- read_excel("E:/Datasets/AirPollution.xls")
names(AirPollution)
AirPollution$Site<-NULL
AirPollution$X__1<-NULL
AirPollution$Season<-NULL
AirPollution$Zn<-NULL
AirPollution$...10<- NULL
str(AirPollution)
boxplot(AirPollution$PM10)
summary(AirPollution$PM10)
upper<-89.82+1.5*IQR(AirPollution$PM10);upper
AirPollution$PM10[AirPollution$PM10>upper]<-upper
boxplot(AirPollution$PM10)
summary(AirPollution$PM10)

boxplot(AirPollution$Pb)
summary(AirPollution$Pb)
upper<-0.965+1.5*IQR(AirPollution$Pb);upper
AirPollution$Pb[AirPollution$Pb>upper]<-upper
boxplot(AirPollution$Pb)
summary(AirPollution$Pb)

boxplot(AirPollution$Cd)
summary(AirPollution$Cd)
AirPollution$Cd<-NULL
##upper<-0+1.5*IQR(AirPollution$Cd);upper
##AirPollution$Cd[AirPollution$Cd>upper]<-upper
##boxplot(AirPollution$Cd)
##summary(AirPollution$Cd)

boxplot(AirPollution$Cu)
summary(AirPollution$Cu)
upper<-0.5350+1.5*IQR(AirPollution$Cu);upper
AirPollution$Cu[AirPollution$Cu>upper]<-upper
boxplot(AirPollution$Cu)
summary(AirPollution$Cu)

boxplot(AirPollution$Cr)
summary(AirPollution$Cr)
upper<-0.58+1.5*IQR(AirPollution$Cr);upper
AirPollution$Cr[AirPollution$Cr>upper]<-upper
boxplot(AirPollution$Cr)
summary(AirPollution$Cr)

boxplot(AirPollution$NOx)
summary(AirPollution$NOx)
upper<-0.58+1.5*IQR(AirPollution$NOx);upper
AirPollution$NOx[AirPollution$NOx>upper]<-upper
boxplot(AirPollution$NOx)
summary(AirPollution$NOx)


boxplot(AirPollution$SO2)
summary(AirPollution$SO2)
upper<-27.42+1.5*IQR(AirPollution$SO2);upper
AirPollution$SO2[AirPollution$SO2>upper]<-upper
boxplot(AirPollution$SO2)
summary(AirPollution$SO2)

##data partition
library(caret)
Train<-createDataPartition(AirPollution$PM10,p=0.70,list = FALSE)
Train<-as.numeric(Train)
training<-AirPollution[Train,]
testing<-AirPollution[-Train,]

View(training)
View(testing)

cor(AirPollution)

model<-(lm(PM10~.,data=training))
model
summary(model)

library(car)
par(mfrow=c(2,2))
plot(model)

##Assumptions are met so no need to perfrom VCF and Dw test

model1<-step(lm(PM10~.,data=training),direction="backward")
summary(model1)
vif(model1)

  