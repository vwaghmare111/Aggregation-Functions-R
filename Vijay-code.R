library("psych")
library("lpSolve")
library(readr)
library(tidyverse)

###Importing original data###

setwd("D:/11 Personal projects/07 R Software")
getwd()

cols = c('Kitchen_Temp','Kitchen_Humidity','Outisde_Temp','Outside_Humidity','Visibility','Energy')
cols

data<-as.matrix(read_csv("Energy19.csv",col_names = cols))
View(data)


####Extracting sample from the data####
set.seed(3)
my.data <- data[sample(1:671,300),c(1:6)]
View(my.data)
write.csv(my.data,"Sample.csv")

###Histogram of orginal sample data###
hist(my.data[,1],ylab = "Density", xlab = "Kitchen_Temperature", main = "Kitchen_Temperature Histogram",col = "skyblue2",prob = TRUE)
lines(density(my.data[,1]), col = "red", lwd = 2 )
hist(my.data[,2],ylab = "Density", xlab = "Kitchen_Humidity", main = "Kitchen_Humidity Histogram",col = "skyblue2",prob = TRUE)
lines(density(my.data[,2]), col = "red", lwd = 2 )
hist(my.data[,3],ylab = "Density", xlab = "Outside_Temperature", main = "Outside_Temperature Histogram",col = "skyblue2",prob = TRUE)
lines(density(my.data[,3]), col = "red", lwd = 2 )
hist(my.data[,4],ylab = "Density", xlab = "Outside_Humidity", main = "Outside_Humidity Histogram",col = "skyblue2",prob = TRUE)
lines(density(my.data[,4]), col = "red", lwd = 2 )
hist(my.data[,5],ylab = "Density", xlab = "Visibilityy", main = "Visibility Histogram",col = "skyblue2",prob = TRUE)
lines(density(my.data[,5]), col = "red", lwd = 2 )
hist(my.data[,6],ylab = "Density", xlab = "Energy_Consumption", main = "Energy_Consumption",col = "skyblue2",prob = TRUE)
lines(density(my.data[,6]), col = "red", lwd = 2 )

###### Statistical summary of original sample data####
summary(my.data)
describe(my.data)



###Scatter plot of original sample data####
plot(my.data[,1],my.data[,6],xlab = "Kitchen Temperature", ylab = "Energy Consumption", main = "Energy Consumption and Kitchen Temperature")
plot(my.data[,2],my.data[,6],xlab = "Kitchen Humidity", ylab = "Energy Consumption", main = "Energy Consumption and Kitchen Humidity")
plot(my.data[,3],my.data[,6],xlab = "Outside Temperature", ylab = "Energy Consumption", main = "Energy Consumption and Outside Temperature")
plot(my.data[,4],my.data[,6],xlab = "Outside Humidity", ylab = "Energy Consumption", main = "Energy Consumption and Outside Humidity")
plot(my.data[,5],my.data[,6],xlab = "Visibility", ylab = "Energy Consumption", main = "Energy Consumption and Visibility")


###Pearson correlation matrix of original sample data####
View(cormat<-round(cor(my.data), digits = 3))

####Regression on standardized variables######
impvar<-read.csv("D:/01 MS Data Science/04 Academics/01 Sem/03 Real World Analytics - SIT718/Assignment/03 Assessment/Excel trial/Regression_Standardize variables.csv")
fit.lm<-lm(Energy_Consumption~.,impvar)
summary(fit.lm)

###Transforming sample data - Linear Feature Scaling####
Kitchen_temperature<-(my.data[,1]- min(my.data[,1]))/(max(my.data[,1]) - min(my.data[,1]))
Outside_temperature<-(my.data[,3]- min(my.data[,3]))/(max(my.data[,3]) - min(my.data[,3]))
Outside_humidity<-(my.data[,4]- min(my.data[,4]))/(max(my.data[,4]) - min(my.data[,4]))
Visibility<-(my.data[,5]- min(my.data[,5]))/(max(my.data[,5]) - min(my.data[,5]))
Energy_Consumption<-(my.data[,6]- min(my.data[,6]))/(max(my.data[,6]) - min(my.data[,6]))

#### Transforming step 2 - Polynomial #####
Kitchen_temperature<-Kitchen_temperature^0.67
Outside_temperature<-Outside_temperature^1.13
Outside_humidity<-Outside_humidity^1.33
Visibility<-Visibility^0.74
Energy_Consumption<-Energy_Consumption^0.41


###Histogram of transformed sample data####
hist(Kitchen_temperature, col = "blue",prob = TRUE)
lines(density(Kitchen_temperature), col = "red", lwd = 2 )
hist(Outside_temperature,col = "blue",prob = TRUE)
lines(density(Outside_temperature), col = "red", lwd = 2 )
hist(Outside_humidity,col = "blue",prob = TRUE)
lines(density(Outside_humidity), col = "red", lwd = 2 )
hist(Visibility,col = "blue",prob = TRUE)
lines(density(Visibility), col = "red", lwd = 2 )
hist(Energy_Consumption,col = "blue",prob = TRUE)
lines(density(Energy_Consumption), col = "red", lwd = 2 )



####Combining transformed data####
transdata<-cbind(Kitchen_temperature,Outside_temperature,Outside_humidity,Visibility,Energy_Consumption)
write.table(transdata,"D:/01 MS Data Science/04 Academics/01 Sem/03 Real World Analytics - SIT718/Assignment/03 Assessment/Submission/Vijay-transformed.txt")


###Scatter plot of transformed sample data####
plot(transdata[,1],transdata[,5],xlab = "Kitchen Temperature", ylab = "Energy Consumption", main = "Energy Consumption and Kitchen Temperature")
plot(transdata[,2],transdata[,5],xlab = "Outside Temperature", ylab = "Energy Consumption", main = "Energy Consumption and Outside Temperature")
plot(transdata[,3],transdata[,5],xlab = "Outside Humidity", ylab = "Energy Consumption", main = "Energy Consumption and Outside Humidity")
plot(transdata[,4],transdata[,5],xlab = "Visibility", ylab = "Energy Consumption", main = "Energy Consumption and Visibility")

####Fitting function on transformed data####
fit.QAM(transdata) ###Generating parameters AM, inAM, PM05, invPM05, QM, invQM was changed while fitting function####
fit.OWA(transdata)
fit.choquet(transdata)

#######Predicting Energy use - Choquet#########
########## New X variables#########
X1<-18
X3<-4
X4<-74.80
X5<-31.40

###Rescaling new X variables####
kitchen.temperature<-((X1-min(my.data[,1]))/((max(my.data[,1]))-(min(my.data[,1]))))^0.67
outside.temperature<-((X3-min(my.data[,3]))/((max(my.data[,3]))-(min(my.data[,3]))))^1.13
outside.humidity<-((X4-min(my.data[,4]))/((max(my.data[,4]))-(min(my.data[,4]))))^1.33
visibility<-((X5-min(my.data[,5]))/((max(my.data[,5]))-(min(my.data[,5]))))^0.74

x<-cbind(kitchen.temperature,outside.temperature,outside.humidity,visibility)
v<-cbind(0.366489225,0.650965721,0.97636038,0.354435672,0.612405403,0.951753279,1,0.462182809,0.557644669,0.779051361,0.97636038,0.595509111,0.612405403,0.951753279,1)
predictedval<-choquet(x,v)####Giving NA values###
predictedval

#######Rescaling and transforming predicted value###########
predictedenergyuse<-((predictedval^(1/0.41))*((max(my.data[,6]))-min(my.data[,6])))+(min(my.data[,6]))
predictedenergyuse



            