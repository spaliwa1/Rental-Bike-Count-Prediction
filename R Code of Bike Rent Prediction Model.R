#Initial Setup
setwd("D:\\Data Science\\Edwise Cert\\Bbike Rent Model")
getwd()
#loading csv file
bike<-read.csv("D:\\Data Science\\Edwise Cert\\Bbike Rent Model\\day.csv",header=TRUE,na.strings = FALSE)
#Analysing the data
str(bike)
summary(bike)

#missing value detection using loop#Check for missing value use is.na funtion
y=0
for(i in 1:ncol(bike)){
  x<-sum(is.na(bike[,i]))
  y=y+x
}
print(y)
# finding location in case there is any missing value
which(is.na(bike))
#Note:-There is no missing value in Data

#Outlier Analysis:-From the plot it's clear that there are ouliers in no. of casual users.
boxplot(bike$instant,bike$season,bike$yr,bike$mnth,bike$holiday,bike$weekday,bike$workingday,bike$weathersit,bike$temp,bike$atemp,bike$hum,bike$windspeed,bike$casual,bike$registered,bike$cnt,names=c("instant","season","yer","month","holiday","weekday","workingday","weathersit","temp","atemp","hum","windspeed","casul","registered","cnt"))
#Removing outlier's from casual varaible using boxplot method
bikenew<-bike
val = bikenew$casual[bikenew$casual %in% boxplot.stats(bikenew$casual)$out]
bikenew = bikenew[which(!bikenew$casual %in% val),]


#DataVisualization using ggplot2
install.packages("ggplot2")
install.packages("ggExtra")
library(ggplot2)
library(ggExtra)
#let's visualize relation btwn Usage count vs adjusted temp
plot1<-ggplot(bike,aes(x=bike$atemp,y=bike$cnt))+geom_point(alpha=0.07,colour="green")+labs(x="Adjusted Temperature",y="bike Usage Count")+geom_smooth(method='auto')+geom_smooth(method='lm',colour='red')
ggMarginal(plot1,type="histogram",fill="transparent",margins=c("x"))
#let's visualize relation btwn Usage count vs humidity
plot2<-ggplot(bike,aes(x=bike$hum,y=bike$cnt))+geom_point(alpha=0.07,colour="green")+labs(x="Humidity",y="bike Usage Count")+geom_smooth(method='auto')+geom_smooth(method='lm',colour='red')
ggMarginal(plot2,type="histogram",fill="transparent",margins =c("x"))
#let's visualize relation btwn Usage count vs temp
plot3<-ggplot(bike,aes(x=bike$temp,y=bike$cnt))+geom_point(alpha=0.07,colour="orange")+labs(x="Temperature",y="bike Usage Count")+geom_smooth(method='auto')+geom_smooth(method='lm',colour='red')
ggMarginal(plot3,type="histogram",fill="transparent",margins=c("x"))
#let's visualize relation btwn Usage count vs WindSpeed
plot4<-ggplot(bike,aes(x=bike$windspeed,y=bike$cnt))+geom_point(alpha=0.07,colour="green")+labs(x="WindSpeed",y="bike Usage Count")+geom_smooth(method='auto')+geom_smooth(method='lm',colour='red')
ggMarginal(plot4,type="histogram",fill="transparent",margins=c("x"))
#let's visualize relation btwn Usage count vs WeatherSituation
max(bike$weathersit)
plot5<-ggplot(bike,aes(x=bike$weathersit,y=bike$cnt))+geom_count(color='yellow')+geom_point(alpha=0.07,colour="green")+labs(x="WeatherSituation",y="bike Usage Count")+geom_smooth(method='loess')
ggMarginal(plot5,type="histogram",fill="transparent",margins=c("x"))

#correlation plot
install.packages("corrplot")
library(corrplot)
corcountdata<-bike[,9:16]
plot6<-corrplot(cor(corcountdata),method='number')

#bivariate relationship between numeric variables
plot6<-ggplot(bike,aes(x=bike$atemp,y=bike$temp))+geom_point()+geom_smooth()
print(plot6)#strong corelation between temp and atemp
#It's clear from the corelatIon plot that there is ver high corelation between temp and atemp variable also there is no relationship between cnt and humidity. Hence we will drop these to variables from our data
bikenew<-subset(bikenew,select=-c(atemp,hum))

#bivariate relationship between numeric variables
plot7<-ggplot(bike, aes(x= temp,y=hum)) +
  geom_point()+
  geom_smooth()
print(plot7)## here  it is showing  Humidity is increses  till temparature is 0.7 and it is decreasing  gradually

# *************visualize the relationship between categorical variable***************

#check relationship between  season and holiday
rel_mnth_holi= table(bike$season,bike$holiday)

rel_mnth_holi

barplot(rel_mnth_holi)#means for all seasons holiday is almost same
#check relationship between  season and weekday

rels_cats_2 <- table(bikenew$season,bikenew$weekday)

barplot(rels_cats_2)

#check relationship between  season and weathersit

rels_cats_3 <- table(bikenew$weathersit,bikenew$season)
rels_cats_3

prop.table(rels_cats_3,2)

barplot(rels_cats_3)#It is stating that in all the season  whether 1 type is large numbers
#Normalisation
cnames = c("casual","registered")

for(i in cnames){
  print(i)
  bikenew[,i] = (bikenew[,i] - min(bikenew[,i]))/
    (max(bikenew[,i] - min(bikenew[,i])))
}
bikenew$casual
bikenew$registered

##################### Model Development#########################
#Divide data in train and test
install.packages("caret")
library(caret)
set.seed(1234)
train.index<-createDataPartition(bikenew$cnt,p=.80,list=FALSE)
train<-bikenew[train.index,]
test<-bikenew[-train.index,]
train<-train[,-c(1,2)]
test<-test[,-c(1,2)]

###################Developing first model using Random Forest##############
install.packages("randomForest")
library(randomForest)
?randomForest
model1<-randomForest(cnt~.,data=train)
plot(model1)
model1

##################Evaluate Model##################################
#predict values on test data
predictvalue<-predict(model1,test[,-12])

#MAPE(test[,12],predictvalue)
#Evaluate  Model using RMSE
actual<-test[,12]
mape<-function(actual,predictvalue){
  mean(abs((actual-predictvalue)/actual))
  
}
mape(actual,predictvalue)

#Evaluate  Model using RMSE

RMSE <- function(y_test,y_predict) {
  
  difference = y_test - y_predict
  root_mean_square = sqrt(mean(difference^2))
  return(root_mean_square)
  
}
RMSE(test[,12],predictvalue)

#################Tuning the parameters################
model2<-randomForest(cnt~.,data=train,mtry=7,ntree=500,nodesize=10,importance=TRUE)
model2

predictedvalu2<-predict(model2,test[,-12])

#Evaluate Model using MAPE
mape(test[,12],predictedvalu2)
#Evaluate Model using RMSE
RMSE(test[,12],predictedvalu2)

#Checking variable importance
varimportance<-importance(model2)
varimportance
sort_var <- names(sort(varimportance[,1],decreasing =T))
sort_var1 <- sort(varimportance[,1],decreasing =T)
hist(sort_var1)
#Varaibleimportanceplot
varImpPlot(model2,type=2)
#it's ovious that "holiday","wethersit" and "windspeed" are not important variables in prediction.Hence droppig them from train and test data set
train2<-train[,-c(4,7,9)]
test2<-test[,-c(4,7,9)]

#########Bilding 3rd model using random forest#########
model3<-randomForest(cnt~.,data=train2,mtry=7,ntree=500,nodesize=10,importance=TRUE)
model3
#####Predict vales########
predicvales<-predict(model3,test2[,-9])
####Evaluate Mape
mape(test2[,9],predicvales)
###Evaluate RMSE
RMSE(test2[,9],predicvales)

#######################Model building usin Linear Regression################
#First Check multicollearity in data
install.packages("usdm")
library(usdm)
install.packages("car")
library(car)
vif(train[,-12])#no mulicolinearity
###############building first linear Model#########
model4<-lm(cnt~.,data=train)
summary(model4)
vif(model4)
###########predicting the output###########
predicting<-predict(model4,test[,-12])

###EEvaluate MAPE
mape(test[,12],predicting)#MAPE is 1.561794e-15

##### Evaluate RMSE
RMSE(test[,12],predicting)#RMSE is 4.597811e-12

#####Checkinking Hetroscadisity
plot(model4$fitted.values,model4$residuals)#Hence no Hetroscadicity

##Hence linear regression model provides is the best model to prdict rental bike count

save.image(file="Bike Rental Prediction.RData")


