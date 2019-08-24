rm(list=ls())
getwd()
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')
library(tidyverse)
library(lubridate)
library(geosphere)
library("dplyr")

lapply(x, require, character.only = TRUE)
rm(x)
train=read.csv("train_cab.csv",stringsAsFactors = F)
test=read.csv("test.csv",stringsAsFactors = F)
head(train)
glimpse(train)
#after taking a look at data we do some data peperation
#convert features into proper data types
#converting pickup datatime into day,month year,week,hour
#using pickup longitude,longitude and dropoff_latitute,dropoff_longtitude to calculate distance
#removing negative and missing of target variable
train$fare_amount=as.numeric(train$fare_amount,na.rm=T)
train<-train[!is.na(train$fare_amount) ,]
train<-train[train$fare_amount>=2.5 & train$fare_amount< 100,]
#checking missing value for rest of the variable
apply(train,2,function(x) {sum(is.na(x))})
#removing missing target value and negative value      
#converting pick_up timing to year,hour,month,day
#train$pickup_datetime=as.POSIXct(train$pickup_datetime,format="%Y-%m-%dT%H:%M:%OS",tz='UTC')
train$pickup_datetime= ymd_hms(train$pickup_datetime)

train= train %>% 
  mutate(year=year(pickup_datetime),
         month=month(pickup_datetime),
         week=weekdays(pickup_datetime),
         hour=hour(pickup_datetime),
         date=day(pickup_datetime)
  )
train$pickup_datetime=NULL
test$pickup_datetime= ymd_hms(test$pickup_datetime)

test= test %>% 
  mutate(year=year(pickup_datetime),
         month=month(pickup_datetime),
         week=weekdays(pickup_datetime),
         hour=hour(pickup_datetime),
         date=day(pickup_datetime)
  )
test$pickup_datetime=NULL

glimpse(train)

#checking outliner in

ggplot(train, aes(y = pickup_latitude, x =pickup_longitude)) +
  geom_point()
train<-train[train$pickup_latitude<100 & train$pickup_longitude< -50,]

ggplot(train, aes(x = dropoff_longitude, y =dropoff_latitude)) +
  geom_point()

train<-train[train$dropoff_latitude>25 & train$dropoff_longitude< -50,]


p=ggplot(train, aes(y = pickup_latitude, x =pickup_longitude)) +
  geom_point()
q=ggplot(train, aes(x = dropoff_longitude, y =dropoff_latitude)) +
  geom_point()

require("gridExtra")
grid.arrange(p, q, nrow = 1)

t=abs(train$pickup_latitude-train$dropoff_latitude)
r=abs(train$pickup_longitude-train$dropoff_longitude)

tz=as.data.frame(list(t=t,r=r))

ggplot(tz, aes(x = t, y =r)) +
  geom_point()

train<-train[(tz$t<0.5) & (tz$r<0.5),]
tz<-tz[tz$t<0.5 & tz$r<0.5,]

ggplot(tz, aes(x = t, y =r)) +
  geom_point()

index_dist=(tz$t==0 & tz$r==0)

train[index_dist,"fare_amount"]=median(train[index_dist,"fare_amount"])
train$fare_amt <- cut(train$fare_amount, breaks=c(0,5,10,15,20,25,100))

train$lat_diff=abs(train$pickup_latitude-train$dropoff_latitude)
train$long_diff=abs(train$pickup_longitude-train$dropoff_longitude)

ggplot(train, aes(lat_diff, long_diff, fill = fare_amt, group = fare_amt)) +
  geom_point(aes(group = fare_amt,color=fare_amt)) 

ggplot(train, aes(lat_diff, long_diff, fill = fare_amt, group = fare_amt)) +
  geom_point(aes(group = fare_amt,color=fare_amt)) 

ggplot(train, aes(pickup_latitude ,pickup_longitude, fill = fare_amt, group = fare_amt)) +
  geom_point(aes(group = fare_amt,color=fare_amt)) 

ggplot(train, aes(dropoff_latitude ,dropoff_longitude, fill = fare_amt, group = fare_amt)) +
  geom_point(aes(group = fare_amt,color=fare_amt)) 
train=na.omit(train)

#install.packages("dbscan")
#there is no specific library similar to hdbscan

glimpse(train)
# remove out of range value
#computing the distance using pickup longitude,longitude and dropoff_latitute,dropoff_longtitude 
#haversine formula is used
R = 6371  #radius of earth in kilometers
phi1 = train$pickup_latitude*(pi/180)
phi2 = train$dropoff_latitude*(pi/180)

delta_phi =(train$dropoff_latitude-train$pickup_latitude)*(pi/180)
delta_lambda = (train$dropoff_longitude-train$pickup_longitude)*(pi/180)

#a = sin²((??B - ??A)/2) + cos ??A . cos ??B . sin²((??B - ??A)/2)
a = sin(delta_phi / 2.0) ** 2 + cos(phi1) * cos(phi2) * sin(delta_lambda / 2.0) ** 2

#c = 2 * atan2( ???a, ???(1???a) )
c = 2 * atan2(sqrt(a), sqrt(1-a))

#d = R*c
d = (R * c) #in kilometers
train$H_Distance = d

q=ggplot(train, aes(y = H_Distance, x = fare_amount)) +
  geom_boxplot()
q
#with the help scatter plot removing outliner having absurd fare/distance,distance/fare ratio and absurd high fare for very low distance
#taking a very conservative assumption

train$fare_km=(train$fare_amount-2.5)/(train$H_Distance)
train[is.infinite(train$fare_km),"fare_km"]=2
train=train[train$fare_km>0.4 & train$fare_km<10,]
ggplot(train, aes(x = H_Distance, y = fare_amount)) +
  geom_point()

#no of passenger

table(train$passenger_count)
#removing the columns with 0,fraction and more than 6 passenger

train=train[!(train$passenger_count>6 )& !(train$passenger_count %in% c(0,0.12,1.3)),]
ggplot(data = train) +
  geom_bar(mapping = aes(x = passenger_count))

#imputing missing value with Mode value 

train[is.na(train$passenger_count),"passenger_count"]=1
train[is.na(train$passenger_count),"passenger_count"]=1

sum(is.na(train$passenger_count))

#function for plotting tapply output
func_plot=function(values,index){
  xnames <- names(tapply(values,index,mean))
  plot(tapply(values,index,mean),xaxt="n",col="blue",type = "o")
  axis(1, at=1:length(xnames), labels=xnames)
}

#average fare on the basis of year

round(tapply(train$fare_amount,train$year,mean,na.rm=T))

#there is increment in fare with evry passing year
func_plot(train$fare_amount,train$year)

#average fare changes wrt month 

table(train$month)
round(tapply(train$fare_amount,train$month,mean,na.rm=T))
func_plot(train$fare_amount,train$month)
#average fare with by month can be divided into 3 groups

#average fare relation day features 
tapply(train$fare_amount,train$week,mean,na.rm=T)
func_plot(train$fare_amount,train$week)

#sunday and monday having slightly higher fare than rest of the days

table(train$hour)
round(tapply(train$fare_amount,train$hour,mean,na.rm=T))
func_plot(train$fare_amount,train$hour)

# there is a price jump from 4 to 7Pm(In afternoon) and 12pm to 5AM(night time)
#average fair depanding on date
table(train$date)
round(tapply(train$fare_amount,train$date,mean,na.rm=T))
func_plot(train$fare_amount,train$date)

#for date wise group there is no substancial pattern or trend So we will make dummies on the round of value only
#average fares depanding of no of passanger

round(tapply(train$fare_amount,train$passenger_count,mean,na.rm=T))
func_plot(train$fare_amount,train$passenger_count)
#fare slightly decrease with increase in no of passanger 

corrgram(train[,-1], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

library(dplyr)

R = 6371  #radius of earth in kilometers
phi1 = test$pickup_latitude*(pi/180)
phi2 = test$dropoff_latitude*(pi/180)

delta_phi =(test$dropoff_latitude-test$pickup_latitude)*(pi/180)
delta_lambda = (test$dropoff_longitude-test$pickup_longitude)*(pi/180)
a = sin(delta_phi / 2.0) ** 2 + cos(phi1) * cos(phi2) * sin(delta_lambda / 2.0) ** 2
c = 2 * atan2(sqrt(a), sqrt(1-a))
d = (R * c) #in kilometers
test$H_Distance = d

new_train =train %>%
  transmute(ll_y1=as.numeric(year %in% c("2009","2010","2011") ),
            ll_y2=as.numeric(year %in% c("2014","2015")),
            ll_m1=as.numeric(month %in% c(9:11) ),
            ll_m2=as.numeric(month==1),
            ll_h1=as.numeric(hour %in% c(0,1,2,3,4,5,14,15,16)),
            ll_d1=as.numeric(date %in% c(1,3,17,20,31)),
            ll_w1=as.numeric(week %in% c("Sunday","Monday","Friday")),
            ll_pc1=as.numeric(passenger_count %in% c(2,3))
  )
new_test =test %>%
  transmute(ll_y1=as.numeric(year %in% c("2009","2010","2011") ),
            ll_y2=as.numeric(year %in% c("2014","2015")),
            ll_m1=as.numeric(month %in% c(9:11) ),
            ll_m2=as.numeric(month==1),
            ll_h1=as.numeric(hour %in% c(0,1,2,3,4,5,14,15,16)),
            ll_d1=as.numeric(date %in% c(1,3,17,20,31)),
            ll_w1=as.numeric(week %in% c("Sunday","Monday","Friday")),
            ll_pc1=as.numeric(passenger_count %in% c(2,3))
  )


new_train$fare_amount=train$fare_amount
new_train$H_Distance=train$H_Distance
new_train$lat_diff=train$lat_diff
new_train$long_diff=train$long_diff

new_test$H_Distance=test$H_Distance

#model development
s=sample(1:nrow(new_train),0.75*nrow(new_train))
ld_train=new_train[s,]
ld_train2=new_train[-s,]

fit=lm(fare_amount~.,data=ld_train)

library(car)
# we'll take vif cutoff as 5

sort(vif(fit))
summary(fit)

#removing feature based on AIC Score 

fit=step(fit)
formula(fit)
fit_reg=lm(fare_amount ~ ll_y1 + ll_y2 + ll_m1 + ll_m2 + ll_h1 + ll_w1 + 
         ll_pc1 + H_Distance,data=ld_train)
summary(fit_reg)
new_test$ll_d1=NULL
val.pred=predict(fit_reg,newdata=ld_train2)

error=ld_train2$fare_amount-val.pred

RMSE=sqrt(mean(error**2,na.rm=T))
#3.61

mape_score=mean(abs((ld_train2$fare_amount-val.pred)/ld_train2$fare_amount)*100,na.rm=T)
#19.28
plot(fit,1)

plot(fit,2)

plot(fit,3)


#Decision Tree

require(rpart)
fit <- rpart(fare_amount ~ ll_y1 + ll_y2 + ll_m1 + ll_m2 + ll_h1 + ll_w1 + 
               ll_pc1 + H_Distance, method = 'anova',parms = list(split = "information"),data=ld_train)
val.pred=predict(fit,newdata=ld_train2)
error=ld_train2$fare_amount-val.pred
RMSE=sqrt(mean(error**2,na.rm=T))
#3.73
Mape_score=mean(abs((ld_train2$fare_amount-val.pred)/ld_train2$fare_amount)*100,na.rm=T)
#23.93
library(rpart.plot)
rpart.plot(fit)

rpart.rules(fit,cover = TRUE)
#random forest implementation

fit <- randomForest(fare_amount ~ ll_y1 + ll_y2 + ll_m1 + ll_m2 + ll_h1 + ll_w1 + 
               ll_pc1 + H_Distance,data=ld_train)
val.pred=predict(fit,newdata=ld_train2)
error=ld_train2$fare_amount-val.pred
RMSE=sqrt(mean(error**2,na.rm=T))
RMSE
#3.73
Mape_score=mean(abs((ld_train2$fare_amount-val.pred)/ld_train2$fare_amount)*100,na.rm=T)
Mape_score
#23.93

#basic model prediction using R Code
new_test$fare_amount=predict(fit_reg,newdata=new_test)
#advannce model is implemented in python code
