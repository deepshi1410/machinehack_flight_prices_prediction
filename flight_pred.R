
#import all the required libraries
#download all the required libraries
library(janitor)
library(earth)
library(e1071)
library(plyr)
library(openxlsx)
#library(lubridate)
library(caret)
library(mlbench)
library(ggplot2)
library(caTools)
library(rpart)
library(chron)
library(ipred)
library(randomForest)
library(neuralnet)
library(pROC)
library(fastDummies)
library(FSelector)
library(mlr)
library(stringr)
library(xgboost)

data1=read.xlsx("Data_Train_flight.xlsx")
data2=read.xlsx("Test_set_flight.xlsx")
data2$Price={0}
data=rbind(data1,data2)
dataf=data

time=data$Dep_Time
arr_time=substr(data$Arrival_Time,start=1,stop=5)
data$Arrival_Time=arr_time
#time=format(strptime(time, format='%H:%M'), '%I:%M')
#arr_time=format(strptime(arr_time,format='%H:%M'),'%I:%M')
#data$Dep_Time=time
#data$Arrival_Time=arr_time

 #processing on arrival time
 Splt <- strsplit(as.character(arr_time), ":", fixed = TRUE)
 h <- sapply(Splt, "[", 1)
 m <- sapply(Splt, "[", 2)
 h=as.array(h)
 m=as.array(m)
 
 #processing on departure time
 splt <- strsplit(as.character(time), ":", fixed = TRUE)
 h1 <- sapply(splt, "[", 1)
 m1 <- sapply(splt, "[", 2)
 h1=as.array(h1)
 m1=as.array(m1)
 
 h=as.numeric(h)
 h1=as.numeric(h1)
 m=as.numeric(m)
 m1=as.numeric(m1)
 
 data$Dep_Time=time
 data$Arrival_Time=arr_time
 data$x={0}
 y=as.array(data$x)
 y=as.numeric(y)
 
 i=1
 while(i<=nrow(data))
 {
  if(h1[i]>h[i])
  { 
    y[i]=1
  }
   else if(h1[i]==h[i])
   { if(m1[i]>m[i])
    {
     y[i]=1
    }
     else 
       y[i]=0
   }
   else
     y[i]=0
   
   i=i+1
  }
 
data$late_night=y
d=as.array(data$Date_of_Journey)

j=1
while(j<=nrow(data))
   {
    
        data$days[j]=difftime(strptime("21/07/2019", format = "%d/%m/%Y"),
                                +          strptime(d[j], format = "%d/%m/%Y"),units="days")
        j=j+1
       }

regexp="[[:digit:]]+"
data$days=str_extract(data$days,regexp)
data$days=as.numeric(data$days)

data=subset(data,select = -c(Dep_Time,Arrival_Time,Route,Date_of_Journey,x))
m <- gregexpr('[0-9]+',data$Duration)
m=regmatches(data$Duration,m)

m=as.array(m)
Splt <- strsplit(as.character(m), ":", fixed = TRUE)
hour <- sapply(m, "[", 1)
minute <- sapply(m, "[", 2)
hour=as.numeric(hour)
minute=as.numeric(minute)
minute[is.na(minute)]=0

data$Duration=hour*60 + minute

#converting categorical to dummy columns
dummy=data.frame(airline=data$Airline,src=data$Source,dest=data$Destination,stop=data$Total_Stops,info=data$Additional_Info,
                 stringsAsFactors = FALSE)
results <- fastDummies::dummy_cols(dummy)
results=results[,6:ncol(results)]

data=cbind(results,data)
data=subset(data,select = -c(Airline,Source,Destination,Total_Stops,Additional_Info,Price))

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

data_scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

final=data_scaled

final$Price=dataf$Price

train=final[1:nrow(data1),]
test=final[-(1:nrow(data1)),]


model = lm(formula = Price~ .,
          data = train) 
y = as.data.frame(predict(model, newdata = test ))

#xgb.fit <- xgboost(data = data.matrix(train), label = train$Price,
                 #booster = "gblinear", objective = "reg:linear",
                # colsample_bytree = 0.2, eta=0.1,gamma = 0.6,
               # learning_rate = 0.05, max_depth = 6,
              # min_child_weight = 1.5, n_estimators = 9000,
             # reg_alpha = 0.9, reg_lambda = 0.6,
            # subsample = 0.2, seed = 42,
           # silent = 1, nrounds =500,eval_metric="rmse")

regressor = svm(formula = Price ~ .,
                data = train,
                type = 'eps-regression',
                kernel = 'radial')
y_pred=predict(regressor,test)
y_pred1=c("Price",y_pred)
#y_pred1=as.data.frame(y_pred1)

file=write.xlsx(y_pred1,file = "answer1.xlsx",colNames=TRUE)                              


