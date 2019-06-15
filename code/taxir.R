install.packages('data.table')
install.packages('readr')
install.packages('geosphere')
install.packages('tidyr')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('lubridate')
install.packages("FNN")
install.packages('Metrics')


library(data.table)
library(readr)
library(geosphere)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(FNN)


#library(randomForest)
rm(list=ls())
train = read_csv("train.csv")
#data = train


train = as.data.table(train)
#train <- train[,distance_km := 
               #  distHaversine(matrix(c(pickup_longitude, pickup_latitude), ncol = 2),
               # #               matrix(c(dropoff_longitude,dropoff_latitude), ncol = 2))/1000
              # ]
#train[,speed:=(distance_km)/(trip_duration/3600)]
##########data cleaning
###extreme data

#longer than a day
#day_plus_trips <- train %>%
  #filter(trip_duration > 10*3600)

#day_plus_trips %>% select(pickup_datetime, dropoff_datetime)

#delete
index = which(train$trip_duration >10*3600)



train = train[-index,]

#train = data
train$pickup_hour <- hour(train$pickup_datetime)
#train$pickup_week <- week(train$pickup_datetime)
#train$pickup_month <- month(train$pickup_datetime)
train$pickup_weekdays <- weekdays(train$pickup_datetime)
#train$pickup_weekend <- ifelse(train$pickup_weekdays==1 | train$pickup_weekdays==7,"Weekend","not-Weekend")
train$pickup_datetime = ymd_hms(train$pickup_datetime)
train$date = date(train$pickup_datetime)
train$dropoff_hour = hour(train$dropoff_datetime)
train$weekday_num = ifelse(train$pickup_weekdays == 'Monday',1,
                           ifelse(train$pickup_weekdays == 'Tuesday',2,
                                  ifelse(train$pickup_weekdays == 'Wednesday',3,
                                         ifelse(train$pickup_weekdays == 'Thursday',4,
                                                ifelse(train$pickup_weekdays == 'Friday',5,
                                                       ifelse(train$pickup_weekdays == 'Saturday',6,7
                                                              ))))))


JFK_pickup = ((distHaversine(matrix(c(train$pickup_longitude, train$pickup_latitude), ncol = 2),matrix(c(-73.792542, 40.644588), ncol = 2))/1000) <= 5)

JFK_dropoff =  ((distHaversine(matrix(c(train$dropoff_longitude, train$dropoff_latitude), ncol = 2),
matrix(c(-73.792542, 40.644588), ncol = 2))/1000) <= 5)

lga_pickup = ((distHaversine(matrix(c(train$pickup_longitude, train$pickup_latitude), ncol = 2),
                            matrix(c(-73.874455, 40.776988), ncol = 2))/1000) <= 2)
lga_dropoff = ((distHaversine(matrix(c(train$dropoff_longitude, train$dropoff_latitude), ncol = 2),
                          matrix(c(-73.874455, 40.776988), ncol = 2))/1000) <= 2)                                                 




train = train %>% mutate(JFK_d = as.numeric(JFK_dropoff),JFK_p=as.numeric(JFK_pickup)
                         ,lga_d = as.numeric(lga_dropoff),lga_p = as.numeric(lga_pickup))



#weather
weather = fread("weather.csv")
weather$date = dmy(weather$date)
weather = weather %>% filter(weather$date <"2016-07-01")
weather$rain = as.numeric(ifelse(weather$precipitation == "T", "0.01", weather$precipitation))
weather$snow_fall = as.numeric(ifelse(weather$`snow fall` == "T","0.01",weather$`snow fall`))
weather$snow_dep = as.numeric(ifelse(weather$`snow depth` == "T","0.01",weather$`snow depth`))
weather$all_precip = weather$snow_fall + weather$rain
weather$snowed = (weather$snow_fall > 0 | weather$snow_dep > 0)
weather$rained = weather$rain>0
#weather$max_temp = weather$`maximum temperature`
#weather$min_temp = weather$`minimum temperature`
#weather1 = weather%>%select(date,rain,snow_fall,snow_dep,
                           #all_precip,snowed,rained,
                          # max_temp,min_temp)
weather1 = weather%>%select(date,snowed,rained
                            )
train = left_join(train, weather1,by = "date")





data = train
#################################cv

cv.error = function(train,fold){
  #temp = train %>% select(trip_duration,pickup_latitude,pickup_longitude, 
                         # dropoff_latitude,dropoff_longitude,pickup_hour,weekday_num)
  temp = train %>% select(trip_duration,pickup_latitude,pickup_longitude, 
                          dropoff_latitude,dropoff_longitude,pickup_hour,weekday_num,rained,snowed)
  
  dd = split(temp, sample(1:fold, nrow(temp), replace=T))
  
  
  rmse_list = c()
for(i in 1:fold){
    test2 = dd[[i]]
    train2 = data.frame()
    
    for(j in 1:fold){
      if(j!=i){
        train2 = rbind(train2,dd[[j]])
      }
    }
    knn1 = FNN::knn.reg(train = train2[,-1],test = test2[,-1],y = train2[,1],k = 20)
    curr_rmse = Metrics::rmse(knn1$pred,test2[,1])
    rmse_list = c(rmse_list,curr_rmse)
    }
  cverror = mean(rmse_list)
  
return(cverror)
  
  
}

s#cv_5_ppddhw = cv.error(train,10)
cv_10_ppdd = cv.error(train,10)
cv_10_ppddhw = cv.error(train,10)
cv_10_ppddhwj = cv.error(train,10)
cv_10_ppddhwjl = cv.error(train,10)

cv_10_ppddhwrs = cv.error(train,10)

########################baseline


appro = approxfun(train$trip_duration)
xx = sample(train$trip_duration,145663,prob = appro(train$trip_duration))
xx = as.data.frame(xx)


##
ggplot()+theme_classic()+geom_density(data = train,aes((trip_duration)),colour = 'darkblue')+xlim(c(0,5000))+geom_density(data = xx,aes(xx),colour = 'red')+xlim(c(0,5000))



##########





nrow_0.1 = 145663

random_list = c()


for(i in 1:10){
  pred_byrandom = sample(train$trip_duration,nrow_0.1,prob = appro(train$trip_duration))
  error_random = Metrics::rmse(pred_byrandom,test_trainset[,1])
  random_list = c(random_list,error_random)
}








  