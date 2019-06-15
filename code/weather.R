library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
train = fread("train.csv")
train$pickup_datetime = ymd_hms(train$pickup_datetime)
train$date = date(train$pickup_datetime)
weather = fread("weather.csv")
weather$date = dmy(weather$date)
weather = weather %>% filter(weather$date <"2016-07-01")
weather$rain = as.numeric(ifelse(weather$precipitation == "T", "0.01", weather$precipitation))
weather$snow_fall = as.numeric(ifelse(weather$`snow fall` == "T","0.01",weather$`snow fall`))
weather$snow_dep = as.numeric(ifelse(weather$`snow depth` == "T","0.01",weather$`snow depth`))
weather$all_precip = weather$snow_fall + weather$rain
weather$snowed = (weather$snow_fall > 0 | weather$snow_dep > 0)
weather$rained = weather$rain>0
weather$max_temp = weather$`maximum temperature`
weather$min_temp = weather$`minimum temperature`
weather1 = weather%>%select(date,rain,snow_fall,snow_dep,
                           all_precip,snowed,rained,
                           max_temp,min_temp)
train = left_join(train, weather1,by = "date")
