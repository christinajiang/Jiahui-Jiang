rm(list = ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(geosphere)
library(corrplot)
library(Rmisc)
library('scales') # visualisation
library('grid') # visualisation
library('RColorBrewer') # visualisation
library("alluvial") # visualisation
library('readr') # input/output
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('leaflet') # maps
library('leaflet.extras') # maps
library('maps') # maps
library('xgboost') # modelling
library('caret') 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
train = fread("train.csv", header = TRUE)

train <- train %>%
  mutate(pickup_datetime = ymd_hms(pickup_datetime),
         dropoff_datetime = ymd_hms(dropoff_datetime),
         vendor_id = factor(vendor_id),
         passenger_count = factor(passenger_count))



pick_coord <- train %>%
  select(pickup_longitude, pickup_latitude)
drop_coord <- train %>%
  select(dropoff_longitude, dropoff_latitude)
train$dist <- distCosine(pick_coord, drop_coord)
##########data cleaning
###extreme data

#longer than a day
day_plus_trips <- train %>%
  filter(trip_duration > 24*3600)

day_plus_trips %>% select(pickup_datetime, dropoff_datetime)

#delete
index = which(train$trip_duration >24*3600)
train = train[-index,]


#close to 24 hours
day_trips <- train %>%
  filter(trip_duration < 24*3600 & trip_duration > 22*3600)

#delete 
index1 = which(train$trip_duration < 24*3600 & train$trip_duration> 22*3600)
train = train[-index1,]

###shorter than a few minutes
#zero dist
zero_dist <- train %>%
  filter(near(dist,0))
index2 = which(near(train$dist,0))
train = train[-index2,]


###shorter than 30 seconds
index3 = which(train$trip_duration <= 30)
train = train[-index3,]


#write.csv(train, "clean_data.csv")

train$wday = weekdays(as.Date(train$pickup_datetime))
train$hour = hour(train$pickup_datetime)
train$speed =  train$dist/train$trip_duration*3.6

###########################
p1 <- train %>%
  group_by(wday, vendor_id) %>%
  summarise(median_speed = median(speed)) %>%
  ggplot(aes(wday, median_speed, color = "red")) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Median speed [km/h]")





p2 <- train %>%
  group_by(hour, vendor_id) %>%
  summarise(median_speed = median(speed)) %>%
  ggplot(aes(hour, median_speed, color = "green")) +
  geom_smooth(method = "loess", span = 1/2) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Median speed [km/h]") +
  theme(legend.position = "none")

##########

p3 <- train %>%
  group_by(wday, hour) %>%
  summarise(median_speed = median(speed)) %>%
  ggplot(aes(hour, wday, fill = median_speed)) +
  geom_tile() +
  labs(x = "Hour of the day", y = "Day of the week") +
  scale_fill_distiller(palette = "Spectral")

layout <- matrix(c(1,2,3,3),2,2,byrow=TRUE)
multiplot(p1, p2, p3, layout=layout)


p4 <- train %>%
  ggplot(aes(pickup_datetime)) +
  geom_histogram(fill = "red", bins = 120) +
  labs(x = "Pickup dates")

p5 <- train %>%
  ggplot(aes(dropoff_datetime)) +
  geom_histogram(fill = "green", bins = 120) +
  labs(x = "Dropoff dates")

layout1 <- matrix(c(4,5),5,4,byrow=FALSE)
multiplot(p4, p5, layout=layout1)

weather = as.tibble(fread("weather.csv"))
weather <- weather %>%
  mutate(date = dmy(date),
         rain = as.numeric(ifelse(precipitation == "T", "0.01", precipitation)),
         s_fall = as.numeric(ifelse(`snow fall` == "T", "0.01", `snow fall`)),
         s_depth = as.numeric(ifelse(`snow depth` == "T", "0.01", `snow depth`)),
         all_precip = s_fall + rain,
         has_snow = (s_fall > 0) | (s_depth > 0),
         has_rain = rain > 0,
         max_temp = `maximum temperature`,
         min_temp = `minimum temperature`)
foo <- weather %>%
  select(date, rain, s_fall, all_precip, has_snow, has_rain, s_depth, max_temp, min_temp)
train$date = date(train$pickup_datetime)
train <- left_join(train, foo, by = "date")
p6 <- train %>%
  group_by(date) %>%
  count() %>%
  ggplot(aes(date,n/1e3)) +
  geom_line(size = 1.5, color = "red") +
  labs(x = "", y = "Kilo trips per day")
p7 <- train %>%
  group_by(date) %>%
  summarise(trips = n(),
            snow_fall = mean(s_fall),
            rain_fall = mean(rain),
            all_precip = mean(all_precip)) %>%
  ggplot(aes(date, snow_fall)) +
  geom_line(color = "green", size = 1.5) +
  labs(x = "", y = "Snowfall") +
  scale_y_sqrt() +
  scale_x_date(limits = ymd(c("2015-12-28", "2016-06-30")))

p8 <- train %>%
  group_by(date) %>%
  summarise(trips = n(),
            snow_depth = mean(s_depth)) %>%
  ggplot(aes(date, snow_depth)) +
  geom_line(color = "blue", size = 1.5) +
  labs(x = "", y = "Snow depth") +
  scale_y_sqrt() +
  scale_x_date(limits = ymd(c("2015-12-29", "2016-06-30")))

p9 <- train %>%
  group_by(date) %>%
  summarise(median_speed = median(speed)) %>%
  ggplot(aes(date, median_speed)) +
  geom_line(color = "black", size = 1.5) +
  labs(x = "Date", y = "Median speed")

layout2 <- matrix(c(6,7,8,9),9,6,byrow=FALSE)
multiplot(p6, p7, p8, p9, layout=layout2)
