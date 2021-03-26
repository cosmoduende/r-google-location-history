
# REQUIRED LIBRARIES

library(dplyr)
library(lubridate)
library(ggplot2)
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(kableExtra)

# READING JSON DATA

myHistory <- fromJSON("Takeout/Location History/Location History.json")

# EXTRACT LOCATION DATAFRAME

myData <- myHistory$locations

# NA'S IN DATAFRAME

lostData = t(t(sapply(myData,function(x)(sum(is.na(x))))))
colnames(lostData)<-c("NAs")
kable(lostData,caption="Número de NA's en Dataframe") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F )

# CLEANING AND TRANSFORMING DATA

myData <- myData %>% 
  filter(activity!="NULL")

myData <- myData %>% 
  mutate(time  = as_datetime(as.numeric(myData$timestampMs)/1000),
                    date = date(time),
                    hour  = paste(hour(time),minute(time),sep=":"),
                    week = isoweek(time),
                    month = paste(month(time)),
                    year = isoyear(time),
                    latitude = latitudeE7/1e7,
                    longitude= longitudeE7/1e7) %>%
  select(-timestampMs,-latitudeE7,-longitudeE7,-time)

timestamp <- as.character(sapply(myData$activity, function(x) (x[[1]][[1]])))

# EXTRACT FIRST ACTIVITY HIGHEST CONFIDENCE 

myActivity <-(sapply(myData$activity, function(x) (x[[2]][[1]][1])))
myActivity <- sapply(myActivity,function(x) (x[[1]][1]))
myActivity <- unlist(myActivity)

myData <- myData %>% 
  mutate(activityTime = as_datetime(as.numeric(timestamp)/1000),
                    activityDate = date(activityTime),
                    activityHour  = hour(activityTime),
                    activityMonth = paste(month(activityTime)),
                    activityHour = paste(activityHour,minute(activityTime),sep=":"),
                    activityWeekday = wday(activityTime, label=T,week_start=1, abbr = F),
                    mainActivity = myActivity)

# DATA COLLECTED BY YEAR

kable(myData %>% group_by(year) %>% 
        summarise(n=n()),col.names=c("Year","No. Observations"), 
      align=c('c','r'),caption="Data collected by year") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed","responsive"), full_width = F )

# TRACKS PER WEEK BY YEAR

myData %>%  group_by(week,year) %>% 
  summarise(n = n()) %>%
  ggplot( aes(x=week, y=n)) +
  geom_bar(stat="identity", aes(fill = n)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  facet_grid(facets = year ~ .) +
  scale_x_continuous(breaks = c(1:54)) +
  labs(x = "Week", y = "Count", 
       title="How many locations have Google tracked about me?", 
       subtitle = "Tracks per week by year") +
  theme_bw()

# DATA POINTS COLLECTED BY GOOGLE 

myData$monthYear <- strftime(myData$date,"%Y%m")
pointsDay <- data.frame(table(myData$date), group = "day")
pointsMonth <- data.frame(table(myData$monthYear), group = "month")
pointsYear <- data.frame(table(myData$year), group = "year")

points <- rbind(pointsDay[, -1], pointsMonth[, -1], pointsYear[, -1])

ggplot(points, aes(x = group, y = Freq)) + 
  geom_point(position = position_jitter(width = 0.2), alpha = 0.3) + 
  geom_boxplot(aes(color = group), size = 1, outlier.colour = NA) + 
  facet_grid(group ~ ., scales = "free") + 
  labs(x = " ", y = "Count", 
       title="How many data points did Google collect about me?", 
       subtitle = "Tracking per day, month and year") +
  theme_bw()

# ACCURACY LEVEL

myData$accuracyLevel <- ifelse(myData$accuracy<100, "high", ifelse(myData$accuracy<10000, "middle", "low"))
myData$accuracyLevel <- factor(myData$accuracyLevel, levels = c("high", "middle", "low"))

myData$accuracy <-as.numeric(unlist(myData$accuracy))

ggplot(myData, aes(x = as.numeric(accuracy), fill = accuracyLevel)) + 
  geom_histogram() + 
  labs(x = "Accuracy level in meters", y = "Count", 
         title="How accurate is the Google location data collected about me?", 
       subtitle = "Accuracy of location points") +
  theme_bw()

# ALTITUDE VARIATION

myData %>% 
  filter(!is.na(altitude)) %>% arrange(date) %>%
  ggplot(aes(x=as.Date(date),y=altitude, color=altitude)) +
  geom_point() +
  theme_bw() + 
  labs(x = "Date", y = "Height above sea level", 
       title="Altitude variations that Google collected about me", 
       subtitle = "Altitude variation by month") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_date(breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 month"),
               minor_breaks = function(x) seq.Date(from = min(x), to = max(x), by = "1 month"))

# TRACKING ACTIVITIES

myData %>%  
  ggplot(aes(x=(mainActivity), group=(mainActivity))) +
  geom_bar(aes(fill=..count..)) + 
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(x = "Activity", y = "Count", 
         title="What main activities does Google know that I do?", 
       subtitle = "Activity type collected by Google") +
  theme_bw()

# ACTIVITY BY TYPE AND WEEKDAY

myData %>% 
  select(mainActivity,activityWeekday) %>%
  filter((!is.na(mainActivity)) & (!is.na(activityWeekday))) %>%
  ggplot(aes(x=mainActivity)) + 
  geom_bar(aes(fill=..count..)) + 
  scale_fill_gradient(low = "yellow", high = "red") +
  facet_wrap(~activityWeekday, scales = 'free', ncol=4) +
  labs(x = "Activity", y = "Count", 
       title="What main activities does Google know that I do", 
       subtitle = "Activity type collected by Google per week day") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# MAP OF LOCATIONS COLLECTED BY GOOGLE

leaflet(myData) %>% 
  addTiles() %>%
  addWebGLHeatmap(size=10,units='px')

# HEATMAP OF LOCATIONS COLLECTED BY GOOGLE

myMap = leaflet(myData) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%  
  addHeatmap(lng = ~longitude, lat = ~latitude, group = "HeatMap", blur = 20, max = 0.01, radius = 15) %>%
  addMarkers(data = myData, ~longitude, ~latitude, clusterOptions = markerClusterOptions(), group = "Points")

myMap

