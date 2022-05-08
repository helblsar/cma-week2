# Exercise 2
# Author: Sarah Helbling
# Module: Patterns and trends in environmental data
# ---------------------------------------------------

# Task 1 - import data
## Load libraries
library (readr)
library(dplyr)
library(ggplot2)
library(sf)
library(terra)
library(lubridate)

## Import csv 
wildschwein<-read_delim("wildschwein_BE_2056.csv", ",")
wildschwein<-st_as_sf(wildschwein, 
                      coords=c("E","N"), 
                      crs=2056, 
                      remove=FALSE)

# ---------------------------------------------------
# Task 2 - get an overview
## calculate time difference between subsequent rows and store it in a new column
wildschwein<- group_by (wildschwein, TierID)
wildschwein<- mutate(wildschwein, 
                     timelag=
                       as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC), 
                       unit="secs"))

## how many individuals tracked
unique(wildschwein$TierID)

## for how long were the individuals tracked? Are there gaps?
ggplot(wildschwein, aes(x=DatetimeUTC, y=TierID))+
  geom_line()

tracked_from_until<-st_drop_geometry(summarise(wildschwein,
                           start=min(DatetimeUTC),
                           end= max(DatetimeUTC),
                           total_timelag= difftime(end, start)
                  ))

## were all individuals tracked concurrently or sequentially?
## what is the temporal sampling interval between locations
#I AM CONFUSED ABOUT THESE QUESTION
wildschwein$timelag[wildschwein$timelag<0]<-NA
ggplot(wildschwein, aes(x=DatetimeUTC, y=timelag, color=TierID))+
  geom_line()+
  geom_point()
#I mean, the first half of the plot looks similar enough to the one 
#in the task, but I don't really know how to interpret it...

# ---------------------------------------------------
# Task 3 - deriving movement parameters
wildschwein<- mutate(wildschwein, 
                     steplength = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2),
                     speed_ms = steplength/timelag)
# ---------------------------------------------------
# Task 4 - Cross-scale movement analysis
caro<-read_delim("caro60.csv", ",")
caro<-st_as_sf(caro, 
                    coords=c("E","N"), 
                    crs=2056, 
                    remove=FALSE)

## reduce granularity 
#(how could this be done more efficiently)
caro_3<-slice(caro, seq(1,(nrow(caro)), by=3))
caro_6<-slice(caro,seq(1,(nrow(caro)), by=6))
caro_9<-slice(caro,seq(1,(nrow(caro)), by=9))

caro$sampling_interval <- ("1 min.")
caro_3$sampling_interval <- ("3 min.")
caro_6$sampling_interval <- ("6 min.")
caro_9$sampling_interval <- ("9 min.")

caros <-rbind(caro, caro_3, caro_6, caro_9)

## calculate timelag, steplength and speed
caros <- mutate(caros,
                 timelag=as.numeric
                 (difftime(lead(DatetimeUTC), DatetimeUTC), 
                   unit="secs"),
                 steplength = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2),
                 speed_ms = steplength/timelag)

## compare speeds with line plot
ggplot(data = caros) +
  geom_line(aes(x = DatetimeUTC, y = speed_ms, color = sampling_interval)) +
  labs(y = "Speed [m/s]", x = "Time") +
  ggtitle("Comparing derived speed at different sampling intervals") 


## visualize trajectories - Comparing original with re-sampled data
ggplot(data = subset(caros, caros$sampling_interval == "1 min."), 
       aes(x = E, y = N, color = sampling_interval)) +
  geom_path(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  geom_path(data = subset(caros, caros$sampling_interval == "3 min."), alpha = 0.8) +
  geom_point(data = subset(caros, caros$sampling_interval == "3 min."), alpha = 0.8) + 
  ggtitle("Comparing original- with 3 minutes-resampled data") +
  theme_minimal()
ggplot(data = subset(caros, caros$sampling_interval == "1 min."), 
       aes(x = E, y = N, color = sampling_interval)) +
  geom_path(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  geom_path(data = subset(caros, caros$sampling_interval == "6 min."), alpha = 0.8) +
  geom_point(data = subset(caros, caros$sampling_interval == "6 min."), alpha = 0.8) + 
  ggtitle("Comparing original- with 6 minutes-resampled data") +
  theme_minimal()
ggplot(data = subset(caros, caros$sampling_interval == "1 min."), 
       aes(x = E, y = N, color = sampling_interval)) +
  geom_path(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  geom_path(data = subset(caros, caros$sampling_interval == "9 min."), alpha = 0.8) +
  geom_point(data = subset(caros, caros$sampling_interval == "9 min."), alpha = 0.8) + 
  ggtitle("Comparing original- with 9 minutes-resampled data") +
  theme_minimal()

# ---------------------------------------------------
# Task 5 - deriving movement parameters

library(zoo)

caro_1<-caro[1:200,]
caro_1 <- mutate(caro_1, rm=speed_ms,
                 window_size="1")
caro_3 <-mutate(caro_1, rm=rollmean(caro_1$speed_ms, k=3, fill=NA, align="left"),
                window_size="3")
caro_6 <-mutate(caro_1, rm=rollmean(caro_1$speed_ms, k=6, fill=NA, align="left"),
                window_size="6")
caro_9 <-mutate(caro_1, rm=rollmean(caro_1$speed_ms, k=9, fill=NA, align="left"),
                window_size="9")
caro_rm<-rbind(caro_1,caro_3,caro_6,caro_9)
                 
ggplot(caro_rm)+
  geom_line(aes(x=DatetimeUTC, y=rm, color=window_size))+
  ggtitle("Comparing derived speed at different moving window sizes")+
  theme_minimal()
