#Changing Working Directory
setwd("E:/CabMe")

test<-read.csv("E:/CabMe/green_tripdata_2016-06.csv",header=TRUE)


#1404726
#Installing Plyr
install.packages("plyr")
library("plyr") 

#To add a column with a serial number
test$ID<-seq.int(nrow(test))

#To get only Date
unique(as.Date(test$lpep_pickup_datetime))

#Creating one column with start and end pickup date only
test$sdate<-as.Date(test$lpep_pickup_datetime)
test$edate<-as.Date(test$lpep_pickup_datetime)

#Counting the number of taxi rides on each day 
day_count<-aggregate(test$ID ~test$sdate, data = test, FUN = length)
write.csv(day_count, file = "day_count.csv", row.names = FALSE)

#Counting the number of days taken by each taxi ride
test$diff_days<-(as.POSIXlt(test$Lpep_dropoff_datetime)-as.POSIXlt(test$lpep_pickup_datetime))/(60*60*24)
test$diff_hours<-(as.POSIXlt(test$Lpep_dropoff_datetime)-as.POSIXlt(test$lpep_pickup_datetime))/(60*60)
test$diff_min<-(as.POSIXlt(test$Lpep_dropoff_datetime)-as.POSIXlt(test$lpep_pickup_datetime))/(60)
test2<-subset(test, select=c("ID", "lpep_pickup_datetime","Lpep_dropoff_datetime","diff_days","diff_hours"))
write.csv(test2, file = "day_diff.csv", row.names = FALSE)

#Counting the number of taxi drops taking place in 24 hours of the day
install.packages("lubridate")
install.packages("stringi")
install.packages("stringr")
library(lubridate)
a <- ymd_hms(test$Lpep_dropoff_datetime)
test$drop_hour<-hour(a)
drop_count_by_hr<-aggregate(test$ID ~test$drop_hour, data = test, FUN = length)
write.csv(drop_count_by_hr, file = "drop_hour_count.csv", row.names = FALSE)

#Counting the number of taxi picks taking place in 24 hours of the day
install.packages("lubridate")
install.packages("stringi")
install.packages("stringr")
library(lubridate)
b <- ymd_hms(test$lpep_pickup_datetime)
test$pick_hour<-hour(b)
pick_count_by_hr<-aggregate(test$ID ~test$pick_hour, data = test, FUN = length)
write.csv(pick_count_by_hr, file = "duration_hour_count.csv", row.names = FALSE)


#Counting the number of trips taking place in various durations.(0-1 hours , 1-2 hours etc)
a <- ymd_hms(test2$lpep_pickup_datetime)
test2$pick_hour<-hour(a)

b <- ymd_hms(test2$Lpep_dropoff_datetime)
test2$drop_hour<-hour(b)

test2$hr_0<-0
test2$hr_1<-0
test2$hr_2<-0
test2$hr_3<-0
test2$hr_4<-0
test2$hr_5<-0
test2$hr_6<-0
test2$hr_7<-0
test2$hr_8<-0
test2$hr_9<-0
test2$hr_10<-0
test2$hr_11<-0
test2$hr_12<-0
test2$hr_13<-0
test2$hr_14<-0
test2$hr_15<-0
test2$hr_16<-0
test2$hr_17<-0
test2$hr_18<-0
test2$hr_19<-0
test2$hr_20<-0
test2$hr_21<-0
test2$hr_22<-0
test2$hr_23<-0
#Computing trips between 2 and 3:those started between 2:00:00 and 2:59:59
if(day(a)==day(b))
{
  if((hour(a)>=2&min(a)>=0&second(a)>=0)||(hour(a)<=2&min(a)<=59&second(a)<=59))
  {
    test2$hr_2<-1
  }else if((hour(a)<=2&min(a)>=0&second(a)<=0)||(hour(a)<=2&min(a)<=59&second(a)<=59))
  {
    test2$hr_2<-1}else
    { test2$hr_2<-1}
}else
{
  
}



#Counting the number of trips by each unique vendor avaiable
vendor_count<-aggregate(test$ID ~test$VendorID, data = test, FUN = length)
write.csv(vendor_count, file = "vendor_count.csv", row.names = FALSE)

#Location Analytics in R
install.packages('ggmap')
install.packages('Rcpp')
install.packages('sp')
install.packages('raster')

library(ggmap)
library(Rcpp)
library(raster)   

  #Plotting pick up point in R
plot(test$Pickup_longitude,test$Pickup_latitude,xlab="longitude",ylab="latitude")
  #Plotting drop off point in R
plot(test$Dropoff_longitude,test$Dropoff_latitude,xlab="longitude",ylab="latitude")

  #Plotting maps in R
maps<-qmap("New York", zoom = 10)
maps+             
       geom_point(aes(x = Pickup_longitude, y = Pickup_latitude), data = test)

maps+             
  geom_point(aes(x = Dropoff_longitude, y = Dropoff_latitude), data = test)

#Plotting on USA Map                               
USA<-getData('GADM', country='USA', level=1)  
plot(USA)  
points(test$Pickup_longitude,test$Pickup_latitude,col="red",pch=16)

#Plotting on World map
install.packages("maps")
install.packages("ggmap")
install.packages("maptools")

library("ggmap")
library(maptools)
library(maps)

map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
