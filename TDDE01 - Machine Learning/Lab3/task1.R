library(ggplot2)
library(geosphere)
set.seed(1234567890)

setwd("/Users/christoffergardin/Desktop/TDDE01/Lab3")

stations <- read.csv("stations.csv", fileEncoding="latin1")
temps <- read.csv("temps50k.csv", fileEncoding="latin1")
st <- merge(stations,temps,by="station_number")
h_distance <- 3e4# These three values are up to the students
h_date <- 7.5
h_time <- 3
a <- 59.334591 # The point to predict (up to the students) #Coordinates?
b <- 18.063240
date <- "1998-08-15" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")
temp <- vector(length=length(times))

N = length(st$air_temperature)
#plot(temp, type="o")

#Function to remove measurements that were taken after given date.
filter_dates <- function(){
  numeric_date <- as.numeric(as.POSIXct(date))
  numeric_dates <- as.numeric(as.POSIXct(st$date))
  fd <- replicate(N, numeric_date) - numeric_dates #Calculate the difference between dates, values that are greater are after the specified date and is what we are after.
  filtered_indexes <- which(fd > 0) #Get the indexex for the values which difference is bigger than one.
  return(st[c(filtered_indexes),]) #Return the specified values. 
}


distances <- function(st){
  ## Distance, physical, between station and sample point
  N <- length(st$air_temperature)
  dist = replicate(N,0)
  for (i in 1:N) {
    dist[i] =  distHaversine(c(st$latitude[i], st$longitude[i]), c(a,b)) #Physical distance from a station to the point of interest. 
  }
  #Distance between the day a temperature measurement was made and the day of interest. 
  day_dist = replicate(N,0)
  date_numeric <- as.numeric(strftime(date,"%j")) #Gives the numeric value on which day it is during the year.
  days_numeric <- as.numeric(strftime(st$date, "%j"))
  days_dist = replicate(N, date_numeric) - days_numeric #Compare the the days
  
  #Distance between the hour of the day a temperature measurement was made and the day of interest.
  hours_numeric <- as.numeric(vapply(strsplit(unlist(st$time),":"), `[`, 1, FUN.VALUE=character(1))) #Change from 12:00:00 TO 12 etc.
  hour_diff = 4 - hours_numeric #Ex: 4 - 18 = -14
  #If any x is less than -12 than new value is -24 - x. Ex: -14 -> -24 + 14 = -10
  hour_diff = apply(as.matrix(hour_diff), 1, function(x) {ifelse(any(x < -12), -24 - x, x)}) #1 indicate row
  return(data.frame(phys=dist, days=days_dist,time=hour_diff))
}

k <- function(d){
  return(exp(-norm(as.matrix(d))^2)) #We have already calculated the difference, so this RBF kernel gives a value on how close they are from each other, so a large difference will result in value close to zero. 
}

kernel_add <- function(dist, days_dist, hour_diff){
  k_dist <- vapply(dist/h_distance, k, FUN.VALUE = numeric(1)) #h_distance is the smoothing paramete.
  k_days <- vapply(days_dist/h_date, k, FUN.VALUE = numeric(1))
  k_hour <- NULL
  #vapply(hour_diff[[times[1]]]/h_time, k, FUN.VALUE = numeric(1))
  r <- replicate(length(times), 0)
  for(i in 1:length(times)){
    hours <- hour_diff + 2*(i-1)
    hours <-apply(as.matrix(hours), 1, function(x) {ifelse(any(abs(x) > 12), 24 - abs(x), x)})
    k_hour <- vapply(unlist(hours)/h_time, k, FUN.VALUE = numeric(1))
    k_tot = (k_hour + k_days + k_dist)/3
    res <- sum(k_tot*(f_data$air_temperature)) / sum(k_tot)#So here we get 
    r[i] <- res
  }
  return(r)
}

kernel_mult <- function(dist, days_dist, hour_diff){
  k_dist <- vapply(dist/h_distance, k, FUN.VALUE = numeric(1))
  k_days <- vapply(days_dist/h_date, k, FUN.VALUE = numeric(1))
  k_hour <- NULL
  #vapply(hour_diff[[times[1]]]/h_time, k, FUN.VALUE = numeric(1))
  r <- replicate(length(times), 0)
  for(i in 1:length(times)){
    hours <- hour_diff + 2*(i-1)
    hours <-apply(as.matrix(hours), 1, function(x) {ifelse(any(abs(x) > 12), 24 - abs(x), x)})
    k_hour <- vapply(unlist(hours)/h_time, k, FUN.VALUE = numeric(1))
    k_tot = k_hour * k_days * k_dist
    res <- sum(k_tot*(f_data$air_temperature)) / sum(k_tot) #So, basically we ex. ktot = 0.9 will give a 
    #tempature that is similiar and 0.1 will give a non similiar temperature.
    #Then summing all the scaled temperatures and divide it by the total ktot.
    r[i] <- res
  }
  return(r)
}

f_data <- filter_dates()
d <- distances(f_data)
kr_add <- kernel_add(unlist(d[1]), unlist(d[2]), d[3])
kr_mult <- kernel_mult(unlist(d[1]), unlist(d[2]), d[3])
print(kr_add)
print(kr_mult)
par(mfrow=c(2,1)) 
plot(kr_add, type='o', main="Additive",
     xlab="Time",
     ylab="Temperature") 
plot(kr_mult, type='o',main="Multiplicative",
     xlab="Time",
     ylab="Temperature") 

k_dist <- vapply(unlist(d[1])/h_distance, k, FUN.VALUE = numeric(1))
k_days <- vapply(unlist(d[2])/h_date, k, FUN.VALUE = numeric(1))
k_hour <- vapply((unlist(d[3]) + 18)/h_time, k, FUN.VALUE = numeric(1))
k_tot = (k_hour * k_days * k_dist)

sum(k_hour)
sum(k_dist)
sum(k_days)


par(mfrow=c(3,1)) 
x <- -5:5
plot(x, exp(-(x/h_time)^2), main="h = 3",
     xlab="Distance",
     ylab="k",
     type='l')
x <- -15:15
plot(x, exp(-(x/h_date)^2), main="h = 7.5",
     xlab="Distance",
     ylab="k",
     type='l')
x <- -75000:75000
plot(x, exp(-(x/h_distance)^2), main="h = 30000",
     xlab="Distance",
     ylab="k",
     type='l')

