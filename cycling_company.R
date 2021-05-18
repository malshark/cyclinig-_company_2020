##title: "cycling_company"
## ASK phase
Divvy_Trips_2020_Q1 <- read.csv("D:/userdata/malshark/My Documents/Divvy/Divvy_Trips_2020_Q1.csv") ## Import first Q1
head(Divvy_Trips_2020_Q1) ## check table
files <- list.files(path = "D:/userdata/malshark/My Documents/Divvy", pattern = "*.csv", full.names = T) ## combine month (4,5,6) 
tbl2 <- sapply(files, read_csv, simplify=FALSE) %>% bind_rows(.id = "id") ## create new df
write.csv(tbl2, 'Divvy_Trips_2020_Q2') ## Save output file
head (Divvy_Trips_2020_Q2) ## New file after combine
files <- list.files(path = "D:/userdata/malshark/My Documents/Divvy", pattern = "*.csv", full.names = T) ##combine month (7,8,9) combin
tbl2 <- sapply(files, read_csv, simplify=FALSE) %>% bind_rows(.id = "id") ## create new df
write.csv(tbl2,'Divvy_Trips_2020_Q3.csv') ## check table of new df
glimpse(Divvy_Trips_2020_Q3) ## New file after combine (202007-divvy-tripdata, 202008-divvy-tripdata,202009-divvy-tripdata)
files <- list.files(path = "D:/userdata/malshark/My Documents/Divvy", pattern = "*.csv", full.names = T)
tbl3 <- sapply(files, read_csv, simplify=FALSE) %>% bind_rows(.id = "id")
write.csv(tbl3,'Divvy_Trips_2020_M10M11.csv')
limpse(Divvy_Trips_2020_M10M11) ## New file after combine (202010-divvy-tripdata, 202011-divvy-tripdata)
## Cleaning procedure
Divvy_Trips_Q1 <- Divvy_Trips_2020_Q1 [,c(1,3,4,6,8,13)] ## Create new df with only need it columns.
install.packages("lubridate")
library (lubridate)
suppressPackageStartupMessages({library(lubridate)}) ## to avoid NA while transform from (chr) to (dttm)
Divvy_Trips_Q1 $started_at <- ymd_hms(Divvy_Trips_Q1 $started_at) ## Transform to dttm class.
Divvy_Trips_Q1 $ended_at <- ymd_hms(Divvy_Trips_Q1 $ended_at) ## Transform to dttm class.
##Transform the last file from df source (202012.divvy.tripdata)
Divvy_Trips_M12 <- `202012.divvy.tripdata` [,c(1,3,4,6,8,13)] ## This file will join Divvy_Trips_10M11##
glimpse (Divvy_Trips_M12)
Divvy_Trips_M12 $started_at <- ymd_hms(Divvy_Trips_M12 $started_at)
Divvy_Trips_M12 $ended_at <- ymd_hms(Divvy_Trips_M12 $ended_at)
glimpse(Divvy_Trips_M12)
write.csv(Divvy_Trips_M12, 'Divvy_trips_M12.csv') ## Save file after transformation complete ##
##Transform and create Q4
Divvy_Trips_M10M11 <- Divvy_Trips_2020_M10M11 [,c(3,5,6,8,10,15)] ## New df ##
Divvy_Trips_M10M11 $started_at <- ymd_hms(Divvy_Trips_M10M11 $started_at) ## Transform from (chr) to (dttm) ##
Divvy_Trips_M10M11 $ended_at <- ymd_hms(Divvy_Trips_M10M11 $ended_at)   ## Transform from (chr) to (dttm) ##
glimpse(Divvy_Trips_M10M11)
write.csv(Divvy_Trips_M10M11,'Divvy_trips_M10M11.csv')  ## save new df ##
##I will combine files (Divvy_trips_M10M11.csv, Divvy_trips_M12) to be one file (Divvy_Trips_Q4)
##I notice that Divvy_trips_M12 have two columns (start_station_id & end_station_id) in (chr) which contradicts with other columns on other files which are in (int) mode so I convert both columns.
Divvy_Trips_M12 $start_station_id <- as.integer (Divvy_Trips_M12 $start_station_id) ## Transform this column to be same as other columns##
Divvy_Trips_M12 $end_station_id <- as.integer (Divvy_Trips_M12 $end_station_id)
glimpse(Divvy_Trips_M12) ## confirm that both columns in the right class (int).##
files <- list.files(path = "D:/userdata/malshark/My Documents/Divvy", pattern = "*.csv", full.names = T) ## Join Divvy_Trips_M10M11 and Divvy_Trips_M12 in oe file ##tbl5 <- sapply(files, read_csv, simplify=FALSE) %>% bind_rows(.id = "id") ## create new df ##
write.csv(tbl5,'Divvy_trips_q4.csv') ## Save the new df ##
glimpse(Divvy_trips_q4)  ## confirm new df and keep only columns need to analysis ##
##I will Join all file under one file (Divvy_Trips_2020)
files <- list.files(path = "D:/userdata/malshark/My Documents/Divvy", pattern = "*.csv", full.names = T)
tbl <- sapply(files, read_csv, simplify=FALSE) %>% bind_rows(.id = "id")
write.csv(table, 'Divvy_trips_2020.csv')
Divvy_Trips_2020 $ ride_length <- difftime(Divvy_Trips_2020 $ end_time,Divvy_Trips_2020 $ start_time) # Add a "ride_length" calculation to all_trips (in seconds)
head(Divvy_trips_2020)  #See the first 6 rows of data frame.
tail(Divvy_trips_2020)
str(Divvy_trips_2020)  #See list of columns and data types (numeric, character, etc)
summary(Divvy_trips_2020)  #Statistical summary of data. Mainly for numerics.
Divvy_Trips_2020_v4 <- Divvy_trips_2020[(Divvy_trips_2020$ride_length<0),] ## to show a table for all ride_length < 0
Divvy_Trips_2020_v4 <- Divvy_trips_2020[!(Divvy_trips_2020$ride_length<0),]
summary(Divvy_Trips_2020_v4$ride_length)
median(Divvy_Trips_2020_v4$ride_length) #midpoint number in the ascending array of ride lengths
max(Divvy_Trips_2020_v4$ride_length) #longest ride
min(Divvy_Trips_2020_v4$ride_length) #shortest ride
Divvy_Trips_2020_v4$day_of_week <- format(as.Date(Divvy_Trips_2020_v4$start_time), "%A")## To add new column
aggregate(Divvy_Trips_2020_v4$ride_length ~ Divvy_Trips_2020_v4$user_type + Divvy_Trips_2020_v4$day_of_week, FUN = mean)
Divvy_Trips_2020_v4$day_of_week <- ordered(Divvy_Trips_2020_v4$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) # Notice that the days of the week are out of order.
aggregate(Divvy_Trips_2020_v4$ride_length ~ Divvy_Trips_2020_v4$user_type + Divvy_Trips_2020_v4$day_of_week, FUN = mean)
##Analyze ridership data by type and weekday
Divvy_Trips_2020_v4 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(user_type, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(user_type, weekday)								# sorts
##Let's visualize the number of rides by rider type
Divvy_Trips_2020_v4 %>% 
  mutate(weekday = wday(start_time, label = TRUE)) %>% 
  group_by(user_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(user_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = user_type)) +
  geom_col(position = "dodge")

##Let's create a visualization for average duration
Divvy_Trips_2020_v4 %>% mutate(weekday = wday(start_time, label = TRUE)) %>% group_by(user_type, weekday) %>% summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% arrange(user_type, weekday)  %>% ggplot(aes(x = weekday, y = average_duration, fill = user_type)) +geom_col(position = "dodge")

counts <- aggregate(Divvy_Trips_2020_v4$ride_length ~ Divvy_Trips_2020_v4$ user_type + Divvy_Trips_2020_v4$day_of_week, FUN = mean)

counts















