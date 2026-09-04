####revised script for adding 2025 prey survey data to the previously cleaned historical data#######


#load needed libraries 
library( tidyverse)
library(ggplot2)
library(lubridate)#####import the new data from 2024 to bind with the historical data #####

#import historical data detection level
hist_data <- read.csv("C:/Users/beer.kara/Dropbox/GYRF_program/2025 Files/2025 Data Cleaning and Final Versions/Clean versions/Prey Surveys/2025-08-26_all_detection_data.csv")


#import the observations for 2026
det_level_2026_raw <- read.csv("C:/Users/beer.kara/Dropbox/GYRF_program/2026 Files/Survey123 raw files/Ptarmigan surveys/2026_prey_det_level.csv")

#check
head(det_level_2026_raw)

#simplify 
det_level_2026_raw <- det_level_2026_raw %>%
  select(ObjectID, "GlobalID" = ParentGlobalID, obs_time, species, males, females, unknowns, detection_type, nearest_mile_marker, bearing, distance, waypoint, device, notes) 

#check for typos and issues - hopefully very minor! 
#obs_time 
unique(det_level_2026_raw$obs_time)#beautiful 

#species 
unique(det_level_2026_raw$species)#beautiful

#males
unique(det_level_2026_raw$males)#beautiful

#females
unique(det_level_2026_raw$females)#beautiful

#unknowns
unique(det_level_2026_raw$unknowns)#beautiful

#detection type 
unique(det_level_2026_raw$detection_type)#need to match existing data

#changing detection type to abbreviations 
det_level_2026_raw <- det_level_2026_raw %>%
  mutate(detection_type = ifelse(detection_type =="Visual", "V", detection_type ))%>%
  mutate(detection_type = ifelse(detection_type =="Audio", "A", detection_type ))

#check again 
unique(det_level_2026_raw$detection_type)#good now 

#nearest mm 
unique(det_level_2026_raw$nearest_mile_marker) #good

#bearing 
unique(det_level_2026_raw$bearing)

nines <- det_level_2026_raw %>%
  filter(bearing < 0 )# all are 99 besides -9 - going to assume that is meant to be NA

#turn -99s into NAs
det_level_2026_raw <- det_level_2026_raw %>%
  mutate(bearing = ifelse(bearing == -99, NA, bearing))

#turn 360's into 0
det_level_2026_raw <- det_level_2026_raw %>%
  mutate(bearing = ifelse(bearing == 360, 0, bearing))

#check again 
unique(det_level_2026_raw$bearing)#looks good 

#distance 
unique(det_level_2026_raw$distance)#seems good 

#way point 
unique(det_level_2026_raw$waypoint)#awesome

#device 
unique(det_level_2026_raw$device)  

#we definitely used number 3 the whole time; change those.
det_level_2026_raw <- det_level_2026_raw %>%
  mutate(device = ifelse(device == "GPS-001", "GPS-003", device)) %>%
  mutate(device = ifelse(device == "GPS-002", "GPS-003", device))

#check
unique(det_level_2026_raw$device)  #good

#make it match the order and column names of hist data 

colnames(hist_data)
colnames(det_level_2026_raw)

#making final detection sheet for 2026 data

#add all columns from hist data that are missing, these will be 
#filled in once we combine the survey level data
setdiff(names(hist_data), names(det_level_2026_raw))

det_level_2026_raw$cluster_size <- det_level_2026_raw$males + det_level_2026_raw$females + det_level_2026_raw$females
det_level_2026_raw$habitat <- NA
det_level_2026_raw$further_habitat_info <- NA
det_level_2026_raw$year <- 2026
det_level_2026_raw$date <- NA
det_level_2026_raw$round <- NA
det_level_2026_raw$road <- NA
det_level_2026_raw$transect <- NA
det_level_2026_raw$observer1 <- NA
det_level_2026_raw$observer2 <- NA
det_level_2026_raw$time <- NA
det_level_2026_raw$notes_behavior <- NA

#filter out columns we want to keep save as new dataframe
det_level_2026_final <- det_level_2026_raw %>%
  select(year, round, road, transect, date, observer1, observer2, species, males, females, unknowns, cluster_size, "time" = obs_time, detection_type, nearest_mile_marker, bearing, distance, habitat,further_habitat_info, "notes_behavior" = notes, waypoint,device, ObjectID, GlobalID )


dim(hist_data);dim(det_level_2026_final)
colnames(hist_data);colnames(det_level_2026_final)

#there is a row name column in historical, let's get rid of it
hist_data <- hist_data %>%
  select(year, round, road, transect, date, observer1, observer2, species, males, females, unknowns, cluster_size, time, detection_type, nearest_mile_marker, bearing, distance, habitat,further_habitat_info, notes_behavior, waypoint,device, ObjectID, GlobalID )

#let's add the survey level data in before we bind the detection level.
#import the survey level data from 2025
survey_level_2026_raw <- read.csv("C:/Users/beer.kara/Dropbox/GYRF_program/2026 Files/Survey123 raw files/Ptarmigan surveys/2026_prey_parent_level.csv")

#check
colnames(survey_level_2026_raw)

#simplify
survey_level_2026_raw <- survey_level_2026_raw %>%
  select(ObjectID, GlobalID, transect, round, "date"= survey_date, start_time, end_time, observer1, observer2, temp_f, wind_speed, wind_direction, snow_cover, precipatation, direction_traveled, visability, "system_phenology_new" = system_phenology, notes )

#checking for typos - should be very minimal! 

#transect 
unique(survey_level_2026_raw$transect)#good 

#round
unique(survey_level_2026_raw$round)#good 

#date
unique(survey_level_2026_raw$date)#wrong format 

#reformat the date 
survey_level_2026_raw$date <- mdy_hms(survey_level_2026_raw$date)

# Extract the date part
survey_level_2026_raw$date <- as.Date(survey_level_2026_raw$date)

# Format the date in the desired format
survey_level_2026_raw$date <- format(survey_level_2026_raw$date, "%Y-%m-%d")

#recheck 
unique(survey_level_2026_raw$date)#format good now 

#start time 
unique(survey_level_2026_raw$start_time)#good 

#end time 
unique(survey_level_2026_raw$end_time)#good 

#ob1 
unique(survey_level_2026_raw$observer1)#good

#ob2
unique(survey_level_2026_raw$observer2)#good

#tempf
unique(survey_level_2026_raw$temp_f) #good 

#wind speed 
unique(survey_level_2026_raw$wind_speed)

#uh oh, one entry says 47mph. Let's just change it to unknown
survey_level_2026_raw <- survey_level_2026_raw %>%
  mutate(wind_speed = ifelse(wind_speed == 47.0, "NA", wind_speed))

#wind direction 
unique(survey_level_2026_raw$wind_direction)#good

#visability 
unique(survey_level_2026_raw$visability)#good

#snow_cover
unique(survey_level_2026_raw$snow_cover)

#let's turn this into a proportion
survey_level_2026_raw <- survey_level_2026_raw %>%
  mutate(snow_cover = snow_cover/100)

unique(survey_level_2026_raw$snow_cover)#good now 

#add variable for snow depth - not collected in 2024 
survey_level_2026_raw$snow_depth <- NA

#precip
unique(survey_level_2026_raw$precipatation)#incorrect format 

survey_level_2026_raw <- survey_level_2026_raw %>%
  mutate(precipatation = ifelse(precipatation == "no_rain", "none", precipatation))%>%
  mutate(precipatation = ifelse(precipatation == "light_rain", "light", precipatation))%>%
  mutate(precipatation = ifelse(precipatation == "heavy_rain", "heavy", precipatation))

#recheck 
unique(survey_level_2026_raw$precipatation)#good now 

#direction traveled 
unique(survey_level_2026_raw$direction_traveled)#good 

#viability
unique(survey_level_2026_raw$visability)#good 

#system_phenology_new 
unique(survey_level_2026_raw$system_phenology_new)#good 

#add variable for system phenology old - not collected 2024 
survey_level_2026_raw$system_phenology_old <- NA

#import historical survey level data
sur_hist_final <- read.csv("C:/Users/beer.kara/Dropbox/GYRF_program/2025 Files/2025 Data Cleaning and Final Versions/Clean versions/Prey Surveys/2025-08-26_survey_level_data.csv")


#match the existing historical data formatting to combine 
colnames(sur_hist_final)
colnames(survey_level_2026_raw)

#make year column for 2026 data 
survey_level_2026_raw$year <- 2026
survey_level_2026_raw$og_date <- NA

#reorder columns to match hist data
survey_level_2026_final <- survey_level_2026_raw %>%
  select(year, transect, round, date, og_date, observer1, observer2, start_time, end_time, temp_f, wind_speed, wind_direction, snow_cover, precipatation, direction_traveled, system_phenology_old, system_phenology_new, visability, notes, ObjectID, GlobalID)

#check column name and order
colnames(sur_hist_final)
colnames(survey_level_2026_final)#visability is misspelled in new data 

colnames(survey_level_2026_final)[colnames(survey_level_2026_final) == "visability"] <- "visibility"

colnames(sur_hist_final)
colnames(survey_level_2026_final)#crap precipitation is spelled wrong too 

colnames(survey_level_2026_final)[colnames(survey_level_2026_final) == "precipatation"] <- "precipitation"

colnames(sur_hist_final)
colnames(survey_level_2026_final) #more misspelling - I need to learn to spell! 


colnames(sur_hist_final)[colnames(sur_hist_final) == "direction_travelled"] <- "direction_traveled"

dim(sur_hist_final)
dim(survey_level_2026_final)

#hist data has an extra column for row names we need to get rid of
sur_hist_final <- sur_hist_final %>%
  select(year, transect, round, date, og_date, observer1, observer2, start_time, end_time, temp_f, wind_speed, wind_direction, snow_cover, precipitation, direction_traveled, system_phenology_old, system_phenology_new, visibility, notes, ObjectID, GlobalID)

#check again that number of columns matches
dim(sur_hist_final)
dim(survey_level_2026_final) #yes, 21

#combine!
all_survey_level <- rbind(sur_hist_final,survey_level_2026_final)

#check 
dim(all_survey_level); length(sur_hist_final$year)+length(survey_level_2026_final$year)#all seems good

#export the csv for the survey level data 
write.csv(all_survey_level, file = "C:/Users/beer.kara/Dropbox/GYRF_program/2026 Files/Annual Cleaning/Prey cleaned data/2026-07-21_survey_level_data.csv")

###great now let's combine the survey level to detection level for 2026
colnames(det_level_2026_final)
colnames(survey_level_2026_final)

#going to do this by removing the columns that would be duplicated in det data
det_level_2026_final <- det_level_2026_final %>% 
  select(species, males, females, unknowns, cluster_size, time, detection_type,
         nearest_mile_marker, bearing, distance, habitat, further_habitat_info,
         notes_behavior, waypoint, device, ObjectID, GlobalID)

#now select only the columns we want to add to detection level from survey level
survey_level_2026_merge <- survey_level_2026_final %>% select(year, transect,
        round, date, observer1, observer2, GlobalID)

#create a column for road and enter corresponding names using the transect ID
survey_level_2026_merge$road <- NA

survey_level_2026_merge <- survey_level_2026_merge %>%
  mutate(road = case_when(
    transect %in% c("T1", "T2", "T3", "T4", "T5", "T6") ~ "Teller",
    transect %in% c("C1", "C2") ~ "Council",
    transect %in% c("K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8") ~ "Kougarok"))

#check
unique(survey_level_2026_merge$road) #good

#great, i think we're ready to merge them
Preyobs2026 <- left_join(det_level_2026_final, survey_level_2026_merge, by
                         ="GlobalID")

#let's check
dim(hist_data)
dim(Preyobs2026) #yay!! both 24 columns

#let's order them correctly
colnames(hist_data) #view historical and select to match in order
Preyobs2026 <- Preyobs2026 %>% select(year, round, road, transect,date,
                observer1, observer2, species, males, females, unknowns,
                cluster_size, time, detection_type, nearest_mile_marker,
                bearing, distance, habitat, further_habitat_info, notes_behavior,
                waypoint, device, ObjectID, GlobalID)

#check that it was still 24 columns
dim(Preyobs2026)#yes

#####back to  detection level to bind data together#####
all_detection_data <- rbind(hist_data, Preyobs2026)

#check
dim(all_detection_data); length(hist_data$year)+length(Preyobs2026$year)# all obs appear to be there 

unique(all_detection_data$year)#all years there.. looks good! 

head(all_detection_data) #looks great!

#save this final version
write.csv(all_detection_data, file = "C:/Users/beer.kara/Dropbox/GYRF_program/2026 Files/Annual Cleaning/Prey cleaned data/2026-07-23_all_prey_data.csv")
#####end of code#### 