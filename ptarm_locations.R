#this is a script to add lat/long data for all waypoints associated with each detection in the data.
#start with a fully updated and cleaned all_prey_data csv.
#right now we will be adding all years of waypoints. this script should be simplified
#in the future to seamlessly add waypoint data every year.

#load packages
library( tidyverse )
library( reshape2 )
library( dplyr )
library(lubridate)

#import clean prey data
prey <- read.csv("C:/Users/beer.kara/Dropbox/GYRF_program/2026 Files/Annual Cleaning/Prey clean data/2026-07-23_all_prey_data.csv")

#let's go ahead and import our waypoint data for each year
setwd("C:/Users/beer.kara/Dropbox/GYRF_program/2026 Files/Annual Cleaning/Prey clean data/waypoint data")
Way2018 <- read.csv("2018_waypoints.csv")
Way2019 <- read.csv("2019_waypoints.csv")
Way2021 <- read.csv("2021_waypoints.csv")
Way2022 <- read.csv("2022_waypoints.csv")
Way2023 <- read.csv("2023_waypoints.csv")
Way2024 <- read.csv("2024_waypoints.csv")
Way2025 <-read.csv("2025_waypoints.csv")
Way2026 <-read.csv("2026_waypoints.csv")


#select only the columns we need
Way2018 <- Way2018 %>% select(name, lat, lon, ele)
Way2019 <- Way2019 %>% select(name, lat, lon, ele)
Way2021 <- Way2021 %>% select(name, lat, lon, ele)
Way2022 <- Way2022 %>% select(name, lat, lon, ele)
Way2023 <- Way2023 %>% select(name, lat, lon, ele)
Way2024 <- Way2024 %>% select(name, lat, lon, ele)
Way2025 <- Way2025 %>% select(name, lat, lon, ele)
Way2026 <- Way2026 %>% select(name, lat, lon, ele)

#change the column names to match the obs df
Way2026 <- Way2026 %>%
  rename(waypoint = name)
Way2025 <- Way2025 %>%
  rename(waypoint = name)
Way2024 <- Way2024 %>%
  rename(waypoint = name)
Way2023 <- Way2023 %>%
  rename(waypoint = name)
Way2022 <- Way2022 %>%
  rename(waypoint = name)
Way2021 <- Way2021 %>%
  rename(waypoint = name)
Way2019 <- Way2019 %>%
  rename(waypoint = name)
Way2018 <- Way2018 %>%
  rename(waypoint = name)

#now we are going to try left joining with an additional conditional column
Way2026$join_year <- 2026
Way2025$join_year <- 2025
Way2024$join_year <- 2024
Way2023$join_year <- 2023
Way2022$join_year <- 2022
Way2021$join_year <- 2021
Way2019$join_year <- 2019
Way2018$join_year <- 2018

#fix any class issues
Way2018$waypoint <- as.character(Way2018$waypoint)
Way2019$waypoint <- as.character(Way2019$waypoint)
Way2021$waypoint <- as.character(Way2021$waypoint)
Way2022$waypoint <- as.character(Way2022$waypoint)
Way2023$waypoint <- as.character(Way2023$waypoint)
Way2024$waypoint <- as.character(Way2024$waypoint)
Way2025$waypoint <- as.character(Way2025$waypoint)
Way2026$waypoint <- as.character(Way2026$waypoint)

#fix formatting issues with 20222
Way2022$waypoint <- gsub(" ", "", Way2022$waypoint)

prey <- prey %>%
  mutate(
    waypoint = case_when(
      year == 2022 & waypoint %in% paste0("22-", 1:9) ~
        gsub("22-", "22-00", waypoint),
      
      year == 2022 & waypoint %in% paste0("22-", 10:99) ~
        gsub("22-", "22-0", waypoint),
      
      TRUE ~ waypoint
    )
  )

all_waypoints <- bind_rows(
  Way2026,
  Way2025,
  Way2024,
  Way2023,
  Way2022,
  Way2021,
  Way2019,
  Way2018
)

prey <- prey %>%
  left_join(
    all_waypoints,
    by = c("waypoint", "year" = "join_year")
  )

#after a THOROUGH check, the main issue seems to be with 2022 formatting. 
#we'll go back and fix this, but otherwise it looks great.

#now seeing an issue with elevation. if it's zero, it really needs to be NA
prey <- prey %>%
  mutate(
    ele = case_when(
      ele == 0 ~ NA_real_,
      TRUE ~ ele
    )
  ) #looks good.

#now for the future, i should be able to just use the waypoint section of code at the beginning. woo!

#save as csv
write.csv(prey, "prey_locations.csv", row.names = FALSE)
