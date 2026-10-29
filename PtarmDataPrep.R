##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )
# Check that you are in the right project folder
getwd()

# load packages:
library( tidyverse ) 
library( hms )
## end of package load ###############
###################################################################
#### Load or create data -----------------------------------------
# set path to where you can access your data #
# Note that the path will be different for your.#
datapath <- datadir <- paste( getwd(), "/Data/", sep = "" )
# load ptarmigan raw detection data
detdf <- read.csv( file = paste( datadir, "2026-07-23_all_prey_data.csv", sep = ""),
                   header = TRUE )
#view
head( detdf ); dim( detdf )
########## end of data load ################################
#######################################################################
######## clean data #############
#database has observations that are not ptarmigan. view all the species in the species column
unique(detdf$species)

#new dataframe with only relevant columns and only for ptarmigan obs.
ptarmdetdf <- detdf %>% 
  dplyr::select(year, round, transect, date, observer1, observer2, species, males, females, cluster_size, time, distance) %>% #select relevant columns (cluster_size = ptarmigan count),
  dplyr::filter(., species == "WIPT" | species == "ROPT" )  #filter out rows that are not willow and rock ptarmigan counts

#view
head(ptarmdetdf); dim(ptarmdetdf)
#this removes 1767 rows 
nrow( detdf ) - nrow(ptarmdetdf)
  
#lubridate to create day of year and time of day
#turn time column into all X:XX (since in the raw data some are XXX format)
ptarmdetdf$time <- gsub(":", "", ptarmdetdf$time)
ptarmdetdf$time <- sub("([0-9]{2})$", ":\\1", ptarmdetdf$time)
#time of day and day of year
ptarmdetdf <- ptarmdetdf %>% 
  mutate(jday = lubridate::yday(mdy(date)),
         time_of_day = as.POSIXct(time, format = "%I:%M"))
#remove the date from time of day column (since it automatically puts today's date in)
ptarmdetdf$time_of_day <- format(ptarmdetdf$time_of_day, "%I:%M")

#Observer pairs will be a random intercept in our final model to account for differences in detection between pairs
#make a column with each unique pair of observers
ptarmdetdf$obsv_pair <- apply(ptarmdetdf[, c("observer1", "observer2")], 1, function(x) {
  last <- word(x, -1)
  tolower(paste(sort(last), collapse = "-"))
})

#view
ptarmdetdf$obsv_pair

#what are the unique observer pairs?
unique(ptarmdetdf$obsv_pair)

#check for NAs just in case
sum(is.na(ptarmdetdf$obsv_pair)) #0

# In my initial data exploration, I discovered that the data in the distance row was character class instead of numeric class due to the inclusion of values such as "<100," ">1200," and "U". There were also one negative distance recorded (-99). My next cleaning step is to turn the column into numeric values and filter out the negative number and the NAs converted from the greater than, less than, and U values. 

# First, check class of distance column
sapply(ptarmdetdf$distance, class)
# all character

#convert to numeric
ptarmdetdf$distance <- as.numeric(ptarmdetdf$distance)

#check again to make sure
sapply(ptarmdetdf$distance, class)
#all numeric

#drop all rows with NAs and the negative numbers
ptarmdetdf1 <- ptarmdetdf %>%
  drop_na(.,"distance") %>%
  subset(., distance >= 0)

#check for NAs
sum(is.na(ptarmdetdf1$distance))
#0 NAs

#view
head(ptarmdetdf1); dim(ptarmdetdf1)
#this removes 31 rows
nrow( ptarmdetdf ) - nrow(ptarmdetdf1)

#wide to long
cleandetdf <- pivot_longer(ptarmdetdf1, cols = c("males","females"), names_to = "sex", values_to = "count")
#view
head(cleandetdf); dim(cleandetdf)
#10378 rows

#filter out unknown sex birds
cleandetdf1 <- cleandetdf %>%
  dplyr::filter(., count >= 1)
#view
head(cleandetdf1); dim(cleandetdf1)
#how many rows lost
nrow( cleandetdf ) - nrow(cleandetdf1) #4650

######## Data Summary ##########################
#how many males in original dataframe (just wipt and ropt round 1?)
sum(ptarmdetdf1$males) #5200
#how many females in original dataframe (just wipt and ropt round 1?)
sum(ptarmdetdf1$females) #836
#how many individuals were originally counted in round 1?
sum(ptarmdetdf1$cluster_size)#6756
#how many individuals in cleaned dataframe with unknowns filtered out?
sum(cleandetdf1$count) #6036. matches sum of previous two
#how many individuals were unknown?
sum(ptarmdetdf1$cluster_size) - sum(cleandetdf1$count) #720

######## data visualization #############
#create long dataframe where each row is 1 individual (there are no 0's in this)
datvis <- ptarmdetdf1 %>%
  pivot_longer(
    cols = c(males, females),
    names_to = "sex",
    values_to = "n"
  ) %>% #turns long and creates column for sex and "n" which is the number of rows that will be needed.
  uncount(n) #expand the rows based on n
#view
head(datvis); dim(datvis) #6036 rows 
#matches sum of males and females in dataset with unknown sex filtered out?
sum(cleandetdf1$count) #6036 individuals

## Create dataframes for each round
#just round 1 surveys
det1 <- datvis %>%
  dplyr::filter(., round == "1" )

#view
head(det1); dim(det1) #3259 rows

#just round 2 surveys
det2 <- datvis %>%
  dplyr::filter(., round == "2" )

#view
head(det2); dim(det2) #2777 rows

##### Round 1 Rock Ptarm 
#create new dataset with just round 1 ropt
det1rp <- det1 %>%
  dplyr::filter(., species == "ROPT")
#view
head(det1rp); dim(det1rp)
#211 rows

#plot
ggplot( det1rp, aes(x = distance, fill = sex)) +
  theme_classic() +
  labs( title = "Round 1 ROPT" ) +
  geom_histogram(binwidth = 50) +
  facet_wrap(~year, scales = "free_y")

##### Round 1 Willow Ptarm 
#create new dataset with just round 1 wipt
det1wp <- det1 %>%
  dplyr::filter(., species == "WIPT")
#view
head(det1wp); dim(det1wp)
#3048 rows

#plot
ggplot( det1wp, aes(x = distance, fill = sex)) +
  theme_classic() +
  labs( title = "Round 1 WIPT" ) +
  geom_histogram(binwidth = 50) +
  facet_wrap(~year, scales = "free_y")

##### Round 2 Rock Ptarm 
#create new dataset with just round 2 ropt
det2rp <- det2 %>%
  dplyr::filter(., species == "ROPT")
#view
head(det2rp); dim(det2rp)
#218 rows

#plot
ggplot( det2rp, aes(x = distance, fill = sex)) +
  theme_classic() +
  labs( title = "Round 2 ROPT" ) +
  geom_histogram(binwidth = 50) +
  facet_wrap(~year, scales = "free_y")

##### Round 2 Willow Ptarm 
#create new dataset with just round 2 wipt
det2wp <- det2 %>%
  dplyr::filter(., species == "WIPT")
#view
head(det2wp); dim(det2wp)
#2559 rows

#plot
ggplot( det2wp, aes(x = distance, fill = sex)) +
  theme_classic() +
  labs( title = "Round 2 WIPT" ) +
  geom_histogram(binwidth = 50) +
  facet_wrap(~year, scales = "free_y")

#Just Round 1. Model we will use doesn't allow replicate surveys, and data for rounds 1 and 2 are similar enough proportionally, but round 1 has more data, so I will be using round 1 data only
cleandetdfr1 <- cleandetdf1 %>%
  dplyr::filter(., round <= "1")
#view
head(cleandetdfr1);dim(cleandetdfr1) #3081
#how many rows lost?
nrow(cleandetdf1) - nrow(cleandetdfr1) #2647

# Table of annual counts by species
sum <- table(cleandetdfr1$species, cleandetdfr1$year )
sum

#dataframe for each species to get counts by sex
#just WIPT
wipt <- cleandetdfr1 %>%
  dplyr::filter(., species == "WIPT" )
#view
head(wipt); dim(wipt) #2879 

#just ROPT
ropt <- cleandetdfr1 %>%
  dplyr::filter(., species == "ROPT" )
#view
head(ropt); dim(ropt) #202 

#count by sex by species for ropt
sumr <- table(ropt$sex, ropt$year )
sumr

#count by sex by species for wipt
sumw <- table(wipt$sex, wipt$year )
sumw

########## Truncate to 500 m ######################
cleandetdf2 <- cleandetdfr1 %>%
  dplyr::filter(., distance <= 500)

#view
head(cleandetdf2); dim(cleandetdf2) #3069
#how many rows lost
nrow( cleandetdfr1 ) - nrow(cleandetdf2) #12

#sum again
sum1 <- table(cleandetdf2$species, cleandetdf2$year )
sum1

#dataframe for each species to get counts by sex
#just WIPT
wipt1 <- cleandetdf2 %>%
  dplyr::filter(., species == "WIPT" )
#view
head(wipt1); dim(wipt1) #2867

#just ROPT
ropt1 <- cleandetdf2 %>%
  dplyr::filter(., species == "ROPT" )
#view
head(ropt1); dim(ropt1) #202 

#count by sex by species for ropt
sumr1 <- table(ropt1$sex, ropt1$year )
sumr1

#count by sex by species for wipt
sumw1 <- table(wipt1$sex, wipt1$year )
sumw1

#There are not enough females. Create datasets for each species of just males
############Just Males#########################
maleropt <- ropt1 %>%
  dplyr::filter(., sex == "males" ) %>% #only males
  dplyr::select(., -cluster_size) #clustersize column is not needed anymore
#view
head(maleropt); dim(maleropt) #177

#does this match?
sum(ropt1$sex == "males") #177

malewipt <- wipt1 %>%
  dplyr::filter(., sex == "males" ) %>% #only males
  dplyr::select(., -cluster_size) #clustersize column is not needed anymore
#view
head(malewipt); dim(malewipt) #2465

#does this match?
sum(wipt1$sex == "males") #2465

##########Legend: Define Objects##############
### All the cleaning I did made for a lot of objects that can't be understood intrisically. Here is a legend:
#detdf = raw data
#ptarmdetdf = selected relevant columns and removed any observations that aren't willow or rock ptarm
#ptarmdetdf1 = removed all rows with weird distance stuff (ie <100, U, -99) and removed round 2
#cleandetdf = pivoted wide to long 
#cleandetdf1 = removed unknown sex birds
#datavis = each row is an individual, no unknown sex birds, just for data visualization
#det1 and det2 = round 1 and round 2 data parsed out from datavis
#det1rp, det1wp, det2rp, det2wp = round 1 and round 2 data split by species for visualization
#cleandetdfr1 = just round 1 from cleandetdf1
#wipt and ropt = data for each species from cleandetdf1
#cleandetdf2 = cleandetdf1 truncated to 500m
#wipt1 and ropt1 = data for each species from cleandetdf2
#malewipt and maleropt = just males from ropt1 and wipt1

################################################################
##########    save relevant data and workspaces     ###########
# full cleaned data:
write.csv( cleandetdf2, paste( getwd(),"/Data/ptarmclean.csv", sep = "" ),  
           row.names = FALSE )

# cleaned willow ptarm just males
write.csv( malewipt, paste( getwd(),"/Data/malewipt.csv", sep = "" ),  
           row.names = FALSE )

#cleaned rock ptarm just males
write.csv( maleropt, paste( getwd(),"/Data/maleropt.csv", sep = "" ),  
           row.names = FALSE )

#save workspace
save.image( "PtarmPrep.RData" )
########## End of saving section ##################################
################## Save your data and workspace ###################

############# END OF SCRIPT ########################################