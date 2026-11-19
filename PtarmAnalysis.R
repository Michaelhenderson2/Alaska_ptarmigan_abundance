##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )
# Check that you are in the right project folder
getwd()

# load packages:
library( tidyverse ) 
library( unmarked ) #runs N-mixture models
## end of package load ###############

#####Load Rdata if needed############
load ("PtarmAnalysis.RData")

#### Load or Create Data -----------------------------------------
# set path to where you can access your data #
# Note that the path will be different for your.#
datadir <- paste( getwd(), "/Data/", sep = "" )
# load ptarmigan raw detection data
#distances and observer pair
mroptlong <- read.csv( file = paste( datadir, "male_ropt.csv", sep = ""),
                   header = TRUE )
mwiptlong <- read.csv( file = paste( datadir, "male_wipt.csv", sep = ""),
                      header = TRUE )
#site level covariates = day of year and minutes since earliest survey time
covdf <- read.csv( file = paste( datadir, "site_covariates.csv", sep = ""),
                      header = TRUE )

#view
head( mroptlong ); dim( mroptlong ) #161 rows

head( mwiptlong ); dim( mwiptlong ) #2465 rows

head( covdf ); dim( covdf ) #150 rows
#### End of data load -------------
####################################################################

##### Ready data for analysis ########
#create your response dataframe:
#mwiptlong[is.na(mwiptlong$count), ] #ignore this was checking for NAs
#we're going to do wipt male first because ropt male might not have enough data

mwipt <- mwiptlong %>% 
  group_by(transect, distance_bins, year) %>%
  mutate(bincount = sum(count)) %>%
  select(transect, year, bincount, distance_bins) 
  
mwipt <- mwipt[!duplicated(mwipt),]

sum(mwiptlong$count)
sum(mwipt$bincount)

yy <- mwipt %>%  
  pivot_wider( names_from = distance_bins, #which criteria is getting turned to wide
               values_from = c("bincount"), #what columns do we include
               )

yyy <- yy %>%  
  pivot_wider( names_from = year, #which criteria is getting turned to wide
               values_from = c("50", "100", "150", "200", "250", "300", "350", "400", "450", "500", "550"), #what columns do we include
               names_glue = "{year}_{.value}") %>%  #new columns need to have year in name bc counts in this unmarked dataframe needs to be MxJT, M = site, JXT = year x survey
  mutate(across(where(is.numeric), ~replace_na(.,0))) #replace NAs with 0s

##OBSERVERPAIRS

############## site level cov in site (transect) by year dataframes #######
jtime <- covdf %>%
  select(transect, year, jtime) %>%
  spread(key = year, value = jtime)

jday <- covdf %>%
  select(transect, year, jday) %>%
  spread(key = year, value = jday)


#generic function to standardise covariates:
standardise <- function( xmat, stdevs = 2, marg = c( 1, 2, 3 ) ) { 
  mean.xmat = mean( as.vector( xmat ), na.rm = TRUE )
  sd.xmat = sd( as.vector( xmat ), na.rm = TRUE ) 
  std.xmat = apply( xmat, marg, function( x ){
    ( x - mean.xmat ) / (stdevs * sd.xmat ) } )
  return( std.xmat )
}

####### Scale site-level Covs #####################
#we are scaling by 2 standard deviations following Gelman (2008) because I have both continuous and binary covariates

##########    save relevant data and workspaces     ###########

#save workspace
save.image( "PtarmAnalysis.RData" )

########## End of saving section ##################################

############# END OF SCRIPT ########################################