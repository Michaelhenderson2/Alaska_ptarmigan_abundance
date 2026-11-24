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

#create your response dataframe:
#sum counts for each distance bin by class by year in prep of turning it into the right format
mwipt <- mwiptlong %>% 
  group_by(transect, distance_bins, year) %>%
  mutate(bincount = sum(count)) %>%
  select(transect, year, bincount, distance_bins, obsv_pair) #we don't need the other columns for the response variable
  
#remove duplicate rows 
mwipt <- mwipt[!duplicated(mwipt),]

##double check
#did observer pairs get dropped?
sort(unique(mwiptlong$obsv_pair)) #11 pairs
sort(unique(mwipt$obsv_pair)) #none dropped

#make sure we didn't drop counts on accident
sum(mwiptlong$count)
sum(mwipt$bincount)

#make it so there's a count in each distance bin for each transectxyear
yy <- mwipt %>%  
  select(-obsv_pair) %>%
  pivot_wider( names_from = distance_bins, #which criteria is getting turned to wide
               values_from = c("bincount")) %>% #what columns do we include
  mutate(across(where(is.numeric), ~replace_na(.,0))) #replace NAs with 0s
#check did we get the right number of dimensions?
head( yy ); dim( yy )
#double checking that no counts were dropped
sum(yy[,3:13]) #still 2627 total ptarms

#sort by increasing distance bins
yy <- yy %>% dplyr::select( str_sort( colnames(yy[,3:13]), numeric = TRUE ) ) 

#final dataframe format
yyy <- yy %>%  
  pivot_wider( names_from = year, #which criteria is getting turned to wide
               values_from = c("50", "100", "150", "200", "250", "300", "350", "400", "450", "500", "550"), #what columns do we include
               #50 = 0-49, 100 = 50-99, etc. so 550 is for any distances with 500
               names_glue = "{year}_{.value}") %>%  #new columns need to have year in name 
  mutate(across(where(is.numeric), ~replace_na(.,0))) %>%  #replace NAs with 0s
  ungroup() #because we grouped earlier to sum counts, we have to ungroup in order to remove a column used as grouping criteria

#double checking that no counts were dropped
sum(yyy[,2:111]) #still 2627 total ptarms

yyy<-yyy[order(yyy$transect), ]


#check did we get the right number of dimensions?
head( yyy ); dim( yyy )

###### Remove rows from transectxyears lost to truncation ###### IGNORE THIS

#I need to remove any year/transect pairs that were tossed when we truncated
#ff <- data.frame(transectyear = unique(paste(mwipt$year, mwipt$transect)))

#trunccov <- covdf %>%
  #mutate(transectyear = paste(year, transect))

#trunccov <- trunccov[(trunccov$transectyear %in% ff$transectyear),]

######## Observer level cov (observer-pair) #############

#create observer level dataframes
obsvp <- covdf %>%  
  select( transect, year, obsv_pair ) %>% 
  pivot_wider( names_from = year, #which criteria is getting turned to wide
               values_from = c("obsv_pair")) %>% 
  ungroup() #because we grouped earlier to sum counts, we have to ungroup in order to remove a column used as grouping criteria 

obsvp<-obsvp[order(obsvp$transect), ]

head( obsvp ); dim( obsvp ) #this is meant to be a random intercept in the final model, so this might not even be the right format

############## site level cov in site (transect) by year dataframes #######
#still are NAs because I guess some sites were not surveyed in certain years, but there are 0s for those cells in the yyy dataframe, so we'll see if that causes issues
jtime <- covdf %>%
  select(transect, year, jtime) %>%
  spread(key = year, value = jtime)

head( jtime ); dim( jtime )

jday <- covdf %>%
  select(transect, year, jday) %>%
  spread(key = year, value = jday) 

head( jday ); dim( jday )

#lastly calculate number of primary periods
T <- length( unique( mwiptlong$year ) )
T #10
I <- length(unique(covdf$transect))
I #16

#If the time gap between primary periods is not constant, an M by T matrix of integers should be supplied using the primaryPeriod argument
p <- as.integer(c(1,2,3,4,6,7,8,9,10,11))

primPer <- as.matrix(do.call("rbind", replicate(I, p, simplify = FALSE)))

###### can't put site names in unmarked dataset ######
##Checked to make sure sites were ordered the same way for all of them (alphabetically, so Cs, then Ks, then Ts)
#Now remove transect column in each dataframe
yyy <- yyy %>% 
  select( -transect )

obsvp <- obsvp %>% 
  select(-transect) 

jtime <- jtime %>% 
  select( -transect )

jday <- jday %>% 
  select(-transect) 

##### Unmarked Dataframe ######

umf <- unmarkedFrameDSO(y = as.matrix(yyy),
                        yearlySiteCovs = list(jtime = jtime,
                                              jday = jday),
                        numPrimary = T,
                        primaryPeriod = primPer,
                        dist.breaks = c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600), #needs to have number of breaks + 1
                        tlength = rep(16093, I), #10 mi transects, so 16093.44 m. 16 transects
                        survey = "line",
                        unitsIn = "m")
#observer level covariates not allowed. We will try to do random intercepts for our observer pairs

# View summary of unmarked dataframe:
summary( umf ) 


####### Scale site-level Covs #####################
#wait are we scaling by 1 or 2 sd? I'll scale by 1 for now but with the custom function scale by 1 sd and can change if needed.

#generic function to standardise covariates (for 2 sd):
standardise <- function( xmat, stdevs = 1, marg = c( 1, 2, 3 ) ) { 
  mean.xmat = mean( as.vector( xmat ), na.rm = TRUE )
  sd.xmat = sd( as.vector( xmat ), na.rm = TRUE ) 
  std.xmat = apply( xmat, marg, function( x ){
    ( x - mean.xmat ) / (stdevs * sd.xmat ) } )
  return( std.xmat )
}

#now scale ecological predictors:
#sc <- apply( siteCovs(umf), MARGIN = 2, FUN = scale )
sctime <- standardise( as.matrix(yearlySiteCovs(umf)[,"jtime"]), 
                       stdevs = 1, marg = 2 )
scday <- standardise( as.matrix(yearlySiteCovs(umf)[,"jday"]), 
                        stdevs = 1, marg = 2 )
# We replace the predictors in our unmarked dataframe with the scaled values:
yearlySiteCovs( umf )[,c("jtime","jday") ] <- cbind(sctime, scday)

##### Model ##########
#obsv pairs as random intercepts in final model

##########    save relevant data and workspaces     ###########

#save workspace
save.image( "PtarmAnalysis.RData" )

########## End of saving section ##################################

############# END OF SCRIPT ########################################