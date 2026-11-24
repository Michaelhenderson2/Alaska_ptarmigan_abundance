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
load ("PtarmAnalysisClass.RData")

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
covdfraw <- read.csv( file = paste( datadir, "site_covariates.csv", sep = ""),
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
  select(transect, year, bincount, distance_bins, obsv_pair) %>% #we don't need the other columns for the response variable
  mutate(transectyear = paste(year, transect))

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
  mutate(across(where(is.numeric), ~replace_na(.,0))) %>% #replace NAs with 0s
  mutate(transectyear = paste(year, transect)) %>% #we are treating each transectxyear as one site for this project
  dplyr::select( str_sort( colnames(yy), numeric = TRUE ) ) #sort columns by increasing distance bins

yy <- ungroup(yy) #because we grouped earlier to prep the data

#check did we get the right number of dimensions?
head( yy ); dim( yy )
#double checking that no counts were dropped
sum(yy[,1:11]) #still 2627 total ptarms

#order by transectyear so all dataframes goes in the same order of "sites"
yy<-yy[order(yy$transectyear), ]

#### Model Parameters #######
I <- length( unique(yy$transectyear )) #transectyear is id for site x year. 130 "sites"
#which years (primary seasons) were sampled:
yrrange <- sort( unique( yy$year ) ) 
#how many years were sampled:
T <- length( yrrange )  #10 years

#### Last bit of covariates data prep ######
#create transect x year column in covariates dataframe
covdf <- covdfraw %>%
  mutate(transectyear = paste(year, transect))

#I need to remove any year/transect pairs that were tossed when we cleaned
ff <- data.frame(transectyear = unique(paste(mwipt$year, mwipt$transect)))

trunccov <- covdf[(covdf$transectyear %in% ff$transectyear),]
dim(trunccov) #should be 130

######## Observer level cov (observer-pair) #############
obsvp <- trunccov  %>%  
  select( transectyear, obsv_pair )

dim(obsvp)

obsvp<-obsvp[order(obsvp$transectyear), ]

head( obsvp ); dim( obsvp ) #130 rows (sites)

############## site level cov in site (transect) by year dataframes #######
#still are NAs because I guess some sites were not surveyed in certain years, but there are 0s for those cells in the yyy dataframe, so we'll see if that causes issues
jtime <- trunccov %>%
  select(transectyear, jtime)

jtime<-jtime[order(jtime$transectyear), ]
head( jtime ); dim( jtime )

jday <- trunccov %>%
  select(transectyear, jday)

jday<-jday[order(jday$transectyear), ]
head( jday ); dim( jday )

year <- trunccov %>%
  select(transectyear, year)
dim(year)
year<-year[order(year$transectyear), ]
head( year ); dim( year )

###### can't put site names in unmarked dataset ######
##Checked to make sure sites were ordered the same way for all of them (alphabetically, so Cs, then Ks, then Ts)
#Now remove transect column in each dataframe
yy <- yy %>% 
  select( -transectyear, -transect, -year )

obsvp <- obsvp %>% 
  select(-transectyear) 

jtime <- jtime %>% 
  select( -transectyear )

jday <- jday %>% 
  select(-transectyear) 

year <- year %>% 
  select(-transectyear) 

##### Unmarked Dataframe ######
### DOESNT ALLOW OBSVCOV. maybe for the class project I just run null models? could still do year on abundance?
umf <- unmarkedFrameDS(y = as.matrix(yy),
                        SiteCovs = list(jtime = jtime,
                                        jday = jday,
                                        obsvp = obsvp), #gonna treat as a site level cov for this project
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
save.image( "PtarmAnalysisClass.RData" )

########## End of saving section ##################################

############# END OF SCRIPT ########################################