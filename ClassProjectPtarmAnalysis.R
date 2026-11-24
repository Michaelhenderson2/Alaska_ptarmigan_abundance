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

head( covdfraw ); dim( covdfraw ) #150 rows
#### End of data load -------------
####################################################################

###### WILLOW PTARMIGAN MALE #########
##### Ready data for analysis ########
#create your response dataframe:
#mwiptlong[is.na(mwiptlong$count), ] #ignore this was checking for NAs
#we're going to do wipt male first because ropt male might not have enough data

#create your response dataframes:
#sum counts for each distance bin by class by year in prep of turning it into the right format
mwipt <- mwiptlong %>% 
  group_by(transect, distance_bins, year) %>%
  mutate(bincount = sum(count)) %>%
  select(transect, year, bincount, distance_bins) %>% #we don't need the other columns for the response variable
  mutate(transectyear = paste(year, transect)) #since we are doing a single season model, each transectxyear is considered a site

#remove duplicate rows 
mwipt <- mwipt[!duplicated(mwipt),]

#make sure we didn't drop counts on accident
sum(mwiptlong$count)
sum(mwipt$bincount)

#make it so there's a count in each distance bin for each transectxyear
yy <- mwipt %>%  
  pivot_wider( names_from = distance_bins, #which criteria is getting turned to wide
               values_from = c("bincount")) %>% #what columns do we include
  mutate(across(where(is.numeric), ~replace_na(.,0))) #replace NAs with 0s

yy <- ungroup(yy) %>% #because we grouped earlier to prep the data
  dplyr::select( str_sort( colnames(yy), numeric = TRUE ) ) #sort columns by increasing distance bins

#check did we get the right number of dimensions?
head( yy ); dim( yy )
#double checking that no counts were dropped
sum(yy[,1:11]) #still 2627 total ptarms

#order by transectyear so all dataframes goes in the same order of "sites"
yy<-yy[order(yy$transectyear), ]

#### Model Parameters #######
I <- length( unique(yy$transectyear )) #transectyear is id for site x year. 130 "sites"
I
#which years (primary seasons) were sampled:
yrrange <- sort( unique( yy$year ) ) 
yrrange
#how many years were sampled:
T <- length( yrrange )  #10 years, though that doesn't matter here since we are considering them all single season
T

#Distance Bins
#create a new column with distance class each row falls into so I can pivot by that column and the counts
interval.width <- 50 #bin width, would be 50 for us. keeping at 50 even for ropt males
strip.width <- max(mwiptlong$distance) #distance of one side of transect. Can set it to max of distance column (should be 500)

strip.width #500

nbins <- strip.width%/%interval.width #number of bins
nbins #10

xbin<-(mwiptlong$distance%/%interval.width)*interval.width
xbin #now we have a list of the distance class of each row 

#distance bins 
db = c(sort(unique(xbin)), max(xbin) + interval.width) #needs to have number of breaks + 1 for unmarked frame
db

#### Last bit of covariates data prep ######
#create transect x year column in covariates dataframe
covdf <- covdfraw %>%
  mutate(transectyear = paste(year, transect))

#I need to remove any year/transect pairs that were tossed when we cleaned
ff <- data.frame(transectyear = unique(paste(mwipt$year, mwipt$transect)))

trunccov <- covdf[(covdf$transectyear %in% ff$transectyear),]
dim(trunccov) #should be 130

######## Site level covs #############
obsvp <- trunccov  %>%  #considered site level for the purposes of class project
  select( transectyear, obsv_pair )

dim(obsvp)

obsvp<-obsvp[order(obsvp$transectyear), ]

head( obsvp ); dim( obsvp ) #130 rows (sites)

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

dim(year) #matches data
year<-year[order(year$transectyear), ]
head( year ); dim( year )

transect <- trunccov %>%
  select(transectyear, transect)

dim(transect) #matches data
transect<-transect[order(transect$transectyear), ]
head( transect ); dim( transect )

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

transect <- transect %>% 
  select(-transectyear) 


##### Unmarked Dataframe ######
umf <- unmarkedFrameDS(y = as.matrix(yy),
                        siteCovs = data.frame(year, jtime, jday, obsvp, transect), #gonna treat obsvp and transect as a site level cov for this project
                        dist.breaks = db, 
                        tlength = rep(16093, I), #10 mi transects, so 16093.44 m. 16 transects
                        survey = "line",
                        unitsIn = "m")
#observer level covariates not allowed.

# View summary of unmarked dataframe:
summary( umf ) 

#### Scale covs ######
#generic function to standardise covariates (for 2 sd, see Gelman):
standardise <- function( xmat, stdevs = 2, marg = c( 1, 2, 3 ) ) { 
  mean.xmat = mean( as.vector( xmat ), na.rm = TRUE )
  sd.xmat = sd( as.vector( xmat ), na.rm = TRUE ) 
  std.xmat = apply( xmat, marg, function( x ){
    ( x - mean.xmat ) / (stdevs * sd.xmat ) } )
  return( std.xmat )
}

#now scale ecological predictors:
#sc <- apply( siteCovs(umf), MARGIN = 2, FUN = scale )
sctime <- standardise( as.matrix(siteCovs(umf)[,"jtime"]), 
                       stdevs = 2, marg = 2 )
scday <- standardise( as.matrix(siteCovs(umf)[,"jday"]), 
                      stdevs = 2, marg = 2 )
# We replace the predictors in our unmarked dataframe with the scaled values:
siteCovs( umf )[,c("jtime","jday") ] <- cbind(sctime, scday)

summary( umf ) 

##### WIPT Model ##########

#male wipt, half normal
fm.whn <- distsamp(~1 + jtime + jday + obsv_pair + transect #detection
                   ~1 + as.factor(year), #abundance
                   data = umf,
                   keyfun = "halfnorm",
                   output = "abund")
fm.whn #drops 76 sites, which actually makes sense cuz those are our NAs. Says we should try fewer covs.

#male wipt, hazard
fm.wha <- distsamp(~1 + jtime + jday + obsv_pair + transect #detection
                   ~1 + as.factor(year), #abundance
                   data = umf,
                   keyfun = "hazard",
                   output = "abund")
fm.wha #took longer to converge (several minutes)

###### ROCK PTARMIGAN MALE #########
##### Ready data for analysis ########

#create your response dataframes:
#sum counts for each distance bin by class by year in prep of turning it into the right format
mropt <- mroptlong %>% 
  group_by(transect, distance_bins, year) %>%
  mutate(bincount = sum(count)) %>%
  select(transect, year, bincount, distance_bins) %>% #we don't need the other columns for the response variable
  mutate(transectyear = paste(year, transect)) #since we are doing a single season model, each transectxyear is considered a site
dim(mropt)

#remove duplicate rows 
mropt <- mropt[!duplicated(mropt),]
dim(mropt)

#make sure we didn't drop counts on accident
sum(mroptlong$count) #168
sum(mropt$bincount) #168

#make it so there's a count in each distance bin for each transectxyear
rr <- mropt %>%  
  pivot_wider( names_from = distance_bins, #which criteria is getting turned to wide
               values_from = c("bincount")) %>% #what columns do we include
  mutate(across(where(is.numeric), ~replace_na(.,0))) #replace NAs with 0s
  
rr <- ungroup(rr) %>% #because we grouped earlier to prep the data
  dplyr::select( str_sort( colnames(rr), numeric = TRUE ) ) #sort columns by increasing distance bins

#check did we get the right number of dimensions?
head( rr ); dim( rr )
#double checking that no counts were dropped
sum(rr[,1:7]) #still 168 total ptarms

#order by transectyear so all dataframes goes in the same order of "sites"
rr<-rr[order(rr$transectyear), ]

#### Model Parameters #######
Ir <- length( unique(rr$transectyear )) #transectyear is id for site x year. 32 "sites"
Ir 
#which years (primary seasons) were sampled:
yrranger <- sort( unique( rr$year ) ) 
yrranger
#how many years were sampled:
T <- length( yrranger )  #10 years, though that doesn't matter here since we are considering them all single season
T

#Distance Bins
#create a new column with distance class each row falls into so I can pivot by that column and the counts
interval.width <- 50 #bin width, would be 50 for us. keeping at 50 even for ropt males
rstrip.width <- max(mroptlong$distance) #distance of one side of transect. Can set it to max of distance column (should be 300)

rstrip.width #300

rnbins <- rstrip.width%/%interval.width #number of bins
rnbins #6, technically 7

rxbin<-(mroptlong$distance%/%interval.width)*interval.width
rxbin #now we have a list of the distance class of each row 

#distance bins 
rdb = c(sort(unique(rxbin)), max(rxbin) + interval.width) #needs to have number of breaks + 1 for unmarked frame
rdb

#### Last bit of covariates data prep ######

#I need to remove any year/transect pairs that were tossed when we cleaned
rff <- data.frame(transectyear = unique(paste(mropt$year, mropt$transect)))

rtrunccov <- covdf[(covdf$transectyear %in% rff$transectyear),]
dim(rtrunccov) #should be 32

############## site level cov in site (transect) by year dataframes #######

#since we're really data poor, going to just use year and transect as site level cov
ryear <- rtrunccov %>%
  select(transectyear, year)

dim(ryear) #matches data
ryear<-ryear[order(ryear$transectyear), ]
head( ryear ); dim( ryear )

rtransect <- rtrunccov %>%
  select(transectyear, transect)

dim(rtransect) #matches data
rtransect<-rtransect[order(rtransect$transectyear), ]
head( rtransect ); dim( rtransect )

###### can't put site names in unmarked dataset ######
##Checked to make sure sites were ordered the same way for all of them (alphabetically, so Cs, then Ks, then Ts)
#Now remove transect column in each dataframe
rr <- rr %>% 
  select( -transectyear, -transect, -year )

ryear <- ryear %>% 
  select(-transectyear) 

rtransect <- rtransect %>% 
  select(-transectyear) 


##### Unmarked Dataframe ######
umfr <- unmarkedFrameDS(y = as.matrix(rr),
                       siteCovs = data.frame(ryear, rtransect), #gonna treat obsvp and transect as a site level cov for this project
                       dist.breaks = rdb, 
                       tlength = rep(16093, Ir), #10 mi transects, so 16093.44 m. 
                       survey = "line",
                       unitsIn = "m")
#observer level covariates not allowed.

# View summary of unmarked dataframe:
summary( umfr ) 

##### ROPT Model ##########

#male ropt, half normal
fm.rhn <- distsamp(~1 + transect #detection
                   ~1 + as.factor(year), #abundance
                   data = umfr,
                   keyfun = "halfnorm",
                   output = "abund")
fm.rhn 

#male ropt, hazard
fm.rha <- distsamp(~1 + transect #detection
                   ~1 + as.factor(year), #abundance
                   data = umfr,
                   keyfun = "hazard",
                   output = "abund")
fm.rha #took longer to converge (like a minute or two)


##########    save relevant data and workspaces     ###########

#save workspace
save.image( "PtarmAnalysisClass.RData" )

########## End of saving section ##################################

############# END OF SCRIPT ########################################