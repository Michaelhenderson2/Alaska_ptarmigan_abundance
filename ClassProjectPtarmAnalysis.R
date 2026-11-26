##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )
# Check that you are in the right project folder
getwd()

install.packages( "lme4" )

# load packages:
library( tidyverse ) 
library( unmarked ) #runs N-mixture models
library(lme4)
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

trunccov <- covdf %>%
  dplyr::filter(transectyear %in% ff$transectyear)

dim(trunccov) #should be 130

######## Site level covs #############

#observer pair
obsvp <- trunccov  %>%  #considered site level for the purposes of class project
  select( transectyear, obsv_pair )

obsvp<-obsvp[order(obsvp$transectyear), ]
head( obsvp ); dim( obsvp ) #130 rows (sites)
sum(is.na(obsvp)) #check for NAs

#time of day
jtime <- trunccov %>%
  select(transectyear, jtime)

jtime<-jtime[order(jtime$transectyear), ]
head( jtime ); dim( jtime )
sum(is.na(jtime)) #check for NAs

#day of year
jday <- trunccov %>%
  select(transectyear, jday)

jday<-jday[order(jday$transectyear), ]
head( jday ); dim( jday )
sum(is.na(jday))

#year
year <- trunccov %>%
  select(transectyear, year)

year<-year[order(year$transectyear), ]
head( year ); dim( year )
sum(is.na(year))

#transect
transect <- trunccov %>%
  select(transectyear, transect)

transect<-transect[order(transect$transectyear), ]
head( transect ); dim( transect )
sum(is.na(transect))

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
#observer level covariates not allowed. automatically turned transect and obsvp into factors.

# View summary of unmarked dataframe:
summary( umf ) 

#### Scale covs ######
#generic function to standardise covariates (for 2 sd, see Gelman):
standardise <- function( xmat, stdevs = 1, marg = c( 1, 2, 3 ) ) { 
  mean.xmat = mean( as.vector( xmat ), na.rm = TRUE )
  sd.xmat = sd( as.vector( xmat ), na.rm = TRUE ) 
  std.xmat = apply( xmat, marg, function( x ){
    ( x - mean.xmat ) / (stdevs * sd.xmat ) } )
  return( std.xmat )
}

#now scale ecological predictors:
#sc <- apply( siteCovs(umf), MARGIN = 2, FUN = scale )
sctime <- standardise( as.matrix(siteCovs(umf)[,"jtime"]), 
                       stdevs = 1, marg = 2 )
scday <- standardise( as.matrix(siteCovs(umf)[,"jday"]), 
                      stdevs = 1, marg = 2 )
# We replace the predictors in our unmarked dataframe with the scaled values:
siteCovs( umf )[,c("jtime","jday") ] <- cbind(sctime, scday)

summary( umf ) 

##### WIPT Model ##########

#male wipt, half normal
fm.whn <- distsamp(~1 + jtime + jday + (1 | obsv_pair) + (1 | transect) #detection, observer pair as a random intercept
                   ~1 + as.factor(year), #abundance. year as factor so we can get annual estimates
                   data = umf,
                   keyfun = "halfnorm",
                   output = "abund")
fm.whn 

#male wipt, hazard
fm.wha <- distsamp(~1 + jtime + jday + (1 | obsv_pair) + (1 | transect) #detection
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
#observer pair
robsvp <- rtrunccov  %>%  
  select( transectyear, obsv_pair )

robsvp<-robsvp[order(robsvp$transectyear), ]
head(robsvp); dim(robsvp)

#year
ryear <- rtrunccov %>%
  select(transectyear, year)

dim(ryear) #matches data
ryear<-ryear[order(ryear$transectyear), ]
head( ryear ); dim( ryear )

#time of day
rjtime <- rtrunccov %>%
  select(transectyear, jtime)

rjtime<-rjtime[order(rjtime$transectyear), ]
head( rjtime ); dim( rjtime )

#day of year
rjday <- rtrunccov %>%
  select(transectyear, jday)

rjday<-rjday[order(rjday$transectyear), ]
head( rjday ); dim( rjday )

#transect
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

robsvp <- robsvp %>% 
  select( -transectyear )

rjtime <- rjtime %>% 
  select( -transectyear )

rjday <- rjday %>% 
  select(-transectyear) 

ryear <- ryear %>% 
  select(-transectyear) 

rtransect <- rtransect %>% 
  select(-transectyear) 


##### Unmarked Dataframe ######
umfr <- unmarkedFrameDS(y = as.matrix(rr),
                       siteCovs = data.frame(ryear, rtransect, rjtime, rjday, robsvp), #gonna treat obsvp and transect as a site level cov for this project
                       dist.breaks = rdb, 
                       tlength = rep(16093, Ir), #10 mi transects, so 16093.44 m. 
                       survey = "line",
                       unitsIn = "m")
# obs level covs not allowed. automatically turned transect and obsvp into factors.

#### Scale covs ######

#now scale ecological predictors:
#sc <- apply( siteCovs(umf), MARGIN = 2, FUN = scale )
rsctime <- standardise( as.matrix(siteCovs(umfr)[,"jtime"]), 
                       stdevs = 1, marg = 2 )
rscday <- standardise( as.matrix(siteCovs(umfr)[,"jday"]), 
                      stdevs = 1, marg = 2 )
# We replace the predictors in our unmarked dataframe with the scaled values:
siteCovs( umfr )[,c("jtime","jday") ] <- cbind(rsctime, rscday)


# View summary of unmarked dataframe:
summary( umfr ) 

##### ROPT Model ##########

#male ropt, half normal
fm.rhn <- distsamp(~1 + jtime + jday + (1 | transect) + (1 | obsv_pair) #detection
                   ~1 + as.factor(year), #abundance. year as factor so we can model yearly estimates
                   data = umfr,
                   keyfun = "halfnorm",
                   output = "abund")
fm.rhn 

#male ropt, hazard
fm.rha <- distsamp(~1 + jtime + jday + (1 | transect) + (1 | obsv_pair) #detection
                   ~1 + as.factor(year), #abundance
                   data = umfr,
                   keyfun = "hazard",
                   output = "abund")
fm.rha #took longer to converge (like a minute or two). 

##### ALL MODELS #####
fm.whn #wipt male half normal
#mean abundance (exponentiated to take out of log scale)
exp(coef(fm.whn[1]))
#confidence intervals of detection coefficients
confint( fm.whn, type = "det" )

fm.wha #wipt male hazard
exp(coef(fm.wha[1]))
confint( fm.wha, type = "det" )

fm.rhn #ropt male half normal
exp(coef(fm.rhn[1]))
confint( fm.rhn, type = "det" )

fm.rha #ropt male hazard
exp(coef(fm.rha[1]))
confint( fm.rha, type = "det" )

#Time of day had an effect on wipt detection but not ropt detection (later in the day = less detections)
#hazard has way higher estimates but has lower AIC

#### Goodness of fit #####
# We rely on unmarked options for boostrap goodness-of-fit option in #
# combination with custom built function fitstats, from Kery and Royle's
# Hierarchical modelling book, Chpt 7 pg 333. Also see example in ?parboot.
# The function computes error sum-of-squares, standard Chi-square and the #
# Freeman-Tukey statistic:
# Function returning three fit-statistics.
# fitstats <- function(fm) {
#   observed <- getY(fm@data)
#   expected <- fitted(fm)
#   resids <- residuals(fm)
#   sse <- sum(resids^2)
#   chisq <- sum((observed - expected)^2 / expected)
#   freeTuke <- sum((sqrt(observed) - sqrt(expected))^2)
#   out <- c(SSE=sse, Chisq=chisq, freemanTukey=freeTuke)
#   return(out)
# We are looking for: p > 0.05
(gof.whn <- parboot(fm.whn, fitstats2, nsim = 100)) #failed all tests
(gof.wha <- parboot(fm.wha, fitstats2, nsim = 100)) #will take like 11 hours
(gof.rhn <- parboot(fm.rhn, fitstats2, nsim = 100)) #failed all tests
(gof.rha <- parboot(fm.rha, fitstats2, nsim = 100)) #took over an hour and gave warning: model fitting.failed in 5 sims. Passed almost all tests and is not overdispersed.

#chat of chi squared based on royle and kery. ratio of observed and mean expected value of chi2
(c.hat <- gof.whn@t0[2] / mean(gof.whn@t.star[,2])) #255.6841, super overdispersed
(c.hat <- gof.wha@t0[2] / mean(gof.wha@t.star[,2]))
(c.hat <- gof.rhn@t0[2] / mean(gof.rhn@t.star[,2])) #1.444632 
(c.hat <- gof.rha@t0[2] / mean(gof.rha@t.star[,2])) #1.210343 


#### Plotting ####
#Marginalized effect/partial prediction plots for mean annual abundances
yearabun <- data.frame( year = factor( yrrange ), levels = yrrange )
#view
yearabun

#Now predict partial relationship between year and abundance
pred.yearabun <- predict( fm.rhn, type = "state", newdata = yearabun, 
                          appendData = TRUE ) #just going to manually change out the model here.
#view
pred.yearabun

# Plot:
plot.yearabun <- pred.yearabun %>%
  # define x and y values
  ggplot(., aes( x = year, y = Predicted, color = year ) ) + 
  #choose preset look
  theme_bw( base_size = 15 ) +
  #remove legend
  theme( legend.position = "none" ) +
  # add labels
  labs( x = "Year", y = "Predicted Abundance", title = "Male ROPT Half Normal" ) +
  #add mean abundance for each year
  geom_point( size = 4 ) +
  # add confidence intervals
  geom_errorbar( aes(ymin = lower, ymax = upper ), 
                 size = 1.5, width = 0.3 ) 
#view
plot.yearabun

## Plot significant det covariates (jtime for just wipt):
n <- 100 #100 evenly spaced values
# we use the observed values unscaled to define our range and create 100 values:
timeday <- seq( min( trunccov[,"jtime"]),max( trunccov[,"jtime"]),
                  length.out = n )
dayyear <- seq( min( trunccov[,"jday"]),max( trunccov[,"jday"]),
                length.out = n )

#standardize/scale them
timeday.std <- standardise( as.matrix(timeday), stdevs = 1, marg = 2 )
dayyear.std <- standardise( as.matrix(dayyear), stdevs = 1, marg = 2 )
#combine standardized predictor into a new dataframe to predict partial relationship
# We replace value of other predictor with its mean
colData <- data.frame( Min_Past_5am = timeday.std, day_of_year = 0 )

#predict partial relationship 
pred.col.jtime <- predict( fm.whn, type = "det", newdata = colData, 
                          appendData = TRUE ) #gives me an error here (invalid type(list) for variable 'jtime') and I can't figure it out
#view
head( pred.col.jtime); dim( pred.col.jtime )

# create plots for ecological submodels
#Starting with sagebrush and colonization:
# select the predicted values we want to plot and combine with unscaled predictor
jtimep <- cbind( pred.col.jtime[,c("Predicted", "lower", "upper") ], Min_Past_5am ) %>%
  # define x and y values
  ggplot(., aes( x = Min_Past_5am, y = Predicted ) ) + 
  #choose preset look
  theme_bw( base_size = 15 ) +
  # add labels
  labs( x = "Minutes Past 5 AM", y = "Detection" ) +
  # add band of confidence intervals
  geom_smooth( aes(ymin = lower, ymax = upper ), 
               stat = "identity",
               size = 1.5, alpha = 0.5, color = "grey" ) +
  # add mean line on top
  geom_line( size = 2 ) 
#view
jtimep

##########    save relevant data and workspaces     ###########

#save workspace
save.image( "PtarmAnalysisClass.RData" )

########## End of saving section ##################################

############# END OF SCRIPT ########################################