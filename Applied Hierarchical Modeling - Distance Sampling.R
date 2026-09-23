#######################################################################
##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )
# Check that you are in the right project folder
getwd()

# Install new packages from "CRAN" repository. # 
# AHM book package
install.packages( "AHMbook" )

# load packages:
library( tidyverse )#includes dplyr, tidyr and ggplot2
options( dplyr.width = Inf, dplyr.print_min = 100 )
library( unmarked ) #main package for frequentist population models
library( AHMbook ) #model validation package
library(raster)
## end of package load ###############
#   Applied hierarchical modeling in ecology
#   Modeling distribution, abundance and species richness using R and BUGS
#   Volume 1: Prelude and Static models
#   Marc K�ry & J. Andy Royle
#
# Chapter 8. Modeling abundance using hierarchical distance sampling (HDS)
# =========================================================================

# Approximate execution time for this code: 12 - 15 mins

# 8.4 Hierarchical Distance Sampling
# ==================================

# 8.4.1 HDS data structure and model (no code)
# 8.4.2 HDS in unmarked (no code)

# 8.4.3 Example: Estimating the global population size of the Island Scrub Jay. 
# 307 point counts up to 300 m binned into 3 100 m classes bc birds came closer to observers (responsive movement), which large distance classes should mitigate
# estimate global pop size, map of distribution E(N) as a function of local hab cov, predict distribution under alternative/historical landscapes. Livestock was just eradicated from the island allowing vegetation to return
# ------------------------------------------------------------------------
# Load, view and format the ISSJ data
data(issj)
round(head(issj), 2)

# Package things up into an unmarkedFrame
covs <- issj[,c("elevation", "forest", "chaparral")]
area <- pi*300^2 / 100^2             # Area in ha, bc point count
jayumf <- unmarkedFrameDS(y=as.matrix(issj[,1:3]),
                          siteCovs=data.frame(covs, area),
                          dist.breaks=c(0, 100, 200, 300),
                          unitsIn="m", survey="point") #survey = point count


# Fit model 1: chaparral as cov on both detection scale o and expected abundance lambda and elevation as cov on lambda (expected mean response of abundance). AIC favors this model
(fm1 <- distsamp(~chaparral ~chaparral + elevation + offset(log(area)),
                 jayumf, keyfun="halfnorm", output="abund"))

# Fit model 2: constant o (detection function parameter)
(fm2 <- distsamp(~1 ~chaparral + elevation + offset(log(area)),
                 jayumf, keyfun="halfnorm", output="abund"))


# check goodness-of-fit by bootstrapping the fitstats function
# (pb <- parboot(fm1, fitstats, nsim=1000, report=5))
(pb <- parboot(fm1, fitstats, nsim=100, report=5))  # ~~~~~ for testing
(c.hat <- pb@t0[2] / mean(pb@t.star[,2]))  # c-hat as ratio of observed
# and mean of expected value of Chi2 (under H0)
# (see, e.g., Johnson et al., Biometrics, 2010)
## model does not fit at all, since not a single bootstrap sample falls to the right of the observed value for any of the three statistics. c-hat indicates high degree of overdispersion: more unexplained variation in data that model assumes. either model is structurally wrong or we have unstructured noise

residuals(fm1)             # Can inspect residuals
# ~~~~~ only plot if using screen device ~~~~~
if(dev.interactive(orNone=TRUE))
  plot(pb)                   # Not shown
print(pb)


# Standardize the covariates to have fitting and analysis functions go more smoothly
sc <- siteCovs(jayumf)
sc.s <- scale(sc)
sc.s[,"area"] <- pi*300^2 / 10000  # Don't standardize area
siteCovs(jayumf) <- sc.s
summary(jayumf)


# Fit a bunch of models and produce a model selection table.
fall <- list()   # make a list to store the models

# With the offset output=abund is the same as output = density
fall$Null <- distsamp(~1 ~offset(log(area)), jayumf, output="abund")
fall$Chap. <- distsamp(~1 ~chaparral + offset(log(area)), jayumf,
                       output="abund")
fall$Chap2. <- distsamp(~1 ~chaparral+I(chaparral^2)+offset(log(area)),
                        jayumf, output="abund")
fall$Elev. <- distsamp(~1 ~ elevation+offset(log(area)), jayumf,
                       output="abund")
fall$Elev2. <- distsamp(~1 ~ elevation+I(elevation^2)+offset(log(area)),
                        jayumf, output="abund")
fall$Forest. <- distsamp(~1 ~forest+offset(log(area)), jayumf,
                         output="abund")
fall$Forest2. <- distsamp(~1 ~forest+I(forest^2)+offset(log(area)),
                          jayumf, output="abund")
fall$.Forest <- distsamp(~forest ~offset(log(area)), jayumf,
                         output="abund")
fall$.Chap <- distsamp(~chaparral ~offset(log(area)), jayumf,
                       output="abund")
fall$C2E. <- distsamp(~1 ~ chaparral + I(chaparral^2) + elevation +
                        offset(log(area)),jayumf, output="abund")
fall$C2F2. <- distsamp(~1 ~chaparral + I(chaparral^2) + forest +
                         I(forest^2)+offset(log(area)), jayumf,  output="abund")
fall$C2E.F <- distsamp(~forest ~chaparral+I(chaparral^2)+elevation+
                         offset(log(area)), jayumf, output="abund")
fall$C2E.C <- distsamp(~chaparral ~chaparral + I(chaparral^2) + elevation +
                         offset(log(area)), jayumf, output="abund")

# Create a fitList and a model selection table
(msFall <- modSel(fitList(fits=fall)))


# Check out the best model
fall$C2E.C


# Check out the goodness-of-fit of this model
(pb.try2 <- parboot(fall$C2E.C, fitstats, nsim=1000, report=5))

# Express the magnitude of lack of fit by an overdispersion factor
(c.hat <- pb.try2@t0[2] / mean(pb.try2@t.star[,2]))  #    Chisq

# Still bad. C-hat only slightly smaller. Jay’s are not uniformly distributed, so some aggregation can be explained by overdispersion (unstructured noise)
# Now use gdistsamp to fit negative binomial
# 
covs <- issj[,c("elevation", "forest", "chaparral")]
area <- pi*300^2 / 100^2             # Area in ha
jayumf <- unmarkedFrameGDS(y=as.matrix(issj[,1:3]),
                           siteCovs=data.frame(covs, area), numPrimary=1,
                           # numPrimary: number of sampling occasions within which it is reasonable to assume closed population. Since we sample once at each point, it’s 1, but if we did multiple at the same site separated by time in normal distance sampling survey, it would be number of temporal surveys (discussed later)
                           dist.breaks=c(0, 100, 200, 300),
                           unitsIn="m", survey="point")

#scale covs again
sc <- siteCovs(jayumf)
sc.s <- scale(sc)
sc.s[,"area"] <- pi*300^2 / 10000  # Don't standardize area
siteCovs(jayumf) <- sc.s
summary(jayumf)


# Fit the model using gdistsamp and look at the fit summary
(nb.C2E.C <- gdistsamp( ~chaparral + I(chaparral^2) + elevation +
                          offset(log(area)), ~1, ~chaparral, data =jayumf, output="abund",
                        mixture="NB", K = 150)) #Negative Binomial

# half-normal distance curve is the norm
# 3 submodels: lambda = abundance covariates, phi = availability covariates, p = detection covariates
gdistsamp(lambdaformula = ~chaparral + I(chaparral^2) + elevation +
            offset(log(area)), phiformula = ~1, pformula = ~chaparral,
          data = jayumf, output = "abund", mixture = "NB", K = 150)

# Goodness of fit
(pb.try3 <- parboot(nb.C2E.C, fitstats, nsim=1000, report=5)) # was going to take an hour

(c.hat <- pb.try3@t0[2] / mean(pb.try3@t.star[,2]))  
# was going to take an hour. Apparently produces a lot of warnings bc there is almost 0 probability of detection in the last cell (individuals > 300 m away). Model fits better according to 2 out of 4 statistics and overdispersion dec by 50%

# *Expected* population size for the sample points
getN <- function(fm, newdata=NULL) #computes sum of predicted values for a given model object
  sum(predict(fm, type="lambda", newdata=newdata)[,1])
getN(nb.C2E.C) # output: 889.6142
#can also input into parboot function to produce uncertainty values (SEs, CIs)

# This does the same thing as the following three commands
X <- model.matrix(~chaparral+I(chaparral^2)+elevation+log(offset(area)),
                  siteCovs(jayumf))
head(X) # The design matrix

# Prediction of total expected population size at the sample points
sum(exp(X %*% c(coef(nb.C2E.C, type="lambda"), 1))) #output: 889.6142

# Empirical Bayes estimates of posterior distribution:
# Pr(N=x | y, lambda, sigma) for x=0,1,...,K
re.jay <- ranef(nb.C2E.C, K = 150)

# *Realized* population size
sum(bup(re.jay, "mean")) # output: 827.4331
#won't be the same as the other methods bc it adjusts towards the observed data and is conditional on sample at hand


summary(jayumf) # Note the range of chaparral which we need to know

# Create a new data frame with area 28.27 ha, the area of a 300 m circle
chap.orig <- seq(0, 1, 0.01)    # Values from 0 to 1 prop. chaparral
chap.pred <- (chap.orig - mean(issj$chaparral)) / sd(issj$chaparral)
newdat <- data.frame(chaparral = chap.pred, elevation = 0, area=28.27)

# Expected values of N for covariate values in "newdat"
E.N <- predict(fall$C2E.C, type="state", newdata=newdat, appendData=TRUE)
head(E.N)

# Make a plot of the response curve for the grid of chaparral values
plot(chap.orig, E.N[,"Predicted"], xlab="Proportion chaparral",
     ylab="Predicted jay abundance", type="l", ylim = c(0, 20),
     frame = FALSE, lwd = 2)
matlines(chap.orig, E.N[,3:4], lty = 1, col = "grey", lwd = 1)

# take habitat map of whole island and predict expected abundance on every pixel (9 ha instead of 28 sample units, so account for that area change) of map
#need to scale landscape variables. Let's look at attributes of scaled cov first:
attributes(sc.s) # means are "scaled:center". SDs are "scaled:scale"

#apply values to mean and SD to landscape variables and predict for each pixel
cruz.s <- cruz   # Created a new data set for the scaled variables
cruz.s$elevation <- (cruz$elevation-202)/125
cruz.s$chaparral <- (cruz$chaparral-0.270)/0.234
cruz.s$area <- (300*300)/10000 # The grid cells are 300x300m=9ha
EN <- predict(nb.C2E.C, type="lambda", newdata=cruz.s)

# Total population size on whole island (by summing predictions for all pixels)
getN(nb.C2E.C, newdata=cruz.s)
# output: 1062.342

# Parametric bootstrap for CI
# A much faster function could be written to doing the sum. Will also take forever.
set.seed(2015)
(EN.B <- parboot(nb.C2E.C, stat=getN, nsim=1000, report=5))

#make a map
cruz.raster <- stack(rasterFromXYZ(cruz.s[,c("x","y","elevation")]),
                     rasterFromXYZ(cruz.s[,c("x","y","chaparral")]),
                     rasterFromXYZ(cruz.s[,c("x","y","area")]))
names(cruz.raster) # These should match the names in the formula

plot(cruz.raster)                      #  not shown
# Elevation map on the original scale (not shown)
plot(cruz.raster[["elevation"]]*125 + 202, col=topo.colors(20),
     main="Elevation (in feet) and Survey Locations", asp = 1)
points(issj[,c("x","y")], cex=0.8, pch = 16)


EN.raster <- predict(nb.C2E.C, type="lambda", newdata=cruz.raster)
plot(EN.raster, col = topo.colors(20), asp = 1)   # See Fig. 8-8

# end of analysis ######

############################################################################
################## Save your data and workspace ###################

# This time we want to save our workspace so that we have access to all #
# the objects that we created during our analyses. #
save.image( "HDS_ScrubJayExample.RData" )
