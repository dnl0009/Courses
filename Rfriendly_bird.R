####### SET THE STAGE ########
setwd("C:/Users/Darien Lozon/Google Drive/SP19 Courses/WMAN633/Project/")
season1 <- read.csv("Data 2016-2017.csv",header=T)
season2 <- read.csv("Data 2017-2018.csv",header=T)
# Clean up data; I don't need notes or easement info
# I also don't need the deviation from 1st and 2nd rounds.

# Katy used flock different from how I did.. 
# I'll get rid of the flock column and create a number column.. it'll help when I aggregate.
# We'll just start with season 1 for now.

season1 <- season1[c(1:1028,1031:2371),c(1:9,11:12)] # Something was funky when calling this csv in...

####### CLEAN UP DATA #######
season1$Temp.C <- round(season1$Temp.C,3)

# Have to make the observer names consistent (Observer could potentially be a detection covariate)
unique(season1$Observer) # The observer codes are a mess
season1$Observer[season1$Observer == "kel"] = 'KEL'
season1$Observer[season1$Observer == "KEL, CDH"] = 'KEL,CDH' # Can't create new levels
season1$Observer[season1$Observer == "KEL/CDH"] = 'KEL,CDH'  # Well, you can, but I don't know how
season1$Observer[season1$Observer == "KEL, TC"] = 'KEL,TC'   # So we'll work with what's already there
season1$Observer <- factor(season1$Observer) # All fixed

# Names in the excel spreadsheet were not consistent either
unique(season1$Site) # I'm cringing
season1$Site[season1$Site == 'dnr ref '] = 'dnr ref'
season1$Site[season1$Site == 'Shabb reference'] = 'shabb ref'
season1$Site[season1$Site == 'Moore-ref'] = 'Moore ref'
season1$Site[season1$Site == 'Wolfe-ref'] = 'Wolfe ref'
season1$Site[season1$Site == 'sheets-brock'] = 'Sheets Brock'
season1$Site[season1$Site == 'Burr-ref'] = 'Burr ref'
season1$Site[season1$Site == 'Thorn Reference'] = 'Thorn ref'
season1$Site[season1$Site == 'allen'] = 'Allen'
season1$Site[season1$Site == 'sites'] = 'Sites'
season1$Site <- factor(season1$Site) # Good deal

unique(season1$Spp) # I'm afraid to look

# Oookay, here we go with the crazy embedded ifelse for a number column we'll aggregate later..
# If someone has a more efficient way to do this, I'm all ears, but this ifelse statement is oddly beautiful.
season1$num <- ifelse(season1$Spp == 'AMGO x9','9',
                      ifelse(season1$Spp == 'CANG x14','14',
                             ifelse(season1$Spp == 'AMGO x7','7',
                                    ifelse(season1$Spp == 'CANG x21','21',
                                           ifelse(season1$Spp == 'MALL x11','11',
                                                  ifelse(season1$Spp == 'CANG x20','20',
                                                         ifelse(season1$Spp == 'UNCH x6','6',
                                                                ifelse(season1$Spp == 'AMCR x4','4',
                                                                       ifelse(season1$Spp == 'AMCR x3','3',
                                                                              ifelse(season1$Spp == 'AMRO x5','5',
                                                                                     ifelse(season1$Spp == 'BLJA x5','5',
                                                                                            ifelse(season1$Spp == 'BLJA x2','2',
                                                                                                   ifelse(season1$Spp == 'DEJU x6','6',
                                                                                                          ifelse(season1$Spp == 'AMGO x2','2',
                                                                                                                 ifelse(season1$Spp == 'BLJA x4','4',
                                                                                                                        ifelse(season1$Spp == '', '0','1')))))))))))))))) # They recorded when no birds were observed

## Now that the numbers are accounted for, I can change the names
season1$Spp[season1$Spp == 'AMGO x9'] = 'AMGO'
season1$Spp[season1$Spp == 'CANG x14'] = 'CANG' # CAGO could be cackling or canada, yikes.
season1$Spp[season1$Spp == 'AMGO x7'] = 'AMGO'
season1$Spp[season1$Spp == 'CANG x21'] = 'CANG'
season1$Spp[season1$Spp == 'MALL x11'] = 'MALL'
season1$Spp[season1$Spp == 'CANG x20'] = 'CANG'
season1$Spp[season1$Spp == 'SOSP '] = 'SOSP'

# I don't think I want any of the unknown anythings, but I'll keep them for now.
season1$Spp[season1$Spp == 'UNCH x6'] = 'UNCH'
levels(season1$Spp)[levels(season1$Spp) == 'unwo'] = 'UNWO'

season1$Spp[season1$Spp == 'AMCR x4'] = 'AMCR'
season1$Spp[season1$Spp == 'AMCR x3'] = 'AMCR'
season1$Spp[season1$Spp == 'AMRO x5'] = 'AMRO'
season1$Spp[season1$Spp == 'BLJA x5'] = 'BLJA'
season1$Spp[season1$Spp == 'BLJA x2'] = 'BLJA'
season1$Spp[season1$Spp == 'BLJA x4'] = 'BLJA'
season1$Spp[season1$Spp == 'DEJU x6'] = 'DEJU'
season1$Spp[season1$Spp == 'AMGO x2'] = 'AMGO'
season1$Spp[season1$Spp == 'AMGO '] = 'AMGO'
season1$Spp[season1$Spp == 'AMCR '] = 'AMCR'

# Need to account for absences as well
levels(season1$Spp)[levels(season1$Spp) == ''] = 'SOSP' 
season1$Spp <- factor(season1$Spp)

season1$Date <- as.Date(season1$Date,format='%m/%d/%Y') # R friendly reformatting
season1$Date <- as.factor(season1$Date) # But I do want dates as factors 

# Fixing date
season1$Date[season1$Date == '0016-12-20'] = '2016-12-20'
season1$Date[season1$Date == '0017-01-13'] = '2017-01-13'
season1$Date[season1$Date == '0017-01-14'] = '2017-01-14'
season1$Date <- factor(season1$Date)

# This is for changing levels themselves... different from the commands above (learn something new every day)
levels(season1$Date)[levels(season1$Date) == "0017-01-15"] = "2017-01-15"
levels(season1$Date)[levels(season1$Date) == "0017-01-20"] = "2017-01-20"
levels(season1$Date)[levels(season1$Date) == "0017-01-21"] = "2017-01-21"
levels(season1$Date)[levels(season1$Date) == "0017-01-28"] = "2017-01-28"
levels(season1$Date)[levels(season1$Date) == "0017-01-29"] = "2017-01-29"
levels(season1$Date)[levels(season1$Date) == "0017-02-03"] = "2017-02-03"
levels(season1$Date)[levels(season1$Date) == "0017-02-04"] = "2017-02-04"

season1$Date <- as.Date(season1$Date,format='%Y-%m-%d') # Don't think this needs to be as.date, but why not?

####### REF vs. ACEP DUMMY CODING #######
# Didn't need this code after all...
ref <- c("Allen ref","Burr ref","dnr ref","doa ref","Donofrio ref","Dunker ref","Mearns ref","Moore ref",
         "shabb ref","sheets-brok/ks ref","Sheets/Brock reference","sites ref","Thorn ref","Wolfe ref")
season1$Site_Type <- as.factor(ifelse(season1$Site %in% ref,'REF','ACEP')) # Ref absorbed into intercept (For n-mix? Not sure)

####### POINT COUNT AGGREGATION #######
# Try to aggregate the whole season1 dataset and calculate abundance by POINT COUNT
# Because each point count is going to be treated as an individual event
season1$num <- as.numeric(season1$num) # crazy comes back with an error saying something is a factor if this doesn't happen
bypt <- aggregate(season1$num,by=list(season1$Date,season1$Point,season1$Spp),FUN=sum)
bypt <- bypt[with(bypt,order(Group.1,Group.2)),] # Something I found on stackoverflow to order data chronologically
colnames(bypt) <- c("Date","Point","Spp","Num")
head(bypt) # This is what I want, but I also want the other data associated with each date/point combination
# test <- merge(season1,crazy,by=c("Date","Point","Spp")) # This is SO close to what I need, but there are two AMCR and SOSP 

sosp <- bypt[bypt$Spp == 'SOSP',] # But this doesn't account for sites that didn't have SOSP (absence is important)

# Can plot, but this doesn't mean much right now.
plot(x=sosp$Date,y=sosp$Num,type='p',pch=16) # Cool.

####### VERTICAL DENSITY DATA #######
# Could possibly use as a site-level covariate.
# Good to take note that the vegetation was only measured at point counts, NOT transects
veg1 <- read.csv("Vegetation Data 2016-2017.csv",header=TRUE)
levels(veg1$date) #Check date formats %y vs %Y
veg1$date <- as.Date(veg1$date,format="%m/%d/%Y")
veg1$date <- as.factor(veg1$date)
levels(veg1$date)[levels(veg1$date) == '0017-01-14'] = '2017-01-14' # The only date that needs to be changed
veg1$date <- as.Date(veg1$date,format="%Y-%m-%d")
veg1 <- veg1[,1:7] # Weird x column with a bunch of NA values

# For some reason, when directly changing the max ht to numeric, it converted the levels, not the values... 
veg1$max.height <- as.character(veg1$max.height)
veg1$max.height <- as.numeric(veg1$max.height) 
veg1 <- aggregate(veg1$max.height,by=list(veg1$date,veg1$sampling.pt),FUN=mean)
veg1 <- veg1[with(veg1,order(Group.1)),]
colnames(veg1) <- c("Date","Point","Mean.Max.Ht")
head(veg1)
veg1$Mean.Max.Ht <- round(veg1$Mean.Max.Ht,3) # Round nasty decimals
head(veg1) # site-level covariate

#mat <- matrix(meta,nrow=nrow(meta),ncol=length(meta)) # Meh
#mat <- meta[,c(1,2,4)]
#mat <- matrix(mat,nrow=length(unique(mat$Date.x)),ncol=length(unique(mat$Point)),dimnames=c('Date','Point'))
#tab <- table(sosp$Point,sosp$Date) # this is what I need, but it's in ones and zeros instead of the actual abundance

###### CATCHING UP ######
setwd("C:/Users/Darien Lozon/Google Drive/SP19 Courses/WMAN633/Project/")
load('bird.RData') # sosp,tab,bypt,season1,veg1


# Need to remove transect birds from sosp because veg is only measured at points

sosp$rowname <- rownames(sosp) # Need a form of ID for the transect point names

# Isolate rows with transect data
tr <- c(6 ,7, 8,18,19,20,21,22,26,27,28,29,30,31,35,36,46,47,48,49,50,
        51,52,53,54,55,56,67,68,69,74,75,76,77,78,80,82,85,86,87,88,90,
        105,106,107,108,109,110,111,112,113,114,118,119,120,127,128,129,130,137,138)

# Take them out
upd_sosp <- sosp[!(sosp$rowname %in% tr),]

# Check work
unique(upd_sosp$Point) # Voici! 
upd_sosp <- upd_sosp[,c(1,2,4)] # Can get rid of the rowname and spp columns
upd_sosp$Point <- factor(upd_sosp$Point) # Update point levels
head(upd_sosp)

# Now I need a table of the updated sosp
# tab <- table(upd_sosp$Point,upd_sosp$Date) # NOT WHAT I WANT.

###### WHAT I WANT!!! ######
pts <- matrix(unique(season1$Point),ncol=1) # Point location names
colnames(pts) <- c('Point')                 # Fancy names
round1 <- upd_sosp[1:84,]; round2 <- sosp[85:138,] # Separate the two visits for each point

# These steps are used to ID which points don't have p or psi covs
test <- merge(pts,round1,by='Point',all=TRUE); head(test) 
test <- merge(test,round2,by='Point',all=TRUE); head(test)
test <- test[,c(1,3,6)]; head(test)
test[is.na(test)] <- 0 # Get rid of NAs
head(test); colnames(test) <- c('point','sosp1','sosp2')
head(test) # AWESOME

test$rowname <- rownames(test) # Need a form of ID for the transect point names

# Isolate rows with transect data to eliminate later
tr <- c(7:12,25:32,39:44,50:53,58:60,72:80,92:98,109:116,124:126,
        130:131,142:150,155:156,160:161,
        178:187,197:203,212:217,221,228:239)
transects <- test$point[tr]

# Take them out
SOSP <- test[!(test$rowname %in% tr),] # HUZZAAHHH!
# Checkpoint: save(bypt,season1,SOSP,sosp,test,upd_sosp,veg1,tr,transect,pts,tab,file='bird.RData')

###### Need to check lengths ######

# What point counts are missing from the covariates?
psi_covs <- read.csv('psi_covs.csv',header=TRUE); psi_covs <- psi_covs[psi_covs$year =='0',]
p_covs <- read.csv('p_covs.csv',header=TRUE); p_covs <- p_covs[1:87,]
trees <- read.csv('trees.csv',header=TRUE); colnames(trees) <- c('site','date','point','count')

check <- merge(SOSP,p_covs,by='point',all.x=TRUE)
check <- merge(check,trees,by='point',all.x=TRUE)
check <- merge(check,psi_covs,by='point',all.x=TRUE); View(check)
check <- na.omit(check); View(check)

# Separate data frames
SOSP <- check[,2:3]
p_covs <- check[,c(5:25)]
psi_covs <- check[,26:38]
# YAAAAAAAS.

###### unmarked... FINALLY ######
library(unmarked)
SOSP <- as.matrix(SOSP) # make matrix
sosp_dat <- unmarkedFramePCount(y=SOSP)
fit <- pcount(~1~1,sosp_dat,K=100) # Intercept only model

# Model and interpretation
summary(fit)
plogis(-0.0534) # Detection probability (49%)
exp(0.674) # Expected abundance (2 birds)

# Check classes of psi_covs
psi_covs$reclass_bg1 <- factor(psi_covs$reclass_bg1)
psi_covs$reclass_shr1 <- factor(psi_covs$reclass_shr1)
psi_covs$reclass_heb1 <- factor(psi_covs$reclass_heb1)
psi_covs$reclass_wood1 <- factor(psi_covs$reclass_wood1)
psi_covs$reclass_wat1 <- factor(psi_covs$reclass_wat1)
psi_covs$reclass_shr5 <- factor(psi_covs$reclass_shr5)
psi_covs$type <- factor(psi_covs$type)

# Check classes of p_covs
p_covs$sky.1 <- factor(p_covs$sky.1)
p_covs$sky.2 <- factor(p_covs$sky.2)

det_covs <- list(
  time = data.frame(p_covs[,c('time.1','time.2')]),
  sky = data.frame(sky.1 = factor(p_covs$sky.1),
                   sky.2 = factor(p_covs$sky.2)),
  wind = data.frame(p_covs[,c('wind.1','wind.2')]),
  temp = data.frame(p_covs[,c('temp.1','temp.2')]),
  dist = data.frame(p_covs[,c('dist.1','dist.2')])
)

nmix_dat <- unmarkedFramePCount(y=SOSP,            # abundance
                                siteCovs=psi_covs, # site covs
                                obsCovs=det_covs)  # detection covs
FIT_FINALLY <- pcount(~temp+wind+sky~count+reclass_shr5+reclass_wat1+reclass_heb1,
                      nmix_dat,K=100)
summary(FIT_FINALLY)

# Poisson fit models
fit1 <- pcount(~time+temp+sky+dist+wind~size+type+reclass_wat1+reclass_shr1+reclass_shr5,nmix_dat,K=100)
fit2 <- pcount(~time+temp+dist+wind~type+reclass_wat1+reclass_shr1+reclass_shr5,nmix_dat,K=100)
fit3 <- pcount(~time+dist+wind~type+reclass_shr1+reclass_shr5,nmix_dat,K=100)
fit4 <- pcount(~time+dist+wind~type+reclass_shr5,nmix_dat,K=100)
fit5 <- pcount(~time+dist+wind~type+reclass_shr1,nmix_dat,K=100)
fit6 <- pcount(~time+dist~type+reclass_shr1+reclass_shr5,nmix_dat,K=100)

# Negative binomial models
fit1N <- pcount(~time+temp+sky+dist+wind~size+type+reclass_wat1+reclass_shr1+reclass_shr5,nmix_dat,K=100,"NB")
fit2N <- pcount(~time+temp+dist+wind~type+reclass_wat1+reclass_shr1+reclass_shr5,nmix_dat,K=100,"NB")
fit3N <- pcount(~time+dist+wind~type+reclass_shr1+reclass_shr5,nmix_dat,K=100,"NB")
fit4N <- pcount(~time+dist+wind~type+reclass_shr5,nmix_dat,K=100,"NB")
fit5N <- pcount(~time+dist+wind~type+reclass_shr1,nmix_dat,K=100,"NB")
fit6N <- pcount(~time+dist~type+reclass_shr1+reclass_shr5,nmix_dat,K=100,"NB")

# Zero-inflated Poisson models
fit1Z <- pcount(~time+temp+sky+dist+wind~size+type+reclass_wat1+reclass_shr1+reclass_shr5,nmix_dat,K=100,"ZIP")
fit2Z <- pcount(~time+temp+dist+wind~type+reclass_wat1+reclass_shr1+reclass_shr5,nmix_dat,K=100,"ZIP")
fit3Z <- pcount(~time+dist+wind~type+reclass_shr1+reclass_shr5,nmix_dat,K=100,"ZIP")
fit4Z <- pcount(~time+dist+wind~type+reclass_shr5,nmix_dat,K=100,"ZIP")
fit5Z <- pcount(~time+dist+wind~type+reclass_shr1,nmix_dat,K=100,"ZIP")
fit6Z <- pcount(~time+dist~type+reclass_shr1+reclass_shr5,nmix_dat,K=100, "ZIP")
# Checkpoint save.image(file='LozonProjectCode.RData')

library(AICcmodavg)
cand.set <- list(
  M1 = fit1, M2 = fit2, M3 = fit3, M4 = fit4, M5 = fit5, M6 = fit6, # Regular poisson
  M1N = fit1N, M2N = fit2N, M3N = fit3N, M4N = fit4N, M5N = fit5N, M6N = fit6N, # Negative binom
  M1Z = fit1Z, M2Z = fit2Z, M3Z = fit3Z, M4Z = fit4Z, M5Z = fit5Z, M6Z = fit6Z # Zero-inflated poisson
)
mods <- aictab(cand.set=cand.set,second.ord=F); mods

## SLOPE COEFFICIENT AVERAGE FIDDLING ##

# Detection
time <- modavgShrink(cand.set=cand.set,
                     parm = 'time',
                     second.ord=F,
                     parm.type='detect')
dist <- modavgShrink(cand.set=cand.set,
                     parm = 'dist',
                     second.ord=F,
                     parm.type='detect')
wind <- modavgShrink(cand.set=cand.set,
                     parm = 'wind',
                     second.ord=F,
                     parm.type='detect')
detcoef <- list(time$Mod.avg.beta,dist$Mod.avg.beta,wind$Mod.avg.beta); detcoef
coef(fit3Z)[7:9] # Compare

# Lambda

# We did 'type' in class, so I will use that as a reason to not analyze it here.

shr1 <- modavgShrink(cand.set=cand.set,
                     parm = 'reclass_shr12',
                     second.ord=F,
                     parm.type='lambda')
shr52 <- modavgShrink(cand.set=cand.set,
                     parm = 'reclass_shr52',
                     second.ord=F,
                     parm.type='lambda')
shr53 <- modavgShrink(cand.set=cand.set,
                     parm = 'reclass_shr53',
                     second.ord=F,
                     parm.type='lambda')

# Second-order covariate removals from both detection and abundance
temp <- modavgShrink(cand.set=cand.set,
                     parm = 'temp',
                     second.ord=F,
                     parm.type='detect')

water <- modavgShrink(cand.set=cand.set,
                      parm = 'reclass_wat11',
                      second.ord=F,
                      parm.type='lambda')

# Why were these removed from the global model?

# These both end up being positive, which means they PROMOTE detection and 
# abundance, respectively, but they were not incorporated in other models because
# of high p-values. 

# This can be a reason of why it is not wise to incorporate the two paradigms 
# of p-values and model selection because there can be parameters that are positive
# but do not reach the "significance" threshold..?
###### THE END #####