### Database Check

## libraries
library(tidyverse)
library(RColorBrewer)

## load data
chl <- read.csv("databasePublished//chl.csv")
lakeID <- read.csv("databasePublished//uniqueID.csv")

## functions
se <- function(x) sqrt(var(x)/length(x))


## plot distribution of Chl
ggplot(chl, aes(x=log(ChlValues))) + geom_density(fill="#9ACC7C") + xlab("Log transformed Chlorophyll a (mg/L)") + ylab("Frequency of observered values") + 
  annotate("text", x=1, y=0.3, label=paste("minimum = 0 mg/L"), size=5) + ## min
  annotate("text", x=1, y=0.27, label=paste("maximum = ", round(max(chl$ChlValues),2), "mg/L"), size=5)  + ## max
  annotate("text", x=1, y=0.24, label=paste("median ± se = ",round(median(chl$ChlValues),3), "±", round(se(chl$ChlValues),5), "mg/L"), size=5) +theme(axis.text=element_text(size=12),
                                                                                                                                                            axis.title=element_text(size=14,face="bold"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + xlim(-10,6)
## Map of Lakes

## define trophic levels
## based on Carlson, R.E. (1977) A trophic state index for lakes. Limnology and Oceanography. 22:2 361–369.
lakeChl <- merge(lakeID, chl, by="uniqueID")
lakeChl[,"TSI"] <- ifelse(lakeChl$ChlValues<0.0026, "1. Oligotrophic",
                          ifelse(lakeChl$ChlValues<0.02, "2. Mesotrophic",
                                 ifelse(lakeChl$ChlValues<0.056, "3. Eutrophic","4. Hypereutrophic")))

## plot out global distribution of lake chl
mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld

mp <- mp+ geom_point(data=lakeChl , aes(x=Lon, y=Lat, color=TSI),  size=2, alpha=0.5)  +
  theme(legend.position="right", text = element_text(size=10)) + scale_colour_brewer()
mp


## check values of other variables
chem <- read.csv("databasePublished//waterchem.csv")
lakechar <- read.csv("databasePublished//LakeChar.csv")


### check proportion of data with values
reported <- function(x){
  (length(x)-sum(is.na(x)))/length(x)*100
}

reported(chem$TP.value)
reported(lakechar$pH)