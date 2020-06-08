## necessary libraries
library(raster)
library(tidyverse)
library(ncdf4)

## load data
data <- read.csv("database//uniqueID.csv")

## Create spatial points dataframe
gps <- data ## assign data to separate dataframe
coordinates(gps) <- ~Lon+Lat ## define lat and lon
crs.world <-CRS("+proj=longlat +datum=WGS84") ## define CRS
proj4string(gps) <- crs.world ## assign CRS to coordinates

## list variables
occurdat <- list.files("C:\\Users\\Alessandro\\Downloads\\", pattern="*.nc", recursive = TRUE,  full.names = TRUE)


## run loop to extract each band
crudat <- data.frame() ## create empty dataframe
for(i in 548:1404){ ## start with the first year of study
temp <- raster(occurdat[4], band=i) ## select band iteration
values <- raster::extract(temp, gps) ## extract from raster based on coordinates
time.frame <- temp@z[[1]] ## extract timeframe
temporary <- data.frame(gps$uniqueID, temp=values, date=time.frame)  ## assign to temporary dataframe 
crudat <- rbind(crudat, temporary) ## join temporary dataframe to master dataframe
print(i) ## print iteration to ensure its working
}

## assign new date columns
crudat["year.raster"] <- format(as.Date(crudat$date, format="%Y-%m-%d"),"%Y")
crudat["month.raster"] <- format(as.Date(crudat$date, format="%Y-%m-%d"),"%m")


## convert each year to numeric values from characters
year.avg$year.raster <- as.numeric(year.avg$year.raster)
year.avg$year <- as.numeric(year.avg$year)

## select study years only
cur.avg <-  year.avg[year.avg$year==year.avg$year.raster,] ## year study was conducted
lag.avg <-  year.avg[year.avg$year==year.avg$year.raster+1,] ## year before study was conducted



## run loop to download selected year
crudat <- data.frame()

for(i in 1:63){
## Select year for iteration
yearTarget <- ((1955+i)-1901)*12 ## determine surveyed year from multiples of 12
yearRange <- seq(yearTarget-11,yearTarget+12, 1) ## generate range of years in multiples of 12 for dataset

tryCatch({ ## put error catch for years without lakes surveyed
## subset GPS points by that year
gpsYear <- subset(gps, Year==(1955+i))
if (nrow(gpsYear)==0) stop("No lakes surveyed that year")

yearData <- data.frame() ## create empty dataframe
for(j in yearRange){
temp <- raster(occurdat[1], band=j) ## select band iteration
values <- raster::extract(temp, gpsYear) ## extract from raster based on coordinates
time.frame <- temp@z[[1]] ## extract timeframe
temporary <- data.frame(gpsYear$uniqueID, variable=values, date=time.frame,  ## assign to temporary dataframe 
	surveyYear=ifelse(j < max(yearRange)-11, "lag","survey"))  ## create lag year column based on lower 12 months of yearly range
yearData <- rbind(yearData, temporary) ## join temporary dataframe to master dataframe
}
crudat <- rbind(crudat, yearData)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
print(i)
}

## move lag year data into a separate column
crudat[crudat$surveyYear=="survey","variable.lagged"] <- crudat[crudat$surveyYear=="lag", "variable"] ## move lag data into its own column
crudat <- crudat[crudat$surveyYear=="survey",] ## drop lag year data

## assign numeric date columns
crudat["Year"] <- as.numeric(format(as.Date(crudat$date, format="%Y-%m-%d"),"%Y"))
crudat["Month"] <- as.numeric(format(as.Date(crudat$date, format="%Y-%m-%d"),"%m"))


## Repeat for other variables and assign to datframe

## rename variable
names(crudat)[c(2,5)] <- c("pet","pet.lag")
pet <- crudat

## drop repeated columns
tmax <- tmax[,-c(3:4)]
tmin <- tmin[,-c(3:4)]
pre <- pre[,-c(3:4)]
pet <- pet[,-c(3:4)]

## rename unique ID column
names(tmax)[1] <- "uniqueID"
names(tmin)[1] <- "uniqueID"
names(pre)[1] <- "uniqueID"
names(pet)[1] <- "uniqueID"

fullCru <- merge(tmax, tmin, by=c("uniqueID","Year","Month")) ## join tmax and tmin 
fullCru <- merge(fullCru, pre, by=c("uniqueID","Year","Month")) ## join precipitation
fullCru <- merge(fullCru, pet, by=c("uniqueID","Year","Month")) ## join potential evapotranspiration

write.csv(fullCru, "database//CRUmissing.csv", row.names=FALSE)



## solar radiation dataset

## list variables
occurdat <- list.files("C:\\Users\\Alessandro\\Downloads\\", pattern="*.tif", recursive = TRUE,  full.names = TRUE)

srad <- data.frame()
for(i in 1:12){
solar <- raster(occurdat[15+i])
values <- data.frame(raster::extract(solar, gps))
values[,"month"] <- i
values[,"uniqueID"] <- gps$uniqueID
srad <- rbind(srad, values)
print(i)
}

names(srad)[1] <- "values"

srad.wide <- tidyr::spread(srad, month, values)
write.csv(srad.wide, "database//solarRadiation.csv", row.names=FALSE)

