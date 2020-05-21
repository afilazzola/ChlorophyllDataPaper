library(tidyverse)

## Load data and select only important variables

## Load lakes
lakes <- read.csv("LAGOS//LAGOSNE_lakeslocus.csv")
names(lakes)
lakes <- lakes[,c("lagoslakeid","nhd_lat","nhd_long","elevation_m")]

## Load Nutrients
chem <- read.csv("LAGOS//LAGOSNE_epinutr.csv")
chem <- chem[,c("lagoslakeid", "sampleyear", "samplemonth","chla","tn","tp","doc","secchi")]

## Load morphology
morph <- read.csv("LAGOS//LAGOSNE_lakeslimno.csv")
morph <- morph[,c("lagoslakeid","meandepth","maxdepth")]

## Merge all variables
allvars <- Reduce(merge, list(lakes, chem, morph))

## drop missing chl
allvars <- allvars %>% filter(! is.na(chla) )

## unique lakes only and round to the cloest 500 m
uniqueLake <- allvars[!duplicated(allvars[,c("nhd_lat","nhd_long")]),]
uniqueLake$Lat <- round(uniqueLake$nhd_lat,2)
uniqueLake$Lon <-  round(uniqueLake$nhd_long,2)


## Load Chl data and round to the cloest 500 m
chldata <- read.csv("data//ChlaData.csv")
chldata <- chldata[,c("Lat","Lon","StudyID","UniqueLakeName")]
chldata <- chldata[!duplicated(chldata[,c("Lat","Lon")]),]
chldata$Lat <- round(chldata$Lat ,2)
chldata$Lon <- round(chldata$Lon ,2)

## Find similar lakes based on 500 m
joinLake <- merge(uniqueLake, chldata, by=c("Lat","Lon"), all.x=T)
missingLakes <- joinLake %>% filter(is.na(UniqueLakeName)) ## drop 
matchLakes <- joinLake %>% filter(!is.na(UniqueLakeName))

write.csv(matchLakes, "matchingID.csv")

## Drop repo14 because largely duplicated
chldata <- chldata %>% filter(StudyID != "repo14")

## find lakes that are likely independent (>10 km) 
library(sp)
library(rgeos)

## Make spatial points
coordinates(chldata) <- ~Lon+Lat
proj4string(chldata) <- CRS("+proj=longlat +datum=WGS84")
coordinates(missingLakes) <- ~Lon+Lat
proj4string(missingLakes) <- CRS("+proj=longlat +datum=WGS84")

## Crop to same extent
chldata <- raster::crop(chldata, missingLakes)


write.csv()

## Drop lakes with points >100 km likely independent
missingLakes$nearest_in_set2 <-geosphere::distGeo( coordinates(missingLakes), coordinates(chldata))

missingLakes <- subset(missingLakes, nearest_in_set2<100)

write.csv(data.frame(missingLakes), "remainingLAGOS.csv")
write.csv(data.frame(chldata), "remainingChla.csv")

### Join all vars to lake ID
lakeCodes <- read.csv("LAGOSids.csv")
allvarsID <- merge(allvars, lakeCodes, by="lagoslakeid")

write.csv(allvarsID, "LAGOSwID.csv", row.names=F)

## Revise remaining lakes
edits <- read.csv("LAGOS//edits.csv", stringsAsFactors = F)
lagosID <- read.csv("LAGOS//LAGOSwID.csv" , stringsAsFactors = F)

## Cycle through edits based on maps
for(i in 1:nrow(edits)){
editIter <- edits[i,"LAGOS.ID"]
editRevise <- edits[i,"Unique.ID"]
lagosID[lagosID$lagoslakeid==editIter, "LakeID"] <- editRevise
}

write.csv(lagosID, "LAGOS//LAGOSwID.csv" , row.names = F)