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


## Revise some of the Lakes identified by Kevin
chla <- read.csv("data//ChlaDataV2.csv", stringsAsFactors = F)

## Combine some lake identifiers
new1 <- c("L1132","L1396","L1754","L2567","L2570","L2583","L2616","L2626","L2640","L2645","L2883","L2884","L3198","L3277","L3337","L4336","L5808","L6159","L6532")
old1 <- c("L8852","L1399","L1755","L8939","L8939","L8945","L8954","L8954","L8960","L8960","L8996","L8996","L9045","L8569","L9069","L9637","L9262","L9630","L6534")

## Cycle through edits based on maps
for(i in 1:length(new1)){
  editIter <- old1[i]
  editRevise <- new1[i]
  chla[chla$UniqueLakeName==editIter, "UniqueLakeName"] <- editRevise
}

## Revise some LAGOS datasets that were split
ID <- c("newid020591","newid023899","newid023900","newid023901","newid023903","newid023904","newid023905","newid023927","newid023928","newid023929","newid023930","newid023931","newid023932","newid023933","newid023934","newid023935","newid023936","newid023908","newid023909","newid023910","newid023911","newid023912","newid023913","newid023914","newid023915","newid023916","newid023917","newid023918","newid023919","newid023920","newid023921","newid023922","newid023923","newid023924","newid023925","newid023926","newid023937","newid023939","newid023940","newid023941","newid023946","newid023947","newid023948","newid023949","newid023950","newid023951","newid023952","newid023953","newid023956","newid023957","newid023958","newid023959","newid023960","newid023961","newid023962","newid023963","newid023964","newid023971","newid023979","newid023980","newid023981","newid023982","newid023983","newid009806","newid009808","newid006377","newid012380","newid023411","newid022301","newid009090","newid006518","newid020197","newid008798","newid022268","newid022269","newid022270")
lakeName <- c("LAGOS133931","LAGOS18242","LAGOS18242","LAGOS18242","LAGOS18242","LAGOS18242","LAGOS18242","LAGOS18242","LAGOS18242","LAGOS18242","LAGOS18242","LAGOS18242","LAGOS18242","LAGOS18242","LAGOS18242","LAGOS18242","LAGOS18242","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2161","LAGOS2287","LAGOS2287","LAGOS2501","LAGOS2961","LAGOS3575","LAGOS3688","LAGOS4712","LAGOS7544","LAGOS7544","LAGOS8199","LAGOS8199","LAGOS8199","LAGOS8199")
old2 <- c("L2962","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L9351","L4644","L4644","L2414","L6531","L7052","L3985","L3974","L2531","L2531","L3823","L3823","L3823","L3823")


## Cycle through edits based on maps
for(i in 1:length(ID)){
  ## Find the lake name
  replaceNew <- unique(chla[chla$LakeName==lakeName[i], "UniqueLakeName"])
  editIter <- ID[i]
  chla[chla$uniqueID==editIter, "UniqueLakeName"] <- replaceNew
}

write.csv(chla, "data//ChlaDataV2.csv", row.names=F)

## Append data to include surface area
chla <- read.csv("data//ChlaDataV2.csv", stringsAsFactors = F)
SA <- read.csv("LAGOS//LAGOSsurfacearea.csv", stringsAsFactors = F)

## Cycle through edits based on maps
for(i in 1:nrow(SA)){
  addSA <- SA[i, "surfaceArea"]
  lakeName <- SA[i, "LagosName"]
  chla[chla$LakeName==lakeName, "SurfaceArea"] <- addSA
}

sum(!is.na(chla$SurfaceArea))

write.csv(chla, "data//ChlaDataV2.csv", row.names=F)
