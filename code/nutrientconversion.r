library(tidyverse)

## Get water chemistry into its own dataset
data <- read.csv("data//master.database.csv")
data$ChlValues <-  as.numeric(as.character(data$ChlValues))
data <- data[!is.na(data$ChlValues),] ## drop NA


## omit 614 because anomalous
data <- data %>% dplyr::filter(!StudyID %in% c("614","876"))

## drop chl other than averages
data <- subset(data, ChlMeasure == "Avg")

## calculate means for unique year, month, study, and location
means <- data %>% group_by(StudyID, Year, Month, Lat, Lon, ChlUnits) %>% summarize(ChlValues=mean(ChlValues, na.rm=T))

id <- read.csv("database//uniqueID.csv")

## connect identifier with main dataset
chemdata <- merge(means, id, by=c("StudyID","Year","Month","Lat","Lon"))

## extract chlorophyll only

## Convert units first
## Switch text of mg/m2 to mg/m3 
chemdata[chemdata$ChlUnits=="mg/m2","ChlUnits"] <- "mg/m3"

## Switch text ppm to mg/L because equivalent
chemdata[chemdata$ChlUnits=="ppm","ChlUnits"] <- "mg/L"

## Switch text mg/m3 to mg/L
chemdata[chemdata$ChlUnits=="mg/m3","ChlUnits"] <- "ug/L"

## Convert  ug/L to mg/L
chemdata[chemdata$ChlUnits=="ug/L","ChlValues"] <-  chemdata[chemdata$ChlUnits=="ug/L","ChlValues"]/1000
chemdata[chemdata$ChlUnits=="ug/L","ChlUnits"] <- "mg/L" ## switch text

## Round ug/L values to the 1000th because accuracy below a nanogram unlikely
chemdata[chemdata$ChlMeasure=="Avg","ChlValues"] <- round(chemdata[chemdata$ChlMeasure=="Avg","ChlValues"],4)

## Omit other units
chemdata <- chemdata %>% filter(ChlUnits== "mg/L")


## simplified dataset
chl <- chemdata[,c("uniqueID","ChlUnits","ChlValues")]

## write to csv
write.csv(chl, "database//chl.csv", row.names=FALSE)


## other nutrients

## convert units first
source("database//unitConvert.r")

## phosphorus
means <- data %>% group_by(StudyID, Year, Month, Lat, Lon, TP.units) %>% summarize(TP.value=mean(TP.value, na.rm=T))
chemdata <- merge(means, id, by=c("StudyID","Year","Month","Lat","Lon"))
phosphorus <- convert(chemdata, "TP.units", "TP.value") ## convert phosphorus

##nitrogen
means <- data %>% group_by(StudyID, Year, Month, Lat, Lon, TN.units) %>% summarize(TN.value=mean(TN.value, na.rm=T))
chemdata <- merge(means, id, by=c("StudyID","Year","Month","Lat","Lon"))
nitrogen <- convert(chemdata, "TN.units", "TN.value") ## convert nitrogen

##nitrates
means <- data %>% group_by(StudyID, Year, Month, Lat, Lon, NO3NO2.units) %>% summarize(NO3NO2.value=mean(NO3NO2.value, na.rm=T))
chemdata <- merge(means, id, by=c("StudyID","Year","Month","Lat","Lon"))
nitrates <- convert(chemdata, "NO3NO2.units", "NO3NO2.value") ## convert nitrates

## Ammonia
means <- data %>% group_by(StudyID, Year, Month, Lat, Lon, NH3.units) %>% summarize(NH3.value=mean(NH3.value, na.rm=T))
chemdata <- merge(means, id, by=c("StudyID","Year","Month","Lat","Lon"))
ammonia <- convert(chemdata, "NH3.units", "NH3.value") ## convert ammonia

## Phosphates
means <- data %>% group_by(StudyID, Year, Month, Lat, Lon, PO4.units) %>% summarize(PO4.value=mean(PO4.value, na.rm=T))
chemdata <- merge(means, id, by=c("StudyID","Year","Month","Lat","Lon"))
phosphates <- convert(chemdata, "PO4.units", "PO4.value") ## convert phosphates

## Disolved oxy
means <- data %>% group_by(StudyID, Year, Month, Lat, Lon, DO.units) %>% summarize(DO.value=mean(DO.value, na.rm=T))
chemdata <- merge(means, id, by=c("StudyID","Year","Month","Lat","Lon"))
doxy <- convert(chemdata, "DO.units", "DO.value") ## convert dissolved oxygen

## DOC
means <- data %>% group_by(StudyID, Year, Month, Lat, Lon, DOC.units) %>% summarize(DOC.value=mean(DOC.value, na.rm=T))
chemdata <- merge(means, id, by=c("StudyID","Year","Month","Lat","Lon"))
DOC <- convert(chemdata, "DOC.units", "DOC.value") ## convert dissolved organic carbon

## merge all the dataframes together
waterchem <- Reduce(function(...) merge(..., all=TRUE), list(phosphorus, nitrogen, phosphates, nitrates, DOC, doxy, ammonia))

write.csv(waterchem, "database//waterchem.csv", row.names=FALSE)