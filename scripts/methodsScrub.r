## Clean up methods spreadsheet
method <- read.csv("data//methodsDataV2.csv", stringsAsFactors = F)

## drop PI column
method <- subset(method, select=-c(PI))

## Combine methods for survey type
method[method$Survey.Type %in% c("not described", ""),"Survey.Type"] <- "undescribed"
method[method$Survey.Type %in% c("satellite","modelled"),"Survey.Type"] <- "satellite/modelled"
unique(method$Survey.Type)

## Clean up methods
method[method$Chl.Method %in% c("not described", ""),"Chl.Method"] <- "undescribed"
unique(method$Chl.Method)

##  Measurement Type
method[method$MeasurementType %in% c("mean ", ""),"MeasurementType"] <- "mean"

## Depth qual
method[method$Depth.qual %in% c("not described", ""),"Depth.qual"] <- "undescribed"

## Depth Quant
method$Depth.quant <- gsub("-", " to ",  method$Depth.quant)
splitDepth <- str_split_fixed( method$Depth.quant, pattern= " to ", n=2)
method[,"ShallowDepth"] <- splitDepth[,1]
method[,"DeepDepth"] <- splitDepth[,2]

str(method)


## Temporal qual
method[method$Temporal.qual %in% c("monthly ", "monthy"," monthly"),"Temporal.qual"] <- "monthly"
method[method$Temporal.qual %in% c("biweekly ", "bi-weekly","bimonthly","bi-monthly","semimonthly","biweekly"),"Temporal.qual"] <- "twice-monthly"
method[method$Temporal.qual %in% c("4"),"Temporal.qual"] <- "seasonally"
method[method$Temporal.qual %in% c("hourly "),"Temporal.qual"] <- "hourly"
method[method$Temporal.qual %in% c("summer"),"Temporal.qual"] <- "once"
method[method$Temporal.qual %in% c("biyearly"),"Temporal.qual"] <- "once"
method[method$Temporal.qual %in% c("yearly"),"Temporal.qual"] <- "annually"
unique(method$Temporal.qual)


## generate number of observations
data <- read.csv("data//ChlaDataV2.csv")
obs <- data %>% group_by(StudyID) %>% summarize(NumObs=length(Year))
method2 <- merge(method, obs, by="StudyID")

write.csv(method2, "data//methodsDataV2.csv", row.names=F)


