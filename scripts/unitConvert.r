## Unit conversion

convert <- function(data, unitCol, values){
  
## convert units to character vector from factor
data[,unitCol] <- as.character(data[,unitCol])
  
data <- data[!is.na(data[,values]),] ## omit missing values
  
## Switch text of mg/m2 to mg/m3 
data[data[,unitCol]=="mg/m2",unitCol] <- "mg/m3"

## Switch text ppm to mg/L because equivalent
data[data[,unitCol]=="ppm",unitCol] <- "mg/L"

## Switch text mg/m3 to mg/L
data[data[,unitCol]=="mg/m3",unitCol] <- "ug/L"

## Convert  ug/L to mg/L
data[data[,unitCol]=="ug/L",values] <-  data[data[,unitCol]=="ug/L",values]/1000
data[data[,unitCol]=="ug/L",unitCol] <- "mg/L" ## switch text

## Round ug/L values to the 10,000th because accuracy below a microgram unlikely
data[,values] <- round(data[,values],5)


## Omit other units
data <- data[data[,unitCol] == "mg/L",]


data <- data[,c("uniqueID",unitCol,values)]

return(data)
}
