## Join files by UniqueID

## read in files
datafiles <- list.files("data//", pattern=".csv", full.names = T)

## main identifier
uniqueID <- read.csv(datafiles[6])

## files to combine
FilesForMerge<- c("data//chl.csv", "data//LakeChar.csv","data//waterchem.csv")

## loop the merge
for(i in FilesForMerge){
  uniqueID <- merge(uniqueID, read.csv(i), by="uniqueID", all.x=T)
}

