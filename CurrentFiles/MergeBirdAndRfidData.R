#I should probably make this so it doesn't crash if they input something wrong.

library(data.table)

#Read in the data and select the columns that I'm interested in
MBDFile <- readline(prompt="Enter Master Banding Data File Name (in csv format): ")
rawMasterBandingData <- read.csv(MBDFile,col.names=c("NA","Year","DN","BIH","E",
  "Metal","ColorCombo","Sex","DateLastCaptured","StatusLastCaptured",
  "LocationLastCaptured","AgeAtLastCapture","CrestColor","LowerMandible",
  "BloodTaken","FeathersTaken","Mass","Tarsus","Wing","RFID","TarsusCollectedBy",
  "IANestBoxOfOrigin","Notes","OtherNotes","RecapDate","RecapLocation","NA"))
masterBandingData <- rawMasterBandingData[, c("Metal","ColorCombo","Sex",
  "DateLastCaptured","StatusLastCaptured",
  "LocationLastCaptured","AgeAtLastCapture","CrestColor",
  "LowerMandible","BloodTaken","FeathersTaken","Mass","Tarsus",
  "Wing","RFID","IANestBoxOfOrigin")]

#Clear out all duplicates, keeping the most recent data
prunedMBD <- masterBandingData[!duplicated(masterBandingData$RFID, fromLast = TRUE),]

#For each feeder, read in the file and name the columns
feederDirectory <- readline(prompt="Enter Folder Containing RFID Feeder Data: ")
feeders = LETTERS[1:19]
feederList <- lapply(feeders, function(letter){
  feederOccurances <- read.csv(paste0(feederDirectory,"/000",letter,"DATA.TXT"),
                               col.names = c("RFID","N","DateTime"), header = FALSE)
  #Log the feeder each bird was seen at
  feederOccurances$Feeder <- letter
  #Convert DateTime to POSIX
  feederOccurances$DateTime <- as.POSIXct(feederOccurances$DateTime, format = "%m/%d/%Y %H:%M:%S")
  #Reorder all of the information to be concerned with location and time
  feederOccurances <- feederOccurances[order(feederOccurances$Feeder,feederOccurances$DateTime),]
  #Calculate the gaps between each occurence
  feederOccurances$GapToPrevious <- as.numeric(feederOccurances$DateTime - shift(feederOccurances$DateTime))
  #Track what the previous bird was and log it
  feederOccurances$PreviousBird <- shift(feederOccurances$RFID)
  feederOccurances
})

#Merge all the individual feeder files into a master file
unprunedMasterFeederList <- do.call(rbind, feederList)

#Merge the master banding data with the feeder info
mergedData <- merge(unprunedMasterFeederList, prunedMBD, by = "RFID", all.x = TRUE)
mergedData <- mergedData <- mergedData[order(mergedData$Feeder, mergedData$DateTime), ]

#Remove all examples where the gap between two entries is less than the tolerance 
#and the bird has not changed
tolerance <- as.numeric(readline(prompt="What is the tolerance for time at a feeder?:"))
prunedMergedData <- mergedData[!((mergedData$GapToPrevious < tolerance) &
                              (mergedData$PreviousBird==mergedData$RFID)),]

#prunedMergedData$N <- NULL
#prunedMergedData$PreviousBird <- NULL
rownames(prunedMergedData) <- NULL

#Write data out to CSVs
outputName <- readline(prompt="Enter the name/destination of the output: ")
if(tolerance==0){
  write.csv(mergedData, outputName, row.names = FALSE)
}else{
  write.csv(prunedMergedData, outputName, row.names = FALSE)
}
