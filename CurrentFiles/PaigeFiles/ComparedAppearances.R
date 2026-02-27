#This file is to answer Dr. Murphy's question of:
#Compare where birds have been caught to where seen on RFID.
#Written by Paige Grantz
#PS this also really doesn't work

#Load Rfid and capture info.
rfidData <- read.csv("CurrentFiles/PaigeFiles/MergedRfidData.csv")
masterData <- read.csv("CurrentFiles/PaigeFiles/MasterBandingData23Jan2026.csv")

#Get rid of all capture locations that aren't at feeders
prunedMasterData <- subset(masterData, startsWith(Location, "F"))
prunedRfidData <- rfidData[!duplicated(rfidData$RFID, fromLast = TRUE), ]
#prunedMasterData <- masterData[!is.na(masterData$Location) & startsWith(masterData$Location, "F"), ]
#prunedMasterData <- masterData[!is.na(masterData$Location) & masterData$Location != "" & startsWith(masterData$Location, "F"), ]

#Get the list of lists that has rfid and their feeders
infol <- lapply(split(rfidData$Feeder, rfidData$RFID),unique)

#Make a table of info, concatenating all the feeders into a string.
locations <- data.frame(id = names(infol),
                     RfidFeeders = sapply(infol, function(x) paste(x, collapse = ", ")),
                     row.names=NULL)

locations <- merge(locations, prunedRfidData[, c("RFID", "ColorCombo")], 
                by.x = "id", by.y = "RFID", 
                all.x = TRUE)

#Get the list of lists that has rfid and their capture locations
infoc <- lapply(split(prunedMasterData$Location, masterData$RFID),unique)

#Make a table of infoc, concatenating all the feeders into a string.
captured <- data.frame(id = names(infoc),
                        capturedLocations = sapply(infoc, function(x) paste(x, collapse = ", ")),
                        row.names=NULL)

captured_filtered <- captured[captured$capturedLocations != "" & !is.na(captured$capturedLocations), ]

combined <- merge(locations, captured_filtered[, c("id","capturedLocations")], by.x = "id", by.y = "id", all.x=TRUE)

