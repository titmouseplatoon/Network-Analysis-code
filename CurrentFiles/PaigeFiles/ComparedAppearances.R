#This file is to answer Dr. Murphy's question of:
#Compare where birds have been caught to where seen on RFID.
#Written by Paige Grantz

#Load Rfid and capture info.
rfidData <- read.csv("CurrentFiles/PaigeFiles/MergedRfidData.csv")
masterData <- read.csv("CurrentFiles/PaigeFiles/MasterBandingData23Jan2026.csv")

#Get rid of all capture locations that aren't at feeders, and get rid of all entries outside 2024-2026
prunedMasterData <- subset(masterData, startsWith(Location, "F"))
prunedMasterData <- subset(prunedMasterData, Year==2024 | Year==2025 | Year==2026)

#Grabs the list of unique RFID tags from the rfidData
prunedRfidData <- rfidData[!duplicated(rfidData$RFID, fromLast = TRUE), ]
#prunedMasterData <- masterData[!is.na(masterData$Location) & startsWith(masterData$Location, "F"), ]
#prunedMasterData <- masterData[!is.na(masterData$Location) & masterData$Location != "" & startsWith(masterData$Location, "F"), ]

#Get the list of lists that has rfid and their feeders
infol <- lapply(split(rfidData$Feeder, rfidData$RFID),unique)

#Make a table of info, concatenating all the feeders into a string.
locations <- data.frame(id = names(infol),
                     RfidFeeders = sapply(infol, function(x) paste(x, collapse = ", ")),
                     row.names=NULL)

#Add the color combo to this list of locations
locations <- merge(locations, prunedRfidData[, c("RFID", "ColorCombo")], 
                by.x = "id", by.y = "RFID", 
                all.x = TRUE)

#This just gets rid of the empty entries.
presentation <- na.omit(locations)

#Get the list of lists that has rfid and their capture locations
infoc <- lapply(split(prunedMasterData$Location, prunedMasterData$RFID),unique)

#Make a table of infoc, concatenating all the feeders into a string.
captured <- data.frame(id = names(infoc),
                        capturedLocations = sapply(infoc, function(x) paste(x, collapse = ", ")),
                        row.names=NULL)

#This gets rid of all the empty or null values fron the list of capture locations
captured_filtered <- captured[captured$capturedLocations != "" & !is.na(captured$capturedLocations), ]

#Combine the two tables to see where a bird is captured vs seen at rfid. There may be a lot more rfid data than capture data fyi
combined <- merge(locations, captured_filtered[, c("id","capturedLocations")], by.x = "id", by.y = "id", all.x=TRUE)

#Outputs the combined table to a csv for comparison
#write.csv(combined, "./CurrentFiles/PaigeFiles/CapturedVsRfid2024-6", row.names = FALSE)
