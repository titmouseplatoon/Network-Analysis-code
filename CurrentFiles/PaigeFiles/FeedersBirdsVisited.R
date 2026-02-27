#This shows what feeders, each bird has been to based on the "Merged_RFID_Data"
masterFeederList <- read.csv("CurrentFiles/PaigeFiles/MergedRfidData.csv")
info <- lapply(split(masterFeederList$Feeder, masterFeederList$RFID),unique)
frame <- table(info)