#This shows what feeders, each bird has been to based on the "Merged_RFID_Data"
masterFeederList <- read.csv("SydneyCode/Merged_RFID_data.csv")
info <- lapply(split(masterFeederList$Feeder, masterFeederList$RFID),unique)