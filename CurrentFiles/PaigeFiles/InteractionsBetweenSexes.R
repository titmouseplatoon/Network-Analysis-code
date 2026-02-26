#This file is to answer the natural history question of:
#How often are birds of a given sex seen with their own sex as opposed to the other?
#Author: Paige Grantz

#Reads in the Pruned RFID data sheet.
rfidData <- read.csv("CurrentFiles/PrunedRFID_Data_With_MBS.csv")

#Reads in the master banding data sheet.
masterBandingData <- read.csv("CurrentFiles/MasterBandingDataBracken.csv")

#Prune the master banding data to get the most recent info.
prunedMBD <- masterBandingData[!duplicated(masterBandingData$RFID, fromLast = TRUE),]

#Merge the prunedMBD with the rfidData to have bird color Combo and sexes.
combinedInfo <- merge(rfidData, prunedMBD, by.x="RFID",by.y="RFID..")[, c("RFID","DateTime","Feeder","GapToPrevious","Color.Combo","Sex")]

#From the list of RFID data, gets the vector of unique individuals that visit the feeder.
vectorOfIndividuals <- unique(rfidData$RFID)

#Convert the vector of individuals so data can be added in the next step.
tableofIndividuals <- data.frame(RFID=vectorOfIndividuals)

#Now to add the individual's sex to the list of individuals.
IndividualsAndSex <- merge(tableofIndividuals, prunedMBD, by.x="RFID",by.y="RFID..")[, c("RFID","Sex")]

#Separate out the sex of individuals, using filter() from the dplyr library (Make sure you have that)
#Mid fall 2025 info for the comments
maleIndividuals <- filter(IndividualsAndSex, Sex=="M") # As of writing, this has 42 observations of 70, 60%
femaleIndividuals <- filter(IndividualsAndSex, Sex=="F") # 10 observations, 14.29% of population
unknownIndividuals <- filter(IndividualsAndSex, Sex=="") # 18 observations, 25.71%

#The tolerance level for what counts as an interaction. Adjustable.
interval <- 3




#This was just for personal use, getting a table of all the unsexed RFID and color combos for the genetics team.
unknownsAndName <- merge(unknownIndividuals, masterBandingData, by.x="RFID",by.y="RFID..")[, c("RFID","Color.Combo")]
