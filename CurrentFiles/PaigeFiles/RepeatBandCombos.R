#This file has the goal of locating repeat band combos in the master banding data.
#Written by Paige Grantz

#import master banding data
masterData <- read.csv("CurrentFiles/PaigeFiles/MasterBandingData23Jan2026.csv")

#Removes duplicated rfid codes so This is a list of all the rfid codes in use. The extra info isn't relevent.
uniqueRfid <- masterData[!duplicated(masterData$RFID,fromLast=TRUE),]

##Get the list of lists that has rfid and their metal combos
splitMetal <- lapply(split(masterData$Metal, masterData$RFID),unique)

#Makes a table of the lists of metal combos
tableMetal <- data.frame(id=names(splitMetal),
                         MetalCombos = sapply(splitMetal, function(x) paste(x, collapse = ", ")),
                         row.names=NULL)
#Remove null entries from the metal combo table
filteredTableMetal <- tableMetal[tableMetal$MetalCombos != "" &
                                   !is.na(tableMetal$MetalCombos) &
                                   tableMetal$id != "-" &
                                   tableMetal$id != "" &
                                   tableMetal$id != "*" &
                                   tableMetal$id != "- " &
                                   tableMetal$id != ".", ]

#Get the lists of lists that has rifd and their color combos
splitColor <- lapply(split(masterData$ColorCombo, masterData$RFID),unique)

#Makes a table of the lists of color combos
tableColor <- data.frame(id=names(splitColor),
                         ColorCombos = sapply(splitColor, function(x) paste(x, collapse = ", ")),
                         row.names=NULL)

#Remove null entries from the color combo table
filteredTableColor <- tableColor[tableColor$ColorCombos != "" &
                                   !is.na(tableColor$ColorCombos) &
                                   tableColor$id != "-" &
                                   tableColor$id != "" &
                                   tableColor$id != "*" &
                                   tableColor$id != "- " &
                                   tableColor$id != ".", ]

combined <- merge(filteredTableColor,filteredTableMetal, by="id",all.x=TRUE,all.y=TRUE)

#Removes any entries that are fine, that only have one metal combo and color combo
issues <- subset(combined, grepl(",", MetalCombos) | grepl(",",ColorCombos))
