#THIS SHOULD NOT CRASH

library(data.table)

# Read master banding data
MBDFile <- readline(prompt = "enter the file path to the master banding file, ex: C/user/datafile/master banding.csv, be sure to include the .csv")

# Current master banding column names. columns with nothing are NA or blank
MBD_columns <- c(
  "Year",
  "MissingData",
  "Metal",
  "ColorCombo",
  "Sex",
  "DateCaptured",
  "TimeCaptured",
  "YearRecapture",
  "LocationCaptured",
  "Status",
  "IA_Status",
  "AgeFirstCaught",
  "YearFirstCaught",
  "LocationFirstCaught",
  "ExactAge",
  "Age",
  "CrestColor",
  "LowerMandible",
  "Tarsus",
  "Wing",
  "MeasuredBy",
  "BloodTaken",
  "FeathersTaken",
  "Spec",
  "CrestPhoto",
  "Mass",
  "RFID",
  "CurrentBreedingLocation",
  "CurrentBreedingPartner",
  "PastBreedingYear",
  "PastBreedingLocation",
  "PastBreedingPartner",
  "Notes",
  "OtherNotes",
  "RecapDate",
  "RecapLocation",
  "Blank"
)

rawMasterBandingData <- fread(
  MBDFile,
  header = FALSE,
  col.names = MBD_columns,
  fill = TRUE
)

# Select relevant columns
masterBandingData <- rawMasterBandingData[, .(
  Metal,
  ColorCombo,
  Sex,
  DateCaptured,
  Status,
  LocationCaptured,
  Age,
  CrestColor,
  LowerMandible,
  BloodTaken,
  FeathersTaken,
  Mass,
  Tarsus,
  Wing,
  RFID,
  CurrentBreedingLocation
)]


#Clear out all duplicates, keeping the most recent data
prunedMBD <- masterBandingData[
  !duplicated(RFID, fromLast = TRUE)
]


# Read RFID feeder files
feederDirectory <- readline(prompt = "type the file path to the folder where all data.txt files are from rfid reads")

feeders <- LETTERS[1:19]

feederList <- lapply(feeders, function(letter){
  
  feederFile <- paste0(
    feederDirectory,
    "/000",
    letter,
    "DATA.TXT"
  )
  
  feederOccurrences <- fread(
    feederFile,
    header = FALSE,
    col.names = c("RFID","N","DateTime")
  )
  
  
  # Add feeder ID
  feederOccurrences[, Feeder := letter]
  
  
  # Convert datetime
  feederOccurrences[, DateTime :=
                      as.POSIXct(
                        DateTime,
                        format = "%m/%d/%Y %H:%M:%S"
                      )
  ]
  
  
  # Sort by time
  setorder(
    feederOccurrences,
    Feeder,
    DateTime
  )
  
  
  # Calculate previous detection gap
  feederOccurrences[, GapToPrevious :=
                      as.numeric(
                        DateTime - shift(DateTime)
                      )
  ]
  
  
  # Record previous bird
  feederOccurrences[, PreviousBird :=
                      shift(RFID)
  ]
  
  
  return(feederOccurrences)
  
})


# Combine feeder files
masterFeederList <- rbindlist(feederList)


# Merge feeder reads with banding data
mergedData <- merge(
  masterFeederList,
  prunedMBD,
  by = "RFID",
  all.x = TRUE
)


setorder(
  mergedData,
  Feeder,
  DateTime
)


# Remove repeated RFID reads within tolerance
tolerance <- as.numeric(
  readline(prompt = "enter time as digits no units; ex 15 is 15 seconds")
)


prunedMergedData <- mergedData[
  !(
    GapToPrevious < tolerance &
      PreviousBird == RFID
  )
]


setDF(prunedMergedData)
rownames(prunedMergedData) <- NULL


# Output
outputName <- readline(
  prompt = "make a file path for where the merged data will go and name file, ex c/datafolder/mergeddata.csv"
)


if(tolerance == 0){
  
  write.csv(
    mergedData,
    outputName,
    row.names = FALSE
  )
  
} else {
  
  write.csv(
    prunedMergedData,
    outputName,
    row.names = FALSE
  )
}