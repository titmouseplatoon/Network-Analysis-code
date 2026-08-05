#THIS SHOULD NOT CRASH - Cole Robinson (crobins4@trinity.edu)

# Congrats! You found the starting place! 
#This code combines the data you just collected (good job! you survived!) with your current Master Banding Sheet
#This code should only have to be run ONCE whenever you collect RFID data 
  # eg, this script dumps all the ALL the files from ALL the RFID setups into ONE. BIG. CSV File. 
      # THEN It attaches Master Banding Sheet info about the "Bird"(RFID tag number) to EVERY entry

#It is a Fantastic and MASSIVE file (in terms of data. it is still way smaller than a medium sized excel file b/c it is a csv)

#So, what Do you need to prep?
  #You need to take all the files you pulled of the RFID SD cards and put them in a folder
  #The files need to be named like this: 000DATA.TXT
    #NOTE!!! VERY IMPORTANT!!!!
      # the letter in the file name needs to match the PHYSICAL feeder/location  it was at. (ex: 00ADATA.TXT for Feeder A) 
        #eg. If you had RFID setup B at feeder B, but then a raccoon ate it and therefore you had to trade in RFID setup 07,
            # then you need to modify the file Named 007DATA.TXT to-> 00ADATA.TXT 
            # because this code pulls the information about location from the file name.

#So, Your Prep-Checklist is this:
  #[]confirm the file names match their physical location
  #[]dump all the files into one folder you can find easy-DO NOT make sub-folders (you may need to combine files if you have multiple files form one location, and your computer keeps trying to add a "2" or something to the file name)
  #[]Put the 000LOG.TXT files in a separate folder- you do not need them
  #[]download the Master Banding Sheet as a csv file

#A final note about tolerance- 
  #when it asks you for tolerance, this is the interval that, within which, repeated reads of the SAME bird are discarded. 
  #Suggestion: leave as 1, this excludes the least data while still standardizing between RFID systems 
    #the old RFID Systems already clean out repeated reads within one second, the new systems let you adjust this- so this input is mostly just a backup to maintain consistency.
    # why is it needed? The plot code determines how closely associated birds are based on the NUMBER of interactions they have, if one system is keeping more reads than the other, it's interactions will inherently be stronger

# Ok, This is lots of info and this is only the tip of the iceberg 
# If you have questions-> EMAIL! 
#Seriously, I read my emails aloud to my fish- and they crave biology! Please Send Inquiries!
# hlbaetge@gmail.com (Hannah Baetge)


library(data.table)

# Read master banding data
#MBDFile <- readline(prompt = "enter the file path to the master banding file, ex: C/user/datafile/master banding.csv, be sure to include the .csv")
# This will open a standard file selection window
MBDFile <- file.choose()

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
  fill = TRUE,
  select = 1:37        # Strictly reads only columns 1 through 37
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
#feederDirectory <- readline(prompt = "type the file path to the folder where all data.txt files are from rfid reads")
# Use a visual pop-up to pick your folder safely (Mac compatible)
cat("Please select the folder containing your 000DATA.TXT files...\n")
feederDirectory <- rstudioapi::selectDirectory()


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
#tolerance <- as.numeric(
  #readline(prompt = "enter time as digits no units; ex 15 is 15 seconds")
#)
# This forces RStudio to pop open a small text box on your screen
tolerance_input <- rstudioapi::showPrompt(
  title = "Enter Tolerance", 
  message = "Enter time as digits (ex: 15 for 15 seconds)", 
  default = "1"
)

# Convert the pop-up text into a usable number
tolerance <- as.numeric(tolerance_input)

prunedMergedData <- mergedData[
  !(
    GapToPrevious < tolerance &
      PreviousBird == RFID
  )
]


setDF(prunedMergedData)
rownames(prunedMergedData) <- NULL


# Output
#outputName <- readline(
  #prompt = "make a file path for where the merged data will go and name file, ex c/datafolder/mergeddata.csv"
#)
# Opens a window. Type your desired file name (e.g., merged.csv) and hit Save
outputName <- file.choose(new = TRUE)


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

# Congrats! Time to take this csv file to the plot code!
