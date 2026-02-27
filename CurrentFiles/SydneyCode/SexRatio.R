#Nice and quick program, This doesn't work as intended

raw <- read.csv("SydneyCode/Merged_RFID_Data.csv")[, c("RFID","Feeder","ColorCombo","Sex")]
unique <- raw[!duplicated(raw$ColorCombo),]
info <- table(unique$Feeder)