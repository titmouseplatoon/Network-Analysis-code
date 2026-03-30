#This script is mean to show if any of the rfid files keeps pings within one second, 
#measured both between individuals and per individual. 
#Author: Paige Grantz

feeders = LETTERS[1:19]
for(letter in feeders){
  feederOccurances <- read.csv(paste0("FeederFiles","/000",letter,"DATA.TXT"),
                               col.names = c("RFID","N","DateTime"), header = FALSE)
  personalDupes <- any(duplicated(feederOccurances))
  timeDupes <-any(duplicated(feederOccurances$DateTime))
  cat(letter, ":",
        "\nHas duplicate times for one individual: ",personalDupes,
        "\nHas duplicate times between individuals: ",timeDupes)
}
#Feeder RFID info as of 3/30/2026 says it keeps pings within a second only if they're different individuals