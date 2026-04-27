#This is for the Caroline Anthony's Natural History Question of:
#Are females bigger/smaller than males on average?

#Import banding data
bandingData <- read.csv("CurrentFiles/PaigeFiles/MasterBandingData23Jan2026.csv")

#Separate out male
maleIndividuals <- bandingData[(bandingData$Sex=="M"), ]
#Remove odd characters
cleanedMale <- maleIndividuals[maleIndividuals$Mass != "." & 
                               maleIndividuals$Mass != "*" & 
                               maleIndividuals$Mass != "-", ]
#Make Mass Numeric
cleanedMale$Mass <- as.numeric(cleanedMale$Mass)
#Clear out empty mass entries
cleanedMale <- cleanedMale[!is.na(cleanedMale$Mass), ]
#Clear out babies so there's only adults
adultMale <- cleanedMale[!(cleanedMale$Age=="Nestling") &
                         !(cleanedMale$Age=="HY"),]


#Separate out female
femaleIndividuals <- bandingData[(bandingData$Sex=="F"), ]
#Remove odd characters
cleanedFemale <- femaleIndividuals[femaleIndividuals$Mass != "." & 
                                   femaleIndividuals$Mass != "-", ]
#Make Mass Numeric
cleanedFemale$Mass <- as.numeric(cleanedFemale$Mass)
#Clear out empty mass entries
cleanedFemale <- cleanedFemale[!is.na(cleanedFemale$Mass), ]
#Clear out babies so there's only adults
adultFemale <- cleanedFemale[!(cleanedFemale$Age=="Nestling") &
                             !(cleanedFemale$Age=="HY"),]


#Take average mass of each
cat("Male mean adult weight:", mean(adultMale$Mass, na.rm = TRUE), "\n")
cat("Female mean adult weight:", mean(adultFemale$Mass, na.rm = TRUE), "\n")

# Histogram to visually check normality
hist(adultMale$Mass)
hist(adultFemale$Mass)

# Shapiro-Wilk normality test to numerically check normality
print(shapiro.test(adultMale$Mass))
print(shapiro.test(adultFemale$Mass))

#Doesn't look normal, so wilcox test
print(wilcox.test(adultMale$Mass, adultFemale$Mass))

#If it had looked normal, t.test
#t.test(adultMale$Mass, adultFemale$Mass)