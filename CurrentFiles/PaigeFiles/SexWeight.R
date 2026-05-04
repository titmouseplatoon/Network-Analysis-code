#This is for the Caroline Anthony's Natural History Question of:
#Are females bigger/smaller than males on average?

#Import banding data
bandingData <- read.csv("CurrentFiles/PaigeFiles/MasterBandingData27Apr2026.csv")

#Separate out male
maleIndividuals <- bandingData[(bandingData$Sex..M.F.=="M"), ]
#Remove odd characters
cleanedMale <- maleIndividuals[maleIndividuals$Wing..10th.mm...xx.x. != "." & 
                               maleIndividuals$Wing..10th.mm...xx.x. != "*" & 
                               maleIndividuals$Wing..10th.mm...xx.x. != "-", ]
#Make Wing..10th.mm...xx.x. Numeric
cleanedMale$Wing..10th.mm...xx.x. <- as.numeric(cleanedMale$Wing..10th.mm...xx.x.)
#Clear out empty Wing..10th.mm...xx.x. entries
cleanedMale <- cleanedMale[!is.na(cleanedMale$Wing..10th.mm...xx.x.), ]
cleanedMale <- cleanedMale[cleanedMale$Wing..10th.mm...xx.x. > 50, ]
#Clear out babies so there's only adults
adultMale <- cleanedMale[!(cleanedMale$Age=="Nestling") &
                         !(cleanedMale$Age=="HY"),]


#Separate out female
femaleIndividuals <- bandingData[(bandingData$Sex..M.F.=="F"), ]
#Remove odd characters
cleanedFemale <- femaleIndividuals[femaleIndividuals$Wing..10th.mm...xx.x. != "." & 
                                   femaleIndividuals$Wing..10th.mm...xx.x. != "*" & 
                                   femaleIndividuals$Wing..10th.mm...xx.x. != "-", ]
#Make Wing..10th.mm...xx.x. Numeric
cleanedFemale$Wing..10th.mm...xx.x. <- as.numeric(cleanedFemale$Wing..10th.mm...xx.x.)
#Clear out empty Wing..10th.mm...xx.x. entries
cleanedFemale <- cleanedFemale[!is.na(cleanedFemale$Wing..10th.mm...xx.x.), ]
#Clear out babies so there's only adults
adultFemale <- cleanedFemale[!(cleanedFemale$Age=="Nestling") &
                             !(cleanedFemale$Age=="HY"),]


#Take average Wing..10th.mm...xx.x. of each
cat("Male mean adult weight:", mean(adultMale$Wing..10th.mm...xx.x., na.rm = TRUE), "\n")
cat("Female mean adult weight:", mean(adultFemale$Wing..10th.mm...xx.x., na.rm = TRUE), "\n")

# Histogram to visually check normality
hist(adultMale$Wing..10th.mm...xx.x., breaks=10)
hist(adultFemale$Wing..10th.mm...xx.x., breaks=10)
# Boxplot to visualize the data
boxplot(Wing..10th.mm...xx.x. ~ Sex..M.F., data = rbind(adultMale, adultFemale),
        xlab = "Sex", ylab = "Wing",
        main = "Male vs Female Wing")

# Shapiro-Wilk normality test to numerically check normality
print(shapiro.test(adultMale$Wing..10th.mm...xx.x.))
print(shapiro.test(adultFemale$Wing..10th.mm...xx.x.))

#Doesn't look normal, so Wilcoxon test
print(wilcox.test(adultMale$Wing..10th.mm...xx.x., adultFemale$Wing..10th.mm...xx.x.))

#Test for checking equal variance
print(leveneTest(Wing..10th.mm...xx.x. ~ Sex..M.F., data = rbind(adultMale, adultFemale)))
#Also, independent t-test assuming equal variance
print(t.test(adultMale$Wing..10th.mm...xx.x., adultFemale$Wing..10th.mm...xx.x., var.equal = TRUE))