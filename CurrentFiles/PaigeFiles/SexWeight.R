#This is for the Caroline Anthony's Natural History Question of:
#Are females bigger/smaller than males on average?

#Import banding data
bandingData <- read.csv("CurrentFiles/PaigeFiles/MasterBandingData27Apr2026.csv")
library(effsize)
library(car)

#Separate out male
maleIndividuals <- bandingData[(bandingData$Sex..M.F.=="M"), ]
#Remove odd characters
cleanedMale <- maleIndividuals[maleIndividuals$Tarsus..10th.mm...xx.x. != "." & 
                               maleIndividuals$Tarsus..10th.mm...xx.x. != "*" & 
                               maleIndividuals$Tarsus..10th.mm...xx.x. != "-", ]
#Make Tarsus..10th.mm...xx.x. Numeric
cleanedMale$Tarsus..10th.mm...xx.x. <- as.numeric(cleanedMale$Tarsus..10th.mm...xx.x.)
#Clear out empty Tarsus..10th.mm...xx.x. entries
cleanedMale <- cleanedMale[!is.na(cleanedMale$Tarsus..10th.mm...xx.x.), ]
#Just used on wing since there’s an outlier at about 20
#cleanedMale <- cleanedMale[cleanedMale$Tarsus..10th.mm...xx.x. > 50, ]
#Clear out babies so there's only adults
adultMale <- cleanedMale[!(cleanedMale$Age=="Nestling") &
                         !(cleanedMale$Age=="HY"),]


#Separate out female
femaleIndividuals <- bandingData[(bandingData$Sex..M.F.=="F"), ]
#Remove odd characters
cleanedFemale <- femaleIndividuals[femaleIndividuals$Tarsus..10th.mm...xx.x. != "." & 
                                   femaleIndividuals$Tarsus..10th.mm...xx.x. != "*" & 
                                   femaleIndividuals$Tarsus..10th.mm...xx.x. != "-", ]
#Make Tarsus..10th.mm...xx.x. Numeric
cleanedFemale$Tarsus..10th.mm...xx.x. <- as.numeric(cleanedFemale$Tarsus..10th.mm...xx.x.)
#Clear out empty Tarsus..10th.mm...xx.x. entries
cleanedFemale <- cleanedFemale[!is.na(cleanedFemale$Tarsus..10th.mm...xx.x.), ]
#Clear out babies so there's only adults
adultFemale <- cleanedFemale[!(cleanedFemale$Age=="Nestling") &
                             !(cleanedFemale$Age=="HY"),]


#Take average Tarsus..10th.mm...xx.x. of each
cat("Adult male mean:", mean(adultMale$Tarsus..10th.mm...xx.x., na.rm = TRUE), "\n")
cat("Adult male sd:", sd(adultMale$Tarsus..10th.mm...xx.x., na.rm = TRUE), "\n")
cat("Adult female mean:", mean(adultFemale$Tarsus..10th.mm...xx.x., na.rm = TRUE), "\n")
cat("Adult female sd:", sd(adultFemale$Tarsus..10th.mm...xx.x., na.rm = TRUE), "\n")
cat("Difference: ",trunc(mean(adultMale$Tarsus..10th.mm...xx.x.) * 10) / 10 -
                  trunc(mean(adultFemale$Tarsus..10th.mm...xx.x.) * 10) / 10)
print(cohen.d(adultMale$Tarsus..10th.mm...xx.x., adultFemale$Tarsus..10th.mm...xx.x.))

# Histogram to visually check normality
hist(adultMale$Tarsus..10th.mm...xx.x., breaks=10)
hist(adultFemale$Tarsus..10th.mm...xx.x., breaks=10)
# Boxplot to visualize the data
boxplot(Tarsus..10th.mm...xx.x. ~ Sex..M.F., data = rbind(adultMale, adultFemale),
        xlab = "Sex", ylab = "Wing",
        main = "Male vs Female Wing")

# Shapiro-Wilk normality test to numerically check normality
print(shapiro.test(adultMale$Tarsus..10th.mm...xx.x.))
print(shapiro.test(adultFemale$Tarsus..10th.mm...xx.x.))

#Doesn't look normal, so Wilcoxon test
print(wilcox.test(adultMale$Tarsus..10th.mm...xx.x., adultFemale$Tarsus..10th.mm...xx.x.))

#Test for checking equal variance
print(leveneTest(Tarsus..10th.mm...xx.x. ~ Sex..M.F., data = rbind(adultMale, adultFemale)))
#Also, independent t-test assuming equal variance
print(t.test(adultMale$Tarsus..10th.mm...xx.x., adultFemale$Tarsus..10th.mm...xx.x., var.equal = FALSE))