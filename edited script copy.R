#This is my best attempt at creating a social network from RFID Data.
#I am not a "coder," so there are likely many things I'm doing that
# could be done in a more efficient way. I based all the code off of
# Dr. Dai Shizuka's tutorial: https://dshizuka.github.io/networkanalysis/example_RFID_to_Networks.html
# If a Murphy lab student picks this up at some point and has questions, I 
# can be contacted at joslyn.c.boyer@gmail.com

#one issue that I was not yet able to resolve is that I can't use all the
#days in the dataset. I suspect it is because there's not enough entries
# on some days.

#We don't have sex for most of the birds yet, but this is a component we
# would like to add when we have that information. Also, genetic relatedness. 


# this code copy is being eddited by Hannah Baetge (hlbaetge@gmail.com)




#SETUP
rm(list = ls())
library(dplyr)
library(tidyr)
library(lubridate)
library(asnipe)
library(igraph)
setwd("/Users/hannahbaetge/Library/CloudStorage/OneDrive-TrinityUniversity/NETwork ANalYsis/RFID Data copy")
#I need to reformat the data to match the columns from the worked example
# Step 1: Read the CSV file into R
FeederA.1 <- read.csv("000ADATA.TXT", header = TRUE)
# Step 2: Check the column names
colnames(FeederA.1)
#############
# Step 3: If there's a conflict (like 'date' being interpreted as a function), rename the columns
# Rename 'date' and 'time' columns to avoid any conflicts with function names
    #colnames(FeederA.1)[colnames(FeederA.1) == "date"] <- "Date_column"
    #colnames(FeederA.1)[colnames(FeederA.1) == "time"] <- "Time_column"
# Step 4: Combine 'Date_column' and 'Time_column' into a new DateTime column
    #FeederA.1$DateTime <- paste(FeederA.1$Date_column, FeederA.1$Time_column)
# Step 5: Convert the combined DateTime column to a proper datetime format
FeederA.1$DateTime <- as.POSIXct(FeederA.1[[3]], format = "%m/%d/%Y %H:%M:%S")
    #FeederA.1$DateTime <- as.POSIXct(FeederA.1$DateTime, format = "%Y-%m-%d %H:%M:%S")
####### lots of cutting as SD card had already formatted mostly (M/D/Y HH:MM:SS -> M-D-Y HH:MM:SS)
# Step 6: Write the updated data to a new CSV file
write.csv(FeederA.1, "FeederA.1", row.names = FALSE)
# Check the first few rows of the data
head(FeederA.1)
# Check the column types to confirm they are in the expected format
str(FeederA.1)


#troubleshooting
# Step 1: Convert 'Date_column' to Date format
    #FeederA.1$Date_column <- as.Date(FeederA.1$Date_column, format = "%m/%d/%Y")
# Step 2: Combine 'Date_column' and 'Time_column' into a single DateTime string
    #FeederA.1$DateTime <- paste(FeederA.1$Date_column, FeederA.1$Time_column)
# Step 3: Convert the combined DateTime column to POSIXct format
    #FeederA.1$DateTime <- as.POSIXct(FeederA.1$DateTime, format = "%Y-%m-%d %H:%M:%S")
# Check the result
    #head(FeederA.1)
# Step 1: Remove the 'Date_column' and 'Time_column'
    FeederA.1 <- FeederA.1 %>% select(-Date_column, -Time_column)
# Step 2: Add the 'Feeder' column filled with "A"
    FeederA.1$Feeder <- "A"
    
# clean down to just the selected data 
    FeederA_clean <- select(FeederA.1, tag = X3B001903BE, feeder = Feeder, DateTime)
 # View the first few rows
    head(FeederA_clean)
# Check the result
    #head(FeederA.1)
    #FeederA.1

 ####### ok now  the data for feeder 1 is called "FeederA_clean"  
    
    
#Now I will do the same thing for FeederB
FeederB.1 <- read.csv("~/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/FeederB.csv")
colnames(FeederB.1)
colnames(FeederB.1)[colnames(FeederB.1) == "date"] <- "Date_column"
colnames(FeederB.1)[colnames(FeederB.1) == "time"] <- "Time_column"
FeederB.1$DateTime <- paste(FeederB.1$Date_column, FeederB.1$Time_column)
FeederB.1$DateTime <- as.POSIXct(FeederB.1$DateTime, format = "%Y-%m-%d %H:%M:%S")
#write.csv(FeederB.1, "FeederB.1", row.names = FALSE)
head(FeederB.1)
str(FeederB.1)
#FeederB.1$Date_column <- as.Date(FeederB.1$Date_column, format = "%m/%d/%Y")
FeederB.1$Date_column <- as.Date(FeederB.1$Date_column, format = "%m/%d/%y")
FeederB.1$DateTime <- paste(FeederB.1$Date_column, FeederB.1$Time_column)
FeederB.1$DateTime <- as.POSIXct(FeederB.1$DateTime, format = "%Y-%m-%d %H:%M:%S")
head(FeederB.1)
FeederB.1 <- FeederB.1 %>% select(-Date_column, -Time_column)
FeederB.1$Feeder <- "B"
FeederB.1 <- FeederB.1 %>%
  select(ID, Feeder, DateTime)
head(FeederB.1)

#Now I will do the same thing for FeederC
FeederC.1 <- read.csv("~/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/FeederC.csv")
colnames(FeederC.1)
colnames(FeederC.1)[colnames(FeederC.1) == "date"] <- "Date_column"
colnames(FeederC.1)[colnames(FeederC.1) == "time"] <- "Time_column"
FeederC.1$DateTime <- paste(FeederC.1$Date_column, FeederC.1$Time_column)
FeederC.1$DateTime <- as.POSIXct(FeederC.1$DateTime, format = "%Y-%m-%d %H:%M:%S")
#write.csv(FeederC.1, "FeederC.1", row.names = FALSE)
head(FeederC.1)
str(FeederC.1)
FeederC.1$Date_column <- as.Date(FeederC.1$Date_column, format = "%m/%d/%y")
FeederC.1$DateTime <- paste(FeederC.1$Date_column, FeederC.1$Time_column)
FeederC.1$DateTime <- as.POSIXct(FeederC.1$DateTime, format = "%Y-%m-%d %H:%M:%S")
head(FeederC.1)
FeederC.1 <- FeederC.1 %>% select(-Date_column, -Time_column)
FeederC.1$Feeder <- "C"
FeederC.1 <- FeederC.1 %>%
  select(ID, Feeder, DateTime)
head(FeederC.1)

#Now I will do this for FeederD
FeederD.1 <- read.csv("~/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/FeederD.csv")
colnames(FeederD.1)
colnames(FeederD.1)[colnames(FeederD.1) == "date"] <- "Date_column"
colnames(FeederD.1)[colnames(FeederD.1) == "time"] <- "Time_column"
FeederD.1$DateTime <- paste(FeederD.1$Date_column, FeederD.1$Time_column)
FeederD.1$DateTime <- as.POSIXct(FeederD.1$DateTime, format = "%Y-%m-%d %H:%M:%S")
#write.csv(FeederD.1, "FeederD.1", row.names = FALSE)
head(FeederD.1)
str(FeederD.1)
FeederD.1$Date_column <- as.Date(FeederD.1$Date_column, format = "%m/%d/%y")
FeederD.1$DateTime <- paste(FeederD.1$Date_column, FeederD.1$Time_column)
FeederD.1$DateTime <- as.POSIXct(FeederD.1$DateTime, format = "%Y-%m-%d %H:%M:%S")
head(FeederD.1)
FeederD.1 <- FeederD.1 %>% select(-Date_column, -Time_column)
FeederD.1$Feeder <- "D"
FeederD.1 <- FeederD.1 %>%
  select(ID, Feeder, DateTime)
head(FeederD.1)

#Now I will do this for FeederE
FeederE.1 <- read.csv("~/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/FeederE.csv")
colnames(FeederE.1)
colnames(FeederE.1)[colnames(FeederE.1) == "date"] <- "Date_column"
colnames(FeederE.1)[colnames(FeederE.1) == "time"] <- "Time_column"
FeederE.1$DateTime <- paste(FeederE.1$Date_column, FeederE.1$Time_column)
FeederE.1$DateTime <- as.POSIXct(FeederE.1$DateTime, format = "%Y-%m-%d %H:%M:%S")
#write.csv(FeederE.1, "FeederE.1", row.names = FALSE)
head(FeederE.1)
str(FeederE.1)
FeederE.1$Date_column <- as.Date(FeederE.1$Date_column, format = "%m/%d/%y")
FeederE.1$DateTime <- paste(FeederE.1$Date_column, FeederE.1$Time_column)
FeederE.1$DateTime <- as.POSIXct(FeederE.1$DateTime, format = "%Y-%m-%d %H:%M:%S")
head(FeederE.1)
FeederE.1 <- FeederE.1 %>% select(-Date_column, -Time_column)
FeederE.1$Feeder <- "E"
FeederE.1 <- FeederE.1 %>%
  select(ID, Feeder, DateTime)
head(FeederE.1)


# #now I'm saving these as files so I can compile them
file_path <- "~/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/trial/FeederA.1.csv"
write.csv(FeederA.1, file = file_path, row.names = FALSE)
file_path <- "~/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/trial/FeederB.1.csv"
write.csv(FeederB.1, file = file_path, row.names = FALSE)
file_path <- "~/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/trial/FeederC.1.csv"
write.csv(FeederC.1, file = file_path, row.names = FALSE)
file_path <- "~/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/trial/FeederD.1.csv"
write.csv(FeederD.1, file = file_path, row.names = FALSE)
file_path <- "~/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/trial/FeederE.1.csv"
write.csv(FeederE.1, file = file_path, row.names = FALSE)


# THIS IS STEP 2.2, BIND ROWS TO CREATE COMPILED DATA
file_locations=list.files("/Users/joslynboyer/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/trial",pattern=".csv", full.names=T)
files=lapply(file_locations, read.csv)
str(files)
files=lapply(files, function(x) x %>% select(ID, Feeder, DateTime))
compiled_data=bind_rows(files)
head(compiled_data) 
nrow(compiled_data)

#THIS IS STEP 3: CROSS-REFERENCE RFIDS WITH BANDING DATA
data_count=compiled_data %>% group_by(ID) %>% summarize(n=n())
band_dat=read.csv("~/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/_MASTER Banding data.xlsx - All Birds - Bracken.csv")
band_dat_trim=band_dat %>% select(RFID, Metal_ID, Color_Combo, Sex, Age)
band_dat_trim
id_confirm=left_join(data_count, band_dat_trim, by=join_by("ID"=="RFID"))
id_confirm                                  

#STEP 3.2: ADD THE TEST TAG TO THE DATA.
#I determined what was a test tag by checking a file in the 
#shared drive called test tags. Also, if an RFID was in this dataset
# but didn't appear anywhere in the masterbanding spreadsheet,
#I determined it was a possible text tag. 
test_tag="3B001903BE"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="01103FDA4B"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="3B0018D44C"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="3B00181BD6"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="3B00191853"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag= "3B00194A46"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="3B0018B8CD"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="3B001841B8"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="01103F7A01"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="3B0018A7AD"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="3B00182F57"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="3B00191A07"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="3B0018C29F"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="3B001929EB"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="3B0018598F"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="3B001879AF"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="3B00194230"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="3B00194284"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="380018B16B"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="3B001A3D85"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"
test_tag="3B001908FD"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="test tag"

#these are potential test tags and I'm going to record them as such
test_tag="3B00193E5A"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="possible test tag"
test_tag="3B004B0357"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="possible test tag"
test_tag="0110275FF0"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="possible test tag"
test_tag="3B0018D863"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="possible test tag"
test_tag="3B0018537E"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="possible test tag"
test_tag="01103FDE75"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="possible test tag"
test_tag="3B0018BBAA"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="possible test tag"
test_tag="01103FC725"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="possible test tag"
id_confirm

#I think these are test tags from my own observation
test_tag="3B00192274"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="possible test tag"
test_tag="3B004C9D50"
id_confirm$Metal_ID[which(id_confirm$ID==test_tag)]="possible test tag"

#NOW I NEED TO UPDATE THE AGES OF THE BIRDS BECAUSE MANY WERE CAUGHT
#BEFORE THE DATASET BEGINS, OR THEY HAVE BEEN CAUGHT MULTIPLE TIMES
#AND HAVE MULTIPLE AGES ON FILE

#THERE WERE A COUPLE ISSUES WITH THIS PROCESS...
updated_age="01103FD86E"
id_confirm$Age[is.na(id_confirm$Age)] <- ""
id_confirm <- id_confirm %>%
  mutate(Age = ifelse(ID == updated_age, "AHY", Age))
id_confirm %>% filter(ID == updated_age)

updated_age="01103FD948"
id_confirm <- id_confirm %>%
  mutate(Age = ifelse(ID == updated_age, "AHY", Age))


updated_age="01103FD9D0"
id_confirm <- id_confirm %>%
  mutate(Age = ifelse(ID == updated_age, "AHY", Age))

updated_age="01103FD9D0"
id_confirm <- id_confirm %>%
  mutate(Age = ifelse(ID == updated_age, "AHY", Age))

updated_age="01103FE63F"
id_confirm <- id_confirm %>%
  mutate(Age = ifelse(ID == updated_age, "AHY", Age))

updated_age="01103FE6B6"
id_confirm <- id_confirm %>%
  mutate(Age = ifelse(ID == updated_age, "AHY", Age))

updated_age="3B00181EC1"
id_confirm <- id_confirm %>%
  mutate(Age = ifelse(ID == updated_age, "AHY", Age))

updated_age="3B00185AC6"
id_confirm <- id_confirm %>%
  mutate(Age = ifelse(ID == updated_age, "AHY", Age))

updated_age="3B001916B8"
id_confirm <- id_confirm %>%
  mutate(Age = ifelse(ID == updated_age, "AHY", Age))

updated_age="3B001923F9"
id_confirm <- id_confirm %>%
  mutate(Age = ifelse(ID == updated_age, "AHY", Age))

updated_age="01103FE2F4"
id_confirm <- id_confirm %>%
  mutate(Age = ifelse(ID == updated_age, "AHY", Age))
id_confirm %>% filter(ID == updated_age)

#this bird has two different metal ID bands in the banding database
#I asked Murphy and we determined that they might be referring to
# the same bird. In any case, they would have the same age for 
# sure. There is an old note in the master data base
updated_age="3B0018D1AF"
id_confirm <- id_confirm %>%
  mutate(Age = ifelse(ID == updated_age, "AHY", Age))

updated_age="3B00193C2B"
id_confirm <- id_confirm %>%
  mutate(Age = ifelse(ID == updated_age, "HY", Age))

updated_age="3B00497288"
id_confirm <- id_confirm %>%
  mutate(Age = ifelse(ID == updated_age, "HY", Age))

updated_age="3B0049B5EF"
id_confirm <- id_confirm %>%
  mutate(Age = ifelse(ID == updated_age, "HY", Age))

#I HAD TO FIX THIS. 3011-89354 AND 3011-89322 HAVE THE SAME RFID:
# 01103FE6AC IN THE MASTERBANDING SPREADSHEET. I ASKED MURPHY AND 
# HE SAID THAT 3011-89322 IS THE ID WE WANT FOR THIS DATASET.
updated_id="01103FE6AC"
id_confirm <- id_confirm %>%
  mutate(ID = ifelse(Metal_ID == updated_id, "3011-89322", Metal_ID))

id_confirm <- id_confirm %>%
  group_by(ID, Metal_ID, Color_Combo, Sex) %>%
  summarize(n = max(n), Age = first(Age), .groups = "drop")
id_confirm %>% count(ID) %>% filter(n > 1)

id_confirm %>% group_by(Metal_ID) %>% summarize(n=n())
print(id_confirm %>% group_by(Metal_ID) %>% summarize(n=n()), n = Inf)


#THIS IS STEP 4.1 PREPARE THE DATA
#troubleshooting
datastream = compiled_data %>% 
  select(ID, Feeder, DateTime) %>%
  left_join(., id_confirm %>% select(ID, Metal_ID)) %>%
  filter(Metal_ID != "test tag") %>%
  filter(Metal_ID != "possible test tag") %>%
  mutate(
    DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"),  # Ensure DateTime is in POSIXct format
    time_num = as.numeric(DateTime)  # Convert DateTime to numeric (seconds since 1970-01-01)
  ) %>%
  select(Metal_ID, Feeder, DateTime, time_num)

# Check the result
head(datastream)
?gmmevents

#THIS IS STEP 4.2: set up a global set of IDs
all_ids=id_confirm %>% 
  filter(Metal_ID!="test tag") %>% #remove the test tag
  filter(Metal_ID!="possible test tag") %>%
  pull(Metal_ID) %>%  #get just the band number as a vector
  unique() #just get the unique numbers 
all_ids
#STEP 4.3: Divide the dataset into days
use_dates=dates.to.use=seq(ymd("2024-6-14"), ymd("2024-7-14"),1)
use_dates
if(dir.exists("~/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/gmm_daily/")==F) 
  dir.create("~/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/gmm_daily/")

#STEP 4.4 test with one date - THIS IS TEMPERMENTAL. ONLY WORKS ON SOME DAYS. I SUSPECT
#THIS IS BECAUSE SOME DATES DON'T HAVE ENOUGH ENTRIES
for(i in 1){
  data1=datastream%>% filter(date(DateTime)==use_dates[i])
  print(head(data1))
  gmm_out=gmmevents(time=data1$time_num, identity=data1$Metal_ID, location=data1$Feeder, global_ids = all_ids)
  save(gmm_out, file=paste("~/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/gmm_daily/","gmm_", use_dates[i], ".rdata", sep=""))
}

#THIS IS 4.5: TEST CODE FOR MULTIPLE DAYS. 
#WARNING: THIS TAKES A VERY LONG TIME TO RUN
for(i in 1:length(use_dates)){
  print(use_dates[i])
  print(i)
  data1=datastream%>% filter(date(DateTime)==use_dates[i])
  print(head(data1))
  print(all_ids)
  gmm_out=gmmevents(time=data1$time_num, identity=data1$Metal_ID, location=data1$Feeder, global_ids = all_ids)
  save(gmm_out, file=paste("~/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/gmm_daily/","gmm_", use_dates[i], ".rdata", sep=""))
}

#THIS IS STEP 5: CREATING NETWORKS FROM OUTPUTS OF GMM EVENTS MODEL
gmm_filename_full=list.files("~/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/gmm_daily/", full.names = T)
gmm_filename_short=list.files("~/Desktop/murphy r data/Combined CSV Feeder Files for Matrixes/gmm_daily/", full.names = F)
gmm_list=list()
for(i in 1:length(gmm_filename_full)){
  load(gmm_filename_full[[i]])
  gmm_list[[i]]=gmm_out
  gmm_list[[i]]$name=gmm_filename_short[i]
}

gbi.list=lapply(gmm_list, function(x) x$gbi)
all.gbi=do.call("rbind", gbi.list)      
adj=get_network(all.gbi)
g=graph_from_adjacency_matrix(adj, mode="undirected", weighted=T)
plot(g, vertex.label="", edge.width=E(g)$weight*20)
V(g)$name

#ONE OF THE BIRDS DIDN'T HAVE AN AGE, SO I ASSIGNED IT DARKBLUE.
#NOT SURE IF THIS IS STILL AN ISSUE OR NOT. 
V(g)$spp=as.character(id_confirm$Age[match(V(g)$name, id_confirm$Metal_ID)])
V(g)$spp
spp_colors=matrix(c("AHY", "HY", "-", "blue", "lightblue", "darkblue"), ncol=2)
V(g)$spp_col=spp_colors[match(V(g)$spp, spp_colors[,1]),2]
plot(g, vertex.label="", edge.width=E(g)$weight*20, vertex.color=V(g)$spp_col)

?gmmevents
