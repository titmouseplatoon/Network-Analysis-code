# To Run This code you will need some of the intermediary outputs from "Gaussian_Plot_Script.R", and the original merged data CSV that you ran through the Gaussian plotting softwhere
  # ensure you have the "gmm_daily" folder saved in your working directory 
    # gmm_daily should ALREDY have the datafiles from all the dates you are interested in. 

#Also, look have the dates the dataset covers at the ready... you will have to input them

# The full explanaiton for these stats can be found here:
# https://dshizuka.github.io/networkanalysis/07_mrqap.html
  # "The Multiple Regression Quadratic Assignment Procedure (MRQAP) 
  # is an extension of this approach to allow for multiple covariate matrices (Krackhardt 1988). 
  # Essentially, MRQAP allows you to determine the influence of one matrix on another, 
  # controlling for the effects of one or more covariate matrices" 


########### The easiest way to run this code is to:
#Select all (Mac: Command + A)
#Press run (Top right of script panel)
#The code has built-in prompts that will ask you to select and name files
#*****# Each run will make two file outputs: 
#the files will be saved to your working directory

######## NOTE!!!!!!
      # The folder "gmm_daily" must be in your working directory for the code to find it!!!!
# If you need to find your Working Directory type: getwd() into the console
# Need to change your Working Directory?
# Click Session in the top menu bar.
# Hover over Set Working Directory.
# Click "Choose Directory"....Select your folder and click Open. OR choose "To Source File Location" to set it to the folder where your active script is saved.

#Questions? Email me!
# This combines two of my favorete things! Birds and Stats!!!!!
#(Hannah Baetge) hlbaetge@gmail.com

library(igraph)
library(tidyverse)
library(asnipe)
library(sna)

#### Select your data of interest ####

# set dataframe you are looking at
#enter start and end date of dataset
start_date <- rstudioapi::showPrompt(
  title = "enter FIRST date of dataset", 
  message = "Enter date in YMD format ex 2021-12-06", 
  default = "yyyy-mm-dd"
)

cat("\n Start date entered:", start_date, "\n")

end_date <- rstudioapi::showPrompt(
  title = "enter LAST date of dataset", 
  message = "Enter date in YMD format ex 2021-12-06", 
  default = "yyyy-mm-dd"
)

cat("\n End date entered:", end_date, "\n")


# Convert entered dates
start_date <- as.Date(start_date)
end_date <- as.Date(end_date)


#### Pull the relevant files from "gmm_daily" folder ####

# Get all files in the folder
gmm_filename_full <- list.files("gmm_daily", full.names = TRUE)
gmm_filename_short <- list.files("gmm_daily", full.names = FALSE)


# Extract YYYY-MM-DD from each filename
file_dates <- as.Date(
  sub(".*(\\d{4}-\\d{2}-\\d{2}).*", "\\1", gmm_filename_short)
)


# Keep files between the entered dates, inclusive
keep_files <- file_dates >= start_date & file_dates <= end_date

gmm_filename_full <- gmm_filename_full[keep_files]
gmm_filename_short <- gmm_filename_short[keep_files]


# show which files were selected
cat("\nFiles included:\n")
print(gmm_filename_short)


# Load only selected files
gmm_list <- lapply(gmm_filename_full, function(file) {
  
  temp_env <- new.env()
  load(file, envir = temp_env)
  
  temp_env$gmm_out
})


#### Make datafile for selected data ####

# Stitch together each day
gbi.list=lapply(gmm_list, function(x) x$gbi)
all.gbi=do.call("rbind", gbi.list)

# create adjacency matrix 
adj=get_network(all.gbi)

# Create igraph object 
g=graph_from_adjacency_matrix(adj, mode="undirected", weighted=T)

#### assign first variable ####

# the adjacency matrix is your dependent variable  

m1<- adj


#### Match up variables of interest with names ####
 # MUST KEEP IN SAME ORDER!!!!


# list names of birds in matrix order
birds <- V(g)$name

# make dataframe of bird attributes 

#### Get bird attributes from original RFID data ####

# Select the original merged RFID CSV
# This is the same type of file used to create the GMM files
cat("\nSelect the original merged RFID CSV containing bird attributes...\n")

compiled_data <- read_csv(file.choose())

# Create df_events - holder for the original RFID data
df_events <- compiled_data %>%
  arrange(Feeder, DateTime) %>%
  group_by(Feeder)


bird_attributes <- df_events %>%
  select(ColorCombo, Sex, Status, LocationCaptured,
         Age, CrestColor, DateCaptured, Feeder) %>%
  mutate(
    Sex = na_if(Sex, ""),
    Status = na_if(Status, ""),
    LocationCaptured = na_if(LocationCaptured, ""),
    Age = na_if(Age, ""),
    CrestColor = na_if(CrestColor, ""),
    DateCaptured = as.Date(DateCaptured, format = "%m/%d/%y")
  ) %>%
  group_by(ColorCombo) %>%
  summarise(
    Sex = first(na.omit(Sex)),
    Status = first(na.omit(Status)),
    LocationCaptured = first(na.omit(LocationCaptured)),
    Age = first(na.omit(Age)),
    CrestColor = first(na.omit(CrestColor)),
    DateCaptured = first(na.omit(DateCaptured)),
    .groups = "drop"
  )

# Determine the year represented the most in this RFID dataset (cuts our strange reads from start-up)
network_year <- as.integer(
  names(which.max(table(format(df_events$DateTime, "%Y"))))
)

# Determine the year of the most recent capture
capture_year <- as.integer(format(bird_attributes$DateCaptured, "%Y"))

# Years between capture and network
years_elapsed <- network_year - capture_year

# print original age data for comparison b4 update
old_age <- bird_attributes$Age 

# Function to update age
update_age <- function(age, years){
  
  if(is.na(age) | is.na(years))
    return(age)
  
  if(age == "Nestling"){
    if(years == 0) return("Nestling")
    if(years == 1) return("SY")
    return("ASY")
  }
  
  if(age == "HY"){
    if(years == 0) return("HY")
    if(years == 1) return("SY")
    return("ASY")
  }
  
  if(age == "AHY"){
    if(years == 0) return("AHY")
    return("ASY")
  }
  
  if(age == "SY"){
    if(years == 0) return("SY")
    return("ASY")
  }
  
  if(age == "ASY"){
    return("ASY")
  }
  
  age
}

# Update every bird's age automatically
bird_attributes$Age <- mapply(
  update_age,
  bird_attributes$Age,
  years_elapsed
)

# compare old and new ages and see where changes were 
changed <- old_age != bird_attributes$Age &
  !is.na(old_age)

if(any(changed)){
  cat("\nUpdated bird ages:\n")
  print(
    data.frame(
      Bird = bird_attributes$ColorCombo[changed],
      OldAge = old_age[changed],
      NewAge = bird_attributes$Age[changed]
    )
  )
}



attributes <- bird_attributes %>%
  filter(ColorCombo %in% birds) %>%
  arrange(match(ColorCombo, birds))

# remove "." - formatting blip
attributes$Age <- trimws(attributes$Age)
attributes$Age[attributes$Age == "ASY."] <- "ASY"
cat( "\n confirm the orders of birds match \n")

attributes$ColorCombo
birds

# another format to check
data.frame(
  NetworkBird = birds,
  AttributeBird = attributes$ColorCombo,
  Sex = attributes$Sex,
  Age = attributes$Age
)





#### TESTING ####

# Make Matrixs of interest 

### AGE ###
# Age: 1 = same age class, 0 = different age class
age_matrix <- outer(
  attributes$Age,
  attributes$Age,
  FUN = "=="
) * 1

#remove self comparisons 
diag(age_matrix) <- 0

#easy ID
m3<- age_matrix

# check age values 
table(attributes$Age)



### SEX ###
# Since some sexes are unknown (NA), the matrix must be modified
# Keep only birds whose sex is known
keep_sex <- !is.na(attributes$Sex)

# Network matrix for those birds
m1_sex <- m1[keep_sex, keep_sex, drop = FALSE]

# Sex information for those birds
sex <- attributes$Sex[keep_sex]

# 1 = same sex
# 0 = different sex
m2_sex <- outer(
  sex,
  sex,
  FUN = "=="
) * 1

# Remove self-comparisons
diag(m2_sex) <- 0


### AGE matrix when also comparing sex ###
# Only birds with both known sex AND age
keep_both <- !is.na(attributes$Sex) & !is.na(attributes$Age)

m1_both <- m1[keep_both, keep_both, drop = FALSE]

sex_both <- attributes$Sex[keep_both]
age_both <- attributes$Age[keep_both]

m2_both <- outer(
  sex_both,
  sex_both,
  FUN = "=="
) * 1

m3_both <- outer(
  age_both,
  age_both,
  FUN = "=="
) * 1

diag(m2_both) <- 0
diag(m3_both) <- 0


#### Easy ID key ####

  # m1 = all 70 birds Network variables
  # m1_sex = network variables for ONLY birds with known sex
  # m1_both = network variables for ONLY birds with both known sex and age

  # m2_sex = sex data for only birds with data (eg. no NA)
  # m2_both = sex data for ONLY birds with both known sex and age

  # m3 = age matrix  of all birds 
  # m3_both = age data for ONLY birds with both known sex and age


#### Sex influence test ####
netlm(m1_sex, m2_sex, mode="graph", nullhyp="qap", test.statistic="t-value")

#### Age influence test ####
netlm(m1, m3, mode="graph", nullhyp="qap", test.statistic="t-value")

#### Sex & Age influence test ####
#test the effect of m2 on m1, controlling for m3. sna package function.
netlm(m1_both, m2_both+m3_both, mode="graph", nullhyp="qap", test.statistic="t-value")

