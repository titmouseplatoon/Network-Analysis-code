# Files Needed:
  # the most recent copy of the Master Banding Sheet
  # the merged RFID datasheets (includes Master Banding Sheet data)
  # Nesting log for years of interest 
    #first column must be Nestbox numbers, remove "nestbox used" column if present
    # collum names must match the 2026 log ex: 1:Nestbox	2:PAIRS, first round 3:-blank- 4:-blank-	5:PAIRS, second round" 6:-blank- 7:-blank-		
  # "Bird_Community_Flow_Table.csv" - a longitudinal analysis output
  # a standardized community CSV that follows logic of the Sankey 
      # this was made manualy - see "community standerdization.csv" for example

########### The easiest way to run this code is to:
#Select all (Mac: Command + A)
#Press run (Top right of script panel)
#The code has built-in prompts that will ask you to select and name files
# Each run will make 1 file output : the metadata CSV

# If you need to find your Working Directory type: getwd() into the console
# Need to change your Working Directory?
# Click Session in the top menu bar.
# Hover over Set Working Directory.
# Click "Choose Directory"....Select your folder and click Open. OR choose "To Source File Location" to set it to the folder where your active script is saved.

# Now lets create a massive bird lookup table! 

# Not sure what to do with all this wonderful data???
#   \/  \/  \/  
#hlbaetge@gmail.com (Hannah Baetge)


#### Load your files ####

# Master Banding Sheet 
cat("select your most recent Master Banding Sheet csv ")
mbs <- read_csv(file.choose()) 

# Nest Logs
cat( "load the nesting logs (as many datasheets as you need)")

# Create an empty list to store datasets

nesting_datasets <- list()

# Counter so each dataset gets a unique number

dataset_number <- 1

repeat {
  
  # Tell user what to do
  
  cat("\nPick nesting Dataset", dataset_number,
      "(Press Cancel when finished selecting files)\n")
  
  # Choose file
  selected_file <- tryCatch(
    file.choose(),
    error = function(e) NULL
  )
  
  # Stop if cancel pressed
  if (is.null(selected_file)) {
    cat("\nFinished selecting datasets.\n")
    break
  }
  
  # Extract filename
  file_name <- tools::file_path_sans_ext(
    basename(selected_file)
  )
  
  # Read dataset
  nesting_datasets[[file_name]] <- read_csv(selected_file)
  cat("Loaded:", file_name, "\n")
  dataset_number <- dataset_number + 1
  
}

# Check nesting datasets
length(nesting_datasets)
names(nesting_datasets)


# Community flow table
cat("select the Bird_Community_Flow_Table.csv")
flow_table <- read_csv(file.choose()) 


# Standardized Communities 
cat("select your standardized community csv ")
map <- read_csv(file.choose()) 


# RFID Data
cat( "load the RFID merged datasheets (as many datasheets as you need)")

# Create an empty list to store datasets

rfid_datasets <- list()
rfid_seasons <- list()

# Counter so each dataset gets a unique number

dataset_number <- 1

repeat {
  
  # Tell user what to do
  
  cat("\nPick RFID Merged Dataset", dataset_number,
      "(Press Cancel when finished selecting files)\n")
  
  # Choose file
  selected_file <- tryCatch(
    file.choose(),
    error = function(e) NULL
  )
  
  # Stop if cancel pressed
  if (is.null(selected_file)) {
    cat("\nFinished selecting datasets.\n")
    break
  }
  
  # Extract filename
  file_name <- tools::file_path_sans_ext(
    basename(selected_file)
  )
  
  # Read dataset
  rfid_datasets[[file_name]] <- read_csv(selected_file)
  
  # Ask for season
  season <- rstudioapi::showPrompt(
    title = "Dataset Season",
    message = paste(
      "What season is this RFID dataset?\n\n",
      file_name
    ),
    default = ""
  )
  
  if (is.null(season) || season == "") {
    season <- "Unknown"
  }
  
  # Save season associated with this file
  rfid_seasons[[file_name]] <- season
  
  cat("Loaded:", file_name, "\n")
  cat("Season:", season, "\n")
  
  dataset_number <- dataset_number + 1
}

# Check nesting datasets
length(rfid_datasets)
names(rfid_datasets)

#### YOU CAN START OVER FROM HERE WITHOUT RE-LOADING FILES ####
  #hallelujah 
#### Check files ####

head(mbs)
head(nesting_datasets)
head(flow_table)
head(map)
head(rfid_datasets)


#### lets do some cleaning ####

# start with the community map
map <- map %>%
  select(standardized, fall, spring, summer) %>%
  filter(!is.na(fall) | !is.na(spring) | !is.na(summer))

print(map, n = Inf)




# now let's clean the nest logs

# Make a cleaning function
clean_bird <- function(x) {
  x <- as.character(x)
  x[x %in% c(".", "?", "", NA)] <- NA
  str_extract(x, "^[A-Za-z0-9]+")
}

# Create an empty list for the cleaned datasets
nesting_clean <- list()

# Clean each nesting file
for (file_name in names(nesting_datasets)) {
  
  cat("\nCleaning:", file_name, "\n")
  
  nest_df <- nesting_datasets[[file_name]]
  
  cleaned_df <- nest_df %>%
    transmute(
      Nestbox = .[[1]],
      FirstRound_Bird1 = .[[2]],
      FirstRound_Bird2 = .[[3]],
      FirstRound_Bird3 = .[[4]],
      SecondRound_Bird1 = .[[5]],
      SecondRound_Bird2 = .[[6]],
      SecondRound_Bird3 = .[[7]]
    ) %>%
    pivot_longer(
      cols = -Nestbox,
      names_to = c("Round", "BirdNumber"),
      names_pattern = "(FirstRound|SecondRound)_(Bird\\d+)",
      values_to = "Bird"
    ) %>%
    mutate(
      Bird = clean_bird(Bird),
      Year = as.numeric(str_extract(file_name, "\\d{4}"))
    ) %>%
    filter(
      !is.na(Bird),
      Bird != "color",
      Bird != "metal",
      Bird != "UNB",
      nchar(Bird) > 1
    )
  
  # Add cleaned dataset to the list
  nesting_clean[[file_name]] <- cleaned_df
  
  cat("Finished:", file_name, "\n")
  cat("Rows:", nrow(cleaned_df), "\n")
  cat("Year:", unique(cleaned_df$Year), "\n")
}

# Check
cat("\nCleaned datasets:\n")
print(names(nesting_clean))
print(length(nesting_clean))


#now check the info
print(nesting_clean[[1]], n = 20)



# Now clean the RFID data... long code... sorry!
rfid_processed <- lapply(rfid_datasets, function(df_events) {
  
  # Clean bird color codes
  df_events <- df_events %>%
    mutate(
      ColorCombo = clean_bird(ColorCombo)
    )
  
  # Make sure DateTime is actually a date/time
  df_events <- df_events %>%
    mutate(
      DateTime = as.POSIXct(DateTime)
    )
  
  # Determine the year represented the most
  # This ignores strange startup reads from other years
  network_year <- as.integer(
    names(which.max(
      table(format(df_events$DateTime, "%Y"))
    ))
  )
  
  # Get one row of bird attributes per ColorCombo
  bird_attributes <- df_events %>%
    select(
      ColorCombo,
      Sex,
      DateCaptured,
      LocationCaptured,
      Age
    ) %>%
    filter(!is.na(ColorCombo)) %>%
    distinct(ColorCombo, .keep_all = TRUE)
  
  network_season <- rfid_seasons[[file_name]]
  
  # Make sure DateCaptured is a date
  bird_attributes <- bird_attributes %>%
    mutate(
      DateCaptured = mdy(DateCaptured)
    )
  
  # Calculate capture year
  capture_year <- as.integer(
    format(bird_attributes$DateCaptured, "%Y")
  )
  
  # Calculate years between capture and network
  years_elapsed <- network_year - capture_year
  
  # Save original age
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
  
  # Update ages
  bird_attributes$Age <- mapply(
    update_age,
    bird_attributes$Age,
    years_elapsed
  )
  
  # Make feeder list
  feeder_list <- df_events %>%
    select(
      ColorCombo,
      Feeder
    ) %>%
    filter(
      !is.na(ColorCombo),
      !is.na(Feeder)
    ) %>%
    distinct(ColorCombo, Feeder)
  
  # Collapse feeders into one cell per bird
  feeder_list <- feeder_list %>%
    group_by(ColorCombo) %>%
    summarise(
      Feeders = paste(sort(unique(Feeder)), collapse = ", "),
      .groups = "drop"
    )
  
  # Put bird information + feeder information together
  bird_attributes %>%
    left_join(
      feeder_list,
      by = "ColorCombo"
    ) %>%
    mutate(
      NetworkSeason = network_season
    )
})

# Check the clean rfid datasheet
print(rfid_processed[[1]], n = 20)
print(rfid_processed[[2]], n = 20)
print(rfid_processed[[3]], n = 20)


##### Make Bird Master List ####

all_birds <- sort(unique(c(
  unlist(lapply(nesting_clean, function(x) x$Bird)),
  unlist(lapply(rfid_processed, function(x) x$ColorCombo))
)))

bird_history <- tibble(
  Bird = all_birds
)

#check
print (bird_history)


#### add stable bird history ####

bird_history <- bird_history %>%
  left_join(
    mbs %>%
      arrange(Color.bands..RFID..top.....bottom...ABC., Date..mm.dd.yy.) %>%
      group_by(Color.bands..RFID..top.....bottom...ABC.) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      select(
        Color.bands..RFID..top.....bottom...ABC.,
        Sex..M.F.,
        Date..mm.dd.yy.,
        Location..F..MN..NB....yellow.if.snap.trapped.in.NB..thus.a.breeder.
      ) %>%
      rename
        (Bird = Color.bands..RFID..top.....bottom...ABC., 
          Date_Captured = Date..mm.dd.yy.,
          Location_Captured = Location..F..MN..NB....yellow.if.snap.trapped.in.NB..thus.a.breeder.,
          Sex = Sex..M.F.),
    by = "Bird"
  )

#check
print (bird_history)


#### add nesting info ####

for (nest_name in names(nesting_clean)) {
  
  nest_df <- nesting_clean[[nest_name]]
  
  nest_year <- unique(nest_df$Year)
  
  # Create names for this nesting period
  nest_df <- nest_df %>%
    select(
      Bird,
      Nestbox,
      Round,
      BirdNumber
    ) %>%
    rename(
      !!paste0("Nest_", nest_year, "_Nestbox") := Nestbox,
      !!paste0("Nest_", nest_year, "_Round") := Round,
      !!paste0("Nest_", nest_year, "_BirdNumber") := BirdNumber
    )
  
  # Add to master table
  bird_history <- bird_history %>%
    left_join(
      nest_df,
      by = "Bird",
      relationship = "many-to-many"
    )
}

#check
print (bird_history)
tail (bird_history)

#### add rfid data ####

for (rfid_name in names(rfid_processed)) {
  
  rfid_df <- rfid_processed[[rfid_name]]
  
  # The period you entered when the file was loaded
  network_period <- rfid_seasons[[rfid_name]]
  
  # Make the period safe for use in column names
  period_name <- gsub(" ", "_", network_period)
  
  rfid_df <- rfid_df %>%
    select(
      ColorCombo,
      Age,
      Feeders
    ) %>%
    mutate(
      NetworkSeason = network_period
    ) %>%
    rename(
      Bird = ColorCombo,
      !!paste0("RFID_", period_name, "_Season") := NetworkSeason,
      !!paste0("RFID_", period_name, "_Age") := Age,
      !!paste0("RFID_", period_name, "_Feeders") := Feeders
    )
  bird_history <- bird_history %>%
    left_join(
      rfid_df,
      by = "Bird"
    )
}

#check
print(bird_history) # look at bottom text in gray to see additional variables 



#### add community data #### 
# may need to edit this in the future if using more datasets

#clean bird names
# Clean bird IDs first
flow_table <- flow_table %>%
  mutate(
    ColorCombo = clean_bird(ColorCombo)
  )

# Rename columns only if they have not already been renamed
if ("Fall 2025 Pre-Nesting_Communities" %in% names(flow_table)) {
  flow_table <- flow_table %>%
    rename(
      Fall_2025_Community = `Fall 2025 Pre-Nesting_Communities`,
      Spring_2026_Community = `Spring 2026 Nesting Period_Communities`,
      Summer_2026_Community = `Summer 2026 Post-Fledging_Communities`
    )
}


bird_history <- bird_history %>%
  left_join(
    flow_table %>%
      rename(Bird = ColorCombo),
    by = "Bird"
  )


#### add standardized communities ####

# reformat map table 
    #standardized   Period   CommunityNumber
    # 1              fall     1
    # 1              spring   1
    # 1              summer   2
    # 2              fall     10
    # ect....

community_map_long <- map %>%
  pivot_longer(
    cols = c(fall, spring, summer),
    names_to = "Period",
    values_to = "CommunityNumber"
  ) %>%
  mutate(
    Period = str_to_title(Period)
  )

# extract original community numbers
community_observations <- flow_table_standard %>%
  transmute(
    Bird = ColorCombo,
    
    Fall = as.numeric(
      str_extract(Fall_2025_Community, "\\d+")
    ),
    
    Spring = as.numeric(
      str_extract(Spring_2026_Community, "\\d+")
    ),
    
    Summer = as.numeric(
      str_extract(Summer_2026_Community, "\\d+")
    )
  )

# compare to map
community_standardized <- community_observations %>%
  pivot_longer(
    cols = c(Fall, Spring, Summer),
    names_to = "Period",
    values_to = "CommunityNumber"
  ) %>%
  left_join(   
    relationship = "many-to-many",
    community_map_long,
    by = c("Period", "CommunityNumber")
  )

# find best match

standardized_assignments <- community_standardized %>%
  
  # Remove periods where the bird was not detected
  filter(
    !is.na(CommunityNumber),
    !is.na(standardized)
  ) %>%
  
  # Multiple rows for the same Bird + Period + standardized
  # should count as only ONE match
  distinct(
    Bird,
    Period,
    standardized
  ) %>%
  
  # Count the number of periods supporting each
  # standardized community
  count(
    Bird,
    standardized,
    name = "Matches"
  ) %>%
  
  # Find the maximum number of matches for each bird
  group_by(Bird) %>%
  mutate(
    BestMatches = max(Matches)
  ) %>%
  
  # Keep only standardized communities tied for the
  # highest number of matches
  filter(
    Matches == BestMatches
  ) %>%
  
  # Now determine whether there is one winner or a tie
  summarise(
    StandardizedCommunity = if (n() == 1) {
      first(standardized)
    } else {
      NA_real_
    },
    
    Matches = first(BestMatches),
    
    Ambiguous = n() > 1,
    
    .groups = "drop"
  )


#check 
standardized_assignments %>%
  arrange(Bird) %>%
  head(30)


# add to csv

bird_history <- bird_history %>%
  left_join(
    standardized_assignments %>%
      select(
        Bird,
        StandardizedCommunity,
        Matches
      ),
    by = "Bird"
  )


# check 
print(bird_history)
# Shows all rows, but only the last 3 columns
tail(bird_history[ , (ncol(bird_history) - 2):ncol(bird_history)])



#### write CSV ####

write.csv(
  bird_history,
  file="Bird_Metadata.csv",
  row.names = FALSE
)

