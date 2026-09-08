# Files Needed:
  # Master Banding Sheet
  # Nesting log for years of interest
  # "Bird_Community_Flow_Table.csv" - a longitudinal analysis output
  # a standardized community CSV that follows logic of the Sankey 
      # this was made manualy - see "community standerdization.csv" for example


#### Load your files ####

cat("select the most recent master banding sheet")
mbs <- read_csv(file.choose()) 

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



cat("select the Bird_Community_Flow_Table.csv")
flow_table <- read_csv(file.choose()) 

cat("select your standardized community csv ")
map <- read_csv(file.choose()) 

#### Check files ####

head(mbs)
head(nesting_datasets)
head(flow_table)
head (map)

#### lets do some cleaning ####

# start with the community map
map <- map %>%
  select(standardized, fall, spring, summer) %>%
  filter(!is.na(fall) | !is.na(spring) | !is.na(summer))

print(map, n = Inf)

# now lets clean the nest logs

# make a cleaning function:
clean_bird <- function(x) {
  x <- as.character(x)
  x[x %in% c(".", "?", "", NA)] <- NA
  str_extract(x, "^[A-Za-z0-9]+")
}

# and clean each file 

nesting_clean <- lapply(nesting_datasets, function(nest_df) {
  
  nest_df %>%
    transmute(
      Nestbox = `11`,
      FirstRound_Bird1 = `PAIRS, first round`,
      FirstRound_Bird2 = `...3`,
      SecondRound_Bird1 = `PAIRS, second round`,
      SecondRound_Bird2 = `...6`,
      SecondRound_Bird3 = `...7`
    ) %>%
    mutate(
      across(
        FirstRound_Bird1:SecondRound_Bird3,
        clean_bird
      )
    )
})

#now check 
print(nesting_clean[[names(nesting_clean)[1]]], n = 20)
