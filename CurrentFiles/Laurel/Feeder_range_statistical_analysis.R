library(dplyr)
library(here)

# Read data
bird_matrix <- read.csv(
  here("Laurel", "Bird_Feeder_Matrix.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

feeders <- colnames(bird_matrix)[5:23]

# Force numeric + replace NA 
bird_matrix[feeders] <- lapply(bird_matrix[feeders], as.numeric)
bird_matrix[feeders][is.na(bird_matrix[feeders])] <- 0

# Calculate metrics
bird_matrix$range_size <- rowSums(bird_matrix[feeders] > 0)
bird_matrix$total_visits <- rowSums(bird_matrix[feeders])
bird_matrix$visits_per_feeder <- ifelse(
  bird_matrix$range_size > 0,
  bird_matrix$total_visits / bird_matrix$range_size,
  NA
)

bird_analysis <- bird_matrix %>%
  select(ColorCombo, RFID, Age, Sex,
         range_size, total_visits, visits_per_feeder)

# Plot 
hist(
  bird_analysis$range_size,
  breaks = 8,
  main = "Distribution of Feeder Range Size",
  xlab = "Number of Feeders Visited"
)

# Save bird-level data
write.csv(
  bird_analysis,
  here("Laurel", "Bird_Feeder_Statistical_Analysis.csv"),
  row.names = FALSE
)

# Overall summary 
overall_summary <- bird_analysis %>%
  summarise(
    mean_range = mean(range_size),
    sd_range   = sd(range_size),
    min_range  = min(range_size),
    max_range  = max(range_size)
  )

write.csv(
  overall_summary,
  here("Laurel", "Range_Overall_Summary.csv"),
  row.names = FALSE
)

# By sex
sex_summary <- bird_analysis %>%
  filter(Sex %in% c("M", "F")) %>%
  group_by(Sex) %>%
  summarise(
    mean_range = mean(range_size),
    sd_range   = sd(range_size),
    n = n(),
    .groups = "drop"
  )

write.csv(
  sex_summary,
  here("Laurel", "Range_By_Sex.csv"),
  row.names = FALSE
)

# By age
age_summary <- bird_analysis %>%
  group_by(Age) %>%
  summarise(
    mean_range = mean(range_size),
    sd_range   = sd(range_size),
    n = n(),
    .groups = "drop"
  )

write.csv(
  age_summary,
  here("Laurel", "Range_By_Age.csv"),
  row.names = FALSE
)
