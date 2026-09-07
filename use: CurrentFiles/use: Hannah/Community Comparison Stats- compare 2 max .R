# this code is intended to be run after the V2 Plot code WITH community grouping.
# the V2 plot code has two outputs
  # the plots that are visually interesting and information dense, but hard to parse
  # the "data name"_Communities.csv that holds hard data from the networks that can now be compared. 

#Note! Some Questions are best answered in plot based statistical analysis
  # there is a file called "Stats for Plots.R" Look at this for measures of centrality

#Note! This code can only compare to communities at a time.
#       e.g.. Fall to spring, fall to summer, or spring to summer.
#     if you need to compare multiple communities at once, use: Longitudinal Analysis.R
#     This code makes a Sankey plot comparing 2 seasons and lots of ARI and Jaccard stats- but it is one step at a time


#This code will use the _Communities.csv files, not the plots. Have those files at the ready!

# This code spits out a LOT of files (for redundancy)
# I suggest making a new folder each time you run this code 
    #AND temporarily set your working directory to to that folder
# If you need to find your Working Directory type: getwd() into the console
# Need to change your Working Directory?
# Click Session in the top menu bar.
# Hover over Set Working Directory.
# Click "Choose Directory"....Select your folder and click Open. OR choose "To Source File Location" to set it to the folder where your active script is saved.


#Questions about this code? Email me! Being a Pre-Med can get dull, Send me bird questions to spice it up!
  #hlbaetge@gmail.com (Hannah Baetge)

#################### LETS GO!!! ###########################
# Load packages

library(dplyr)
library(tidyr)
library(readr)
library(mclust) # big package... for info: vignette("mclust")
library(ggalluvial)
library(ggplot2)




# Matrix Function ######################################################
# write function: make_pair_matrix()

# Purpose:
#   Takes one season's community assignments and creates a table containing every possible pair of birds.

#   For each pair, it records:
#      - Bird 1
#      - Bird 2
#      - Community of Bird 1
#      - Community of Bird 2
#      - TRUE/FALSE if they belong to the same community
#      - Season name

# Inputs:
#   df = one season's dataframe
#   season_name = text label (ex. "Fall2025")

# Output:
#   A dataframe containing one row for every pair of birds.

make_pair_matrix <- function(df, season_name){
  
  birds <- df$ColorCombo # List of all birds By Color combo name 
  
  # Create Matrix, Every bird name X Every bird name 
  pairs <- expand.grid(
    Bird1 = birds,
    Bird2 = birds,
    stringsAsFactors = FALSE
  )
  
  # Remove duplicate pairs and self-pairs. eg,
  #   Keep:    CRY-GNN
  #   Remove:  GNN-CRY
  #   Remove:  CRY-CRY
  pairs <- pairs %>%  
    filter(Bird1 < Bird2)
  
  #join the Community column from the original data onto the Bird1 column.
  
  pairs <- pairs %>%
    left_join(
      df %>%
        select(ColorCombo, Community),
      by = c("Bird1" = "ColorCombo")
    )
  
  # Rename Bird1's community so we know whose it is.
  pairs <- pairs %>%
    rename(Comm1 = Community)
  
  # Repeat for Bird2.

  pairs <- pairs %>%
    left_join(
      df %>%
        select(ColorCombo, Community),
      by = c("Bird2" = "ColorCombo")
    )
  
  pairs <- pairs %>%
    rename(Comm2 = Community)
  
  

  # Compare the two community numbers.

  # If both birds belong to the same community: TRUE
  # Otherwise:FALSE
  # Also add season name so we know which dataset this comparison came from.
  
  pairs <- pairs %>%
    mutate(
      SameCommunity = Comm1 == Comm2,
      Season = season_name
    )
  
  # Return the completed dataframe.

  
  return(pairs)
  
}


# Import Datasets ###################################
# Now you have the function to make the Pairs Matrix 
# Now it must be run on each community dataset

# Create an empty list to store all selected datasets
community_datasets <- list()

# Counter so each dataset gets a unique name
dataset_number <- 1

repeat {
  
  # Tell the user what to do
  cat("\nPick Community Dataset", dataset_number,
      "(Press Cancel when finished selecting files)\n")
  
  # Try to let the user choose a file
  selected_file <- tryCatch(
    file.choose(),
    error = function(e) NULL   # If Cancel is pressed, return NULL instead of stopping
  )
  
  # If Cancel was pressed, exit the loop
  if (is.null(selected_file)) {
    cat("\nFinished selecting datasets.\n")
    break
  }
  
  # Extract just the filename (without the folder path)
  file_name <- tools::file_path_sans_ext(basename(selected_file))
  
  # Read the file and store it using the filename as its name
  community_datasets[[file_name]] <- read_csv(selected_file)
  
  cat("Loaded:", file_name, "\n")
  
  # Increase the counter
  dataset_number <- dataset_number + 1
  

}

# Check how many datasets were loaded
length(community_datasets)

# Show the names of all imported datasets
names(community_datasets)

# Clean Data ###########################
# Check imported datasets have required columns

required_columns <- c(
  "ColorCombo",
  "Community",
  "Sex",
  "Age",
  "Status",
  "Location"
)


for(name in names(community_datasets)){
  
  missing <- setdiff(
    required_columns,
    names(community_datasets[[name]])
  )
  
  if(length(missing) > 0){
    
    cat(
      "\nWARNING:",
      name,
      "is missing:",
      paste(missing, collapse=", "),
      "\n"
    )
    
  }
  
}

# No Note? - Then it all looks good!
# Great job, you imported the right files!

#Run Matrix #########################################
# Now run all imported data sets through the Matrix function.

pair_matrices <- lapply(
  names(community_datasets),
  function(name) {
    
    community_matrix <- make_pair_matrix(
      community_datasets[[name]],
      season_name = name
    )
    
    write.csv(
      community_matrix,
      paste0(name, "_matrix.csv"),
      row.names = FALSE
    )
    
    return(community_matrix)
  }
)

# Keep the names on the output list, too
names(pair_matrices) <- names(community_datasets)


#ARI Values ######################################################
#New Function - Find ARI Value

# Function: compare_seasons_ARI()

# Purpose:
#   Compare the community assignments between two seasons
#   using the Adjusted Rand Index (ARI).
#       -0.5 groups are "activly" changing - lack of consistency even more than random assignmet
#       0.0  if group assignment were perfectly random
#       1.0 if the groups are identical
# Inputs:
#   df1 = First season dataframe
#   df2 = Second season dataframe
#
# Output:
#   A single ARI value.
#
# IMPORTANT:
#   This calculation includes ONLY birds detected in both seasons.
#   Birds absent from either season are excluded rather than treated
#   as community changes.

compare_seasons_ARI <- function(df1, df2){
  
  # Find birds that occur in both datasets
  common_birds <- intersect(df1$ColorCombo,
                            df2$ColorCombo)
  
  # Keep only those birds
  s1 <- df1 %>%
    filter(ColorCombo %in% common_birds)
  
  s2 <- df2 %>%
    filter(ColorCombo %in% common_birds)
  
  # Put birds in the same order
  s1 <- s1[order(s1$ColorCombo), ]
  s2 <- s2[order(s2$ColorCombo), ]
  
  # Calculate ARI
  adjustedRandIndex(
    s1$Community,
    s2$Community
  )
  
}

# Now Run for our loaded data sets
# Create an empty dataframe to store results
ARI_results <- data.frame()

dataset_names <- names(community_datasets)

for(i in 1:(length(dataset_names)-1)){
  
  for(j in (i+1):length(dataset_names)){
    
    season1 <- dataset_names[i]
    season2 <- dataset_names[j]
    
    ari <- compare_seasons_ARI(
      community_datasets[[season1]],
      community_datasets[[season2]]
    )
    
    ARI_results <- rbind(
      ARI_results,
      data.frame(
        Season1 = season1,
        Season2 = season2,
        ARI = ari
      )
    )
    
  }
  
}

write.csv(
  ARI_results,
  "ARI_results.csv",
  row.names = FALSE
)

cat("\nARI results saved.\n")

# stop and ponder the joy of science


# Jaccard Similarity ######################################################
#Compare Communities Between Two Seasons Using Jaccard Similarity

# Function: compare_communities()

# Purpose:
# Compare every community in one season with every community in another season.

# For every possible community comparison, calculate:
#   - Number of shared birds
#   - Number of unique birds
#   - Jaccard Similarity
#   - Birds shared between communities
#   - Birds lost
#   - Birds gained

# Inputs:
# df1 = First season dataframe
# df2 = Second season dataframe

# season1_name = Name of first season
# season2_name = Name of second season

# Output:
# One row for every community comparison.

compare_communities <- function(df1,
                                df2,
                                season1_name,
                                season2_name){
  
  # Create an empty dataframe to store the results
  
  comparison_table <- data.frame()
  
  # Find every community number in each season
  
  communities1 <- sort(unique(df1$Community))
  communities2 <- sort(unique(df2$Community))
  
  # Compare every community in Season 1 to every community in Season 2
  
  for(comm1 in communities1){
    
    for(comm2 in communities2){
      
      # Get every bird in Community 1
      
      birds1 <- df1 %>%
        filter(Community == comm1) %>%
        pull(ColorCombo)
      
      # Get every bird in Community 2
      
      birds2 <- df2 %>%
        filter(Community == comm2) %>%
        pull(ColorCombo)
      
      # Find birds shared between both communities
      
      shared_birds <- intersect(birds1, birds2)
      
      # Find every unique bird found in either community
      
      all_birds <- union(birds1, birds2)
      
      # Birds present only in Season 1
      
      lost_birds <- setdiff(birds1, birds2)
      
      # Birds present only in Season 2
      
      gained_birds <- setdiff(birds2, birds1)
      
# Calculate the Jaccard Similarity
# Jaccard = Shared Birds / Total Unique Birds
      
      shared_n <- length(shared_birds)
      
      union_n <- length(all_birds)
      
      if(union_n == 0){
        
        jaccard <- 0
        
      } else {
        
        jaccard <- shared_n / union_n
        
      }
      
# Save this comparison
      
      comparison_table <- rbind(
        
        comparison_table,
        
        data.frame(
          
          Season1 = season1_name,
          Community1 = comm1,
          
          Season2 = season2_name,
          Community2 = comm2,
          
          SharedBirds = paste(shared_birds,
                              collapse = ", "),
          
          LostBirds = paste(lost_birds,
                            collapse = ", "),
          
          GainedBirds = paste(gained_birds,
                              collapse = ", "),
          
          Shared = shared_n,
          
          Union = union_n,
          
          Jaccard = round(jaccard, 3),
          
          stringsAsFactors = FALSE
          
        )
        
      )
      
    }
    
  }
  
  # Return the completed comparison table
  
  return(comparison_table)
  
}

# Now, re-do for all season comparisons
# Create an empty list to store all comparison tables

community_comparisons <- list()

# Get the names of every imported dataset

dataset_names <- names(community_datasets)

# Compare every season to every other season
# Example:
# Fall -> Winter
# Fall -> Spring
# Fall -> Summer
# Winter -> Spring
# Winter -> Summer
# Spring -> Summer


for(i in 1:(length(dataset_names)-1)){
  
  for(j in (i+1):length(dataset_names)){
    
    season1 <- dataset_names[i]
    season2 <- dataset_names[j]
    
    cat("Comparing", season1, "to", season2, "\n")
    
    comparison <- compare_communities(
      
      community_datasets[[season1]],
      community_datasets[[season2]],
      
      season1,
      season2
      
    )
    
    community_comparisons[[paste0(season1,
                                  "_to_",
                                  season2)]] <- comparison
    
    
    write.csv(
      
      comparison,
      
      paste0(season1,
             "_to_",
             season2,
             "_CommunityComparison.csv"),
      
      row.names = FALSE
      
    )
    
  }
  
}

# View Results

# Display the names of all comparison tables

names(community_comparisons)

cat("\nCommunity comparisons complete!\n")
cat("Comparison tables have been saved as CSV files.\n")
cat("Quick check- View the first comparison table:\n\n")


community_comparisons[[1]]

cat("\n\n Whoop!!! \n")


#Create Ploting Tables ######################################################
# time to plot changes in community membership over time hot-darn-wowza!



# Keep Best Jaccard Match (% of community that moved on)

best_matches <- lapply(
  
  community_comparisons,
  
  function(df){
    
    df %>%
      
      group_by(Season1, Community1) %>%
      
      slice_max(
        Jaccard,
        n = 1,
        with_ties = FALSE
      ) %>%
      
      ungroup() %>%
      
      filter(Shared > 0)
    
  }
  
)

# Save best match tables

best_match_names <- names(best_matches)


for(i in seq_along(best_matches)){
  
  write.csv(
    
    best_matches[[i]],
    
    paste0(
      best_match_names[i],
      "_BestMatches.csv"
    ),
    
    row.names = FALSE
    
  )
  
}


cat("\nBest community matches saved.\n")


# Build Alluvial Plot Tables

sankey_tables <- list()

comparison_names <- names(best_matches)


for(i in seq_along(best_matches)){
  
  comparison <- best_matches[[i]]
  
  season1 <- unique(comparison$Season1)
  
  season2 <- unique(comparison$Season2)
  
  
  # Create labels for first season
  
  labels1 <- make_node_labels(
    community_datasets[[season1]],
    season1
  )
  
  
  # Create labels for second season
  
  labels2 <- make_node_labels(
    community_datasets[[season2]],
    season2
  )
  
  
  # Add labels to the comparison table
  
  sankey <- comparison %>%
    
    left_join(
      labels1,
      by = c(
        "Community1" = "Community"
      )
    ) %>%
    
    rename(
      Source = Label
    ) %>%
    
    left_join(
      labels2,
      by = c(
        "Community2" = "Community"
      )
    ) %>%
    
    rename(
      Target = Label
    )
  
  
  # Save in list
  
  sankey_tables[[comparison_names[i]]] <- sankey
  
  
  # Export figure-ready table
  
  write.csv(
    
    sankey,
    
    paste0(
      comparison_names[i],
      "_AlluvialPlot_Table.csv"
    ),
    
    row.names = FALSE
    
  )
  
}


cat("\nAlluvial plot tables saved.\n")


# Metadata Table ############################################################

# Create empty dataframe to store summary information

transition_metadata <- data.frame()


# Get the names of all comparisons

comparison_names <- names(community_comparisons)


for(i in seq_along(community_comparisons)){
  
  
  # Pull out the current comparison table
  
  comparison <- community_comparisons[[i]]
  
  
  # Identify the two seasons
  
  season1 <- unique(comparison$Season1)
  
  season2 <- unique(comparison$Season2)
  
  
  # Pull the original community datasets
  
  df1 <- community_datasets[[season1]]
  
  df2 <- community_datasets[[season2]]
  
  
  # Count birds in each season
  
  birds_season1 <- length(
    unique(df1$ColorCombo)
  )
  
  birds_season2 <- length(
    unique(df2$ColorCombo)
  )
  
  
  # Count birds appearing in both seasons
  
  shared_birds <- length(
    intersect(
      df1$ColorCombo,
      df2$ColorCombo
    )
  )
  
  
  # Count communities
  
  communities_season1 <- length(
    unique(df1$Community)
  )
  
  communities_season2 <- length(
    unique(df2$Community)
  )
  
  
  # Count the number of best community matches
  
  best_match_count <- nrow(
    best_matches[[i]]
  )
  
  
  # Add one row to metadata table
  
  transition_metadata <- rbind(
    
    transition_metadata,
    
    data.frame(
      
      Season1 = season1,
      
      Season2 = season2,
      
      Birds_Season1 = birds_season1,
      
      Birds_Season2 = birds_season2,
      
      Shared_Birds = shared_birds,
      
      Communities_Season1 = communities_season1,
      
      Communities_Season2 = communities_season2,
      
      Community_Transitions = best_match_count,
      
      stringsAsFactors = FALSE
      
    )
    
  )
  
}


# View metadata table

transition_metadata


# Save metadata table

write.csv(
  
  transition_metadata,
  
  "Community_Transition_Metadata.csv",
  
  row.names = FALSE
  
)


cat("\nTransition metadata saved.\n")






# JUMP HERE #############################
# FOR PLOTS #############################

# Node Lables ##########################
# Function: make_node_labels()

# Purpose:
# Create labels for each community that include:

# Season
# Community Number
# Number of Birds
# Bird Names

# Output:
# A dataframe with one row per community.

make_node_labels <- function(df, season_name){
  
  node_table <- data.frame()
  
  communities <- sort(unique(df$Community))
  
  for(comm in communities){
    
    birds <- df %>%
      filter(Community == comm) %>%
      arrange(ColorCombo) %>%
      pull(ColorCombo)
    
    label <- paste0(
      
      "Community ",
      
      comm,
      
      "\n",
      
      "n = ",
      
      length(birds),
      
      " birds",
      
      "\n",
      
      paste(birds,
            collapse="\n")
      
    )
    
    node_table <- rbind(
      
      node_table,
      
      data.frame(
        
        Community = comm,
        
        Label = label,
        
        stringsAsFactors = FALSE
        
      )
      
    )
    
  }
  
  return(node_table)
  
}



#Plot Community Sankey ###########################################################

library(readr)
library(ggplot2)
library(ggalluvial)



# Select the comparison you want to plot

# For example:
# Fall2025_to_Winter2025
cat("\n\n select desired _AlluvialPlot_Table.csv \n")

sankey_data <- read_csv(file.choose()) # pick data file you wish to graph

#Make plot labels
labels1 <- make_node_labels(
  community_datasets[[unique(sankey_data$Season1)]]
)


labels2 <- make_node_labels(
  community_datasets[[unique(sankey_data$Season2)]]
)

# remove in-data labels (too long and detailed)
sankey_data <- sankey_data %>%
  select(
    -any_of(c("Source", "Target", "Label"))
  )


# add labels to plot

sankey_data <- sankey_data %>%
  
  left_join(
    labels1,
    by = c("Community1" = "Community")
  ) %>%
  
  rename(
    Source = Label
  ) %>%
  
  left_join(
    labels2,
    by = c("Community2" = "Community")
  ) %>%
  
  rename(
    Target = Label
  )


# Create the alluvial plot

community_sankey <- ggplot(
  
  sankey_data,
  
  aes(
    
    axis1 = Source,
    
    axis2 = Target,
    
    y = Shared
    
  )
  
) +
  
  geom_alluvium(
    
    aes(fill = Source),
    
    alpha = 0.7
    
  ) +
  
  geom_stratum(
    
    width = 0.25,
    
    fill = "grey80",
    
    color = "black"
    
  ) +
  
  geom_text(
    
    stat = "stratum",
    
    aes(label = after_stat(stratum)),
    
    size = 3
    
  ) +
  
  scale_x_discrete(
    
    limits = c("Previous Season",
               "Next Season"),
    
    expand = c(.1, .1)
    
  ) +
  
  labs(
    
    title = paste0(
      
      unique(sankey_data$Season1),
      
      " to ",
      
      unique(sankey_data$Season2),
      
      " Community Change"
      
    ),
    
    y = "",
    
    x = ""
    
  ) +
  
  
  theme_minimal() +
  
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  
  theme(
    legend.position = "none"
  )
  





# Display plot

community_sankey

