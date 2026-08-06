# this code is intended to be run after the V2 Plot code WITH community grouping.
# the V2 plot code has two outputs
# the plots that are visually interesting and information dense, but hard to parse
# the "data name"_Communities.csv that holds hard data from the networks that can now be compared. 

# This code is mostly a VISUAL Tool - it does not have hard core stats 

#Therefore ....

#1) Some Questions are best answered in plot based statistical analysis
# there is a file called "Stats for Plots.R" Look at this for measures of centrality

#2) The Community Comparison code makes a Sankey plot comparing 2 seasons 
#   and has lots of ARI and Jaccard stats- but it is one step at a time eg fall-spring THEN spring-summer


#This code will use the _Communities.csv files, not the plots. Have those files at the ready!


# unfortunately, due to all the filtering which has to be copied to verbatim, 
# pop-up boxes are not the best fit for this code. 
# Be prepared to go in and manually edit anytime you change data sets 
# Most areas that will need change whould Be anotated with #$$$


#Questions about this code? Email me! I'm sure I will have something to procrastinate...please email
#hlbaetge@gmail.com (Hannah Baetge)




# Load packages

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggalluvial)




# Create an empty list to store datasets

community_datasets <- list()


# Counter so each dataset gets a unique number

dataset_number <- 1


repeat {
  
  # Tell user what to do
  
  cat("\nPick Community Dataset", dataset_number,
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
  
  community_datasets[[file_name]] <- read_csv(selected_file)
  
  
  cat("Loaded:", file_name, "\n")
  
  
  dataset_number <- dataset_number + 1
  
}



# Check datasets

length(community_datasets)

names(community_datasets)


# Function: Indiv. Bird community movement file creation #######################

# Track individual birds as they move between communities across seasons.
# Combine all seasons into one table.
# Each row = one bird in one season

# Example:
#
# ColorCombo   Season        Community
# CRY          Fall2025      1
# CRY          Winter2025    4
# CRY          Spring2026    2
#



community_timeline <- bind_rows(
  
  lapply(
    
    names(community_datasets),
    
    function(season_name){
      
      
      community_datasets[[season_name]] %>%
        
        select(
          ColorCombo,
          Community
        ) %>%
        
        mutate(
          Season = season_name
        )
      
    }
    
  )
  
)



# View timeline

community_timeline



# Save timeline table

write.csv(
  
  community_timeline,
  
  "Bird_Community_Timeline.csv",
  
  row.names = FALSE
  
)



# RESHAPE FOR ALLUVIAL PLOT ###########################

# ggalluvial requires:

# One row per bird
# One column per season

# Example:

# ColorCombo    Fall       Winter       Spring
# CRY           1          4            2
# GNN           1          4            2
#

#reshape so one entry per bird as seen above
bird_flow <- community_timeline %>%
  
  pivot_wider(
    
    names_from = Season,
    
    values_from = Community
    
  )

# remove NAs
bird_flow <- bird_flow %>%
  
  mutate(
    
    across(
      
      -ColorCombo,
      
      ~replace_na(as.character(.), "Not detected")
      
    )
    
  )

# rename communities from numbers to informative
#$$$ This Needs to be  changed If you are using different data sets
# the single back-quoted items ( `Fall 2025 Pre-Nesting_Communities`) are the column names form the bird flow dataset
  
  bird_flow <- bird_flow %>%
  
  mutate(
    
    `Fall 2025 Pre-Nesting_Communities` =
      ifelse(
        `Fall 2025 Pre-Nesting_Communities` == "Not detected",
        "Fall Not detected",
        paste0("Fall C", `Fall 2025 Pre-Nesting_Communities`)
      ),
    
    
    `Spring 2026 Nesting Period_Communities` =
      ifelse(
        `Spring 2026 Nesting Period_Communities` == "Not detected",
        "Spring Not detected",
        paste0("Spring C", `Spring 2026 Nesting Period_Communities`)
      ),
    
    
    `Summer 2026 Post-Fledging_Communities` =
      ifelse(
        `Summer 2026 Post-Fledging_Communities` == "Not detected",
        "Summer Not detected",
        paste0("Summer C", `Summer 2026 Post-Fledging_Communities`)
      )
    
  )

# View reshaped table

bird_flow



# Save for future plotting

write.csv(
  
  bird_flow,
  
  "Bird_Community_Flow_Table.csv",
  
  row.names = FALSE
  
)

# Make a copy for the "persistent birds only" plot
#$$$ This Needs to be  changed If you are using different data sets
# the single back-quoted items ( `Fall 2025 Pre-Nesting_Communities`) are the column names form the bird flow dataset

bird_flow_persistent <- bird_flow %>%
  
  filter(
    
    `Fall 2025 Pre-Nesting_Communities` != "Fall Not detected",
    
    `Spring 2026 Nesting Period_Communities` != "Spring Not detected",
    
    `Summer 2026 Post-Fledging_Communities` != "Summer Not detected"
    
  )

write.csv(
  bird_flow_persistent,
  "Bird_Community_Flow_PersistentBirds.csv",
  row.names = FALSE
)



#PLOT ###########################


#Assign Season Names###################################################
  #$$$ An additional designation will need to be added here and as marked below if additional seasons are analyzed
  # If you are still only comparing 3 datasets, the pop-up boxes will do the trick

A1 <- rstudioapi::showPrompt(
  title = "Enter Season Name", message= "This will be displayed on the plot", 
  default = "Fall 2025 - Pre-Nesting"
)

A2 <- rstudioapi::showPrompt(
  title = "Enter Season Name",  message= "This will be displayed on the plot", 
  default = "Spring 2026 - Nesting Period"
)

A3 <- rstudioapi::showPrompt(
  title = "Enter Season Name",  message= "This will be displayed on the plot", 
  default = "Summer 2026 - Post-Fledging"
)



# Make Plot ##########################################
#$$$ Be sure to update the axis below 

cat("\nEnter names of axis in code script to pull data \n 
    use names(bird_flow) to find axis names \n\n")

community_flow_plot <- ggplot(
  
  bird_flow,
  
  aes(
    
    axis1 = `Fall 2025 Pre-Nesting_Communities`,
    
    axis2 = `Spring 2026 Nesting Period_Communities`,
    
    axis3 = `Summer 2026 Post-Fledging_Communities`,
    ##$$$##############################################################
    # Run: names(bird_flow) -> these are your axis data columns       #
    #   here you are entering data NOT names                          #
    ##################################################################
    
    y = 1
    
  )
  
) +
  
  
  # Each bird contributes one unit of flow
  
  geom_alluvium(
    aes(fill = `Fall 2025 Pre-Nesting_Communities`),
    alpha = 0.7
  ) +
  
  #make not detected gray
  scale_fill_brewer(
    palette = "Set3"
  ) +
  
  # Community boxes
  
  geom_stratum(
    width = 0.25,
    fill = "grey85",
    color = "black"
  ) +
  
  
  # Community labels
  
  geom_text(
    
    stat = "stratum",
    
    aes(
      label = after_stat(stratum)
    ),
    
    size = 3
    
  ) +
  
  
  labs(
    title = "Individual Social Group Membership Across Seasons",
    subtitle = "Ribbon width represents number of individuals following the same community trajectory",
    y = NULL,
    x = NULL
  ) +
  
  
  theme_minimal() +
  
  
  theme(
    
    axis.text.y = element_blank(),
    
    axis.ticks.y = element_blank(),
    
    axis.text.x = element_blank(),
    
    axis.ticks.x = element_blank()
    
  )+
  
  theme(
    legend.position = "none"
  )



# Display plot

community_flow_plot



#################### DONE ###########################

cat("\nBird community movement plot complete!\n")

cat("\nTimeline table saved as Bird_Community_Timeline.csv\n")

cat("\nPlot table saved as Bird_Community_Flow_Table.csv\n")

#######################################################################
# Persistent Birds Only Plot ##########################################
#$$$ Be sure to update the axis below 

cat("\nEnter names of axis in code script to pull data \n 
    use names(bird_flow) to find axis names \n\n")

stable_community_flow_plot <- ggplot(
  
  bird_flow_persistent,
  
  aes(
    
    axis1 = `Fall 2025 Pre-Nesting_Communities`,
    
    axis2 = `Spring 2026 Nesting Period_Communities`,
    
    axis3 = `Summer 2026 Post-Fledging_Communities`,
    ##$$$##############################################################
    # Run: names(bird_flow) -> these are your axis data columns       #
    #   here you are entering data NOT names                          #
    ##################################################################
    
    y = 1
    
  )
  
) +
  
  
  # Each bird contributes one unit of flow
  
  geom_alluvium(
    aes(fill = `Fall 2025 Pre-Nesting_Communities`),
    alpha = 0.7
  ) +
  

  # Community boxes
  
  geom_stratum(
    width = 0.25,
    fill = "grey85",
    color = "black"
  ) +
  
  
  # Community labels
  
  geom_text(
    
    stat = "stratum",
    
    aes(
      label = after_stat(stratum)
    ),
    
    size = 3
    
  ) +
  
  
  labs(
    title = "Individual Social Group Membership Across Seasons",
    subtitle = "Ribbon width represents number of individuals following the same community trajectory",
    caption = "In this figure, Birds are exluded when they are Not Detected in a given season",
    y = NULL,
    x = NULL
  ) +
  
  
  theme_minimal() +
  
  
  theme(
    
    axis.text.y = element_blank(),
    
    axis.ticks.y = element_blank(),
    
    axis.text.x = element_blank(),
    
    axis.ticks.x = element_blank()
    
  )+
  
  theme(
    legend.position = "none"
  )



# Display plot

stable_community_flow_plot





