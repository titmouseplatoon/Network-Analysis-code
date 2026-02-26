
#SETUP
rm(list = ls())
library(dplyr)
library(tidyr)
library(lubridate)
library(asnipe)
library(igraph)
setwd("/Users/hannahbaetge/Library/CloudStorage/OneDrive-TrinityUniversity/NETwork ANalYsis/RFID Data copy")
#I need to reformat the data to match the columns from the worked example


# ok, turn this into a function so I can just plug in feeders!


process_feeder <- function(Data) {
  
  
# Step 1: Read the CSV file into R
FeederA.1 <- read.csv("000ADATA.TXT", header = TRUE)
# Step 2: Check the column names
colnames(FeederA.1)
#############

# Convert the combined DateTime column to a proper datetime format
FeederA.1$DateTime <- as.POSIXct(FeederA.1[[3]], format = "%m/%d/%Y %H:%M:%S")
#FeederA.1$DateTime <- as.POSIXct(FeederA.1$DateTime, format = "%Y-%m-%d %H:%M:%S")
####### lots of cutting from the original as SD card had already formatted mostly (M/D/Y HH:MM:SS -> M-D-Y HH:MM:SS)
#  Write the updated data to a new CSV file
write.csv(FeederA.1, "FeederA.1", row.names = FALSE)
# Check the first few rows of the data
head(FeederA.1)
# Check the column types to confirm they are in the expected format
str(FeederA.1)
### ok, but this data sheet has date/time twice and is missing the feeder letter
#Remove the 'Date_column' and 'Time_column'
#FeederA.1 <- FeederA.1 %>% select(-Date_column, -Time_column)
# Step 2: Add the 'Feeder' column filled with "A"
FeederA.1$Feeder <- "A"

# clean down to just the selected data 
FeederA_clean <- select(FeederA.1, tag = X3B001903BE, feeder = Feeder, DateTime)
# View the first few rows
head(FeederA_clean)


####### ok now  the data for feeder 1 is called "FeederA_clean" 



#### this is just a test of the basics before we get more feeders involved
#in future the other feeders A-S will be processed the same way then combined before movieng on
# but for now, Lets play with plots!




# Try agin with just uplowding master banding to the enviroment
# fix collum names
band_dat_trim <- `_MASTER.Banding.data.xlsx...All.Birds...Bracken.(1)` %>%
  slice(-1) %>%   # removes the metadata header row
  select(
    RFID = V20,
    Metal_ID = V6,
    Color_Combo = V7,
    Sex = V8,
    Age = V12
  )
head(band_dat_trim) # check

    
#Join FeederA data with the banding IDs

    id_confirm <- (
      FeederA_clean %>%
        left_join(band_dat_trim, by = c("tag" = "RFID"))
    )
  head(id_confirm)
    
    
# Check
  View(id_confirm)
  
  
  
  
  
###Plot!!!!###
  ###############################################################################
  # LOAD PACKAGES
  ###############################################################################
  
  # install.packages("tidyverse")
  # install.packages("lubridate")
  # install.packages("igraph")
  
  library(tidyverse)
  library(lubridate)
  library(igraph)
  
  ###############################################################################
  # STEP 1 — READ IN YOUR DATA
  ###############################################################################
  
  df <- id_confirm   # use the existing object
  
  ###############################################################################
  # STEP 2 — CLEAN UP DATE AND TIME
  ###############################################################################
  
  # Convert DateTime column to proper POSIXct format
  # (Your format looks like: 2025-09-27 09:02:48)
  df$DateTime <- ymd_hms(df$DateTime)
  
  ###############################################################################
  # STEP 3 - SELECT COLUMNS WE ACTUALLY NEED
  # We only need feeder, datetime, and Color_Combo (bird ID)
  ###############################################################################
  
  df_clean <- df %>% 
    select(feeder, DateTime, Color_Combo, Sex, Age) %>%  # include sex
    filter(!is.na(Color_Combo))                     # Remove rows with no bird ID
  
  ###############################################################################
  # STEP 4 - DEFINE "EVENTS" OR "GROUPS"
  #
  # Birds that appear very close together in time at the SAME feeder are 
  # considered part of the same group. 
  #
  # We will assign events by saying:
  #     "If two records are within 2 minutes at the same feeder, they are a group"
  #
  ###############################################################################
  
  # You may change time_window to anything (in seconds)
  time_window <- 120   # 120 seconds = 2 minutes
  
  df_events <- df_clean %>%
    arrange(feeder, DateTime) %>%  # Important: sort by feeder & time
    group_by(feeder) %>%
    mutate(
      # Compute "time difference" from the previous bird detection
      time_diff = as.numeric(DateTime - lag(DateTime), units="secs"),
      
      # Start a new event if the time gap is bigger than the window
      new_event = ifelse(is.na(time_diff) | time_diff > time_window, 1, 0),
      
      # Event IDs: cumulative sum of group boundaries
      Event_ID = cumsum(new_event)
    ) %>%
    ungroup()
  
  ###############################################################################
  # STEP 5 - MAKE PAIRS OF BIRDS THAT APPEARED IN THE SAME EVENT
  ###############################################################################
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(igraph)
  
  # STEP 5: make pairs
  pair_edges <- df_events %>%
    group_by(Event_ID) %>%
    summarise(birds = list(unique(Color_Combo)), .groups = "drop") %>%
    filter(lengths(birds) > 1) %>%
    # create all bird-to-bird combinations as data frame with proper column names
    mutate(edges = map(birds, function(x) {
      combos <- t(combn(x, 2))
      df <- data.frame(from = combos[,1], to = combos[,2], stringsAsFactors = FALSE)
      return(df)
    })) %>%
    select(edges) %>%
    unnest(edges)
  
  # STEP 6: count co-occurrences
  edge_counts <- pair_edges %>%
    group_by(from, to) %>%
    summarise(weight = n(), .groups = "drop")
  
  # STEP 7: create adjacency matrix
  g <- graph_from_data_frame(edge_counts, directed = FALSE)
  adj_matrix <- as.matrix(get.adjacency(g, attr = "weight"))
  
  # STEP 8: print results
  cat("Adjacency Matrix:\n")
  print(adj_matrix)
  
  cat("\nGraph summary:\n")
  print(g)
  
}


# OUTSIDE OF FUNCTION 

### NEW ################################################

process_feeder <- function(feeder_df, feeder_letter) {
  
  # Step 1: Add feeder column
  feeder_df$feeder <- feeder_letter
  
  # Step 2: Select columns we need
  df_clean <- feeder_df %>%
    select(tag = 1, feeder, DateTime=3) %>%   # use column 1 as tag
    filter(!is.na(tag))
  
  # Step 3: Join with banding info
  band_dat_trim <- `_MASTER.Banding.data.xlsx...All.Birds...Bracken.(1)` %>%
    slice(-1) %>%
    select(RFID = V20, Color_Combo = V7, Sex = V8, Age = V12)
  
  id_confirm <- df_clean %>%
    left_join(band_dat_trim, by = c("tag" = "RFID"))
  
  # Step 4: Convert DateTime
  id_confirm$DateTime <- ymd_hms(id_confirm$DateTime)
  
  # Step 5: Define events
  time_window <- 120
  df_events <- id_confirm %>%
    arrange(feeder, DateTime) %>%
    group_by(feeder) %>%
    mutate(
      time_diff = as.numeric(DateTime - lag(DateTime), units="secs"),
      new_event = ifelse(is.na(time_diff) | time_diff > time_window, 1, 0),
      Event_ID = cumsum(new_event)
    ) %>%
    ungroup()
  
  ###############################################################################
  # STEP 6 — MAKE PAIRS OF BIRDS IN THE SAME EVENT
  ###############################################################################
  
  pair_edges <- df_events %>%
    group_by(Event_ID) %>%
    summarise(birds = list(unique(Color_Combo)), .groups = "drop") %>%
    filter(lengths(birds) > 1) %>%
    mutate(
      edges = purrr::map(birds, function(bird_vec) {
        if(length(bird_vec) < 2) return(NULL)
        combos <- t(combn(bird_vec, 2))
        # Explicitly name columns
        df <- data.frame(from = combos[,1],
                         to   = combos[,2],
                         stringsAsFactors = FALSE)
        return(df)
      })
    ) %>%
    select(edges) %>%
    tidyr::unnest(edges)  # NOW 'from' and 'to' exist as proper columns
  
  ###############################################################################
  # STEP 7 — COUNT CO-OCCURRENCES TO WEIGHT EDGES
  ###############################################################################
  
  edge_counts <- pair_edges %>%
    group_by(from, to) %>%
    summarise(weight = n(), .groups = "drop")
  ###############################################################################
  # STEP 8 — CREATE GRAPH AND ADJACENCY MATRIX
  ###############################################################################
  
  g <- igraph::graph_from_data_frame(edge_counts, directed = FALSE)
  
  adj_matrix <- as.matrix(igraph::get.adjacency(g, attr = "weight"))
  
  # Print results
  cat("Adjacency Matrix:\n")
  print(adj_matrix)
  
  cat("\nGraph Summary:\n")
  print(g)
  
  return(list(df_events = df_events, graph = g))
}


######################################################




# run function! process_feeder(data name, "letter")
process_feeder(`000FDATA`, "F")




  # now plot!
 
  plot(g)

  # tree layout 
  
  plot(g,
       layout = layout_as_tree(g),)
  
# and make look nice

  df_clean$Sex[df_clean$Sex == ""] <- NA
  
  # Create lookup
  sex_lookup <- setNames(df_clean$Sex, df_clean$Color_Combo)
  
  sex_lookup <- setNames(df_clean$Sex, df_clean$Color_Combo) # assign sex
  
  
  V(g)$sex <- sex_lookup[V(g)$name]
  V(g)$color <- ifelse(V(g)$sex == "F", "pink",
                       ifelse(V(g)$sex == "M", "lightblue", "grey")) #set colors
# check
  data.frame(name = V(g)$name,
             sex = sex_lookup[V(g)$name])
  
  
  plot(g,
       layout = layout_as_tree(g),
       vertex.color = V(g)$color,   # use the colors we assigned
        ,           #put nothing, keep labels
       vertex.size = 25,            # adjust node size
       edge.width = E(g)$weight/5,    # edge width proportional to weight
       edge.color = "black")
  
  # Add legend
  legend("topleft",
         legend = c("Female", "Male", "Unknown"),
         pch = 21,
         pt.bg = c("pink", "lightblue", "grey"),
         pt.cex = 2)   # adjust legend point size
  
  
  
  
  
  
  
  
  
  
  
  
  ######## try again #########
  
  
  
  process_feeder <- function(feeder_df, feeder_letter) {
    library(dplyr)
    library(tidyr)
    library(purrr)
    library(lubridate)
    library(igraph)
    library(stringr)
    
    # STEP 1 — Add feeder column
    feeder_df$feeder <- feeder_letter
    
    # STEP 2 — Select needed columns
    # Use column 1 as 'tag' and column 3 as 'DateTime'
    df_clean <- feeder_df %>%
      select(tag = 1, feeder, DateTime = 3) %>%
      filter(!is.na(tag))
    
    # STEP 3 — Join with banding info
    band_dat_trim <- `_MASTER.Banding.data.xlsx...All.Birds...Bracken.(1)` %>%
      slice(-1) %>%
      select(RFID = V20, Color_Combo = V7, Sex = V8, Age = V12)
    
    id_confirm <- df_clean %>%
      left_join(band_dat_trim, by = c("tag" = "RFID")) %>%
      filter(!is.na(Color_Combo) & Color_Combo != "")  # remove missing bird IDs
    
    # STEP 4 — Convert DateTime (from M/D/Y H:M:S format)
    id_confirm$DateTime <- mdy_hms(str_trim(id_confirm$DateTime))
    
    # STEP 5 — Define events (birds within 'time_window' seconds at same feeder)
    time_window <- 120  # 2 minutes
    df_events <- id_confirm %>%
      arrange(feeder, DateTime) %>%
      group_by(feeder) %>%
      mutate(
        time_diff = as.numeric(DateTime - lag(DateTime), units = "secs"),
        new_event = ifelse(is.na(time_diff) | time_diff > time_window, 1, 0),
        Event_ID = cumsum(new_event)
      ) %>%
      ungroup()
    
    # STEP 6 — Make bird pairs for each event
    pair_edges <- df_events %>%
      group_by(Event_ID) %>%
      summarise(birds = list(unique(Color_Combo)), .groups = "drop") %>%
      filter(lengths(birds) > 1) %>%
      mutate(
        edges = purrr::map(birds, function(bird_vec) {
          combos <- t(combn(bird_vec, 2))
          data.frame(from = combos[,1], to = combos[,2], stringsAsFactors = FALSE)
        })
      ) %>%
      select(edges) %>%
      tidyr::unnest(edges)
    
    # STEP 7 — Count co-occurrences to weight edges
    edge_counts <- pair_edges %>%
      group_by(from, to) %>%
      summarise(weight = n(), .groups = "drop")
    
    # STEP 8 — Create graph and adjacency matrix
    g <- graph_from_data_frame(edge_counts, directed = FALSE)
    adj_matrix <- as.matrix(get.adjacency(g, attr = "weight"))
    
    # STEP 9 — Print results
    cat("Adjacency Matrix:\n")
    print(adj_matrix)
    
    cat("\nGraph Summary:\n")
    print(g)
    
    # Return events and graph
    return(list(df_events = df_events, graph = g))
  }
  
  
  
  
  # Load your feeder F data first
  FeederF_data <- read.csv("000FDATA.TXT", header = TRUE)
  
  # Run the function
  result <- process_feeder(FeederF_data, "F")
  
  # Access outputs:
  df_events <- result$df_events
  graph <- result$graph
  
  
  
  
  
  # now plot!
  # Extract the graph
  g <- result$graph
  
  plot(g)
  
  # tree layout 
  
  plot(g,
       layout = layout_as_tree(g),)
  
  # and make look nice
  
  df_clean$Sex[df_clean$Sex == ""] <- NA
  
  # Create lookup
  sex_lookup <- setNames(df_clean$Sex, df_clean$Color_Combo)
  
  sex_lookup <- setNames(df_clean$Sex, df_clean$Color_Combo) # assign sex
  
  
  V(g)$sex <- sex_lookup[V(g)$name]
  V(g)$color <- ifelse(V(g)$sex == "F", "pink",
                       ifelse(V(g)$sex == "M", "lightblue", "grey")) #set colors
  # check
  data.frame(name = V(g)$name,
             sex = sex_lookup[V(g)$name])
  
  
  plot(g,
       layout = layout_as_tree(g),
       vertex.color = V(g)$color,   # use the colors we assigned
       ,           #put nothing, keep labels
       vertex.size = 25,            # adjust node size
       edge.width = E(g)$weight/5,    # edge width proportional to weight
       edge.color = "black")
  
  # Add legend
  legend("topleft",
         legend = c("Female", "Male", "Unknown"),
         pch = 21,
         pt.bg = c("pink", "lightblue", "grey"),
         pt.cex = 2)   # adjust legend point size
  
  
  ### group! 
  library(igraph)
  library(asnipe)      # for get_network()
  library(RColorBrewer)
  
  
  # Suppose process_feeder returned:
  res <- process_feeder(`000FDATA`, "F")
  df_events <- res$df_events  # your events table
  g <- res$graph             # your igraph object
  
  # STEP 1: Make a group-by-individual matrix
  # Rows = Event_ID, Columns = unique birds
  birds <- sort(unique(df_events$Color_Combo))
  events <- sort(unique(df_events$Event_ID))
  gbi <- matrix(0, nrow = length(events), ncol = length(birds),
                dimnames = list(events, birds))
  
  for(i in seq_along(events)) {
    e <- events[i]
    birds_in_event <- df_events$Color_Combo[df_events$Event_ID == e]
    gbi[i, birds_in_event] <- 1
  }
  
  # STEP 2: Create adjacency matrix using SRI
  mat <- get_network(gbi, association_index = "SRI")
  
  # STEP 3: Convert to igraph
  g_sparrow <- graph_from_adjacency_matrix(mat, mode = "undirected", weighted = TRUE)
  
  # STEP 4: Community detection
  com <- cluster_louvain(g_sparrow)
  
  # STEP 5: Color assignment
  n_com <- length(unique(membership(com)))
  colors <- if(n_com <= 8) brewer.pal(max(3, n_com), "Accent") else rainbow(n_com)
  V(g_sparrow)$color <- colors[membership(com)]
  
  # STEP 6: Plot
  set.seed(2)
  plot(g_sparrow)
  
  
  
 