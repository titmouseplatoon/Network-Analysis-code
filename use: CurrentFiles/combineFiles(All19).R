
######## NOW STICH ALL DATASHEETS TOGETHER  #########

# set working directory
setwd("/Users/hannahbaetge/Library/CloudStorage/OneDrive-TrinityUniversity/NETwork ANalYsis")
  list.files("Feeder Files") # test WD was setup right
  
# bring in the master banding sheet as it likes to disappere 
library(readr)
  master_banding <- read_csv("/Users/hannahbaetge/Library/CloudStorage/OneDrive-TrinityUniversity/NETwork ANalYsis/RFID Data copy/_MASTER Banding data.xlsx - All Birds - Bracken (1).csv")
  library(dplyr)
  
  master_banding <- master_banding %>%
    # rename columns to simple names
    rename(
      RFID        = `RFID #`,
      Color_Combo = `Color Combo`,
      Sex         = Sex,
      Age         = `Age (ASY,AHY,HY(Juv),Nestling)`
    ) %>%
    # (optional) remove “#” or other weird chars to make clean variable names:
    # janitor::clean_names()  # if you like snake_case & simpler names
    select(RFID, Color_Combo, Sex, Age, everything())
  
  
  # use only the latest entries for each RFID
  library(dplyr)
  
  master_banding_unique <- master_banding %>%
    # Optionally convert date column to Date or POSIX if present, e.g.:
    # mutate( banding_date = as.Date(Date, format = "%m/%d/%Y") ) %>%
    group_by(RFID) %>%
    slice_tail(n = 1) %>%  # picks the *last* row per RFID group
    ungroup()
  
  
# start with the original function 
  
  

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
  band_dat_trim <- master_banding %>%
    slice(-1) %>%   # if you still need to remove metadata row
    slice_tail(n = 1) %>%  # take the last row per RFID 
    ungroup() %>%
    select(RFID, Color_Combo, Sex, Age)

  
  id_confirm <- df_clean %>%
    left_join(band_dat_trim, by = c("tag" = "RFID"),
              relationship = "many-to-one") %>% # optional: enforce one-to-one from feeder → banding
    #filter(!is.na(Color_Combo) & Color_Combo != "")  # remove missing bird IDs- should not need to do
  
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
  
  # after unnested edges:
  if (!("from" %in% names(pair_edges) && "to" %in% names(pair_edges))) {
    message("Feeder ", feeder_letter, ": no valid bird-pairs (pair_edges has no from/to). Skipping edge counting.")
    # return an empty result, or at least df_events
    return(list(df_events = df_events,
                graph = igraph::make_empty_graph()))
  }
  
  # if there *are* from/to columns, proceed:
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



# now run all 19 files through the cleaning function 

library(readr)
library(purrr)
library(dplyr)

# ------------------------------------------------------------
# 1. Point to your folder containing the 000ADATA.TXT files
# ------------------------------------------------------------
path <- "Feeder Files"   # <-- change if needed

# ------------------------------------------------------------
# 2. Get all the TXT files in that folder
# ------------------------------------------------------------
files <- list.files(path, pattern = "\\.TXT$", full.names = TRUE)

# ------------------------------------------------------------
# 3. Extract feeder letter (4th character of filename)
# ------------------------------------------------------------
feeder_letters <- substr(basename(files), 4, 4)

# ------------------------------------------------------------
# 4. Read each file + run your process_feeder() function
# ------------------------------------------------------------

safe_proc <- purrr::possibly(process_feeder, otherwise = NULL) # safety wrap so it keeps going

results_list <- map2(
  files,
  feeder_letters,
  ~ {
    cat("Processing feeder:", .y, " — file:", basename(.x), "\n")   # <-- debug print
    df <- read_csv(.x, col_names = FALSE)
    process_feeder(df, .y)
  }
)


# now stich all 19 files together 
all_events <- bind_rows(map(results_list, "df_events"))


write.csv(all_events, "all_events.csv", row.names = FALSE)

############ Plots 


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

