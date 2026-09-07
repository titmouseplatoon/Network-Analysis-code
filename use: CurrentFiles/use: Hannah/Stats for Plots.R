#This is not a Stand-alone code. 
# these statistical analysis codes to analyze g
  #g is the graphical data set made BUT NOT SAVED by the plot code (V1 or V2 or Vn)
# these stats are to be run at the end of making a plot to analyze it 


# But Hannah! Why are they not part of the plot code already!?!?
  # short answer:
    # they are in plot code V1
    # in plot code V2, it is best run all at once, and it was annoying to run the stats each time -> so I backed them up here
    #you don't actually need the plot to get these stats, just "g" which is the data set that gets plotted.
        #Because I am Kind ;) I have copied just that part here under "Make "g""
  #Long answer OR QUESTONS:
    # Email me! I don't bite! hlbaetge@gmail.com (Hannah Baetge)

######### Make "g" #############################
# first! load packages 
library(dplyr)
library(tidyr)
library(tidyverse)



# then, call merged data set (includes all feeders & master banding info)
dataset<- dataset <- read_csv(file.choose()) # pick data file you wish to graph

# You may change time_window to anything (in seconds)
# this is the maximum length of time between birds being at a feeder 
#and those birds still being considered interacted or linked
#time_window <- 12   # seconds 
# This forces RStudio to pop open a small text box on your screen
time_window <- rstudioapi::showPrompt(
  title = "Enter time_window", 
  message = "Enter time as digits (ex: 12 for 12 seconds)", 
  default = "12"
)

# Convert the pop-up text into a usable number
time_window <- as.numeric(time_window)

# describe what data set this is 
# this is used in title printed on plot 
plot_title <- rstudioapi::showPrompt(
  title = "Plot Title",
  message = "Enter the dataset for this plot (used in title printed on plot):",
  default = "Platoon Bird Co-occurrence Network"
)

# If Cancel is pressed or left blank
if (is.null(plot_title) || plot_title == "") {
  plot_title <- "Platoon Bird Co-occurrence Network"
}

df_events <- dataset%>%
  arrange(Feeder, DateTime) %>%  # Important: sort by feeder & time
  group_by(Feeder) %>%
  mutate(
    # Compute "time difference" from the previous bird detection
    time_diff = as.numeric(DateTime - lag(DateTime), units="secs"),
    
    # Start a new event if the time gap is bigger than the window
    new_event = ifelse(is.na(time_diff) | time_diff > time_window, 1, 0),
    
    # Event IDs: cumulative sum of group boundaries
    Event_ID = cumsum(new_event)
  ) %>%
  ungroup()

head(df_events) #check to make sure things look right



##### NEXT step: make pairs
pair_edges <- df_events %>%
  group_by(Feeder, Event_ID) %>%
  summarise(birds = list(unique(ColorCombo)), .groups = "drop") %>%
  filter(lengths(birds) > 1) %>%
  
  # create all bird-to-bird combinations as data frame with proper column names
  mutate(edges = purrr::map(birds, function(x) {
    combos <- t(combn(x, 2))
    df <- data.frame(from = combos[,1], to = combos[,2], stringsAsFactors = FALSE)
    return(df)
  })) %>%
  select(edges) %>%
  unnest(edges)

head(pair_edges) #make sure this looks good

#### count co-occurrences
edge_counts <- pair_edges %>%
  group_by(from, to) %>%
  summarise(weight = n(), .groups = "drop")

edge_counts <- edge_counts %>% 
  filter(!is.na(from), !is.na(to)) #remove birds with NO edges (prevents "NA" node later)

edge_counts <- edge_counts %>%   
  filter(!weight == 1) #remove bird combos with only 1 edge ****** CHANGE THIS if you don't  want to cut off
#                                             interactions that only happen once

head(edge_counts) #make sure this looks good

# create adjacency matrix
g <- graph_from_data_frame(edge_counts, directed = FALSE)
adj_matrix <- as.matrix(as_adjacency_matrix(g, attr = "weight"))


summary(g) # should say name (v/c), weight(e/n) - this means Undirected, named, weighted network- this is good
any(is.na(adj_matrix)) # confirms No "NA" nodes (want "FALSE")

#print results - make sure this looks good#
cat("Adjacency Matrix:\n")
print(adj_matrix)
#&
cat("\nGraph summary:\n")
print(g)



###### Want to see info on a bird? #############

get_bird_info <- function(BirdName) { # BirdName= "3 letter color code of the bird" IN QUOTES!
  df_events %>%
    filter(ColorCombo == BirdName) %>%
    select(ColorCombo, Feeder, DateTime, Event_ID, Sex,DateLastCaptured,Status,Location,Age, ColorCrest )
}





####  stats   #################################
cat("Centrality Statistics for ", plot_title, "\n")
cat(" \n Betweenness for", plot_title, "\n
    Number of geodesic paths that go through a given node \n")
#	Number of geodesic paths that go through a given node
be=betweenness(g, normalized=T)
#plot(g,  vertex.label="", vertex.color="gold", edge.color="slateblue", vertex.size=be*50, edge.width=E(g)$weight*5)
be
sort(be, decreasing = TRUE, na.last = NA)
hist(be, breaks=10)

cat(" \n Degree for", plot_title, "\n
    Number of edges connected to node \n")
# 	Number of edges connected to node
degree(g)
sort(degree(g), decreasing = TRUE, na.last = NA)
hist(degree(g), breaks=10, col="gray")

cat(" \n graph strenght for", plot_title, "\n 
    Sum of edge weights connected to a node (aka weighted degree) \n")
#	Sum of edge weights connected to a node (aka weighted degree)
graph.strength(g)
sort(graph.strength(g), decreasing = TRUE)
hist(graph.strength(g), breaks=10)

