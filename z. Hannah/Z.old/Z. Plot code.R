# Birds that appear very close together in time at the SAME feeder are 
# considered part of the same group. 
#
# We will assign events by saying:
#     "If two records are within 30 seconds at the same feeder, they are a group"
#

# first! load packages 
library(dplyr)
library(tidyr)
library(purrr)
library(igraph)

# then, call merged data set (includes all feeders & master banding info)
######CHANGE THIS IN FUTURE#########
dataset<-read_csv ("FullRFID_Data_With_MBS.csv")

# You may change time_window to anything (in seconds)
time_window <- 3   # seconds 

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
  mutate(edges = map(birds, function(x) {
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


# Birds that appear very close together in time at the SAME feeder are 
# considered part of the same group. 
#
# We will assign events by saying:
#     "If two records are within 30 seconds at the same feeder, they are a group"
#

# first! load packages 
library(dplyr)
library(tidyr)
library(purrr)
library(igraph)

# then, call merged data set (includes all feeders & master banding info)
######CHANGE THIS IN FUTURE#########
dataset<-read_csv ("FullRFID_Data_With_MBS.csv")

# You may change time_window to anything (in seconds)
time_window <- 3   # seconds 

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
  mutate(edges = map(birds, function(x) {
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


######## NOW PLOT #######

plot(g)

# tree layout 

plot(g,
     layout = layout_as_tree(g),)

###### Want to wee info on a bird? #######

get_bird_info <- function(BirdName) { # BirdName= 3 letter color code of the bird
  df_events %>%
    filter(ColorCombo == BirdName) %>%
    select(ColorCombo, Feeder, DateTime, Event_ID, Sex,DateLastCaptured,Status,Location,Age, ColorCrest )
}


# make a look-up to color-code & sort by atrabutes 

bird_attributes <- df_events %>%
  select(ColorCombo, Sex, Status, Location, Age, ColorCrest) %>%
  mutate(
    Sex = na_if(Sex, ""),           # turn "" into NA
    Status = na_if(Status, ""),
    Location = na_if(Location, ""),
    Age = na_if(Age, ""),
    ColorCrest = na_if(ColorCrest, "")
  ) %>%
  group_by(ColorCombo) %>%
  summarise(
    Sex        = first(na.omit(Sex)),
    Status     = first(na.omit(Status)),
    Location   = first(na.omit(Location)),
    Age        = first(na.omit(Age)),
    ColorCrest = first(na.omit(ColorCrest)),
    .groups = "drop"
  )

# create lookup vectors for graph vertex attributes
sex_lookup <- setNames(bird_attributes$Sex, bird_attributes$ColorCombo)
status_lookup <- setNames(bird_attributes$Status, bird_attributes$ColorCombo)
location_lookup <- setNames(bird_attributes$Location, bird_attributes$ColorCombo)
age_lookup <- setNames(bird_attributes$Age, bird_attributes$ColorCombo)
crest_lookup <- setNames(bird_attributes$ColorCrest, bird_attributes$ColorCombo)

# Attach to graph vertices
V(g)$sex      <- sex_lookup[V(g)$name]
V(g)$status   <- status_lookup[V(g)$name]
V(g)$location <- location_lookup[V(g)$name]
V(g)$age      <- age_lookup[V(g)$name]
V(g)$crest    <- crest_lookup[V(g)$name]

# Vertex colors based on sex

V(g)$color <- ifelse(
  V(g)$sex == "F", "pink",
  ifelse(V(g)$sex == "M", "lightblue", "grey")  # default for everything else
)
    V(g)$color[is.na(V(g)$color)] <- "grey"  # Make NAs grey


# check
data.frame(name = V(g)$name,
           sex = sex_lookup[V(g)$name])


plot(g,
     layout = layout_with_fr(g), #clasic layout
     vertex.color = V(g)$color,   # use the colors we assigned
     ,           #put nothing, keep labels
     vertex.size = 25,            # adjust node size
     edge.width = E(g)$weight/5,    # edge width proportional to weight
     edge.color = "black",
     main = paste("Bird Co-occurrence Network (time window =", time_window, "seconds)")
    )

# Add legend
legend("topleft",
       legend = c("Female", "Male", "Unknown"),
       pch = 21,
       pt.bg = c("pink", "lightblue", "grey"),
       pt.cex = 2)   # adjust legend point size


#### start highlight for large, high control PDF network
# print big! - will save to WD
pdf("bird_network (window= 3 sec) (10X10 size).pdf", width = 10, height = 10)

plot(g,
     layout = layout_with_fr, # layout style
     vertex.color = V(g)$color,   # use the colors we assigned
     ,           #put nothing, keep labels
     vertex.size = 5,            # adjust node size
     edge.width = E(g)$weight/5,    # edge width proportional to weight
     edge.color = "black",
     edge.curved = FALSE     # forces straight edges
    )

title(
  main = sprintf("Bird Co-occurrence Network (time window = %d seconds)", time_window),
  cex.main = 1,  # scales the title text
  font.main = 2,     # bold
  line = 0        # increases vertical distance from top
)

legend("topleft",
       legend = c("Female", "Male", "Unknown"),
       pch = 21,
       pt.bg = c("pink", "lightblue", "gray"),
       pt.cex = 2,         # circle size in legend
       cex = 1,          # text size in legend
       bty = "n"           # no box around legend
      )

#remove double edges (eg A:B = B:A based on this code segment)
g <- simplify(
  g,
  remove.multiple = TRUE,
  remove.loops = TRUE,
  edge.attr.comb = list(weight = "sum")
)

dev.off() #turn off print
#### stop highlight
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


######## NOW PLOT #######

plot(g)

# tree layout 

plot(g,
     layout = layout_as_tree(g),)

###### Want to wee info on a bird? #######

get_bird_info <- function(BirdName) { # BirdName= 3 letter color code of the bird
  df_events %>%
    filter(ColorCombo == BirdName) %>%
    select(ColorCombo, Feeder, DateTime, Event_ID, Sex,DateLastCaptured,Status,Location,Age, ColorCrest )
}


# make a look-up to color-code & sort by atrabutes 

bird_attributes <- df_events %>%
  select(ColorCombo, Sex, Status, Location, Age, ColorCrest) %>%
  mutate(
    Sex = na_if(Sex, ""),           # turn "" into NA
    Status = na_if(Status, ""),
    Location = na_if(Location, ""),
    Age = na_if(Age, ""),
    ColorCrest = na_if(ColorCrest, "")
  ) %>%
  group_by(ColorCombo) %>%
  summarise(
    Sex        = first(na.omit(Sex)),
    Status     = first(na.omit(Status)),
    Location   = first(na.omit(Location)),
    Age        = first(na.omit(Age)),
    ColorCrest = first(na.omit(ColorCrest)),
    .groups = "drop"
  )

# create lookup vectors for graph vertex attributes
sex_lookup <- setNames(bird_attributes$Sex, bird_attributes$ColorCombo)
status_lookup <- setNames(bird_attributes$Status, bird_attributes$ColorCombo)
location_lookup <- setNames(bird_attributes$Location, bird_attributes$ColorCombo)
age_lookup <- setNames(bird_attributes$Age, bird_attributes$ColorCombo)
crest_lookup <- setNames(bird_attributes$ColorCrest, bird_attributes$ColorCombo)

# Attach to graph vertices
V(g)$sex      <- sex_lookup[V(g)$name]
V(g)$status   <- status_lookup[V(g)$name]
V(g)$location <- location_lookup[V(g)$name]
V(g)$age      <- age_lookup[V(g)$name]
V(g)$crest    <- crest_lookup[V(g)$name]

# Vertex colors based on sex

V(g)$color <- ifelse(
  V(g)$sex == "F", "pink",
  ifelse(V(g)$sex == "M", "lightblue", "grey")  # default for everything else
)
    V(g)$color[is.na(V(g)$color)] <- "grey"  # Make NAs grey


# check
data.frame(name = V(g)$name,
           sex = sex_lookup[V(g)$name])


plot(g,
     layout = layout_with_fr(g), #clasic layout
     vertex.color = V(g)$color,   # use the colors we assigned
     ,           #put nothing, keep labels
     vertex.size = 25,            # adjust node size
     edge.width = E(g)$weight/5,    # edge width proportional to weight
     edge.color = "black",
     main = paste("Bird Co-occurrence Network (time window =", time_window, "seconds)")
    )

# Add legend
legend("topleft",
       legend = c("Female", "Male", "Unknown"),
       pch = 21,
       pt.bg = c("pink", "lightblue", "grey"),
       pt.cex = 2)   # adjust legend point size


#### start highlight for large, high control PDF network
# print big! - will save to WD
pdf("bird_network (window= 3 sec) (10X10 size).pdf", width = 10, height = 10)

plot(g,
     layout = layout_with_fr, # layout style
     vertex.color = V(g)$color,   # use the colors we assigned
     ,           #put nothing, keep labels
     vertex.size = 5,            # adjust node size
     edge.width = E(g)$weight/5,    # edge width proportional to weight
     edge.color = "black",
     edge.curved = FALSE     # forces straight edges
    )

title(
  main = sprintf("Bird Co-occurrence Network (time window = %d seconds)", time_window),
  cex.main = 1,  # scales the title text
  font.main = 2,     # bold
  line = 0        # increases vertical distance from top
)

legend("topleft",
       legend = c("Female", "Male", "Unknown"),
       pch = 21,
       pt.bg = c("pink", "lightblue", "gray"),
       pt.cex = 2,         # circle size in legend
       cex = 1,          # text size in legend
       bty = "n"           # no box around legend
      )

#remove double edges (eg A:B = B:A based on this code segment)
g <- simplify(
  g,
  remove.multiple = TRUE,
  remove.loops = TRUE,
  edge.attr.comb = list(weight = "sum")
)

dev.off() #turn off print
#### stop highlight