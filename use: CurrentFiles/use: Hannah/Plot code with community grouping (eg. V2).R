# To Run This code you will need the output file from "feeder+MBS_merging_4MAC.R" 
#It should be a large CSV file (name it with the general time frame the data is from)
                                # ex. Spring 2025 - this prevents confusion later


########### The easiest way to run this code is to:
  #Select all (Mac: Command +A)
  #Press run (Top right of script panel)
#The code has built-in prompts that will ask you to select and name files
# Each run will make two file outputs: the plot PDF and the Community assignment csv
  #the files will be saved to your working directory
    # If you need to find your Working Directory type: getwd() into the console
    # Need to change your Working DIrectory?
        # Click Session in the top menu bar.
        # Hover over Set Working Directory.
        # Click "Choose Directory"....Select your folder and click Open. OR choose "To Source File Location" to set it to the folder where your active script is saved.

#Anymore Questions? Email me! I'm like a grandparent, I LOVE emails!!!
  #(Hannah Baetge) hlbaetge@gmail.com

#####Now on to the Biology!!!!######
#Birds that appear very close together in time at the SAME feeder are considered part of the same group. 

# We will assign events by saying:
#     "If two records are within 12 seconds at the same feeder, they are a group"

# Why 12 seconds? https://docs.google.com/document/d/1PCgzIrbiQs-jgxxuqGXfAgdFJHJBLPKTJ14V4SqF38Q/edit?usp=sharing
  #saved in the RFID folder of the drive > Data >Feeder Data > 2025 Data

# first! load packages 
library(dplyr)
library(tidyr)
library(purrr)
library(igraph)
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


######## NOW PLOT ####################################

# basic test
plot(g)



# make a look-up to color-code & sort by attributes 

bird_attributes <- df_events %>%
  select(ColorCombo, Sex, Status, LocationCaptured,
         Age, CrestColor, DateCaptured) %>%
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

# compare old and dew ages and see whare changes were 
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

# create lookup vectors for graph vertex attributes
sex_lookup <- setNames(bird_attributes$Sex, bird_attributes$ColorCombo)
status_lookup <- setNames(bird_attributes$Status, bird_attributes$ColorCombo)
location_lookup <- setNames(bird_attributes$LocationCaptured, bird_attributes$ColorCombo)
age_lookup <- setNames(bird_attributes$Age, bird_attributes$ColorCombo)
crest_lookup <- setNames(bird_attributes$CrestColor, bird_attributes$ColorCombo)

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


# vertex border color based on age 
V(g)$age <- trimws(V(g)$age) # remove hidden spaces from age values

V(g)$frame.color <- ifelse(
  V(g)$age %in% c("Nestling", "HY"), "orange",
  ifelse(V(g)$age %in% c("AHY", "SY", "ASY"), "red", "black")
)

# check sex
data.frame(name = V(g)$name,
           sex = sex_lookup[V(g)$name])
#check age
data.frame(name = V(g)$name,
           age = age_lookup[V(g)$name])

#NEW###########################################################
# Detect social communities

communities <- cluster_louvain(g)

community_output <- data.frame(
  ColorCombo = V(g)$name,
  Community = membership(communities),
  Sex = V(g)$sex,
  Age = V(g)$age,
  Status = V(g)$status,
  Location = V(g)$location
)

write.csv(
  community_output,
  paste0(plot_title, "_Communities.csv"),
  row.names = FALSE
)

# make color palette for social communities 
community_colors <- hcl.colors(
  length(communities),
  palette = "Dark 3"
)

########### Make plot w/in R to check components - will be crowded############
plot(g,
     layout = layout_with_fr(g), #clasic layout
     vertex.color = V(g)$color,   # use the colors we assigned
     ,           #put nothing, keep labels
     vertex.size = 25,            # adjust node size
     edge.width = E(g)$weight/5,    # edge width proportional to weight
     edge.color = "black",
     # Add translucent community clouds
     mark.groups = communities,
     mark.col = adjustcolor(community_colors, alpha.f = 0.15),
     mark.border = NA,
     mark.lwd = 0,
     main = paste(plot_title, "(time window =", time_window, "seconds)")
)

# Add legends
legend("topleft",
       legend = c("Female", "Male", "Unknown"),
       pch = 21,
       pt.bg = c("pink", "lightblue", "grey"),
       pt.cex = 2)   # adjust legend point size

legend("topright",
       legend = c("Juvenile", "Adult"),
       pch = 21,
       pt.bg = "white",
       pt.cex = 2,
       pt.lwd = 2,
       col = c("orange", "red"),
       bty = "n")




#### start highlight for large, high control PDF network ############################

### BE SURE TO CHANGE PDF NAME !!!!!!!!!
### start highlight for large, high control PDF network ### 

# Ask for a short description of this version
plot_description <- rstudioapi::showPrompt(
  title = "Plot Description",
  message = "PDF Title:",
  default = "NewPlot"
)

# If Cancel is pressed or left blank
if (is.null(plot_description) || plot_description == "") {
  plot_description <- "NoDescription"
}

# Replace spaces with underscores
plot_description <- gsub(" ", "_", plot_description)

# Create a timestamp
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# Create the filename
pdf_name <- sprintf(
  "%s_%s_%dsec.pdf",
  timestamp,
  plot_description,
  time_window
)

cat("Saving PDF as:", pdf_name, "\n") 


# print big! - saves your plot ###########################
pdf(pdf_name, width = 10, height = 10)


# Create or load a stable network layout that updates each season

layout_file <- "BirdLayout.csv"


if (!file.exists(layout_file)) {
  
  cat("No saved layout found. Creating first layout...\n")
  
  set.seed(1)
  
  layout_spread <- layout_with_fr(
    g,
    weights = E(g)$weight,
    niter = 8000,
    area = vcount(g)^6
  )
  
} else {
  
  cat("Loading previous layout and allowing network to relax...\n")
  
  old_layout <- read.csv(layout_file)
  
  # create starting coordinates for current graph
  start_layout <- matrix(
    NA,
    nrow = vcount(g),
    ncol = 2
  )
  
  rownames(start_layout) <- V(g)$name
  
  
  # put existing birds where they were last season
  common_birds <- intersect(
    V(g)$name,
    old_layout$Bird
  )
  
  start_layout[common_birds,1] <- 
    old_layout$x[
      match(common_birds, old_layout$Bird)
    ]
  
  start_layout[common_birds,2] <- 
    old_layout$y[
      match(common_birds, old_layout$Bird)
    ]
  
  
  # new birds get random starting positions
  new_birds <- setdiff(
    V(g)$name,
    old_layout$Bird
  )
  
  if(length(new_birds) > 0){
    
    cat("New birds:", new_birds, "\n")
    
    start_layout[new_birds,1] <- runif(
      length(new_birds),
      min(old_layout$x),
      max(old_layout$x)
    )
    
    start_layout[new_birds,2] <- runif(
      length(new_birds),
      min(old_layout$y),
      max(old_layout$y)
    )
  }
  
  
  set.seed(1)
  
  layout_spread <- layout_with_fr(
    g,
    weights = E(g)$weight / 1000, #strength of edge weights- divide by LARGER number to decrease pull of strong edges
    niter = 20000,    # number of iterations -  larger number lets the network settle/optimize
    area = vcount(g)^7 #increases the plot area- increases the repulsive force and usually gives a cleaner layout without changing the overall structure.
  )
  
  #stretch everything out, use as backup if things get very crouded 
  #layout_spread <- layout_spread * 1.3
  
}


# save updated layout

layout_df <- data.frame(
  Bird = V(g)$name,
  x = layout_spread[,1],
  y = layout_spread[,2]
)


write.csv(
  layout_df,
  layout_file,
  row.names = FALSE
)

# Stop if duplicates are found
old_layout <- layout_df
  stopifnot( nrow(old_layout) == length(unique(old_layout$Bird)))

plot(g,
     layout = layout_spread,    # layout style
     vertex.color = V(g)$color, # use the colors we assigned- sex
     vertex.label.cex = 0.6,    # make label a little bigger 
     vertex.label.color = "black" ,
     vertex.size = 8,           # adjust node size
     vertex.frame.color = V(g)$frame.color,  # age border color
     vertex.frame.width = 2,                 # border thickness
     edge.width = E(g)$weight/5,             # edge width proportional to weight
     edge.color = "black", #edges will be black                
     edge.curved = FALSE,   # forces straight edges
     mark.groups = communities,
     mark.col = adjustcolor(community_colors, alpha.f = 0.25),
     mark.border = NA,
     mark.lwd = 0
)


title(
  main = sprintf(
    "%s (time window = %d seconds)",
    plot_title,
    time_window
  ),
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

legend("topright",
       legend = c("Juvenile", "Adult"),
       pch = 21,
       pt.bg = "white",
       pt.cex = 2,
       pt.lwd = 2,
       col = c("orange", "red"),
       bty = "n")

# Community membership
membership_vec <- membership(communities)

# Community centers in the ORIGINAL layout coordinates
community_centers <- t(sapply(sort(unique(membership_vec)), function(comm){
  
  verts <- which(membership_vec == comm)
  
  c(
    mean(layout_spread[verts,1]),
    
  #pick one below based on what your graph needs
    mean(layout_spread[verts,2]) +
      0.20 * diff(range(layout_spread[verts,2]))
    
    #max(layout_spread[verts,2]) +
      #0.03 * diff(range(layout_spread[,2]))
  )
}))

# ---- Apply the SAME scaling that igraph uses ----

usr <- par("usr")

x_rng <- range(layout_spread[,1])
y_rng <- range(layout_spread[,2])

community_centers[,1] <-
  (community_centers[,1] - x_rng[1]) /
  diff(x_rng) *
  diff(usr[1:2]) + usr[1]

community_centers[,2] <-
  (community_centers[,2] - y_rng[1]) /
  diff(y_rng) *
  diff(usr[3:4]) + usr[3]

# White circles with colored border
points(
  community_centers[,1],
  community_centers[,2],
  pch = 21,
  bg = "white",
  col = community_colors[sort(unique(membership_vec))],
  cex = 1.8,
  lwd = 2
)

# Draw labels
text(
  community_centers[,1],
  community_centers[,2],
  labels = sort(unique(membership_vec)),
  cex = 0.5,
  font = 2,
  col = "black"
)


dev.off() #turn off print
#### stop highlight


################################################



# Quick proof of concept for Gaussian (mclust)

library(mclust)

# Louvain
louvain_membership <- membership(communities)

# Mclust
mclust_result <- Mclust(adj_matrix)
mclust_membership <- mclust_result$classification

# Put them together
comparison <- data.frame(
  Bird = names(louvain_membership),
  Louvain = louvain_membership,
  Mclust = mclust_membership[names(louvain_membership)]
)

write.csv(
  comparison,
  "Louvain_vs_Mclust.csv",
  row.names = FALSE
)


