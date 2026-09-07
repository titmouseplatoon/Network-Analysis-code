# To Run This code you will need the output file from "feeder+MBS_merging_4MAC.R" 
#It should be a large CSV file (name it with the general time frame the data is from)
# ex. Spring 2025 - this prevents confusion later

#Also, look have the dates the dataset covers at the ready... you will have to input them

# How is this code different than the (many) Previous versions
  # This code is based in this example- very explicitly
  # https://dshizuka.github.io/networkanalysis/example_RFID_to_Networks.html
    # "This method improves on the alternative, which would be to simply set an arbitrary time window" (what we did )
    # and assign birds that show up within such time windows to be part of the same flock" (we added a clustering code to avoid this... but we are starting w/ lower quality data due to the cut-off)
    # (i.e., saying “birds that come to the feeder within 5 minutes of each other are part of the same flock”)"
  #This new code uses an algorithm to assess the probability of each individual interaction resulting from a social grouping. 
    # It looks at the big picture to decide if an interaction is likely to be informative



########### The easiest way to run this code is to:
#Select all (Mac: Command + A)
#Press run (Top right of script panel)
#The code has built-in prompts that will ask you to select and name files
# Each run will make two file outputs: the plot PDF and the Community assignment csv (other files are made and saved along the way - KEEP THEM they can be analyzed later)
#the files will be saved to your working directory
# If you need to find your Working Directory type: getwd() into the console
# Need to change your Working Directory?
# Click Session in the top menu bar.
# Hover over Set Working Directory.
# Click "Choose Directory"....Select your folder and click Open. OR choose "To Source File Location" to set it to the folder where your active script is saved.

#Questions? Email me!- Fair warning, the complexity of this project is starting to exceed my ability to explain things in writing...
# STILL EMAIL... but zoom may be required... ;)
#(Hannah Baetge) hlbaetge@gmail.com



library(igraph)
library(tidyverse)
library(asnipe)

#Select the data 
compiled_data <- read_csv(file.choose()) # pick data file you wish to graph

#### Clean Data ####
# modified from tutorial as test tags are already removed and band codes aleredy added
# Joining with `by = join_by(RFID)`
datastream=compiled_data %>% 
  select(RFID, Feeder, DateTime, ColorCombo) %>%
  filter(ColorCombo!="NA") %>%
  mutate(time_num=as.numeric(DateTime)) %>%
  select(ColorCombo, Feeder, DateTime, time_num)

# Check output
head(datastream)


# create list of birds in data set 
all_ids=datastream %>% 
  pull(ColorCombo) %>%  #get just the band number as a vector
  unique() #just get the unique numbers 
all_ids

#### Divide Data into days ####

#enter start and end date of dataset
start_date <- rstudioapi::showPrompt(
  title = "enter FIRST date of dataset", 
  message = "Enter date in YMD format ex 2021-12-06", 
  default = "yyyy-mm-dd"
)

cat("\n Start date entered:", start_date, "\n")

end_date <- rstudioapi::showPrompt(
  title = "enter LAST date of dataset", 
  message = "Enter date in YMD format ex 2021-12-06", 
  default = "yyyy-mm-dd"
)

cat("\n End date entered:", end_date, "\n")


use_dates=dates.to.use=seq(ymd(start_date), ymd(end_date),1)
use_dates

# make folder to hold each day's data (one folder)
# only makes folder if one does not already exist in your wd
# folder is saved in Working Directory 
if(dir.exists("gmm_daily/")==F) dir.create("gmm_daily/")


#### Make GMM File ####

#test with one date
for(i in 1){
  data1=datastream%>% filter(date(DateTime)==use_dates[i])
  gmm_out=gmmevents(time=data1$time_num, identity=data1$ColorCombo, location=data1$Feeder, global_ids = all_ids)
  save(gmm_out, file=paste("gmm_daily/","gmm_", use_dates[i], ".rdata", sep=""))
}

# Run for all dates 
for(i in 1:length(use_dates)){
  data1=datastream%>% filter(date(DateTime)==use_dates[i])
  gmm_out=gmmevents(time=data1$time_num, identity=data1$ColorCombo, location=data1$Feeder, global_ids = all_ids)
  save(gmm_out, file=paste("gmm_daily/","gmm_", use_dates[i], ".rdata", sep=""))
}


# note, "run it in parallel" step in tutorial skiped b/c I have patience and don't trust my computer not to explode if I ask it to multitask ...
  # but, if you have a big datafile and less time, refer to step 4.6 in the tutorial 

#### Create Networks ####

# make list of ALL GMM files
all_gmm_files <- list.files(
  "gmm_daily/",
  full.names = TRUE
)

# Keep ONLY files whose date is in the selected date range
gmm_filename_full <- all_gmm_files[
  basename(all_gmm_files) %in%
    paste0("gmm_", use_dates, ".rdata")
]

gmm_filename_short <- basename(gmm_filename_full)

gmm_list <- list()


# create group by indiv. matrix for each day
for(i in 1:length(gmm_filename_full)){
  load(gmm_filename_full[[i]])
  gmm_list[[i]]=gmm_out
  gmm_list[[i]]$name=gmm_filename_short[i]
}


# Stitch together each day
gbi.list=lapply(gmm_list, function(x) x$gbi)
all.gbi=do.call("rbind", gbi.list)

# create adjacency matrix
adj=get_network(all.gbi)

# Create igraph object 
g=graph_from_adjacency_matrix(adj, mode="undirected", weighted=T)



#### Plot!!!! ####

plot(g, vertex.label="", edge.width=E(g)$weight*20)
          # Murphy, behold, your gmm plot ;)

 # hahahhahahhhaha - ok, now lets make it informative



#### make a look-up to color-code & sort by attributes ####

# create df_events- just a big holder of data 
df_events <- compiled_data %>%
  arrange(Feeder, DateTime) %>%  # Important: sort by feeder & time
  group_by(Feeder)
  
  
bird_attributes <- df_events %>%
  select(ColorCombo, Sex, Status, LocationCaptured,
         Age, CrestColor, DateCaptured, Feeder) %>%
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

# compare old and new ages and see where changes were 
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

#### plot w/in R to check components - will be crowded ####
plot(g,
     layout = layout_with_fr(g), #clasic layout
     vertex.color = V(g)$color,   # use the colors we assigned
     ,           #put nothing, keep labels
     vertex.size = 25,            # adjust node size
     edge.width = E(g)$weight,    # edge width proportional to weight
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



#### PDF network #####

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
  "%s_%s.pdf",
  timestamp,
  plot_description
)

cat("Saving PDF as:", pdf_name, "\n") 


# print big! - saves your plot
pdf(pdf_name, width = 10, height = 10)


# Create or load a stable network layout that updates each season

layout_file <- "gmmBirdLayout.csv"


if (!file.exists(layout_file)) {
  
  cat("No saved layout found. Creating first layout...\n")
  
  set.seed(1)
  
  layout_spread <- layout_with_fr(
    g,
    weights = E(g)$weight,
    niter = 8000,
    area = vcount(g)^2 * 10
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
    coords = start_layout,
    weights = E(g)$weight, #strength of edge weights- divide by LARGER number to decrease pull of strong edges
    niter = 20000,    # number of iterations -  larger number lets the network settle/optimize
    area = vcount(g)^2 * 10 #increases the plot area- increases the repulsive force and usually gives a cleaner layout without changing the overall structure.
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


# make the edge weights visable 
edge_weights <- E(g)$weight

edge_widths <- 0.5 + 8 * (
  edge_weights - min(edge_weights)
) / (
  max(edge_weights) - min(edge_weights)
)



#create plot
plot(g,
     layout = layout_spread,    # layout style
     vertex.color = V(g)$color, # use the colors we assigned- sex
     vertex.label.cex = 0.6,    # make label a little bigger 
     vertex.label.color = "black" ,
     vertex.size = 8,           # adjust node size
     vertex.frame.color = V(g)$frame.color,  # age border color
     vertex.frame.width = 2,                 # border thickness
     edge.width = edge_widths,             # edge width proportional to weight
     edge.color = "black", #edges will be black                
     edge.curved = FALSE,   # forces straight edges
     mark.groups = communities,
     mark.col = adjustcolor(community_colors, alpha.f = 0.25),
     mark.border = NA,
     mark.lwd = 0
)


title(
  main = sprintf(
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

# Apply the SAME scaling that igraph uses

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



