#This is not a Stand-alone code. 
# these statistical analysis codes to analyze g
  #g is the graphical data set made BUT NOT SAVED by the plot code (V1 or V2)
# these stats are to be run at the end of making a plot to analyze it 


# But Hannah! Why are they not part of the plot code already!?!?
  # short answer:
    # they are in plot code V1
    # in plot code V2, it is best run all at once, and it was annoying to run the stats each time -> so I backed them up here
  #Long answer OR QUESTONS:
    # Email me! I don't bite! hlbaetge@gmail.com (Hannah Baetge)




###### Want to see info on a bird? #######

get_bird_info <- function(BirdName) { # BirdName= "3 letter color code of the bird" IN QUOTES!
  df_events %>%
    filter(ColorCombo == BirdName) %>%
    select(ColorCombo, Feeder, DateTime, Event_ID, Sex,DateLastCaptured,Status,Location,Age, ColorCrest )
}





####  stats   #################################

#	Number of geodesic paths that go through a given node
be=betweenness(g, normalized=T)
#plot(g,  vertex.label="", vertex.color="gold", edge.color="slateblue", vertex.size=be*50, edge.width=E(g)$weight*5)
be
sort(be, decreasing = TRUE, na.last = NA)
hist(be, breaks=10)

# 	Number of edges connected to node
degree(g)
sort(degree(g), decreasing = TRUE, na.last = NA)
hist(degree(g), breaks=10, col="gray")

#	Sum of edge weights connected to a node (aka weighted degree)
graph.strength(g)
sort(graph.strength(g), decreasing = TRUE)
hist(graph.strength(g), breaks=10)

