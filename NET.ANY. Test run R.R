### setup ###
install.packages(c("igraph", "bipartite", "asnipe", "assortnet", "ggplot2", "ggmap", "rnetcarto", "ecodist", "igraphdata", "statnet", "RColorBrewer", "tidyverse"))
    ## installs packages needed, can be re-run to update version. 
find.package("tidyverse") ## confirms that the last package has been loded and tells you where
library(help='igraph') # open help window 

## load out practice data
library(igraph) #load the package 
?graph_from_adjacency_matrix

### make a network ###
g=make_graph(~A-B-C-A, D-E-F-D, A-F) 
plot(g)

for (i in 1:5) {
  plot(g)
  print(paste("This is run number", i))
} ### plot 5 times more, note the variation.


# lets get some info about our plot
class(g)
g

#interpret: 
##### IGRAPH 7a50406 UN-- 6(nodes) 7(edges) -- 
## + attr: name (v/c)
## + edges from 7a50406 (vertex names):
## [1] A--B A--C A--F B--C D--E D--F E--F

V(g) #look up vertices 
E(g) #look up edges
V(g)$name

### time to edit! ###
# now add some attributes
V(g)$color=c("white", "red", "green", "blue", "orange", "yellow") #a random set of colors 
plot(g)
## does the plot stay the same?
for (i in 1:5) {
  plot(g)
  print(paste("This is run number", i))
} ### plot 5 times more, note the letter color stays the same. 


# now add width to note edge strength
E(g)$width=1:7
plot(g)
 # and we can color them! 
E(g)$color=rainbow(7) #rainbow() function chooses a specified number of colors
plot(g)


### ok, enough fun... back to the basics ###

### Adjacency Matrix ###

library(igraph)
g=make_graph(~A-B-C-A, D-E-F-D, A-F) 
V(g)$color=c("white", "red", "green", "blue", "orange", "yellow")
E(g)$weight=1:7
plot(g)

as_adjacency_matrix(g, sparse=F) # matrix showing related ness 
as_adjacency_matrix(g, sparse=T)# same matrix but 0= a period (.)

as_edgelist(g) # list of edges, with weights if weighted 


### group affiliation matrix ###
A=c(1,1,0,0) 
B=c(1,0,1,0) 
C=c(1,0,1,0) 
D=c(0,1,0,1) 
E=c(0,0,1,1) 
aff=matrix(c(A,B,C,D,E),nrow=5,byrow=TRUE) 
dimnames(aff)=list(c("A","B","C","D","E"),c("Group1","Group2","Group3","Group4"))
aff #The individual-by-group matrix

# make this an adjacency matrix that can graph. 
# if two individuals both occur in two different groups, the value of their intersection will be two
aff %*% t(aff)
#and graph
m2=aff %*% t(aff)
g2=graph_from_adjacency_matrix(m2, "undirected", weighted=T, diag=F)
E(g2)$weight=1:7
plot(g2, edge.width=E(g2)$weight)

#Make an Adjacency list 
# ‘focal’ node on the first column, and then all the other nodes that are connected to it 
as_adj_list(g)

# well, that is hard to read, lets clean it up!

library(igraph)

# Make adjacency list
adj_list <- as_adj_list(g2, mode = "all")

library(igraph)

# Get adjacency list from your graph
adj_list <- as_adj_list(g2, mode = "all")

# Create base data frame with focal nodes
adj_df <- data.frame(
  Focal_Node = names(adj_list),
  stringsAsFactors = FALSE
)

# Find the max number of neighbors any node has
max_neighbors <- max(lengths(adj_list))

# Fill in each Neighbor column (use "" instead of NA)
for (i in 1:max_neighbors) {
  adj_df[[paste0("Neighbor_", i)]] <- sapply(
    adj_list,
    function(x) if (length(x) >= i) x[i] else ""
  )
}

# View the result
print(adj_df)

### lets add weights to our matrix###
as_adjacency_matrix(g, sparse=F, attr="weight")
as_data_frame(g)
# note "to" and "from" do not need anything here, this is not directional
# things are simply alphabetized



### making network from edge list###
edge.dat=read.csv("https://dshizuka.github.io/network2018/NetworkWorkshop_SampleData/sample_edgelist.csv") 
edge.dat # call data

set.seed(2)
eg=graph_from_data_frame(edge.dat, directed=FALSE) 
eg #call edge list

plot(eg, edge.width=E(eg)$weight) # plot :)


# make adjacecy matrix
am=as.matrix(read.csv("https://dshizuka.github.io/network2018/NetworkWorkshop_SampleData/sample_adjmatrix.csv", header=T, row.names=1))
am

# now make igraph
g=graph_from_adjacency_matrix(am, mode="undirected", weighted=T)
plot(g, edge.width=E(g)$weight)
plot(g, edge.width=E(g)$weight/4, edge.curved=T, layout=layout_in_circle(g))

### time to look at social grouping ###
#use a association index 
  # note, this network is very dependient on how many times an individula was seen 
assoc=as.matrix(read.csv("https://dshizuka.github.io/networkanalysis/SampleData/Sample_association.csv", header=T, row.names=1))
assoc
library(asnipe) #load the asnipe package
gbi=t(assoc) #assoc is in individual-by-group format, but this package likes group-by-individual, so we will transpose the matrix.
adj.m=get_network(t(assoc), association_index="SRI") #this function converts the association matrix into an adjacency matrix using the Simple Ratio Index.
assoc.g=graph_from_adjacency_matrix(adj.m, "undirected", weighted=T) #create a graph object
plot(assoc.g, edge.width=E(assoc.g)$weight*10) #Since the edge weights are small (values = 0 to 1), let's multiply by 10 for edge widths.
#ok, make it pretty:
plot(assoc.g, edge.width=E(assoc.g)$weight*10, vertex.label="", vertex.color="gold", edge.color="cornflowerblue", vertex.frame.color="black")
# add the labels back in:
plot(assoc.g, edge.width=E(assoc.g)$weight*10, vertex.color="gold", edge.color="cornflowerblue", vertex.frame.color="black")



### how do you shape network layouts?###
plot(g, layout=layout_with_fr(g))
# plots w/ default Fruchterman-Reingold algorithm, stonger edges pull nodes closer
  # weaker edges push nodes out 
set.seed(10)# makes network reproducible 
# more layout options:
layouts = c("layout_with_fr", "layout_with_kk", "layout_with_dh", "layout_with_gem", "layout_as_star", "layout_as_tree", "layout_in_circle", "layout_on_grid")
par(mfrow=c(2,4), mar=c(1,1,1,1))
for(layout in layouts){
  l=do.call(layout, list(g))
  plot(g, layout=l, edge.color="black", vertex.label="", main=layout)
}

l=matrix(c(1,2,3,4,5,6,7, 1,2,3,4,5,6,7),ncol=2)# set nodes - line
plot(g,layout=l,edge.curved=TRUE, )


dev.off() # clear plot area 


### and vertices?###

attrib=read.csv("https://dshizuka.github.io/networkanalysis/SampleData/sample_attrib.csv")
attrib

# vertices are placed alphabetical, so need to sort out input so the data in in order in the matrix (eg, 1-7 = A-G)
V(g)$sex=factor(attrib[match(V(g)$name, attrib$Name), "Sex"]) # factor() preserves data as M/F
V(g)$age=attrib[match(V(g)$name, attrib$Name), "Age"]
g

# now, color dots based on atrabute 
V(g)$color=c("pink","lightblue")[as.numeric(V(g)$sex)]
#converts the sex to numbers by alphabetical order, i.e., 1 = female and 2 = male

# now graph 
set.seed(10)
l=layout_with_fr(g)
plot(g, layout=l,vertex.label="", vertex.size=V(g)$age, edge.width=E(g)$weight, edge.color="black")
legend("topleft", legend=c("Female", "Male"), pch=21, pt.bg=c("pink","lightblue"))


# now graph w/ layouts so you can pick one 
layouts = c("layout_with_fr", "layout_with_kk", "layout_with_dh", "layout_with_gem", "layout_as_star", "layout_as_tree", "layout_in_circle", "layout_on_grid")
par(mfrow=c(2,4), mar=c(1,1,1,1))
for(layout in layouts){
  l=do.call(layout, list(g))
  plot(g, layout=l,vertex.label="", vertex.size=V(g)$age, edge.width=E(g)$weight, edge.color="black",  main=layout)
  legend("topleft", legend=c("Female", "Male"), pch=21, pt.bg=c("pink","lightblue"))
}


# I like the tree it gives good centrality- let's pick it!

plot(g, # what are we ploting 
     layout=layout_as_tree(g),# pick the layout
     vertex.label="",# remove label on node (vertex =node)
     vertex.size=V(g)$age, #size of node = relative age
     edge.width=E(g)$weight, # set ege width = strength of relationship
     edge.color="black") # edge color 
legend("topleft", # legend location 
       legend=c("Female", "Male"), # legend labels
       pch=21, # type of indicator on legend (circle in this case)
       pt.bg=c("pink","lightblue")) # point colors- IN LEGAND ONLY- set node colors in V(g)




##############################
### QUANTITATIVE :) ###

# lets get some data to play with 
library(asnipe)
library(igraph)
degree=igraph::degree
betweenness=igraph::betweenness
closeness=igraph::closeness
assoc=as.matrix(read.csv("https://dshizuka.github.io/networkanalysis/SampleData/Sample_association.csv", header=T, row.names=1))
gbi=t(assoc) 
mat=get_network(t(assoc), association_index="SRI") 
g=graph_from_adjacency_matrix(mat, "undirected", weighted=T) #create a graph object

# plot the network
set.seed(10)
l=layout_with_fr(g)
plot(g, layout=l, vertex.label="", vertex.color="gold", edge.color="slateblue", edge.width=E(g)$weight*5)

###node-level measures - analyzing an individual###

#degree centrality. Degree centrality is simply the number of edges connected to a given node.
degree(g)


###- tangent- min/max function!!!!- mostly practice, but may be helpfull for identifing outlier birds 

MaxMin<-function(a) { #a= igraph,  ex. degree(g)
  max_node  <- names(a)[which.max(a)]
  max_value <- max(a)
  
  min_node  <- names(a)[which.min(a)]
  min_value <- min(a)
  
  list(
    max_node  = max_node,
    max_value = max_value,
    min_node  = min_node,
    min_value = min_value
  )
}  

MaxMin(degree(g))


###betweenness centrality: # of geodesic paths (shortest paths) that go through a given node

be=betweenness(g, normalized=T)
plot(g,  vertex.label="", vertex.color="gold", edge.color="slateblue", vertex.size=be*50, edge.width=E(g)$weight*5)

MaxMin(be)

### others###

#degree	                    degree()	            Number of edges connected to node
#strength	                  graph.strength()	    Sum of edge weights connected to a node (aka weighted degree)
#betweenness              	betweenness()	        Number of geodesic paths that go through a given node
#closeness	                closeness()	          Number of steps required to access every other node from a given node
#eigenvector centrality	    eigen_centrality()	  Values of the first eigenvector of the graph adjacency matrix. The values are high for vertices that are connected to many other vertices that are, in turn, connected many others, etc.


### make table
names=V(g)$name
de=degree(g)
st=graph.strength(g)
be=betweenness(g, normalized=T)


#assemble dataset
d=data.frame(node.name=names, degree=de, strength=st, betweenness=be) 
head(d) #display first 6 lines of data




#### Community Structure###

# note all thse methods of grouping (such as to assign platoons)
  #suffer from the basic assumption that each node belongs to just one community

# options:
#edge.betweenness.community()	
    #One of the first in the class of “modularity optimization” algorithms. It is a “divisive” method. Cut the edge with highest edge betweenness, and recalculate. Eventually, you end up cutting the network into different groups.	Newman & Girvan 2004
#fastgreedy.community()	
    #Hierarchical agglomerative method that is designed to run well in large networks. Creates “multigraphs” where you lump groups of nodes together in the process of agglomeration in order to save time on sparse graphs.	Clauset et al. 2004
#walktrap.community()	
    #Uses random walks to calculate distances, and then use agglomerative method to optimize modularity	Pons & Latapy 2005
#spinglass.community()	
    #This method uses the analogy of the lowest-energy state of a collection of magnets (a so-called spin glass model).	Reichardt & Bornholdt 2006
#leading.eigenvector.community()	
    #This is a “spectral partitioning” method. You first define a ‘modularity matrix’, which sums to 0 when there is no community structure. The leading eigenvector of this matrix ends up being useful as a community membership vector.	Newman 2006
#label.propagation.community()	
    #I have not used this one…	Raghavan et al. 2007
#cluster_louvain()	
    #The “Louvain” method, so-called because it was created by a group of researchers at Louvain University in Belgium. Community aggregation	Blondel et al. 2008
#rnetcarto::netcarto()	
    #Simulated Annealing method. Thought to be useful for smaller networks. Available through ‘rnetcarto’ package (Doulcier 2015).



#### practice w/ clear structure 
g=make_graph(~A:B:C:D:E-A:B:C:D:E, F:G:H:I:J-F:G:H:I:J, A-F, B-G)
set.seed(7)
l=layout_with_fr(g)
plot(g, layout=l, edge.color="black")

# ex: use edge betweeness 
eb=edge.betweenness.community(g) 
eb
length(eb) #number of communities!
modularity(eb) #modularity
membership(eb) #assignment of nodes to communities !!!!!! this is SO IMPORTANAT

plot(eb, g, layout=l) # and plot!

# more complicated data, more fancy figure - but same idea 
library(asnipe)
library(igraph)
assoc=as.matrix(read.csv("https://dshizuka.github.io/networkanalysis/SampleData/Sample_association.csv", header=T, row.names=1)) #import individual-by-group data
gbi=t(assoc) #transpose the data into group-by-individual
mat=get_network(t(assoc), association_index="SRI") #create adjacency matrix with "simple ratio index"
g.sparrow=graph_from_adjacency_matrix(mat, "undirected", weighted=T) #make into igrpah object

com=cluster_louvain(g.sparrow)
com
set.seed(2)
plot(com, g.sparrow, vertex.label="", edge.width=E(g.sparrow)$weight*5)

#fun colors!
library(RColorBrewer)
colors=brewer.pal(length(com),'Accent') #make a color palette
V(g.sparrow)$color=colors[membership(com)] #assign each vertex a color based on the community assignment

set.seed(2)
plot(g.sparrow, vertex.label="", edge.width=E(g.sparrow)$weight*5)
# note- seed changes shape, but color palette stays the same ( lone group always green)


# assortment ( is there an atrabute nodes are grouping by?) - maybe age in our project?
set.seed(3)
V(g)$size=c(rnorm(5, mean=20, sd=5), rnorm(5, mean=30, sd=5)) #assign sizes to nodes using two normal distributions with different means

plot(g, layout=l, edge.color="black")

assortativity(g, V(g)$size, directed=F)# check leval of assortment by size

V(g)$size.discrete=(V(g)$size>25)+0 #shortcut to make the values = 1 if large individual and 0 if small individual, with cutoff at size = 25
assortativity(g, V(g)$size.discrete, directed=F) # assortmet as a binary (result a little different)















