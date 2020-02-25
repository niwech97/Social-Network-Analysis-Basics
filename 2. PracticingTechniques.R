
################################################################################

#                              PRACTICING TECHNIQUES                           #       

################################################################################

############################# CREATING NETWORK GRAPHS ##########################

#First create a sample dataset to work with
#Here we created a dataset with 20 nodes, the probability of drawing an edge being 
#0.2 with the graph being non-directed and not having any loops. 
sample <-sample_gnp(60,.2,directed=FALSE, loops=FALSE)

#observe data
head(sample)

#Visualize the data
plot(sample)

#This looks good but we can alter the colours, layout and other features to make 
#it look better

#Layout options
#Set seed so that the graph is reproduced the same each time we run it
set.seed(3952)
#Use the fruchterman.reingold layout to visualize the data 
layout1<- layout.fruchterman.reingold(sample) #this is a spring network which 
#reflects distances within network 

#Node options - the V refers to vertices
V(sample)$color<- "grey"
V(sample)[degree(sample, mode = "in")>15]$color <- "orange"

#Edge options - the E refers to edges (lines)
E(sample)$color <- "grey" 

#Now plot the new graph 
plot(sample)

#Notice that the nodes are quite large and overpower the lines. We can fix this. 

#Layout options
set.seed(3952)
layout1<- layout.fruchterman.reingold(sample) 

#Node options
V(sample)$color<- "grey"
V(sample)[degree(sample, mode = "in")>15]$color <- "orange"

#Edge options
E(sample)$color <- "grey" 

#Fix the size of the nodes 
V(sample)$size= degree(sample, mode= "in")/1.2

#Now plot again with nodes size changes
plot(sample)

#Now we can plot our smample with arrow sizes that are small enough that they 
#don't overpower the graph. 

#Layout options
set.seed(3952)
layout1<- layout.fruchterman.reingold(sample) 

#Node options
V(sample)$color<- "grey"
V(sample)[degree(sample, mode = "in")>15]$color <- "orange"
#Node size
V(sample)$size= degree(sample, mode= "in")/1.2

#Edge options
E(sample)$color <- "grey" 

#Now plot the new graph with arrow sizes changes 
plot(sample, edge.arrow.size=0.25, edge.arrow.mode= "-")

#we can save this graph for future reference:
pdf(file=paste(p.path,"Sample IN Network.pdf",sep="/"))
plot(sample, edge.arrow.size=0.25, edge.arrow.mode= "-")
dev.off()

#The above plot was looking at the network with an "in" centrality meaning that 
#it is how many people they nominated

#Now we can plot the network with an "out" centrality meaning that it is 
#how many people nominated them

#Layout options
set.seed(3952)
layout1<- layout.fruchterman.reingold(sample) 

#Node options
V(sample)$color<- "grey"
V(sample)[degree(sample, mode = "out")>15]$color <- "orange"
#Node size
V(sample)$size= degree(sample, mode= "in")/1.2

#Edge options
E(sample)$color <- "grey" 

#Now plot to see the difference between "in" and "out" centrality 
plot(sample, edge.arrow.size=0.25, edge.arrow.mode= "-")

#we can save this graph for future reference:
pdf(file=paste(p.path,"Sample OUT Network.pdf",sep="/"))
plot(sample, edge.arrow.size=0.25, edge.arrow.mode= "-")
dev.off()

#Now we can plot the network with an "all" centrality meaning that it is 
#how many people nominated them

#Layout options
set.seed(3952)
layout1<- layout.fruchterman.reingold(sample) 

#Node options
V(sample)$color<- "grey"
V(sample)[degree(sample, mode = "all")>15]$color <- "orange"
#Node size
V(sample)$size= degree(sample, mode= "in")/1.2

#Edge options
E(sample)$color <- "grey" 

#Now plot to see the differnce between "in", "out" and "all" centrality 
plot(sample, edge.arrow.size=0.25, edge.arrow.mode= "-")

#we can save this graph for future reference:
pdf(file=paste(p.path,"Sample ALL Network.pdf",sep="/"))
plot(sample, edge.arrow.size=0.25, edge.arrow.mode= "-")
dev.off()

#We created this graph so that all the orange nodes had over 15 connections. By 
#doing this, we are able to easily see which of the nodes have the most amount of 
#connections through looking at the colours as well as the size of the nodes. 

############################# CONNECTIVIY AND POSITION #########################

#This section includes outputs that describe the data that can be depicted through 
#numerical values. 

#Density - how connected you are vs how connected you could be 
graph.density(sample, loop=FALSE)
# We get a value of 0.19 which means that only about 19% of all possible connections 
# exist. This is a fairly low density as density ranges from 0-1, with 1 being 
# all possible connections exist.But this network is more dense (connected) that 
# the original dataset we were working with. 

#Average path length - how quickly can something (ex. communicable disease) move 
#through the network
#larger path distance potentially can mean less dense network 
mean_distance(sample)
# We have an average path distance of 1.9 which means that on average something 
# such as a disease can get from random node A to random node B in 1.9 jumps. 
#This means that this network is really connected - more than our original dataset 

#degree distribution - Plotting the probability that any certain node has that number 
# of connections (Degree) - how connected you are to 1 person vs more people  
degree_distribution(sample)
Degree.dist <- degree.distribution(sample)

#Make into a dataframe
Degree.dist2 <- as.data.frame(Degree.dist)

#plot as a histogram 
hist(Degree.dist, breaks=25, 
       ylab= "Number of Connections", 
       xlab= "Probability of Number of Connections",
       main= "Histogram of Probability of Connections")

#we can save this for future reference
pdf(file=paste(p.path,"Sample Degree Distribution.pdf",sep="/"))
hist(Degree.dist, breaks=25, 
     ylab= "Number of Connections", 
     xlab= "Probability of Number of Connections",
     main= "Histogram of Probability of Connections")
dev.off()

#clustering coefficients 
# Transitivity measures how "cliquey" the network is. For example if "A" and "B" 
# and "A" and "C" are connected, high transitifity would be if "B" and "C"
# are connected. Clustering coefficients can be broken down into global custering 
#coefficient (which is known as transitivity). Transitivity is calculated by dividing 
# the number of exsisting number of closed triads by all possible triads within the structure. 
# This tells us how "cliquey" the network is. 
transitivity (sample)
# we have a transitivity measure of 0.19 meaning that of all possible triads within
# the network, only about 19% of these are closed. 


############################### END PRACTICING TECHNIQUES ######################

