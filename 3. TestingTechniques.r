
################################################################################

#                              TESTING TECHNIQUES                           #       

################################################################################

############################# CREATING NETWORK GRAPHS ##########################
# In our Main document, we read in our SN and ZN networks, which are Songbird 
# and Zebra respectively.

# We can check that they are still loaded and ready to go by doing a simple plot

plot(ZN)
plot(SN)


#To make this a reproducible graph so we are looking at the same thing, we can 
# set a seed and make a layout.

LayoutZN<-layout.auto(ZN)
set.seed(2222020)
plot(ZN)

# And again with our songbird network

LayoutSN<-layout.auto(SN)
set.seed(2222020)
plot(SN)

# We can write these graphs into PDFs for Future Reference

pdf(file=paste(t.path,"Zebra Network Initial.pdf",sep="/"))
LayoutZN<-layout.auto(ZN)
set.seed(2222020)
plot(ZN)
dev.off()

#and again for Songbirds
pdf(file=paste(t.path,"Songbird Network Initial.pdf",sep="/"))
LayoutSN<-layout.auto(SN)
set.seed(2222020)
plot(SN)
dev.off()


############################## PERFORMING ANALYSIS #############################

# Next we want to think about what analysis we need to run to answer our question
# Our question is, which players should be immunized to slow the spread of a 
# communicable diseases in these networks? This question can be answered in many ways
# we aim to answer it assuming we can immunize:
# 1 player
# 3% of players
# and 10% of players

#First lets look at how quickly a disease may move through these networks.
#We can use the average path lengths between the networks to get an idea of how quickly
#a disease may move through the network.

mean_distance(ZN)
mean_distance(SN)



# The metric that seems like it is most likely to shed light on this is Betweeness
# Centrality. This metric gives us a value for each node of how often that node is
# used as a bridge between other nodes. We have a formula in our document.

# Now that we know what measure we want to use, we can begin our analysis

#First calculated Betweeness
ZNbetween.list<-betweenness(ZN)

#Now we make a dataframe to analyze the values of each node 
ZNbetween<-as.data.frame(ZNbetween.list)

# We can create a column that has the node ID next to each value
ZNbetween$Nodelist<-(1:27)

# lastly we can arrange them in descending order, so we know to only highlight
# the top IDs in the final graphing portion
arrange(ZNbetween,-ZNbetween.list)

#Now again with Songbirds
#First calculated Betweeness
SNbetween.list<-betweenness(SN)

#Remake into a dataframe for analysis
SNbetween<-as.data.frame(SNbetween.list)

# We can create a column that has their node ID next to each value
SNbetween$Nodelist<-(1:117)

#Next we can arrange them in descending order
arrange(SNbetween,-SNbetween.list)

#Now we can write these into csvs for future reference

#Zebra network
write.csv(ZNbetween,file=paste(t.path,"Zebra Betweeness Values.csv",sep="/"))

#Songbird network
write.csv(SNbetween,file=paste(t.path,"Songbird Betweeness Values.csv",sep="/"))

# Below we graph who we would immunize with specific immunization criteria


############################## Final Graphs ####################################
# Now that we know which players have the highest Betweeness Centrality, we can 
# regraph our networks with them highlighted in a different color. 


# Graphs of 1 player

#Zebra
#First we find our top 1 player as measured in Betweenness
top1ZN<-ZNbetween%>%top_n(1,ZNbetween.list)

#An important note is that it is our second column we want
#next we highlight those players

LayoutZN<-layout.auto(ZN)
set.seed(2222020)

#node options
V(ZN)$color<-"grey"
V(ZN)[top1ZN[,2]]$color<-"yellow"

#Now we can plot it!
plot(ZN)

#lets save this as a pdf for future reference
pdf(file=paste(t.path,"Zebra top 1.pdf",sep="/"))
LayoutZN<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top1ZN[,2]]$color<-"yellow"
plot(ZN)
dev.off()

# Songbird
#First we find our top player
top1SN<-SNbetween%>%top_n(1,SNbetween.list)

#Again note our second column is the nodelist
#next we highlight those players
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top1SN[,2]]$color<-"yellow"
plot(SN)


#save as a pdf
pdf(file=paste(t.path,"Songbird top 1.pdf",sep="/"))
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top1SN[,2]]$color<-"yellow"
plot(SN)
dev.off()
# Graphs of 3% (same as 1 player for Zebra)

#Zebra
#First we find our top 3% of players
#total nodes in Zebra are 27, 3% comes out to.81, 
#we round up to 1 player (same as last time)
top1ZN<-ZNbetween%>%top_n(1,ZNbetween.list)

#Again note our second column is the nodelist
#next we highlight those players
LayoutZN<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top1ZN[,2]]$color<-"yellow"
plot(ZN)

#save as a pdf
pdf(file=paste(t.path,"Zebra top 3.pdf",sep="/"))
LayoutZN<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top1ZN[,2]]$color<-"yellow"
plot(ZN)
dev.off()


#Songbird
#First we find our top 3% of players
#total nodes in Songbirds are 117 3% comes 
#out to 3.5 we round up to 4 players

top4SN<-SNbetween%>%top_n(4,SNbetween.list)

#Again note our second column is the nodelist
#next we highlight those players
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top4SN[,2]]$color<-"yellow"
plot(SN)

#save as pdf
pdf(file=paste(t.path,"Songbird top 3.pdf",sep="/"))
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top4SN[,2]]$color<-"yellow"
plot(SN)
dev.off()


# Graphs of 10%

#Zebra
#First we find our top 10% of players
#total nodes in Zebra are 27, 10% comes out to 2.7, 
#we round up to 3 players
top3ZN<-ZNbetween%>%top_n(3,ZNbetween.list)


#Again note our second column is the nodelist
#next we highlight those players
LayoutZN<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top3ZN[,2]]$color<-"yellow"
plot(ZN)


#Save as a pdf
pdf(file=paste(t.path,"Zebra top 10.pdf",sep="/"))
LayoutZN<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top3ZN[,2]]$color<-"yellow"
plot(ZN)
dev.off()

#Songbird
#First we find our top 10% of players
#total nodes in Songbirds are 117 10% comes 
#out to 11.7 we round up to 12 players

top12SN<-SNbetween%>%top_n(12,SNbetween.list)

#Again note our second column is the nodelist
#next we highlight those players
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top12SN[,2]]$color<-"yellow"
plot(SN)

#save as pdf
pdf(file=paste(t.path,"Songbird top 10.pdf"))
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top12SN[,2]]$color<-"yellow"
plot(SN)
dev.off()

################################# Additional Graphs ###############################
# Additionally we can graph this with the size of the node correlated to the betweenness value
# This could be useful if you are trying to decide how much of the population you should immunize,
# but that would require more information about the disease and how it transfers and also requires
# judgement calls on acceptable losses within the network

#Zebra betweenness by size
LayoutZN<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"yellow"
V(ZN)$size=betweenness(ZN)/2# this "/2" is so some nodes dont 
#swallow the whole screen when plotted
plot(ZN)

#save as pdf
pdf(file=paste(t.path,"Zebra betweenness by size.pdf",sep="/"))
LayoutZN<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"yellow"
V(ZN)$size=betweenness(ZN)/2
plot(ZN)
dev.off()



#Songbird betweenness by size
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"yellow"
V(SN)$size=betweenness(SN)/45 # this "/45" is so some nodes dont 
#swallow the whole screen when plotted
plot(SN)

#save as pdf
pdf(file=paste(t.path,"Songbird betweenness by size.pdf",sep="/"))
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"yellow"
V(SN)$size=betweenness(SN)/45  
plot(SN)
dev.off()

#After messing with sizes of networks, it is important to reset size to 15
V(ZN)$size=15
V(SN)$size=15

#Additionally it may be informative to have a histogram of the results, so we made
#those below and saved them as pdfs
ZN.dist<-degree.distribution(ZN)

hist(ZN.dist, breaks=25,
     ylab= "Number of Connections", 
     xlab= "Probability of Number of Connections",
     main= "Histogram of Probability of Connections")

#save as pdf
pdf(file=paste(t.path,"Zebra Degree Distribution.pdf",sep="/"))
hist(ZN.dist, breaks=25,
     ylab= "Number of Connections", 
     xlab= "Probability of Number of Connections",
     main= "Histogram of Probability of Connections")
dev.off()

#and again for songbirds

SN.dist<-degree.distribution(SN)
hist(SN.dist, breaks=25,
     ylab= "Number of Connections", 
     xlab= "Probability of Number of Connections",
     main= "Histogram of Probability of Connections")

#and save as pdf
pdf(file=paste(t.path,"Songbird Degree Distribution.pdf",sep="/"))
hist(SN.dist, breaks=25,
     ylab= "Number of Connections", 
     xlab= "Probability of Number of Connections",
     main= "Histogram of Probability of Connections")
dev.off()
################  attempts to do this with functions #####################
#This was simpler in R than we thought it might be, however we used a lot
#of farmer coding. Out of curiosity I started making functions to see
#if we can do this more simply.

#We started by making a function that returns the relevant nodelist
     #EXPLANATION OF FUNCTION
#input:test we are using,network we are testing, and the number of nodes in the network
#first, we need to perform the test
#then we store the results into a dataframe
#next we have to add a node ID list
#and lastly we arrange it in descending order

give.nodelists<-function(test,network,lengthnet1){ 
  temp.dataframe1<-test(network)                   
  temp.dataframe1<-as.data.frame(temp.dataframe1)   
  temp.dataframe1$NodeID<-(1:lengthnet1)            
  temp.dataframe1%>%arrange(desc(temp.dataframe1))  
  return(temp.dataframe1)
}

# We can then test it
ZND<-give.nodelists(betweenness,ZN,27)
SND<-give.nodelists(betweenness,SN,117)

#IT WORKS!!!
head(ZND)
head(SND)


#then we made a function that extracts the relevant values from that list
   #EXPLANATION OF FUNCTION
#take the relevant network and the # of
#targets and use a top_n function
top.node<-function(relevant.nodelist,n){
  top1.t.1<-relevant.nodelist%>%top_n(n,relevant.nodelist[,1])
}
#again we test it
highlight<-top.node(SND,1)

#It also works!!
head(highlight)


#and the big question
#Can I make a function that does it all??
     #EXPLANATION OF FUNCTION
#input:network, test you want to run, which column in the dataframe the ID is
##of nodes, #of targets, and a seed for reproducibility
#use our give.nodelist function use out top.node to find the right ones
#it is important to use auto because we can use a far greater range of networks and tests
#the seed is purely for reproducibility
#we want a dull color for the rest of them that way the yellow of our targets pops
#and finally we plot our network 
identify.targets<-function(network,test,node.ID.column.number, 
                           length,number.of.targets,seed){     
  long.targets<-give.nodelists(test,network,length)            
  final.targets<-top.node(long.targets,number.of.targets)      
  layout.t2<-layout.auto(network)                               
  set.seed(seed)                                               
  V(network)$color="grey"                                      
  V(network)[final.targets[,node.ID.column.number]]$color<-"yellow"
  plot(network)                                                
}

#YES I CAN!!!
identify.targets(ZN,betweenness,2,27,3,34554)


#lets try it with a new network:
#here is an ant colony
AN <-read_graph(paste(d.path,"weighted_network_col2_day11.graphml",
                      sep="/"),format=c("graphml"))

identify.targets(AN,betweenness,2,131,20,343434)

#after a little more testing, this seems to only work with betweenness, but thats okay as thats
#what we are mainly interested in. 
#we can still do the other analysis a little more farmer style, an I could make a function that 
#does them as well, but I don't think it's worth our time.

#I will however adjust it in 5.Final.tech to reflect only betweenness

#out of curiosity:
#Can we make another function that gives us a graph with vertex size changing by the test
#this gives us a slightly more comprehensive view which can be nice if we want a little more
#information on who should be immunized or whatever your quesiton is.
    #EXPLANATION OF FUNCTION:
#Big change in input, no target number to input
#only other big change is there is no top.node function use and size is set to =test(network)

identify.targets.size<-function(network,test,length,seed){     
                                                               
  long.targets<-give.nodelists(test,network,length)            
  layout.t3<-layout.auto(network)                               
  set.seed(seed)                                               
  V(network)$color="yellow"                                    
  V(network)$size=test(network)
  plot(network)                                                
}

identify.targets.size(ZN,betweenness,27,33333)

identify.targets.size(AN,betweenness,131,3435343)
#You can see in this last one it is definitely not perfect, perhaps a default value setting 
#could come in handy, we will experiment in 5.Final.tech

####################### Eigenvector Centrality ###################################
# The eigenvector centrality measure is a way of quantifying the influence that a 
# specific node has for example, while both of the top 2 people in terms of all
# degree (the most connected two) have many connections, if they do not share 
# any common nodes but one, that one common node would be the most influential of
# that network because he influences the top two connected nodes.

#below we use the eigenvector centrality measure to help us find the most influential 
#nodes in our networks

#Eigen Vector Centrality - this gives us a manipulation matrix. 
ZNeigen<- eigen_centrality(ZN)

#Now we make a dataframe to analyze the values of each node 
ZNeigen<-as.data.frame(ZNeigen)

#Look at dataframe to see the variation between eigen values. We can see that 
#there is quite a bit of variation between the values. 

# We can create a column that has the node ID next to each value
ZNeigen$Nodelist<-(1:27)

#Now we repeat with the Songbirds data 
#First calculated Betweeness
SNeigen<-eigen_centrality(SN)

#Remake into a dataframe for analysis
SNeigen<-as.data.frame(SNeigen)

# We can create a column that has their node ID next to each value
SNeigen$Nodelist<-(1:117)

#Now we can write these into csvs for future reference

#Zebra network
write.csv(ZNeigen,file=paste(t.path,"Zebra Eigen Vetor Values.csv",sep="/"))

#Songbird network
write.csv(SNeigen,file=paste(t.path,"Songbird Eigen Vetor Values.csv",sep="/"))

#Now we can graph the eigen vector centrality to see who is most connected to 
#other very connected people. 

# Now that we know which players have the highest Betweeness Centrality, we can 
# regraph our networks with them highlighted in a different color. 

############################## Final Graphs Eigenvector ####################################
#RESET SIZE OF NODES
V(ZN)$size=15
V(SN)$size=15

# Graphs of 1 player

#Zebra
#First we find our top 1 player as measured for Eigen Vector centrallity 
top1ZNeigen<-ZNeigen%>%top_n(1,ZNeigen$vector)

#set our layout and seed
LayoutZNeigen<-layout.auto(ZN)
set.seed(2222020)

#node options
V(ZN)$color<-"grey"

#An important note is that it is our 23rd column we want
#next we highlight those players
V(ZN)[top1ZNeigen[,23]]$color<-"yellow"

#Now we can plot it!
plot(ZN)

#lets save this as a pdf for future reference
pdf(file=paste(t.path,"Zebra top 1 Eigen.pdf",sep="/"))
LayoutZNeigen<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top1ZNeigen[,23]]$color<-"yellow"
plot(ZN)
dev.off()

# Songbird
#First we find our top player
top1SNeigen<-SNeigen%>%top_n(1,SNeigen$vector)

LayoutSNeigen<-layout.auto(SN)
set.seed(2222020)

#node options
V(SN)$color<-"grey"
V(SN)[top1SNeigen[,23]]$color<-"yellow"

#Now we can plot it!
plot(SN)


#save as a pdf
pdf(file=paste(t.path,"Songbird top 1 Eigen.pdf",sep="/"))
LayoutSNeigen<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top1SNeigen[,23]]$color<-"yellow"
plot(SN)
dev.off()

# Graphs of 3% (same as 1 player for Zebra)

#Zebra
#First we find our top 3% of players
#total nodes in Zebra are 27, 3% comes out to.81, 
#we round up to 1 player (same as last time)
top1ZNeigen<-ZNeigen%>%top_n(1,ZNeigen$vector)


LayoutZNeigen<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top1ZNeigen[,23]]$color<-"yellow"
plot(ZN)

#save as a pdf
pdf(file=paste(t.path,"Zebra top 3 Eigen.pdf",sep="/"))
LayoutZNeigen<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top1ZNeigen[,23]]$color<-"yellow"
plot(ZN)
dev.off()


#Songbird
#First we find our top 3% of players
#total nodes in Songbirds are 117 3% comes 
#out to 3.5 we round up to 4 players

top4SNeigen<-SNeigen%>%top_n(4,SNeigen$vector)

LayoutSNeigen<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top4SNeigen[,23]]$color<-"yellow"
plot(SN)

#save as pdf
pdf(file=paste(t.path,"Songbird top 3 Eigen.pdf",sep="/"))
LayoutSN<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top4SNeigen[,23]]$color<-"yellow"
plot(SN)
dev.off()


# Graphs of 10%

#Zebra
#First we find our top 10% of players
#total nodes in Zebra are 27, 10% comes out to 2.7, 
#we round up to 3 players
top3ZNeigen<-ZNeigen%>%top_n(3,ZNeigen$vector)


LayoutZNeigen<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top3ZNeigen[,23]]$color<-"yellow"
plot(ZN)


#Save as a pdf
pdf(file=paste(t.path,"Zebra top 10 Eigen.pdf",sep="/"))
LayoutZNeigen<-layout.auto(ZN)
set.seed(2222020)
V(ZN)$color<-"grey"
V(ZN)[top3ZNeigen[,23]]$color<-"yellow"
plot(ZN)
dev.off()

#Songbird
#First we find our top 10% of players
#total nodes in Songbirds are 117 10% comes 
#out to 11.7 we round up to 12 players

top12SNeigen<-SNeigen%>%top_n(12,SNeigen$vector)

#Again note our second column is the nodelist
#next we highlight those players
LayoutSNeigen<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top12SNeigen[,23]]$color<-"yellow"
plot(SN)

#save as pdf
pdf(file=paste(t.path,"Songbird top 10 Eigen.pdf"))
LayoutSNeigen<-layout.auto(SN)
set.seed(2222020)
V(SN)$color<-"grey"
V(SN)[top12SNeigen[,23]]$color<-"yellow"
plot(SN)
dev.off()

