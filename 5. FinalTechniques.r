#----------------------FINAL TECHNIQUES-------------------------------
#In this document, we will create a function that can be used to identify a specificed
#number of targets that should be immunized if a communicable disease were to enter the 
#specified network.
#We also will point you to a source with many networks that you can test

   #EXPLANATION OF FUNCTION
#the major differences between this and identify.targets is that its defaulted to betweenness
#additionally, it has been simplified and uses a vcount()function to reduce the number of 
#arguements that need to be inputed into the function

immunize.targets<-function(network,number.of.targets){
  VN<-vcount(network)
  long.targets<- give.nodelists(betweenness,network,VN) 
  final.targets<- top.node(long.targets,number.of.targets)
  layout.t2<-layout.auto(network)                               
  set.seed(1234)                                               
  V(network)$color="grey"                                      
  V(network)[final.targets[,2]]$color<-"yellow"
  V(network)$size=15
  plot(network)
}
#It works!
immunize.targets(AN,6)


#as stated in 3. Testing.Techniques.r, that function is great if you have a specified number of doses
#but if you need know how important it is to immunize each node, you want a graph with nodes
#that have a size of their betweenness metric

#We can make a function that does that with minimal input as well!

    #EXPLANATION OF FUNCTION
#I used the same streamlining tricks to minimze input in this function as well, but I added
#a bit of manipulatability by means of this x. Sometimes, the size of the nodes is too big to 
#aesthetically represent their importance and igraph will return errors. This is fixed with using
#1 as the normal value of x and if it has issues, you can scale it up until you find a working graph

immunize.size<-function(network,x){     
  VN<-vcount(network)
  long.targets<-give.nodelists(betweenness,network,VN)            
  layout.t3<-layout.auto(network)                               
  set.seed(1234)                                               
  V(network)$color="yellow"                                    
  V(network)$size=betweenness(network)/x
  plot(network)                                                
}

#it works!
immunize.size(AN,1)    #this didn't look great so we scale up
immunize.size(AN,10)   #looks great!



#the following link has tons of networks that can be downloaded and read right into igraph:
#http://networkrepository.com/
#you can use the above functions to analyze any network on this website!



NG<-read_graph(paste(d.path,"weighted_network_ant_Lauren_colony1_day7.graphml",sep="/"),
               format=c("graphml"))
plot(NG)

immunize.targets(NG,20)
immunize.size(NG,100)
