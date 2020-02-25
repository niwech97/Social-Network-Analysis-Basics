################################################################################

#                              MAIN SCRIPT                                     #

################################################################################

#----------------------------TABLE OF CONTENTS----------------------------------

# - Overview
# - Packages downloaded
# - Libraries
# - Data Download 
# - Workflow 
# - Source the rest of the code

################################################################################

#---------------------------------OVERVIEW--------------------------------------

#The following contents within this main directory include the packages, 
#libraries, dataset and the workflow that is used in this analysis. For the 
#purpose of this analysis the scripts should be run in the folling order to gain 
#replicatory results:

#    1. MainScript:(This included data organization with the overview of the 
#                   workflow used in this project)
#
#    2. LearningTechniques: (This includes the dataset we used and the analyses 
#                           that we ran to produce the graphs and numerical values)
#
#
#    3. PracticingTechniques: (This includes our creation of our own dataset and 
#                              the analyses we used on this dataset)
#
#    4. TestingTechniques: (This includes the analyses we ran on a dataset 
#                           provided to us)

#------------------------------PACKAGE DOWNLOAD---------------------------------

# This is a list of the package used for these analyses
if(!require(igraph)){
  install.packages("igraph")
}
if(!require(readr)){
  install.packages("readr")
}
if(!require(haven)){
  install.packages("haven")
}
if(!require(ggplot2)){
  install.packages("ggplot2")
}
if(!require(dplyr)){
  install.packages("dplyr")
}
#---------------------------------LIBRARIES-------------------------------------

# Load the packages in the libraries 

#load libraries
library(igraph)
library(readr)
library(haven)
library(ggplot2)
library(dplyr)

#------------------------------BUILD DIRECTORY----------------------------------
#Set our working directory 

working.dir <- getwd()

# Create the folders we will use
output.folders <- c("1.Learn.tech","2.Pract.tech","3.Test.tech", "4. Data.sets")

# Check to see if the folders exist in the working directory and if they don't, 
# use the following loop. 
# The following loop checks the output.folders list and checks to see 
# if the folders exist in the working directory. If they don't it will create 
# them. 

# Make the folders using this loop code 
for(i in 1:length(output.folders)) 
  if(file.exists(output.folders[i]) == FALSE) 
    dir.create(output.folders[i])

#-------- Pathways----------

# The following is a directory of the pathways to each of our output folders

# Path to 1.Learn.tech
l.path <- paste(working.dir,"/",output.folders[1], "/", sep="")

# Path to 2.Pract.tech
p.path <- paste(working.dir,"/",output.folders[2], "/", sep="")

# Path to 3.Test.tech
t.path <- paste(working.dir,"/",output.folders[3], "/", sep="")

# Path to 4.Data.Set
d.path <- paste(working.dir,"/",output.folders[4], "/", sep="")


#--------------------------------DATA DOWLOAD-----------------------------------

# The following are the raw data files used in the practicing techniques section 
# of this analysis which was found from a youtube channel
#https://www.youtube.com/watch?v=XNw-DZFsFYA&list=PL1M5TsfDV6Vs7tnHGNgowEUwJW-O8QVp5&index=4&fbclid=IwAR0tI0OQna4jte30rqZTZJLHdiiA49viGI2gB6xOnoJ1m_DwXfpT3zcZcpo

CN_edgelist <- read.csv(paste(d.path,"ColleaugueNetwork.csv", sep = "/"))
DN_edgelist <- read.csv(paste(d.path,"DiscussionNetwork.csv", sep = "/"))
CNA <- read.csv(paste(d.path,"ColleaugueAttributes.csv", sep = "/"))
DNA <- read.csv(paste(d.path,"DiscussionAttributes.csv", sep = "/"))

#The following are the datasets we used for the testing techniques section of 
#this analysis. We retrieved this data from 
#https://github.com/jdm286/Animal-Social-Structure-Network-Repository?fbclid=IwAR1_6eLcJ5Kivlzyn_E7b3cpY_NNSNTdC4M2hbW5Oqt0Juq9yhicKd5xtuw

#This next dataset is our Songbird Social network
SN <- read_graph(paste(d.path,"weighted_network_social_songbird.graphml",
                       sep="/"),format=c("graphml"))
#This next dataset is our Zebra Social network
ZN <-read_graph(paste(d.path,"UNweighted_zebra_interaction.graphml",
                      sep="/"),format=c("graphml"))


#-----------------------------------WORKFLOW------------------------------------

# In our working directory we created 4 pathways to different folders with 
# specific outputs 

#           - 1. Learn.tech  -> the path to this folder is: l.path
#                 (This folder contains the outputs we produced from learning   
#                  these social analysis techniques)
#
#           - 2. Pract.tech -> the path to this folder is: p.path
#                 (This folder contains the outputs we produced from our test 
#                 dataset)
#
#           - 3. Test.tech -> the path to this folder is: t.path
#                 (This folder contains the outputs we produced from our real 
#                 dataset)
#
#           - 4. Data.sets -> th path to this folder is d.path
#                 (This folder contains the raw data used in both our learning 
#                 techniques and our testing techniques sections)

#-----------Source the other scripts----------- 

source("1. LearningTechniques.R")
source("2. PracticingTechniques.R")
source("3. TestingTechniques.R")
source("5. FinalTechniques.R")

#################################END MAIN SCRIPT################################
