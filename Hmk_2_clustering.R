
############################################# Part 1 ############################################################

#getwd()
#setwd("C:\\Users\\Louanès\\Documents\\Homework DAMI 2")
#function that computes distance
mydistance <- function(a,b,metric){
  
  if(metric == "euclidian"){
    dist <- sqrt(sum((a-b)^2))
  }else if(metric =="manhattan"){
    dist <- sum(abs(a-b))
  }
  
  return(dist)
}

#check with the respective vectors

a=c(4.2,7.1)
b=c(3.1,5.8)

mydistance(a,b,"euclidian")
mydistance(a,b,"manhattan")

############################################ Part 2 ###########################################

####################################### part A

load("cluster.Rdata")

kMean <- function(x,k,max.iter=20){
  
  debug=0
  
  #1step : Randomly assign each observations (from dataframe x) a cluster within the 1:K possible clusters
  
  #Assign a cluster to each row 
  randomAssignment <- sample(1:k,nrow(x),replace = TRUE)
  
  #add a column to the original dataframe containing these random assignement
  
  dataframeWithClusterID <- cbind(x,ClusterID = randomAssignment)
  
  
  #2Step : Start the loop
  
  #Initialize variable iteration
  iteration=1
  
  #plot the data
  plot(dataframeWithClusterID[,1:2])
  
  #We want to assign new cluster ID to each observation, based on which cluster centroid it is nearest to
  
  #Break conditoon : iteration = max.iterations or clusterID doesnt change anymore.
  
  
  while(TRUE){
    
    ###create a new object "centroid" that holds the mean of all datapoints within its cluster
    
    #create a matrix that has exactly K rows
    #columns of the matrix correspond to the first dataframe X
    #For each cluster we have a table that holds the coordinates of the observations
    
    centroid <-matrix(rep(0,times=k*ncol(x)),nrow=k,ncol=ncol(x))
    if (debug ==1){
    print("centroid matrix initialization")
    print(centroid)
    }
    
    ##Now that we have our clusters filled with datapoints
    
    #We'll compute the mean of observations for each cluster
    
    for(i in 1:k){
      
      #Store row id's of observations in cluster i
      
      IdOfObservationI <- which(dataframeWithClusterID$ClusterID==i)
      
      if(debug==1){
        print("obs")
        print(IdOfObservationI)
        print("loop number")
        print(i)}
      
      #compute the mean of observations contained in ead cluster
      
      #and store it in each centroid value from the matrix
      centroid[i,]<-colMeans(x[IdOfObservationI,])
      
      if(debug==1){
      print(centroid)
      }
     }
    
    
    ###### Plot the centroids
    points(centroid[,1:2],pch=20,cex = 3)
    
    #### Ask user to enter input to continue
    readline(prompt = "Press Enter to continue:")
    
    
    ################################# Calculate the euclidian distance between each observations and centroids
    ## Create new matrix : distFromCentroid
    ## nbcol = k and nrow = number of observations (nrow(x))
    ## each column stores the distance of all observation to this specific cluster (column cluster)
    
    #initialize the matrix
    distFromCentroid <- matrix(rep(0,times = k*nrow(x)),nrow = nrow(x),ncol=k)
    
    
    ## start computing
    
    for(i in 1:nrow(x)){
      
      for(j in 1:nrow(centroid)){
        #mydistance(x[i,1:2],centoid[j,1:2],"euclidian")
        
        distFromCentroid[i,j] <- mydistance(x[i,],centroid[j,],"euclidian")
        
      }
     
    }
    if(debug==1){
    print("this is the distance matrix used to store the euclidian distance from each point to its centroid")
    print(distFromCentroid)
    } 
    ##Now that we have our distance matrix filled,find for each observations which cluster is the nearest
    
    NewObservationsClusterID <- apply(distFromCentroid,1,which.min)
    if(debug==1){
    print("New observations")
    print(NewObservationsClusterID)
    }
    
    #Break conditions : if cluster Id of observations doesnt change anymore, or if we reach max.iterations
    
    if(debug==1){
      print("before new obs")
      print(dataframeWithClusterID)}
    if(all(NewObservationsClusterID == dataframeWithClusterID$ClusterID)){
      finalClusterObservation <- NewObservationsClusterID
      finalCentroidsMatrix <- centroid
      break
    }
       else if(iteration>max.iter){
         break
       }
    
    ## Otherwise, assign the NewObservations to clusterID column
    else{
    #print("before new obs")
    print("Still looking for better cluster")
    dataframeWithClusterID$ClusterID <- NewObservationsClusterID
    iteration=iteration + 1 
    #print("after new obs")
    #print(dataframeWithClusterID)
    }
    
    
  }
  
  if(debug==1){
    print("after new obs")
    print(dataframeWithClusterID)
  }
  ## Plot the final clusters after last iteration
  
  cat("this the final output after",iteration,"iterations\n")
  plot(dataframeWithClusterID[,1:2],col=dataframeWithClusterID$ClusterID)
  points(centroid[,1:2],pch=20,cex=3,col = 1:k)
  
  
  ##Return the list of cluster whitch should contain the final observations and return the list of final centroid
  
  return(list("cluster assignment"=finalClusterObservation, "centroid matrix"=finalCentroidsMatrix))
  
  
}

#head(cluster.data)
kMean(cluster.data,3,max.iter=20)

########################## Part B ############## Elbow Plot #############"

## Elbow heuristic to determine which K is best to use
#initialize vector to store within ss values for different K

withinSS<-numeric(7)

## run Kmean algorithm 7 times
#And store values of withinSS

for(k in 1:7){
  
  km<-kmeans(cluster.data,k,iter.max = 20)
  withinSS[k]<-km$withinss[k]
  }
#print(withinSS)
plot(x=1:7,y=withinSS,type="b",xlab="Number of centers K",ylab="Total within Sum of Squares",main="Elbow curve")

### The best K to take is K = 3, because it is the last point before the curve stagnate in its changing direction == Infection point 

########################################### Part 3 ###############################################

####################### K-Medoids Clustering ####################################
library(cluster)
kmedoid <- pam(cluster.data,3,metric = "manhattan")
#kmedoid$id.med
kmedoid$medoids
#kmedoid$clustering
#kmedoid$id.med
#kmedoid$objective
#kmedoid$isolation


plot(cluster.data,col=kmedoid$clustering,main="K medoids",pch=as.character(kmedoid$clustering))
points(kmedoid$medoids,pch=as.character(unique(kmedoid$clustering)),cex=4)
#kmedoid$silinfo

######### Part B ######################" Compute silhouette for each clustering (from 2:5)


dfSil<-data.frame("K"=2:5,"Silhouette"=numeric(4))
dfSil

for(k in 2:5){
kmedoid<-pam(cluster.data,k)
dfSil$Silhouette[k-1]<-kmedoid$silinfo$avg.width
}

print(dfSil)

# Based on the table above, what value of K gives the best result
# [The Highest silhouette, so the value of K that gives best result is K=3]

#The silhouette computes the similarity between the different clusters. The distance from each observation of a cluster to its center ; is compared to the distances computed in other clusters
## How many distances are we gonna have at the end ? 


################################################### Part 4 ##################################################
######## Hierarichal clustering ################################################

### part A
##create smaller dataset of 20 observations and apply hierarichal function provided by R
set.seed(101)
randomSample <- sample(nrow(cluster.data),size = 20)
randomSample

smallDataset <- cluster.data[randomSample,]
smallDataset

rownames(smallDataset) <-1:20
smallDataset[,1]


#Perform clustering
#compute distance metrics

distMatrix<-dist(smallDataset,"euclidian")
distMatrix
smallDataset

hierarchicalClustering<-hclust(distMatrix,method = "average")
#Plot dendogram
plot(hierarchicalClustering,hang = 0.1,main="Hierarchical CLustering")

##Cut dendogram to obtain 3 clusters
clusters<-cutree(hierarchicalClustering,k = 3)
plot(smallDataset, col=clusters,pch=as.character(clusters))

     