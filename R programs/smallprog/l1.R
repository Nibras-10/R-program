distance<-function(p1,p2){
  
  return(sqrt(sum(p1-p2)^2))
  
}


k1<-function(data,k,max_iter=100){
  
  set.seed(123)
  clusters<-rep(0,nrow(data))
  centroids<-data[sample(1:nrow(data),k), ,drop=FALSE]
  
  for (iteration in 1:max_iter){
    
    cat('Iteration: ',iteration,'\n')
    
    for (i in 1:nrow(data)){
      
      distances<-numeric(k)
      for (j in 1:k){
        
        point<-data[i, ]
        centroid<-centroids[j, ]
        d1<-distance(point,centroid)
        distances[j]<-d1
        
      }
      min_dist<-which.min(distances)
      clusters[i]<-min_dist
      
      
    }
    
    for(j in 1:k){
      cluster_points<-data[clusters==j, ,drop=FALSE]
      cat('cluster: ',j,'\n')
      print(cluster_points)
      cat('\n')
      
    }
    
    new_centroid<-centroids
    for(j in 1:k){
      
      if(sum(centroids)>0){
        new_centroid[j, ]<-colMeans(data[clusters==j, ])
        
      }
      
    }
    
    
    
    if(all(centroids==new_centroid)){
      cat('Iteration converged at: ',iteration)
      break
    }
    centroids<-new_centroid
    
  }
  
  return(list(centroids=centroids,clusters=clusters))
  
}


data <- read.csv("C:/Users/nihal/Documents/R programs/kmeans.csv")
data<-as.matrix(data)

print(data)
r<-k1(data,k=3)
print(r)

plot(data[,1], data[,2], col = as.factor(r$clusters), pch = 19, main = "K-Means Clustering")
points(r$centroids[,1], r$centroids[,2], col = "black", pch = 8, cex = 2)  # Plot centroids

                