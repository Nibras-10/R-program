distance<-function(p1,p2){
  return(sqrt(sum(p1-p2)^2))
  
}

k_means<-function(data,k,max_iter=100){
  
  set.seed(123)
  
  clusters<-rep(0,nrow(data))
  
  centroids<-data[sample(1:nrow(data),k), ,drop=FALSE]
  
  
  for(iteration in 1:max_iter){
    cat('\nIteration: ',iteration,'\n')
    
    for(i in 1:nrow(data)){
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
    
    
    for (j in 1:k){
      
      cluster_points<-data[clusters==j, ,drop=FALSE]
      cat('\nCluster: ',j,'\n')
      print(cluster_points)
      cat('\n')
      
    }
    
    new_centroids<-centroids
    for (j in 1:k){
      
      if(sum(clusters==j) > 0){
        new_centroids[j, ]<-colMeans(data[clusters==j, ])
      }
      
    }
    
    if(all(centroids==new_centroids)){
      cat('iteration converged: ', iteration)
      break
    }
    
    centroids<-new_centroids
    
  }
  return (list(centroids=centroids,clusters=clusters))
  
}

data <- read.csv("C:/Users/nihal/Documents/R programs/kmeans.csv")

data<-as.matrix(data)

result<-k_means(data,k=3)
print(result)
