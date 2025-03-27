# Function to calculate Euclidean distance
distance <- function(point1, point2) {
  return(sqrt(sum((point1 - point2)^2)))
}

# Function to perform K-Means clustering
kmeans_manual <- function(data, k, max_iter = 10) {
  set.seed(123) # For reproducibility
  
  # Randomly initialize k centroids from the data points
  centroids <- data[sample(1:nrow(data), k), ]
  
  clusters <- rep(0, nrow(data)) # To store cluster assignments
  
  for (iteration in 1:max_iter) {
    cat("\nIteration", iteration, "\n")
    
    # Assign each point to the nearest centroid
    for (i in 1:nrow(data)) {
      distances <- sapply(1:k, function(j) distance(data[i, ], centroids[j, ]))
      clusters[i] <- which.min(distances) # Assign to the nearest centroid
    }
    
    # Print cluster members as coordinate points (x, y)
    for (j in 1:k) {
      cluster_points <- data[clusters == j, , drop = FALSE] # Get points in cluster
      cat("Cluster", j, ":\n")
      print(cluster_points)  # Print points in (x, y) format
      cat("\n")
    }
    
    # Compute new centroids as the mean of assigned points
    new_centroids <- centroids
    for (j in 1:k) {
      if (sum(clusters == j) > 0) {
        new_centroids[j, ] <- colMeans(data[clusters == j, ])
      }
    }
    
    # Check for convergence (if centroids do not change)
    if (all(centroids == new_centroids)) {
      cat("\nConverged at iteration", iteration, "\n")
      break
    }
    
    centroids <- new_centroids
  }
  
  return(list(centroids = centroids, clusters = clusters))
}

# Read data from CSV file (Ensure 'data.csv' contains only numeric values)
data <- read.csv("C:/Users/nihal/Documents/R programs/kmeans.csv")

# Convert data into a numeric matrix (removing headers)
data <- as.matrix(data)

# Running the manual K-Means algorithm with k=3 clusters (You can change k)
result <- kmeans_manual(data, k = 3)
