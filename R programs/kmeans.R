# Load necessary library
library(ggplot2)

# Read data from CSV
data <- read.csv("C:/Users/nihal/Documents/R programs/kmeans.csv")

# Set the number of clusters
k <- 3  # Change based on requirement

# Run K-Means clustering
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(data, centers = k, iter.max = 10, nstart = 1)

# Print cluster assignments at each iteration
cat("Final Cluster Assignments:\n")
for (cluster in 1:k) {
  cat("\nCluster", cluster, ":\n")
  print(data[which(kmeans_result$cluster == cluster), ])
}

data$cluster<-as.factor(kmeans_result$cluster)

print(data$cluster)

ggplot(data, aes(x = x, y = y, color = cluster)) +
  geom_point(size = 3) +  # Scatter plot of data points
  geom_point(data = as.data.frame(kmeans_result$centers), aes(x = x, y = y), 
             color = "black", shape = 8, size = 4) +  # Mark cluster centers
  labs(title = "K-Means Clustering", color = "Cluster") +
  theme_minimal()

