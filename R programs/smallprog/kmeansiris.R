# Load iris dataset
data(iris)

# Select only Sepal.Length and Sepal.Width
iris_data <- iris[, c("Sepal.Length", "Sepal.Width")]

# Perform K-means clustering with k=3
set.seed(123)
kmeans_result <- kmeans(iris_data, centers = 3)

print(kmeans_result$cluster)

# Add the cluster result to the original data
iris$Cluster <- kmeans_result$cluster

print(iris$Cluster)
print((iris))

# Create a mapping from cluster to majority species
cluster_labels <- rep(NA, 3)  # To store the majority label for each cluster

for (i in 1:3) {
  species_in_cluster <- iris$Species[iris$Cluster == i]
  most_common <- names(sort(table(species_in_cluster), decreasing = TRUE))[1]
  cluster_labels[i] <- most_common
}

print(cluster_labels)
# Predict species based on majority label in each cluster
predicted_species <- cluster_labels[iris$Cluster]

print(predicted_species)

# Create confusion matrix
conf_matrix <- table(Predicted = predicted_species, Actual = iris$Species)
print("Confusion Matrix:")
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", round(accuracy * 100, 2), "%\n")

print((iris))
