library(data.tree)

entropy <- function(q) {
  # Calculate the entropy for a value.
  -1 * (q * log2(q) + (1 - q) * log2(1 - q))
}

positiveRatio <- function(data, outcomeCol = ncol(data)) {
  # Calculate the ratio of T by the total samples.
  positiveCount <- length(which(data[, outcomeCol] == T))
  sum(positiveCount / nrow(data))
}

gain <- function(data, attributeCol, outcomeCol = ncol(data), precision=3) {
  # Calculate the information gain for an attribute.
  systemEntropy <- round(entropy(positiveRatio(data, outcomeCol)), precision)
  
  positives <- data[which(data[,outcomeCol] == T),]
  negatives <- data[which(data[,outcomeCol] == F),]
  
  attributeValues <- split(data, data[,attributeCol])
  
  gains <- sum(sapply(attributeValues, function(attributeValue) {
    itemRatio <- nrow(attributeValue) / nrow(data)
    outcomeEntropy <- entropy(length(which(attributeValue[,outcomeCol] == T)) / nrow(attributeValue))
    result <- itemRatio * outcomeEntropy
    round(ifelse(is.nan(result), 0, result), precision)
  }))
  
  systemEntropy - gains
}

pure <- function(data, outcomeCol = ncol(data)) {
  length(unique(data[, outcomeCol])) == 1
}

ID3 <- function(node, data, outcomeCol = ncol(data)) {
  node$obsCount <- nrow(data)
  
  if (pure(data, outcomeCol)) {
    child <- node$AddChild(unique(data[,outcomeCol]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  } else {
    gains <- sapply(colnames(data)[-outcomeCol], function(colName) {
      gain(data, which(colnames(data) == colName), outcomeCol)
    })
    
    feature <- names(gains)[gains == max(gains)][1]
    node$feature <- feature
    
    childObs <- split(data[,!(names(data) %in% feature)], data[,feature], drop = TRUE)
    
    for(i in 1:length(childObs)) {
      child <- node$AddChild(names(childObs)[i])
      ID3(child, childObs[[i]])
    }
  }
}

# Read dataset.
data <- read.csv("C:/Users/nihal/Documents/R programs/tennis.csv", header=T)

# Convert the last column to a boolean.
data[, ncol(data)] <- ifelse(tolower(data[, ncol(data)]) == 'yes', T, F)

# Train ID3 to build a decision tree.
tree <- Node$new('Should_Play')
ID3(tree, data)
print(tree, 'feature')
plot(tree)

# Function to prompt the user for input and make a prediction
predict_play <- function(tree, data) {
  # Prompt user for input for each feature
  feature_values <- list()  # Start with an empty list
  
  for (feature in names(data)[-ncol(data)]) {
    value <- readline(paste("Enter value for", feature, ": "))
    
    # Check if the value is boolean (Yes/No)
    if (tolower(value) == 'yes') {
      feature_values[[feature]] <- TRUE
    } else if (tolower(value) == 'no') {
      feature_values[[feature]] <- FALSE
    } else {
      feature_values[[feature]] <- value  # Otherwise, treat as a string (e.g., Sunny, Hot, etc.)
    }
  }
  
  # Convert feature_values list to a data frame for easy indexing
  feature_values_df <- as.data.frame(t(feature_values))
  names(feature_values_df) <- names(data)[-ncol(data)]
  
  # Use the decision tree to make a prediction based on the user's input
  node <- tree
  while(length(node$children) > 0) {
    feature <- node$feature
    value <- feature_values_df[[feature]]
    
    # Navigate to the correct child node based on the feature's value
    node <- node$children[[as.character(value)]]
  }
  
  # The prediction is the name of the final leaf node
  prediction <- node$name
  print(paste("Prediction: ", prediction))
}

# Ask user to input data and make a prediction
predict_play(tree,data)