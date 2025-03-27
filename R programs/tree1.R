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

# Completely revised prediction function
predict_play <- function(tree, data) {
  # Prompt user for input for each feature
  feature_values <- list()
  
  for (feature in names(data)[-ncol(data)]) {
    value <- readline(paste("Enter value for", feature, ": "))
    feature_values[[feature]] <- value  # Store original input
  }
  
  # Convert feature_values list to a data frame
  feature_values_df <- as.data.frame(t(feature_values))
  names(feature_values_df) <- names(data)[-ncol(data)]
  
  # Use the decision tree to make a prediction
  current_node <- tree
  
  # Navigate through the tree until we reach a leaf node
  while (!is.null(current_node) && length(current_node$children) > 0) {
    feature <- current_node$feature
    
    # Check if we've reached a node with no feature (might be a leaf)
    if (is.null(feature) || feature == "") {
      break
    }
    
    # Get the value for this feature from user input
    if (!feature %in% names(feature_values_df)) {
      print(paste("Warning: Feature", feature, "not found in input data"))
      break
    }
    
    value <- as.character(feature_values_df[[feature]])
    print(paste("Looking for child with value:", value))
    print(paste("Available children:", paste(names(current_node$children), collapse=", ")))
    
    # Try to find matching child node
    found_match <- FALSE
    for (child_name in names(current_node$children)) {
      if (tolower(child_name) == tolower(value)) {
        current_node <- current_node$children[[child_name]]
        found_match <- TRUE
        break
      }
    }
    
    if (!found_match) {
      print(paste("No exact match found for", feature, "=", value))
      print("Taking the first available option instead")
      current_node <- current_node$children[[1]]
    }
  }
  
  # We've reached a leaf node or a node with no valid children
  # If this is a TRUE/FALSE node, it's our prediction
  if (current_node$name %in% c("TRUE", "FALSE")) {
    print(paste("Prediction:", ifelse(current_node$name == "TRUE", "Yes", "No")))
  } else {
    # If we somehow ended up at a non-leaf node, check its children
    if (length(current_node$children) > 0) {
      # Take the first child as default
      leaf_node <- current_node$children[[1]]
      print(paste("Prediction:", ifelse(leaf_node$name == "TRUE", "Yes", "No")))
    } else {
      # If no children but also not TRUE/FALSE, use the node name
      print(paste("Reached node:", current_node$name))
      print("Cannot determine prediction")
    }
  }
}

# Ask user to input data and make a prediction
predict_play(tree, data)