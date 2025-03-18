library(rpart)
library(rpart.plot)
library(party)  # For alternative visualization

# Function to calculate entropy of a dataset
calculateEntropy <- function(data, target) {
  # Get unique classes and their frequencies
  class_counts <- table(data[[target]])
  
  # If all examples belong to one class, entropy is 0
  if(length(class_counts) <= 1) {
    return(0)
  }
  
  class_probs <- class_counts / sum(class_counts)
  
  # Calculate entropy
  entropy <- -sum(class_probs * log2(class_probs))
  return(entropy)
}

# Function to calculate information gain
calculateInformationGain <- function(data, feature, target) {
  # Calculate total entropy
  total_entropy <- calculateEntropy(data, target)
  
  # Get unique values of the feature
  feature_values <- unique(data[[feature]])
  
  # Calculate weighted entropy of subsets
  weighted_entropy <- 0
  for (value in feature_values) {
    subset_data <- data[data[[feature]] == value, ]
    weight <- nrow(subset_data) / nrow(data)
    subset_entropy <- calculateEntropy(subset_data, target)
    weighted_entropy <- weighted_entropy + (weight * subset_entropy)
  }
  
  # Calculate information gain
  information_gain <- total_entropy - weighted_entropy
  return(information_gain)
}

# Function to find the best feature to split on
findBestFeature <- function(data, features, target) {
  best_gain <- -1
  best_feature <- NULL
  
  for (feature in features) {
    gain <- calculateInformationGain(data, feature, target)
    
    # Check if gain is NA
    if (!is.na(gain) && gain > best_gain) {
      best_gain <- gain
      best_feature <- feature
    }
  }
  
  return(best_feature)
}

# Function to create ID3 decision tree
buildID3Tree <- function(data, features, target) {
  # Check if all examples have the same class
  if (length(unique(data[[target]])) == 1) {
    return(list(type = "leaf", value = as.character(data[[target]][1])))
  }
  
  # Check if there are no features left
  if (length(features) == 0) {
    # Return the most common class
    majority_class <- names(which.max(table(data[[target]])))
    return(list(type = "leaf", value = majority_class))
  }
  
  # Find the best feature to split on
  best_feature <- findBestFeature(data, features, target)
  
  # If no best feature found (possible with all NA values), return majority class
  if (is.null(best_feature)) {
    majority_class <- names(which.max(table(data[[target]])))
    return(list(type = "leaf", value = majority_class))
  }
  
  # Create a node for the best feature
  tree <- list(type = "node", feature = best_feature, children = list())
  
  # Get unique values of the best feature
  feature_values <- unique(data[[best_feature]])
  
  # Create child nodes for each value of the best feature
  for (value in feature_values) {
    subset_data <- data[data[[best_feature]] == value, ]
    
    # Skip if subset is empty
    if (nrow(subset_data) == 0) {
      # Find the most common class in the parent dataset
      majority_class <- names(which.max(table(data[[target]])))
      tree$children[[as.character(value)]] <- list(type = "leaf", value = majority_class)
    } else {
      # Recursively build subtree
      remaining_features <- setdiff(features, best_feature)
      subtree <- buildID3Tree(subset_data, remaining_features, target)
      tree$children[[as.character(value)]] <- subtree
    }
  }
  
  return(tree)
}

# Function to print the decision tree
printTree <- function(tree, indent = "") {
  if (tree$type == "leaf") {
    cat(indent, "-> ", tree$value, "\n")
  } else {
    cat(indent, tree$feature, "\n")
    for (value in names(tree$children)) {
      cat(indent, "  ", value, " ")
      printTree(tree$children[[value]], paste0(indent, "    "))
    }
  }
}

# Function to predict class for new data using our ID3 tree
predictID3 <- function(tree, new_data) {
  if (tree$type == "leaf") {
    return(tree$value)
  }
  
  feature_value <- as.character(new_data[[tree$feature]])
  
  # Check if the feature value is in the tree
  if (is.null(tree$children[[feature_value]])) {
    # Handle unseen feature values - return the most common class at this node
    return("Unknown")
  }
  
  return(predictID3(tree$children[[feature_value]], new_data))
}

# Function to get user input
get_user_input <- function(prompt, valid_options) {
  cat(prompt)
  user_input <- scan(what = character(), n = 1, quiet = TRUE)
  
  while (!(user_input %in% valid_options)) {
    cat("Invalid input. Please enter one of: ", paste(valid_options, collapse = ", "), "\n")
    cat(prompt)
    user_input <- scan(what = character(), n = 1, quiet = TRUE)
  }
  
  return(user_input)
}

# Main function to run the program
main <- function() {
  # Read the dataset from CSV
  data <- read.csv("C:/Users/nihal/Documents/R programs/tennis.csv", stringsAsFactors = TRUE)
  print("Dataset loaded successfully:")
  print(head(data))
  
  # Define target variable
  target <- "PlayTennis"
  
  # Get feature names
  features <- setdiff(names(data), target)
  
  # Build ID3 tree
  id3_tree <- buildID3Tree(data, features, target)
  
  # Print the tree structure
  cat("\nDecision Tree Structure:\n")
  printTree(id3_tree)
  
  # Use rpart with control parameters to force splitting
  rpart_model <- rpart(PlayTennis ~ ., data = data, method = "class",
                       control = rpart.control(minsplit = 2, cp = 0.001))
  
  # Print the rpart model details
  print(rpart_model)
  
  # Plot the decision tree with enhanced parameters
  cat("\nGenerating Decision Tree Plot...\n")
  rpart.plot(rpart_model, extra = 106, box.palette = "GnBu", 
             shadow.col = "gray", nn = TRUE, type = 4, clip.right.labs = FALSE,
             main = "ID3 Decision Tree for Tennis Dataset",cex=0.9,tweak=0.55)
  
  cat("\n=== Make a prediction for new data ===\n")
  
  # Create a data frame for new data
  new_data <- data.frame(matrix(ncol = length(features), nrow = 1))
  colnames(new_data) <- features
  
  # Get input for each feature
  for (feature in features) {
    # Show possible values
    possible_values <- levels(data[[feature]])
    cat("\nPossible values for", feature, ":", paste(possible_values, collapse = ", "), "\n")
    
    # Get and validate input
    prompt <- paste0("Enter value for ", feature, ": ")
    user_input <- get_user_input(prompt, possible_values)
    
    # Set the value in the new data frame
    new_data[[feature]] <- factor(user_input, levels = levels(data[[feature]]))
    
    # Confirm the input
    cat("You entered:", user_input, "for", feature, "\n")
  }
  
  # Make prediction using ID3 implementation
  prediction <- predictID3(id3_tree, new_data)
  cat("\nPrediction using custom ID3 implementation:", prediction, "\n")
  
  # Make prediction using rpart for comparison
  rpart_prediction <- predict(rpart_model, new_data, type = "class")
  cat("Prediction using rpart:", as.character(rpart_prediction), "\n")
  
}


main()
