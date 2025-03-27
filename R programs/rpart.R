# Load necessary libraries
library(rpart)
library(rpart.plot)  # For better plotting

# Read the dataset
dataset1 <- read.csv("C:/Users/nihal/Documents/R programs/ogtree.csv")

# Convert categorical columns to factors
dataset1$InternetUsage <- factor(dataset1$InternetUsage, levels = c("Low", "Medium", "High"))
dataset1$ExamAnxiety <- factor(dataset1$ExamAnxiety, levels = c("Low", "Medium", "High"))
dataset1$Performance <- factor(dataset1$Performance, levels = c("Low", "Medium", "High"))

# Train the ID3-like Decision Tree model using entropy
model <- rpart(Performance ~ ., data = dataset1, method = "class", 
               parms = list(split = "information"))  # Forces ID3-style splitting (entropy)

# Plot the decision tree (C5.0-like style)
rpart.plot(model, type = 1, main = "Decision Tree (ID3-like)",box.palette="blue")

# Print model summary
print(model)
