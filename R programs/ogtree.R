# Load required library
library(C50)

# Read the dataset
dataset1 <- read.csv('C:/Users/nihal/Documents/R programs/ogtree.csv')

# Convert categorical variables to factors with specified levels
dataset1$InternetUsage <- factor(dataset1$InternetUsage, levels = c("Low", "Medium", "High"))
dataset1$ExamAnxiety <- factor(dataset1$ExamAnxiety, levels = c("Low", "Medium", "High"))
dataset1$Performance <- factor(dataset1$Performance, levels = c("Low", "Medium", "High"))

# Train the decision tree model
model <- C5.0(Performance ~ ., data = dataset1)

# Display model summary
summary(model)

# Function to take user input
user_input <- function() {
  # Get numerical inputs
  study_hours <- as.numeric(readline(prompt = "Enter Study Hours: "))
  sleep_hours <- as.numeric(readline(prompt = "Enter Sleep Hours: "))
  
  # Get categorical inputs
  internet_usage <- readline(prompt = "Enter Internet Usage (Low/Medium/High): ")
  exam_anxiety <- readline(prompt = "Enter Exam Anxiety (Low/Medium/High): ")
  
  # Convert input into a dataframe
  data.frame(
    StudyHours = study_hours,
    SleepHours = sleep_hours,
    InternetUsage = factor(internet_usage, levels = c("Low", "Medium", "High")),
    ExamAnxiety = factor(exam_anxiety, levels = c("Low", "Medium", "High"))
  )
}

# Get user input
new_data <- user_input()

# Make prediction
prediction <- predict(model, new_data, type='class')

# Print the predicted performance
cat("Predicted Performance:", as.character(prediction), "\n")
