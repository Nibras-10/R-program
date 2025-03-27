# Load necessary library
library(C50)

# Function for Decision Tree Program
run_decision_tree_program <- function() {
  # Load the dataset
  play_tennis_data <- read.csv("C:/Users/nihal/Documents/R programs/tennis.csv")
  
  # Convert categorical columns to factors
  play_tennis_data$Outlook <- factor(play_tennis_data$Outlook, levels = c("Sunny", "Overcast", "Rain"))
  play_tennis_data$Temperature <- factor(play_tennis_data$Temperature, levels = c("Hot", "Mild", "Cool"))
  play_tennis_data$Humidity <- factor(play_tennis_data$Humidity, levels = c("High", "Normal"))
  play_tennis_data$Wind <- factor(play_tennis_data$Wind, levels = c("Weak", "Strong"))
  play_tennis_data$PlayTennis <- factor(play_tennis_data$PlayTennis, levels = c("No", "Yes"))
  
  # Train a C5.0 decision tree model
  model <- C5.0(PlayTennis ~ Outlook + Temperature + Humidity + Wind, data = play_tennis_data)
  
  s<-summary(model)
  
  accuracy<-s$apparent
  print(accuracy)
  
  # Function for custom user input
  get_user_input <- function() {
    cat("Enter the following options:\n")
    
    cat("1. Outlook (Sunny, Overcast, Rain): ")
    outlook <- readline()
    
    cat("2. Temperature (Hot, Mild, Cool): ")
    temperature <- readline()
    
    cat("3. Humidity (High, Normal): ")
    humidity <- readline()
    
    cat("4. Wind (Weak, Strong): ")
    wind <- readline()
    
    return(data.frame(
      Outlook = factor(outlook, levels = c("Sunny", "Overcast", "Rain")),
      Temperature = factor(temperature, levels = c("Hot", "Mild", "Cool")),
      Humidity = factor(humidity, levels = c("High", "Normal")),
      Wind = factor(wind, levels = c("Weak", "Strong"))
    ))
  }
  
  # Visualize the decision tree using C5.0's built-in plot function
  plot(model, main = "Decision Tree Visualization", type = "simple")
  
  # Get user input and make a prediction
  cat("Provide input values for prediction:\n")
  test_data <- get_user_input()
  prediction <- predict(model, test_data)
  cat(paste("Predicted Class:", prediction, "\n"))
}

# Run Decision Tree Program
run_decision_tree_program()


