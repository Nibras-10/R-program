# Load necessary library
library(lattice)

# Load Dataset
dataset <- read.csv('C:/Users/nihal/Documents/R programs/Student_Performance.csv')

# Convert categorical variable (Yes/No) to binary (1/0)
dataset$Extracurricular.Activities <- ifelse(dataset$Extracurricular.Activities == 'Yes', 1, 0)

# Build Multiple Linear Regression Model
model <- lm(Performance.Index ~ ., data = dataset)

# Print model summary
summary(model)

# User input function
user_input <- function() {
  data.frame( 
    Hours.Studied = as.numeric(readline(prompt = 'Enter the hours: ')),
    Previous.Scores = as.numeric(readline(prompt = 'Enter the previous: ')),
    Extracurricular.Activities = as.numeric(readline(prompt = 'Enter the extra: ')),
    Sleep.Hours = as.numeric(readline(prompt = 'Enter the sleep: ')),
    Sample.Question.Papers.Practiced = as.numeric(readline(prompt = 'Enter the solved: '))
  )
}

# Get user input
test_data <- user_input()

# Make prediction
prediction <- predict(model, newdata = test_data)
cat('Predicted value of Performance Index: ', round(prediction, 2), "\n")

# Compute R-squared
r2 <- summary(model)$r.squared
cat('R-squared value:', r2, "\n")

# Compute MSE
mse_manual <- mean((dataset$Performance.Index - predict(model, newdata = dataset))^2)
cat('Mean Squared Error (MSE):', mse_manual, "\n")

# Create a dataframe with actual and predicted values
results <- data.frame(
  Actual = dataset$Performance.Index,
  Predicted = predict(model, newdata = dataset)
)

# Lattice plot of Actual vs. Predicted values
xyplot(Predicted ~ Actual, data = results, 
       main = "Lattice Plot: Actual vs. Predicted Performance Index",
       xlab = "Actual Performance Index",
       ylab = "Predicted Performance Index",
       col = "blue", pch = 16, 
       panel = function(x, y) {
         panel.xyplot(x, y, col = "blue", pch = 1) 
         panel.abline(a = 0, b = 1, col = "red", lwd = 2) # Ideal Line (y = x)
       })


grid()
