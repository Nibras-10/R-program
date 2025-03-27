library(nnet)

# Load the dataset
multidata <- read.csv('C:/Users/nihal/Documents/R programs/multinom.csv')

# Train multinomial logistic regression model
model <- multinom(Class ~ ., data = multidata)

# Display model summary
summary(model)

# Function to take user input
user_input <- function() {
  data.frame(
    Feature1 = as.numeric(readline(prompt = 'Enter the f1: ')),
    Feature2 = as.numeric(readline(prompt = 'Enter the f2: ')),
    Feature3 = as.numeric(readline(prompt = 'Enter the f3: '))
  )  
}

# Get user input
r <- user_input()

# Make predictions
pred1 <- predict(model, multidata)
prediction <- predict(model, r)

# Print user input prediction
print(prediction)

# Calculate accuracy
accuracy <- mean(multidata$Class == pred1)

# Correct accuracy printing
cat('Accuracy:', round(accuracy, 2), '\n')
