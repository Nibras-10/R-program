library(e1071)
library(ggplot2)
library(dplyr)

# Load Dataset
dataset <- read.csv('C:/Users/nihal/Documents/R programs/svmog.csv')

# Convert categorical variables
dataset$JobSatisfaction <- factor(dataset$JobSatisfaction, levels = c('High','Medium','Low'))
dataset$WorkLifeBalance <- factor(dataset$WorkLifeBalance, levels = c('Good','Average','Poor','Excellent'))
dataset$Attrition <- as.numeric(dataset$Attrition == 'Yes')  # Convert to binary (1 for Yes, 0 for No)

print(head(dataset))

# Train SVM model
model <- svm(Attrition ~ Age + Salary, data = dataset, kernel = 'radial', type = 'C-classification')
summary(model)

# Function to take user input
user_input <- function() {
  data.frame(
    Age = as.numeric(readline(prompt = 'Enter the age: ')),
    Salary = as.numeric(readline(prompt = 'Enter the salary: '))
  )  
}

testdata <- user_input()
prediction <- predict(model, newdata = testdata)

if (prediction == 1) {
  cat("Predicted Attrition: YES (Employee is likely to leave)\n")
} else {
  cat("Predicted Attrition: NO (Employee is likely to stay)\n")
}

print(prediction)

# Create grid for decision boundary
age_seq <- seq(min(dataset$Age), max(dataset$Age), length.out = 100)
salary_seq <- seq(min(dataset$Salary), max(dataset$Salary), length.out = 100)
grid <- expand.grid(Age = age_seq, Salary = salary_seq)
grid$Prediction <- as.numeric(predict(model, newdata = grid))  # Convert factors to numeric

# Contour Plot
ggplot(dataset, aes(x = Age, y = Salary, color = as.factor(Attrition))) +
  geom_point(size = 3) +
  geom_contour(data = grid, aes(x = Age, y = Salary, z = Prediction), bins = 1, color = "black", lwd = 1.2) +
  labs(title = "SVM Decision Boundary (RBF Kernel)", x = "Age", y = "Salary", color = "Attrition") +
  theme_minimal()
 