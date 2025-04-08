employee_data <- data.frame(
  Emp_ID = 1:8,
  Name = c("Alice", "Bob", "Charlie", "Diana", "Eve", "Frank", "Grace", "Hank"),
  Department = c("Finance", "HR", "Finance", "IT", "Marketing", "Finance", "HR", "IT"),
  Date_of_Joining = as.Date(c("2016-05-10", "2019-03-15", "2017-07-01", "2020-01-01",
                              "2018-11-12", "2015-04-22", "2016-08-30", "2017-09-09")),
  Monthly_Salary = c(50000, 45000, 55000, 60000, 48000, 53000, 47000, 62000),
  Performance_Score = c(85, 92, NA, 70, 60, NA, 88, 55)
)

firstq<-employee_data$Name[employee_data$Department=='Finance' & format(employee_data$Date_of_Joining,'%Y')<'2018']

print(firstq)

employee_data$duration<-as.integer((Sys.Date()-employee_data$Date_of_Joining)/365)
print(employee_data)

sec1<-employee_data$Emp_ID[is.na(employee_data$Performance_Score)]
print(sec1)

depts<-employee_data$Department[is.na(employee_data$Performance_Score)]
print(depts)

x<-employee_data$Performance_Score[employee_data$Department=='Finance']
print(x)

medianvalues<-median(x,na.rm = TRUE)
employee_data$Performance_Score[sec1]<-medianvalues

print(employee_data)


employee_data$rating<-ifelse(employee_data$Performance_Score>=90,'Outstanding',
                             ifelse(       employee_data$Performance_Score>=75,'Good',
                                           ifelse(  employee_data$Performance_Score>=60,'Average',
                                                    ifelse( employee_data$Performance_Score<60,'Poor',NA))))


print(employee_data)


employee_data$High_performer<-ifelse(employee_data$Performance_Score<75,0,1)
print(employee_data)



write.csv(employee_data,'C:/Users/nihal/Documents/R programs/smallprog/employee.csv',row.names=FALSE,quote = FALSE)

model<-glm(formula=High_performer~ Monthly_Salary+Performance_Score+duration,data=employee_data,family='binomial')

summary(model)


user_input<-function(){
  
  data.frame(
    Monthly_Salary=as.numeric(readline(prompt='Enter monthly salary: ')),
    Performance_Score=as.numeric(readline(prompt='Enter performance score: ')),
    duration=as.numeric(readline(prompt='Enter years of experience: '))
    
  )
  
}


new_data <- user_input()

# Predict probability using the model
predicted_prob <- predict(model, newdata = new_data, type = 'response')

# Classify as High Performer (1) or Not (0) using threshold 0.5
predicted_class <- ifelse(predicted_prob >= 0.5, 1, 0)    

print(predicted_class)

employee_data$Predicted_Prob <- predict(model, type = 'response')

employee_data$Predicted_High_Performer <- ifelse(employee_data$Predicted_Prob >= 0.5, 1, 0)

print(prediction)

print(employee_data)