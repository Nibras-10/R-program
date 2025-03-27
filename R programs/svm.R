library(e1071)

dataset<-read.csv('C:/Users/nihal/Documents/R programs/svmog.csv')



dataset$JobSatisfaction<-factor(dataset$JobSatisfaction,
                                levels=c('High','Medium','Low'))


dataset$WorkLifeBalance<-factor(dataset$WorkLifeBalance,
                                levels=c('Good','Average','Poor','Excellent'))

dataset$Attrition<-ifelse(dataset$Attrition=='Yes',1,0)

print(head(dataset))

model<-svm(Attrition ~ .,data=dataset,kernel='linear',type='C-classification')

summary(model)

user_input<-function( ){
  
data.frame(
  Age=as.numeric(readline(prompt='Enter the age: ')),
  Salary=as.numeric(readline(prompt='Enter the salary')),
  WorkHours=as.numeric(readline(prompt='Enter the Hours')),
  JobSatisfaction=factor(readline(prompt='Enter the satisfaction level'),
                            levels=c('High','Medium','Low')),
  WorkLifeBalance=factor(readline(prompt='Enter the worklife balance'),
                            levels=c('Good','Average','Poor','Excellent'))
  
)  
  
}


testdata<-user_input()

prediction<-predict(model,newdata=testdata)

if (prediction == 1) {
  cat("Predicted Attrition: YES (Employee is likely to leave)\n")
} else {
  cat("Predicted Attrition: NO (Employee is likely to stay)\n")
}


print(prediction)


plot(model,dataset)