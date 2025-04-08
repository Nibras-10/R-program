library(e1071)

svmdataset<-read.csv('C:/Users/nihal/Documents/R programs/smallprog/newSVM.csv')

svmdataset$Class<-factor(svmdataset$Class,levels = c('A','B','C'))

print(svmdataset)

model<-svm(Class ~.,svmdataset,type='C-classification',kernel='radial')

print(model)

user_input<-function(){
  data.frame(
    
    Feature1=as.integer(readline(prompt='Enter the value for Feature 1: ')),
    Feature2=as.integer(readline(prompt='Enter the value for Feature 2: ')),
    Feature3=as.integer(readline(prompt='Enter the value for Feature 3: '))
  
  )
  
}


test_data<-user_input()


prediction<-predict(model,newdata = test_data)

plot(model, svmdataset, Feature1 ~ Feature3,
    grid=50,slice=list(Feature2=mean(svmdataset$Feature2))
# Increase grid density for smoother boundaries
    )

summary(model)

