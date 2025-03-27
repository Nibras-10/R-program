library(ggplot2)

data1<-read.csv('C:/Users/nihal/Documents/R programs/linear.csv')
print(data)

model<-lm(formula=Y~X,data=data1)

summary(model)

years<-as.integer(readline(prompt='enter the value of x:  '))


prediction<-predict(model,newdata=data.frame(X=years))
print(prediction)

ggplot()+
  geom_point(aes(x=data1$X,y=data1$Y),color='red',)+
  geom_line(aes(x=data1$X,y=predict(model,data1)),color='blue')+
  geom_point(aes(x=years,y=prediction),color='green')+
ggtitle('X vs Y linear regression')+
  xlab('x')+
  ylab('y')

