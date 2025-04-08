library(C50)

dataset2<-read.csv('C:/Users/nihal/Documents/R programs/smallprog/DCtree.csv')

print(dataset2)


dataset2$AgeGroup<-factor(dataset2$AgeGroup,levels = c('Young','Middle','Senior'))
dataset2$IncomeLevel<-factor(dataset2$IncomeLevel,levels = c('Low','Medium','High'))
dataset2$Employment<-factor(dataset2$Employment,levels = c('No','Yes'))
dataset2$CreditScore<-factor(dataset2$CreditScore,levels = c('Bad','Good'))
dataset2$LoanApproved<-factor(dataset2$LoanApproved, levels=c('Yes','No'))


model<-C5.0(LoanApproved ~.,dataset2)

print(model)

s<-summary(model)
print(summary(model))
accuracy<-s$apparent
print(accuracy)

plot(model,type='simple')