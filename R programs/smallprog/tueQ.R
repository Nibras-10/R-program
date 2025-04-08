
set.seed(123)

months<-c('January','February','March','April','May','June','July','August','September','October','November','December')

product_a<-sample(1000:10000,12,replace=TRUE)
product_b<-sample(1000:10000,12,replace=TRUE)
product_c<-sample(1000:10000,12,replace=TRUE)

data_sales<-data.frame(
  Months=months,
  Product_a=product_a,
  Product_b=product_b,
  Product_c=product_c
)


print(data_sales)

data_sales$Annual_Sales<-apply(data_sales[2:4],1,sum)
  
print(data_sales)
max_Sale<-apply(data_sales[2:4],2,which.max)

print(max_Sale)

hsale_month<-data_sales$Months[max_Sale]

print(hsale_month)

annA<-apply(data_sales[2],2,sum)
cat('annual sale for A: ',annA)

annB<-apply(data_sales[3],2,sum)
cat('annual sale for B: ',annB)

annC<-apply(data_sales[4],2,sum)
cat('annual sale for C: ',annC)


Q1<-data_sales$Months[data_sales$Annual_Sales > 20000 & (data_sales$Product_b > data_sales$Product_a)]

print(Q1)

print(data_sales)


cormatrix<-cor(data_sales[,2:5])
print(cormatrix)

covmatrix<-cov(data_sales[,2:5])
print(covmatrix)

iqr1<-apply(data_sales[,2:5],2,IQR)
print(iqr1)

model<-lm(formula=Annual_Sales~Product_a+Product_b+Product_c,data=data_sales)
print(model)

user_input<-function(){
  
  data.frame(
    Product_a=as.integer(readline(prompt='enter the value of product A: ')),
    Product_b=as.integer(readline(prompt='enter the value of product B: ')),
    Product_c=as.integer(readline(prompt='enter the value of product C: '))
    
  )
  
  
}

test_data<-user_input()

prediction<-predict(model,newdata = test_data)

print(prediction)


#accuracy part

print(data_sales)


actualpredict<-predict(model,data_sales[2:4])

print(actualpredict)

