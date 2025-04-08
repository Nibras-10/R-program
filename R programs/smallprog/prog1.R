


fibonacci<-function(n){
  a<-0
  b<-1
  c<-numeric(n)
  for (i in 1:n){
    c[i]<-a
    s<-a+b
    a<-b
    b<-s
    
}
    return(c)
}


n<-as.integer(readline(prompt='Enter the range: '))
s<-fibonacci(n)

