#load library 
library(C50)
data(iris)
#run same model
tree_mod <- C5.0(Species ~ ., data = iris)


#view model
summary(tree_mod)

plot(tree_mod,type='simple')