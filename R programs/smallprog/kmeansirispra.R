data(iris)

dataset<-iris[,1:2]
print(dataset)

set.seed(123)

kmeans_result<-kmeans(dataset,centers=3)

print(kmeans_result)

dataset$cluster<-kmeans_result$cluster

iris$cluster<-kmeans_result$cluster

print(dataset)

cluster_labels<-rep(NA,3)

for(i in 1:3){
  cluster_in_species<-iris$Species[iris$cluster==i]
  majority<-names(sort(table(cluster_in_species),decreasing = TRUE))[1]
  cluster_labels[i]<-majority

}

print(cluster_labels)

predicted_species<-cluster_labels[iris$cluster]

print(predicted_species)

conf_matrix<-table(predicted=predicted_species,actual=iris$Species)
print(conf_matrix)

accuracy<-sum(diag(conf_matrix))/sum(conf_matrix)

cat('accuracy is : ',round(accuracy,2)*100,'%')



