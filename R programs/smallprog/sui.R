sentence<-readLines('C:/Users/nihal/Documents/R programs/smallprog/file1.txt')

print(sentence)

splitted<-strsplit(sentence,' ')

print(splitted)

for (i in 1:length(splitted)){
  
  reversed<-rev(splitted[[i]])
  print(reversed)
  
  joined<-paste(reversed,collapse=' ')
  print(joined)
  
  
}

writeLines(joined,'C:/Users/nihal/Documents/R programs/smallprog/file1.txt')


