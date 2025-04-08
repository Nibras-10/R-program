employee_log <- data.frame(
  EmployeeID = c(101, 102, 103, 104, 105),
  LoginTime = c("08:45", "09:00", "09:15", "08:30", "09:05"),
  LogoutTime = c("17:15", "17:00", "18:00", "16:45", "17:30")
)



employee_log2<-data.frame(
  EmployeeID = c(101, 102, 103, 104, 105),
  punctuality = seq(1,5)
  
  
)

print(employee_log2)


mergeddf<-merge(employee_log,employee_log2,by='EmployeeID')

print(mergeddf)

print(employee_log)


employee_log$LoginTime<-as.POSIXct(employee_log$LoginTime,format='%H:%M')
employee_log$LogoutTime<-as.POSIXct(employee_log$LogoutTime,format='%H:%M')


employee_log$duration<-difftime(employee_log$LogoutTime,employee_log$LoginTime,units='hours')

print(employee_log)

ans<-subset(mergeddf,mergeddf$punctuality==5)

print(ans)