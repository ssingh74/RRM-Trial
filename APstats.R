APStats <- function(directory) {
  files_list <- list.files(directory, full.names=TRUE)
  
  total_files <- length(files_list)
  Total_APs <- 600
  
  #print(total_files)
  
  summarydata <- matrix(nrow=total_files, ncol=3)
  
  colnames(summarydata) <- c("Mean_Local_BSS_Util", "Mean_Ch_Util", 
                              "Total_Client")
  
  alldata <- matrix(ncol=3)
  
  colnames(alldata) <- c("ALL_Local_BSS_Util", "ALL_Ch_Util", 
                             "ALL_ACC")
  
  
  for (i in 1:total_files) {
    perHr <- read.csv(files_list[i])
    summarydata[i,1] <- mean(perHr[,2])
    summarydata[i,2] <- mean(perHr[,3])
    summarydata[i,3] <- sum(perHr[,11], na.rm = TRUE)
    
    tempdata<-cbind(perHr[,2],perHr[,3],perHr[,7])
    
    alldata<-rbind(alldata,tempdata)
    
  }
  
  #Remove rows with NA values
  alldata<-alldata[complete.cases(alldata),]
  
  write.csv(summarydata, file = "../summary.csv")
  write.csv(alldata, file = "../alldata.csv")
  
  plot(summarydata[,1],type="l", main = "Hourly Local BSS Util Avg.")
  plot(summarydata[,2],type="l", main = "Hourly Total Ch Util Avg.")
  plot(summarydata[,3],type="l", main = "Total connected clients")
  
  print(summary(alldata))
  
  hist(alldata[,1], freq = FALSE, main="Local_BSS", xlab="% util", col="orange")
  lines(density(alldata[,1], na.rm=TRUE), col="blue",lwd = "2")
  
  hist(alldata[,2], freq = FALSE, main="Total_Ch_Util", 
       xlab="% util", col="orange")
  lines(density(alldata[,2], na.rm=TRUE), col="blue",lwd = "2")
  
  hist(subset(alldata[,3], alldata[,3]<1000000), freq = FALSE, main="ACC")
  lines(density(alldata[,3], na.rm=TRUE), col="blue",lwd = "2")
  
  plot(density(alldata[,1], na.rm=TRUE), main="Local_BSS")
  plot(density(alldata[,2], na.rm=TRUE, adjust = 1), main="Total_Ch_Util")
  plot(density(alldata[,3], na.rm=TRUE, from =1, to=1000000), main="ACC")
  
 }
