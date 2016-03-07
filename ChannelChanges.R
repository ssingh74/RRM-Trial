ChannelChanges <- function(directory) {
  files_list <- list.files(directory, full.names=TRUE)
  
  total_files <- length(files_list)
  #print(total_files)
  
  summarydata <- matrix(nrow=total_files, ncol=2)
  
  colnames(summarydata) <- c("# of APs", "# of channel changes")

  
  for (i in 1:total_files) {
    perHr <- read.csv(files_list[i])
    summarydata[i,2] <- sum(perHr[,6] != perHr[,5], na.rm = TRUE)
    summarydata[i,1]<-sum(!is.na(perHr[,6]))
  }
  
  averagechannelchanges <- round(mean(summarydata[,2]),2)
  plot(summarydata[,2],type = "l",ylab = "# of APs changing channel",
       xlab = "Hour instance", 
       main = paste("Avg. # of channel changes = ", averagechannelchanges))
  
  write.csv(summarydata, file = "../channelchangesummary.csv")
  
 }
