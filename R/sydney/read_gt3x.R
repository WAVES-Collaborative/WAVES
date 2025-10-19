read_gt3x <- function(fileName){
  
  info.con <- unz(fileName, "info.txt")
  log.con <- unz(fileName, "log.bin", open = "rb")
  
  info <-parse_info_txt(info.con,verbose = F)
  close(info.con)
  if(info$Device_Type%in%"Link"){
    data<-read_accelerationLink(log.con,info$Acceleration_Scale,info$Sample_Rate)
    close(log.con)
 
    dat = strptime(format(info$Start_Date,"%Y-%m-%d %H:%M:%OS"),"%Y-%m-%d %H:%M:%OS")
    dat<-as.numeric(dat)
    data <- cbind(data$data) #Link read in xyz order
  }else{
    data <- read_acceleration(log.con, info$Acceleration_Scale,info$Sample_Rate)
    close(log.con)
    # Create 13-digit unix timestamp to preserve milliseconds
   
    dat = strptime(format(info$Start_Date,"%Y-%m-%d %H:%M:%OS"),"%Y-%m-%d %H:%M:%OS")
    dat<-as.numeric(dat)
    data <- cbind(data$data[,c(2, 1, 3)]) #gt3x read in yxz order
  }
  colnames(data) <- c("x", "y", "z")
  return(invisible(list(header = info, data = data,dat = dat)))
}
