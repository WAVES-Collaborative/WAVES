SedUp <- function(x, fs) {

  constant <- 10 #set to 10 second window


  coeffs  <- c(-2.420, 2.616, 44.083)
  uni_thr <- 0.372
  nSec    <- 6


  # trim the measurement to match desired window size
  x <- x[1:(floor(length(x)/fs/constant)*fs*constant)]

  # calculate standard deviation per second
  sdv <- apply(matrix(x, nrow = fs), 2, sd)

  # determine windows for metrics
  window1 <- nSec*constant*fs/2
  window2 <- round(nSec*constant/2)

  # allocate memory for metrics and initiate variables
  medacc  <- integer(length(sdv)/constant)
  medsd   <- integer(length(sdv)/constant)
  j1      <- 0
  j2      <- 0

  # calculate metrics
  for (i in seq(from = 1, to = length(sdv), by = constant)) {
    j1   <- j1 + 1
    i1_1 <- (i - 1) * fs + 1 - window1
    i1_2 <- (i - 1) * fs + 1 + constant * fs + window1
    b    <- i1_1:i1_2
    medacc[j1] <- median(x[b[b > 0 & b < length(x)]], na.rm = TRUE)

    j2   <- j2 + 1
    i2_1 <- (i - 1) - window2
    i2_2 <- (i - 1) + 1 + constant + window2
    b    <- i2_1:i2_2
    medsd[j2] <- median(sdv[b[b >0 & b < length(sdv)]], na.rm = TRUE)
  }

  # use Logistic Regression model parameters
  logit <- coeffs[1] + coeffs[2]*medacc + coeffs[3]*medsd
  phat <- exp(logit) / (1+exp(logit))

  # determine posture
  standing <- matrix(data=NA,nrow=length(phat),ncol=1)
  standing[phat >= uni_thr] <- 1
  standing[phat < uni_thr]  <- 0
  standing<-data.frame(standing,medacc,medsd)

  return(standing)
}

# function to compute entropy
shannon.entropy <- function(p) {
  if (any(is.na(p)) || any(is.nan(p)))
    return(NA)
  if (min(p) < 0 || sum(p) <= 0)
    return(NA)
  p.norm <-
    p[p > 0] / sum(p)
  -sum(log2(p.norm) * p.norm)
}
extract.features <- function(Axis, window_size, sf) {
  to <- window_size
  from <- 1
  l <- floor(length(Axis) / to)
  k <- matrix(0, nrow = l, ncol = 16)

  for (i in 1:l) {
    A <- Axis[from:to]
    N <- length(A)
    I <- abs(
      fft(A - mean(A)) / sqrt(N)
    )^2
    P <- (4 / N) * I
    f <- (0:(N / 2)) / N
    c <- cbind(f, P[1:(N / 2 + 1)])
    c2 <- c[1:(window_size / 10), ]
    c3 <- c[(window_size / 3 + 1):(window_size / 2 + 1), ]
    domfreq <- as.numeric(c2[which.max(c2[, 2]), 1])
    k1 <- max(c2[, 2])
    k2 <- max(c3[, 2])
    k3 <- sf * as.numeric(c2[which.max(c2[, 2]), 1])
    k4 <- sf * as.numeric(c3[which.max(c3[, 2]), 1])
    k5 <- mean(A, trim = .05, na.rm = TRUE)
    k6 <- sd(A, na.rm = TRUE)
    k7 <- sum((A * A), na.rm = TRUE)
    # Code for k8 adapted from actimetric package.  https://github.com/PhysicalActivityOpenTools/actimetric/blob/4b806607d54d49d8cded01563b7d61e55d24aca8/R/featuresTrost2017.R#L46
    # this avoids warning when no variability (e.g., idle sleep mode in actigraph)
    if (k6 != 0) {
      k8 <- cor(
        Axis[from:(to - 1)],
        Axis[(from + 1):to],
        use = "pairwise.complete.obs"
      )
    } else {
      k8 <- NA
    }
    k9 <- median(A, na.rm = TRUE)
    k10 <- mad(abs(A), na.rm = TRUE)
    k11 <- as.vector(quantile(A, c(.10), na.rm = TRUE))
    k12 <- as.vector(quantile(A, c(.25), na.rm = TRUE))
    k13 <- as.vector(quantile(A, c(.75), na.rm = TRUE))
    k14 <- as.vector(quantile(A, c(.90), na.rm = TRUE))
    k15 <- shannon.entropy(P)
    k16 <- sum(c[(window_size/3+1):(window_size/2+1),2])

    k[i,] <- c(k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15,k16)
    from <- from + window_size
    to <- to + window_size
  }
  return(k)
}
detect_sleep_periods<-function(data, window){

  sleepperiod<-rep(0,nrow(data))

  if(any(data$class==6)==T){ #determine if at least 1 sleep window was detected for participant

    sleep<-ifelse(data$class==6,1,0) #make all instances into binary sleep 1/0
    sleep<-c(0,diff(sleep))
    start<-which(sleep==1) #identify start of each sleep window
    end<-which(sleep==-1) #identify end of each sleep window
    if(!length(end)==length(start)) {end<-c(end,nrow(data))} #in case monitoring period ends during a sleep window

    for (i in 1:length(start)){ #loop through all sleep windows

      tilt<-data$tilt[start[i]:end[i]]
      sdl1<-rep(0,length(tilt))

      postch = which(abs(diff(tilt)) > 5) #identify when tilt changes by more than 5 degrees between instances
      if (length(postch) > 1) {
        s1 = which(diff(postch) > (5*(60/window))) #identify when there is more than 5 minutes between tilt changes > 5 degrees
      }else{s1=numeric(0)}
      if (length(s1) > 0) {
        for (gi in 1:length(s1)) {
          sdl1[postch[s1[gi]]:postch[s1[gi]+1]] = 1 #periods with less than 5 degree change between instances for at least 5 minutes = 1
        }
      }else{
        sdl1[1:length(sdl1)]<-0 #no periods during sleep window had less than 5 degree change between instances for at least 5 minutes
      }

      sleepperiod[start[i]:end[i]]<-sdl1
    }

  }
  return(sleepperiod)
}
trost2017.extended <- function(raw,
                               Fs,
                               ID,
                               dir_models,# mypath,
                               win,
                               sleep,Classifier = Classifier,
                               start.time = start.time) {

  # Sedup ----
  stand <-
    SedUp(raw[, 2],
          fs = Fs)
  stand_neg <-
    SedUp(raw[, 2] * (-1),
          fs = Fs)

  # sleep variables ----
  if (sleep == TRUE) {
    # https://github.com/PhysicalActivityOpenTools/actimetric/blob/4b806607d54d49d8cded01563b7d61e55d24aca8/R/runActimetric.R#L400
    s.anglez <- (
      atan(raw[, 3] / sqrt(raw[, 1]^2 + raw[, 2]^2))
      / (pi / 180)
    )
    # (atan(raw[, 3] / (sqrt(raw[, 1]^2 + raw[, 2]^2)))) / (pi/180)
    s.anglez <- slide(
      s.anglez,
      width = 5 * Fs,
      FUN = mean
    )
    s.t2 <-
      start.time + 5 * (0:(length(s.anglez) - 1))
    class(s.t2) = c('POSIXt','POSIXct')
    #s.t2<-.POSIXct(s.t2,tz='UTC')
    #s.t2<-.POSIXct(s.t2,tz=Sys.timezone())
    #class(s.t2) = c('POSIXt','POSIXct')
    s.time2 <-
      strptime(s.t2, "%Y-%m-%d %H:%M:%OS")
    s.time2[is.na(s.time2)] <-
      strptime(s.t2[is.na(s.time2)], "%Y-%m-%d")
    s.date <-
      format(s.time2, "%Y-%m-%d")
    s.t2 <-
      format(s.time2, "%H:%M:%OS")
    anglez <-
      data.frame(s.t2, s.date, s.anglez)
  }

  # While loop ----
  NR <-
    (60 / 10) * 60 * 24 * 30 #setting value equal to one month of data at 10s windows
  hold <-
    matrix(9999, NR ,51) #matrix to hold one month of data at 10s windows
  start = increment = constant =
    (Fs * 60 * 60 * 24) #variables used to read data in 24 hr increment
  LD <-
    2
  count <-
    1
  chunk <-
    1
  cat("\n")
  b <-
    round(dim(raw)[1] / Fs / 3600, digits = 2)
  a <-
    Sys.time()

  while (LD > 1) {

    if (increment > nrow(raw)) {
      increment <-
        nrow(raw)
      LDD <-
        1
    }

    if ((1 + (start * (chunk - 1))) > nrow(raw)) break

    acc <-
      raw[(1 + (start * (chunk -1))):(increment), ]
    acc <-
      na.omit(acc) #this is needed for the last day
    LD <-
      nrow(acc)

    if (LD < Fs * 10) break #need a minimum of 1 window length to extract features

    if (chunk == 1) {
      hr <-
        round(LD / Fs / 3600, digits = 2)
    } else {
      hr <-
        hr + round(LD / Fs / 3600, digits = 2)
    }

    increment <-
      increment + constant

    ## features ----
    cat(
      "Extracting features for hours",
      round((1 + (start * (chunk - 1))) / Fs / 3600, digits = 2),
      "to", hr, "out of", b, "\r"
    )

    ax <- suppressWarnings(matrix(
      acc[, 1],
      nrow  = win * Fs,
      ncol  = ceiling(dim(acc)[1] / (win * Fs)),
      byrow = FALSE
    ))
    ay <- suppressWarnings(matrix(
      acc[, 2],
      nrow  = win * Fs,
      ncol  = ceiling(dim(acc)[1] / (win * Fs)),
      byrow = FALSE
    ))
    az <- suppressWarnings(matrix(
      acc[, 3],
      nrow  = win * Fs,
      ncol  = ceiling(dim(acc)[1] / (win * Fs)),
      byrow = FALSE
    ))
    ii <-
      1
    acc2 <-
      matrix(9999, dim(ax)[2], 50)

    while (ii <= dim(ax)[2]) {

      vm <-
        sqrt(ax[, ii]^2 + ay[, ii]^2 + az[, ii]^2)
      Enmo <-
        vm - 1
      Enmo[Enmo < 0] <-
        0
      Enmo <-
        mean(Enmo)
      tilt <-
        mean(acos(ay[, ii] / vm) * (180 / pi))
      wx <-
        extract.features(ax[,ii], window_size = win * Fs, sf = Fs)
      wy <-
        extract.features(ay[,ii], window_size = win * Fs, sf = Fs)
      wz <-
        extract.features(az[,ii], window_size = win * Fs, sf = Fs)
      w <-
        as.matrix(cbind(wx,wy,wz))
      w <-
        c(w, Enmo, tilt)
      if (length(w) < 50) {
        s <-
          50 - length(w)
        s <-
          rep(0, s)
        w <-
          c(w, s)
      }
      acc2[ii, ] <-
        w
      ii <-
        ii + 1
    }

    # nonwear ----
    if (nrow(acc) >= (Fs * 60 * 60 * 1)) {

      #need at least 1hr of data to calculate nonwear
      # nw <- nonwear_vm(acc, Fs = Fs, window = win)
      nw <- detectNonWear(acc, sf = Fs, epoch = win)

      #nw<-rep(nw,each=4) #need to adjust according to window size
      if(length(nw)<nrow(acc2)){
        z<-abs(as.numeric(length(nw)-nrow(acc2)))
        z<-rep(nw[length(nw)],z)
        nw<-c(nw,z)
      }
      if(length(nw)>nrow(acc2)){
        e<-nrow(acc2)
        nw<-nw[1:e]
      }
    } else if(nrow(acc) < (Fs*60*60*1)){
      e<-which(!hold[, 29] == 9999)
      nw<-rep(hold[(e[length(e)]-1),29],nrow(acc2))
    }

    acc2<-cbind(acc2,nw)
    hold[count:(count - 1 + dim(acc2)[1]),]<-as.matrix(acc2) #putting features into matrix

    count = count + nrow(acc2)
    LD<-nrow(acc2)
    chunk=chunk+1

  }

  b<- Sys.time()-a
  cat("\n")
  cat(paste("feature extraction completed:",format(b,digits=2),"\n"))

  # Loop Cleanup ----
  cut = which(hold[, 1] == 9999)
  hold =hold[-cut, ]
  time <-
    start.time + win*(0:(nrow(hold)-1))#################### FIX WINDOW SIZE AND OVERLAP ACCORDINGLY
  class(time) = c('POSIXt','POSIXct')############################### THESE TWO LINES ARE IMPORTANT TO CONVERT NUMERIC STRING TO DATE/TIME
  #time<-.POSIXct(time,tz='UTC')
  #class(time) = c('POSIXt','POSIXct')
  cat("completed timestamp")
  time2<- strptime(time,"%Y-%m-%d %H:%M:%OS")
  time2[is.na(time2)] = strptime(time[is.na(time2)],"%Y-%m-%d")
  date<- format(time2, "%Y-%m-%d")
  time3<- paste0(" ",format(time, "%H:%M:%OS"))
  #if(length(time3)<nrow(hold)){hold<-hold[1:length(time3),]} 2020/11/19 Not needed with new timestamp approach
  raw<- data.frame(subject=ID,date=date,time=time3,hold)

  # Predict ----
  ###########################scoring file
  if(Classifier %in% "Trost Adult Wrist RF"){

    load(file.path(dir_models,"trost_var.RData")) # b
    # c(
    #   "Power253.X",
    #   "Power1315.X",
    #   "DomF253.X",
    #   "DomF1315.X",
    #   "Mean.X",
    #   "SD.X",
    #   "SigPow.X",
    #   "LAG1.X",
    #   "Median.X",
    #   "MAD.X",
    #   "P10.X",
    #   "P25.X",
    #   "P75.X",
    #   "P90.X",
    #   "Shannon.X",
    #   "Sum1315.X",
    #   "Power253.Y",
    #   "Power1315.Y",
    #   "DomF253.Y",
    #   "DomF1315.Y",
    #   "Mean.Y",
    #   "SD.Y",
    #   "SigPow.Y",
    #   "LAG1.Y",
    #   "Median.Y",
    #   "MAD.Y",
    #   "P10.Y",
    #   "P25.Y",
    #   "P75.Y",
    #   "P90.Y",
    #   "Shannon.Y",
    #   "Sum1315.Y",
    #   "Power253.Z",
    #   "Power1315.Z",
    #   "DomF253.Z",
    #   "DomF1315.Z",
    #   "Mean.Z",
    #   "SD.Z",
    #   "SigPow.Z",
    #   "LAG1.Z",
    #   "Median.Z",
    #   "MAD.Z",
    #   "P10.Z",
    #   "P25.Z",
    #   "P75.Z",
    #   "P90.Z",
    #   "Shannon.Z",
    #   "Sum1315.Z"
    # )
    load(file.path(dir_models,"trostRF_7112014.RData")) # trostRF_7112014
    rfmodel<-trostRF_7112014
    rm(trostRF_7112014)
  }

  colnames(raw) <- c(
    "subject",
    "date",
    "time",
    c(b),
    "enmo",
    "tilt",
    "nonwear"
  )
  raw <-
    do.call(data.frame,lapply(raw, function(x) replace(x, is.infinite(x),NA)))
  raw[is.na(raw)] <- 0
  class<-predict(rfmodel,raw) # c("sedentary", "stationary", "walk", "run")
  raw<- cbind(raw,class)
  raw$class<-as.character(raw$class)
  raw$Activity_orig<-raw$class

  raw<-raw[1:nrow(stand),]

  ##add SedUp classifier results to scored activity data
  raw$stand<-stand$standing
  raw$stand_neg<-stand_neg$standing

  ##loop through each day to determine device orientation based on periods of walking where arm will predominantly be swinging with hands pointed downwards
  a<-unique(raw$date)
  raw$sedentary_final<-0
  hold<-list()

  for(xyz in 1:length(a)){

    temp<-raw[raw$date%in%a[xyz],]

    orientation<-as.numeric(base::summary(temp$Median.Y[temp$class==3 ])[3])

    if(orientation%in%NA){ #if there are no walking episodes to assess orientation - default set orientation to pos
      orientation<-1
    }

    pos<-orientation>0

    if(pos==T){
      e<-which(temp$class%in%c(1,2) & temp$stand==0)
      temp$sedentary_final[e]<-1
    }
    if(pos==F){
      e<-which(temp$class%in%c(1,2) & temp$stand_neg==0)
      temp$sedentary_final[e]<-1
    }

    hold[[xyz]]<-temp

  }

  raw<-do.call(rbind,hold)

  # Sleep detection ----
  ########################sleep detection
  if(sleep==T){
    cat("\nDetecting Sleep\n")

    raw$sleep<-NA
    time<-strptime(raw$time, "%H:%M:%S")
    time<-format(time, "%H:%M:%S")
    raw$time<-time
    s.t2<-strptime(anglez$s.t2, "%H:%M:%S")
    s.t2<-format(s.t2, "%H:%M:%S")
    anglez$s.t2<-s.t2
    a<-unique(anglez$s.date)
    # https://github.com/PhysicalActivityOpenTools/actimetric/blob/4b806607d54d49d8cded01563b7d61e55d24aca8/R/classifySleep.R#L44
    b <- 1; c <- 2

    for(sss in 1: length(a)) {
      if(c>length(a)){
        break
      }
      try(
        if(c<=length(a)){z<-which(anglez$s.date==a[b]&anglez$s.t2>="12:00:00")
        z<-z[1]
        x<-which(anglez$s.date==a[c]&anglez$s.t2<="12:00:00")
        x<-x[length(x)]
        if(length(x)==0){break}
        x<-x-1
        ga<-anglez[z:x,]
        ga<-ga[is.na(ga$s.anglez)==FALSE,] #For last day when monitor is plugged in and acc is 0's
        sleepw <- inbed(
          ga$s.anglez,
          k = 60,
          bedblocksize = 30,
          outofbedsize = 30,
          ws3 = 5
        )

        if(sleepw$lightsout[1]==0){sleepw$lightsout[1]<-1}

        d1<-ga$s.date[sleepw$lightsout]
        t1<-ga$s.t2[sleepw$lightsout]
        e<-which(raw$date%in%d1 &raw$time>=t1)
        e<-e[1]
        d1<-ga$s.date[sleepw$lightson]
        t1<-ga$s.t2[sleepw$lightson]
        e1<-which(raw$date%in%d1 &raw$time<=t1)
        e1<-e1[length(e1)]
        for(zz in 1:length(e)){
          raw$sleep[e[zz]:e1[zz]]<-"s"
        }


        }, silent=T)
      b<-b+1
      c<-c+1
      cat(paste("completed sleep for day:",sss,"\r",sep=" "))

    }} else {
      raw$sleep<-0
      time<-strptim.0e(raw$time, "%H:%M:%S")
      time<-format(time, "%H:%M:%S")
      raw$time<-time
    }

  # Adjust sleep/nonwear ----
  ###########adjust sleep and nonwear based on preset 75% Decision Fusion Rule
  raw$nonwear_orig<-raw$nonwear
  hold<-ifelse(raw$sleep%in%"s",1,0)
  if(hold[1]==1){
    hold[1]<-0
  }
  hold<-c(0,diff(hold))

  st<-which(hold==1)
  if(length(st)==0){st<-1}
  end<-which(hold==-1)
  end<-end-1
  if(length(st)>length(end)){end<-c(end,length(hold))}

  for(nw in 1:length(st)){
    non<-length(which(raw$nonwear[st[nw]:end[nw]]==1))
    sl<-length(which(raw$sleep[st[nw]:end[nw]]%in%"s"))
    if(!is.na(non/sl)){
      if(non/sl<.75){raw$nonwear[st[nw]:end[nw]]<-6}}

  }

  e<-which(raw$nonwear==6)
  raw$class[e]<-6
  e<-which(raw$nonwear==1)
  raw$class[e]<-7
  if(sleep==T){
    cat("\nDetecting Sleep Periods\n")
    # https://github.com/PhysicalActivityOpenTools/actimetric/blob/4b806607d54d49d8cded01563b7d61e55d24aca8/R/classifySleep.R#L97
    raw$sleep_windows_orig<-ifelse(raw$sleep%in%'s',1,0)
    raw$sleep_periods<-detect_sleep_periods(raw,win)
  }
  else{
    raw$sleep_windows_orig<-0
    raw$sleep_periods<-0
  }

  # Extended classification ----
  mv<-which(raw$class %in% c("3") & raw$enmo * 1000 >= 100)
  mvpa<-rep(0,nrow(raw))
  mvpa[mv]<-1
  run<-rle(mvpa)
  run<-rep(run$lengths,run$lengths)
  mvpa<-data.frame(mvpa,run)
  mv<-which(mvpa$mvpa==1 & mvpa$run>=2)
  cl2<-rep(0,nrow(raw))
  cl2[mv]<-1
  e<-which(cl2==1)
  raw$class[e]<-"mpa"

  mv<-which(raw[,55 ]%in% c("3","mpa") & raw$enmo * 1000 >= 400)
  mvpa<-rep(0,nrow(raw))
  mvpa[mv]<-1
  run<-rle(mvpa)
  run<-rep(run$lengths,run$lengths)
  mvpa<-data.frame(mvpa,run)
  mv<-which(mvpa$mvpa==1 & mvpa$run>=1)
  cl2<-rep(0,nrow(raw))
  cl2[mv]<-1
  e<-which(cl2==1)
  raw$class[e]<-4

  mv<-which(raw$class%in%c("1","2") & raw$sedentary_final==1)
  raw$class[mv]<-"1"
  mv<-which(raw$class%in%c("1") & raw$sedentary_final==0)
  raw$class[mv]<-"stand_still"

  # Verisense steps don't inform the classes, rather the trost method is informing
  # the steps. Therfore remove this bit of code.
  # load(paste(output,"/scored files",folder_name,"/steps","/",ID,"_steps",".RData",sep=""))
  # min_length <- min(nrow(raw), length(wsteps))
  # raw <- raw[1:min_length, ]
  # wsteps <- wsteps[1:min_length]
  # raw$steps<-wsteps
  # raw$steps[!raw$class%in%c("2","3","4","mpa")]<-0


  raw <- subset(
    raw,
    select = c(
      subject,
      date,
      time,
      enmo,
      tilt,
      class
      # steps
    )
  )
  return(raw)
  # invisible(list(Activity = raw))


}
