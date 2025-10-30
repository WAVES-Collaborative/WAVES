Ellis.feat.extraction <- function(w, Fs) {
  g = matrix(0, nrow(w), 3)
  x = 0.9
  g[1, ] = (1 - x) * w[1, ]
  for (n in 2:nrow(w)) {
    g[n, ] = x * g[n - 1] + (1 - x) * w[n, ]
  }
  g = g[Fs:nrow(g), ]
  gg = colMeans(g)
  w = w - gg
  v = sqrt(rowSums(w^2))
  fMean = mean(v)
  fStd = sd(v)
  if (fMean > 0) {
    fCoefVariation = fStd/fMean
  }
  else {
    fCoefVariation = 0
  }
  fMedian = median(v)
  fMin = min(v)
  fMax = max(v)
  f25thP = quantile(v, 0.25)[[1]]
  f75thP = quantile(v, 0.75)[[1]]
  a = acf(v, plot = FALSE)
  fAutocorr = which.max(abs(a$acf[2:length(a$acf)]))/(nrow(w)/Fs)
  if ((sd(w[, 3]) > 0) & (sd(w[, 2]) > 0)) {
    fCorrxy = cor(w[, 3], w[, 2])
  } else {
    fCorrxy = 0
  }
  if ((sd(w[, 3]) > 0) & (sd(w[, 1]) > 0)) {
    fCorrxz = cor(w[, 3], w[, 1])
  } else {
    fCorrxz = 0
  }
  if ((sd(w[, 2]) > 0) & (sd(w[, 1]) > 0)) {
    fCorryz = cor(w[, 2], w[, 1])
  } else {
    fCorryz = 0
  }
  if (is.na(fCorrxy)) fCorrxy = 0
  if (is.na(fCorrxz)) fCorrxz = 0
  if (is.na(fCorryz)) fCorryz = 0
  fAvgRoll = mean(atan2(w[, 2], w[, 1]))
  fAvgPitch = mean(atan2(w[, 1], w[, 3]))
  fAvgYaw = mean(atan2(w[, 2], w[, 3]))
  fSdRoll = sd(atan2(w[, 2], w[, 1]))
  fSdPitch = sd(atan2(w[, 1], w[, 3]))
  fSdYaw = sd(atan2(w[, 2], w[, 3]))
  fRollG = atan2(gg[2], gg[1])
  fPitchG = atan2(gg[1], gg[3])
  fYawG = atan2(gg[2], gg[3])
  s = signal::specgram(v, n = length(v), Fs = Fs)
  S = abs(s$S)
  f = S/max(S)
  freq = s$f
  f1 = f[freq >= 0.1]
  freq1 = freq[freq >= 0.1]
  fFmax = freq1[which.max(f1)]
  fPmax = max(f1)
  band = f[freq > 0.3 & freq < 3]
  fPmaxBand = max(band)
  freqband = freq[freq > 0.3 & freq < 3]
  fFmaxBand = freqband[which.max(band)]
  fEntropy = -sum(f * log(f))
  s = specgram(v, n = round(Fs), Fs = Fs)
  S = abs(s$S)
  f = S/max(S)
  freq = s$f
  f = rowSums(f)/ncol(f)
  FFT0 = f[1]
  FFT1 = f[2]
  FFT2 = f[3]
  FFT3 = f[4]
  FFT4 = f[5]
  FFT5 = f[6]
  FFT6 = f[7]
  FFT7 = f[8]
  FFT8 = f[9]
  FFT9 = f[10]
  FFT10 = f[11]
  FFT11 = f[12]
  FFT12 = f[13]
  FFT13 = f[14]
  FFT14 = f[15]
  return(c(fMean, fStd, fCoefVariation, fMedian, fMin, fMax,
           f25thP, f75thP, fAutocorr, fCorrxy, fCorrxz, fCorryz,
           fAvgRoll, fAvgPitch, fAvgYaw, fSdRoll, fSdPitch, fSdYaw,
           fRollG, fPitchG, fYawG, fFmax, fPmax, fFmaxBand, fPmaxBand,
           fEntropy, FFT0, FFT1, FFT2, FFT3, FFT4, FFT5, FFT6,
           FFT7, FFT8, FFT9, FFT10, FFT11, FFT12, FFT13, FFT14))
}
# raw         = mtx_data
# Fs          = I$sf
# ID          = fnm_sans_ext
# dir_models  = dir_models
# win         = 10
# sleep       = TRUE
# Classifier  = "Ellis Wrist RF"
# start.time  = rec_start_sec

ellis2016.wrist <- function(raw,
                            Fs,
                            ID,
                            dir_models,
                            win,
                            Classifier,
                            sleep,
                            start.time) {

  # sleep variables ----
  if (sleep == TRUE) {
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
    s.t2<-start.time + 5*(0:(length(s.anglez)-1))
    class(s.t2) = c('POSIXt','POSIXct')
    #s.t2<-.POSIXct(s.t2,tz='UTC')
    #s.t2<-.POSIXct(s.t2,tz=Sys.timezone())
    #class(s.t2) = c('POSIXt','POSIXct')
    s.time2<- strptime(s.t2,"%Y-%m-%d %H:%M:%OS")
    s.time2[is.na(s.time2)] = strptime(s.t2[is.na(s.time2)],"%Y-%m-%d")
    s.date<- format(s.time2, "%Y-%m-%d")
    s.t2<- format(s.time2, "%H:%M:%OS")

    anglez<-data.frame(s.t2,s.date,s.anglez)
  }

  # While loop ----
  NR = (60/60)*60*24*30
  hold<- matrix(9999,NR,44) #matrix to hold one month of data at 60s windows
  start = increment = constant = (Fs*60*60*24) #variables used to read data in 24 hr increment
  LD<-2
  count<-1
  chunk<-1
  cat("\n")
  b<-round(dim(raw)[1]/Fs/3600,2)
  a<- Sys.time()

  while (LD>1) {

    if(increment>nrow(raw)){increment<-nrow(raw);LDD<-1}
    if((1+(start *(chunk -1)))>nrow(raw)){break}
    acc = raw[(1+(start *(chunk -1))):(increment) ,]
    acc<-na.omit(acc) #this is needed for the last day
    LD<-nrow(acc)
    if(LD<Fs*60){#need a minimum of 1 window length to extract features
      break
    }
    if (chunk == 1) {
      hr<-round(((LD/Fs)/3600),2)
    }
    else {
      hr<-hr+round(((LD/Fs)/3600),2)
    }

    increment = increment+constant

    # features ----
    cat(paste("Extracting features for hours",round((1+(start *(chunk -1)))/Fs/3600,2),"to",hr,"out of",b,"\r", sep = " "))
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
    ii=1
    acc2<-matrix(9999,dim(ax)[2],43)

    while(ii <=dim(ax)[2]){

      vm <- sqrt((ax[,ii]^2) + (ay[,ii]^2) + (az[,ii]^2))
      Enmo<- vm - 1
      Enmo[Enmo < 0] <- 0
      Enmo <- mean(Enmo)
      tilt<- mean(acos(ay[,ii] / vm) * (180 / pi))
      # The actimetric function is the same as the provided code in R/sydney,
      # but in the actimetric package its applied on a sliding window. Implement?
      # https://github.com/PhysicalActivityOpenTools/actimetric/blob/4b806607d54d49d8cded01563b7d61e55d24aca8/R/ExtractFeatures.R#L61
      w <- actimetric::featuresEllis2016(
        x = ax[, ii],
        y = ay[, ii],
        z = az[, ii],
        sf = Fs
      )
      # w <- as.matrix(cbind(ax[,ii],ay[,ii],az[,ii]))
      # w <- Ellis.feat.extraction(w,Fs)
      w <- c(w,Enmo,tilt)
      if(length(w)<43){
        s<-43-length(w)
        s<-rep(0,s)
        w<-c(w,s)
      }
      acc2[ii,]<-w
      ii<-ii+1
    }

    # nonwear ----
    if(nrow(acc)>=(Fs*60*60*1)) { #need at least 1hr of data to calculate nonwear
      # nw<-nonwear_vm(acc,Fs=Fs,window=win)
      nw <-
        actimetric::detectNonWear(acc, sf = Fs, epoch = win)

      #nw<-rep(nw,each=4) #need to adjust according to window size
      if(length(nw)<nrow(acc2)){
        z<-abs(as.numeric(length(nw)-nrow(acc2)))
        z<-rep(nw[length(nw)],z)
        nw<-c(nw,z)
      }
      if(length(nw)>nrow(acc2)){
        e<-nrow(acc2)
        nw<-nw[1:e]
      }}
    if(nrow(acc)<(Fs*60*60*1)){
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
  cut = which(hold[, 1] == 9999)
  hold =hold[-cut, ]
  time<-start.time + win*(0:(nrow(hold)-1))#################### FIX WINDOW SIZE AND OVERLAP ACCORDINGLY
  class(time) = c('POSIXt','POSIXct')############################### THESE TWO LINES ARE IMPORTANT TO CONVERT NUMERIC STRING TO DATE/TIME
  cat("completed timestamp")
  time2<- strptime(time,"%Y-%m-%d %H:%M:%OS")
  time2[is.na(time2)] = strptime(time[is.na(time2)],"%Y-%m-%d")
  date<- format(time2, "%Y-%m-%d")
  time3<- paste0(" ",format(time, "%H:%M:%OS"))
  #if(length(time3)<nrow(hold)){hold<-hold[1:length(time3),]} 2020/11/19 Not needed with new timestamp approach
  raw<- data.frame(subject=ID,date=date,time=time3,hold)
  ###########################scoring file
  cat("\nClassifying Activity\n")

  if(Classifier%in%"Ellis Wrist RF"){
    load(file.path(dir_models, "ellis_var.RData"))
    load(file.path(dir_models, "Ellis.Wrist.RF.HMM.RData"))
    rfmodel<-rf
    rm(rf)
    hmmmodel<-hmm
    rm(hmm)

  }

  colnames(raw)<-c("subject","date","time",c(b),"enmo","tilt","nonwear")
  raw<-do.call(data.frame,lapply(raw, function(x) replace(x, is.infinite(x),NA)))
  raw[is.na(raw)] <- 0
  testDat<-raw[,4:44]
  testDat = predict(rfmodel$preProcValues, testDat)
  class<-predict(rfmodel,testDat)
  raw[,4:44]<-testDat
  #post = posterior(hmmmodel, class)
  #filtered = as.character(factor(max.col(t(post)), levels = 1:length(hmmmodel$States),
  #                               labels = hmmmodel$States))
  filtered = viterbi(hmmmodel, class)
  raw<- cbind(raw,class,filtered)
  raw$class<-as.character(raw$class)
  raw$filtered<-as.character(raw$filtered)
  raw$Activity_orig<-raw$class
  raw$HMM_orig<-raw$filtered


  ########################sleep detection
  if(sleep==T){
    cat("\nDetecting Sleep Window\n")

    raw$sleep<-NA
    time<-strptime(raw$time, "%H:%M:%S")
    time<-format(time, "%H:%M:%S")
    raw$time<-time
    s.t2<-strptime(anglez$s.t2, "%H:%M:%S")
    s.t2<-format(s.t2, "%H:%M:%S")
    anglez$s.t2<-s.t2
    a<-unique(anglez$s.date)
    b<-1
    c<-2

    for(sss in 1: length(a))
    {
      if(c>length(a)){
        break
      }
      try(if(c<=length(a)){z<-which(anglez$s.date==a[b]&anglez$s.t2>="12:00:00")
      z<-z[1]
      x<-which(anglez$s.date==a[c]&anglez$s.t2<="12:00:00")
      x<-x[length(x)]
      if(length(x)==0){break}
      x<-x-1
      ga<-anglez[z:x,]
      ga<-ga[is.na(ga$s.anglez)==FALSE,] #For last day when monitor is plugged in and acc is 0's
      sleepw<-actimetric::inbed(ga$s.anglez,outofbedsize = 30,ws3=5,bedblocksize = 30,k=60)

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


      },silent=T)
      b<-b+1
      c<-c+1
      cat(paste("completed Sleep Window for day:",sss,"\r",sep=" "))

    }} else {
      raw$sleep<-0
      time<-strptime(raw$time, "%H:%M:%S")
      time<-format(time, "%H:%M:%S")
      raw$time<-time
    }

  ###########adjust sleep and nonwear based on preset 75% Decision Fusion Rule
  raw$nonwear_orig<-raw$nonwear
  hold<-ifelse(raw$sleep%in%"s",1,0)

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
  #######################
  e<-which(raw$nonwear==6)
  raw$class[e]<-6
  e<-which(raw$nonwear==1)
  raw$class[e]<-7

  if(sleep==T){
    cat("\nDetecting Sleep Periods\n")
    raw$sleep_windows_orig<-ifelse(raw$sleep%in%'s',1,0)
    raw$sleep_periods<-detect_sleep_periods(raw,win)
  } else{
    raw$sleep_windows_orig<-0
    raw$sleep_periods<-0
  }
  raw<-subset(raw,select=-c(nonwear,filtered,sleep))
  return(raw)
  # invisible(list(Activity = raw))

  ######################################################
}

