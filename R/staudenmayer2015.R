pow.625 <- function(vm) {
  mods <- Mod(fft(vm))
  mods <- mods[-1]
  n <- length(mods)
  n <- floor(n/2)
  freq <- 80*(1:n)/(2*n)
  mods <- mods[1:n]
  inds <- (1:n)[(freq>0.6)&(freq<2.5)]
  pow625 <- sum(mods[inds])/sum(mods)
  mods[is.na(mods)] <- 0
  if (sd(vm)==0)
    pow625 <- 0
  return(pow625)
}

dom.freq <- function(vm) {
  if(length(vm)==1)
    return(NA)
  mods <- Mod(fft(vm))
  mods <- mods[-1]
  n <- length(mods)
  n <- floor(n/2)
  freq <- 80*(1:n)/(2*n)
  mods <- mods[1:n]
  dom.ind <- which.max(mods)
  d.f <- as.vector(freq[which.max(mods)])
  return(d.f)
}

frac.pow.dom.freq <- function(vm) {
  mods <- Mod(fft(vm))
  mods <- mods[-1]
  n <- length(mods)
  n <- floor(n/2)
  freq <- 80*(1:n)/(2*n)
  mods <- mods[1:n]
  rat <- max(mods)/sum(mods)
  mods[is.na(mods)] <- 0
  if (sd(vm)==0)
    rat <- 0
  return(rat)

}
staudenmayer2015 <- function(df,freq=100) {
  win.width <- 15

  n <- dim(df)[1]

  mins <- ceiling(n/(freq*win.width))  # this is really the number of 15-sec windows in the file

  df$min <- rep(1:mins,each=win.width*100)[1:n]
  df$VM <- sqrt(df$AxisX^2 + df$AxisY^2 + df$AxisZ^2)
  df$v.ang <- 90*(asin(df$AxisX/df$VM)/(pi/2))
  df_Staud <- data.frame(
    mean.vm  = tapply(df$VM,df$min,mean,na.rm=T),
    sd.vm    = tapply(df$VM,df$min,sd,na.rm=T),
    mean.ang = tapply(df$v.ang,df$min,mean,na.rm=T),
    sd.ang   = tapply(df$v.ang,df$min,sd,na.rm=T),
    p625     = tapply(df$VM,df$min,pow.625),
    dfreq    = tapply(df$VM,df$min,dom.freq),
    ratio.df = tapply(df$VM,df$min,frac.pow.dom.freq)
  )

  # apply the models (estimates are for each 15 second epoch)

  # MET estimates by random forest
  df_Staud$METs.rf <- predict(rf.met.model,newdata=df_Staud)
  df_Staud$METs.rf[df_Staud$sd.vm<.01] <- 1

  # MET estimates by linear regression
  df_Staud$METs.lm <- predict(lm.met.model,newdata=df_Staud)
  df_Staud$METs.lm[df_Staud$sd.vm<.01] <- 1

  # MET level estimates (rf and tree)
  df_Staud$MET.lev.rf <- predict(rf.met.level.model,newdata=df_Staud)
  df_Staud$MET.lev.tr <- predict(tr.met.level.model,newdata=df_Staud,type="class")

  # sedentary or not estimates (rf and tree)
  df_Staud$sed.rf <- predict(rf.sed.model,newdata=df_Staud)
  df_Staud$sed.tr <- predict(tr.sed.model,newdata=df_Staud,type="class")

  # locomotion or not estimates (rf and tree)
  df_Staud$loc.rf <- predict(rf.loc.model,newdata=df_Staud)
  df_Staud$loc.tr <- predict(tr.loc.model,newdata=df_Staud,type="class")

  # append model estimates back to main data frame

  METs.rf = rep(df_Staud$METs.rf, each = win.width)
  METs.lm = rep(df_Staud$METs.lm, each = win.width)
  MET.lev.tr = rep(df_Staud$MET.lev.tr, each = win.width)
  MET.lev.rf = rep(df_Staud$MET.lev.rf, each = win.width)
  sed.rf = rep(df_Staud$sed.rf, each = win.width)
  sed.tr = rep(df_Staud$sed.tr, each = win.width)
  loc.rf = rep(df_Staud$loc.rf, each = win.width)
  loc.tr = rep(df_Staud$loc.tr, each = win.width)

  junk <- rep("sed",length(df_Staud$METs.rf))
  junk[df_Staud$METs.rf>1.5] <- "light"
  junk[df_Staud$METs.rf>=3] <- "mvpa"


  return(rep(junk,each=15)[1:floor(n/100)])
}
