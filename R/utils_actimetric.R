slide <- function(x,
                  width,
                  by = NULL,
                  FUN = NULL,
                  ...) {
  FUN <- match.fun(FUN)
  if (is.null(by)) by <- width

  lenX <- length(x)
  QUT1 <- seq(1, lenX - width + 1, by = by)
  QUT2 <- lapply(QUT1, function(x) x:(x + width - 1))
  QUT3 <- lapply(QUT2, function(a) FUN(x[a], ...))
  QUT4 <- do.call(rbind,QUT3)
  return(QUT4)
}
# same as `nonwear_vm` from "sydney/nonwear_1min.R"
detectNonWear <- function(data,
                          sf,
                          epoch,
                          sdThreshold = 13/1000) {
  # original code provided by Matthew N. Ahmadi
  # look for windows with sd lower than sdThreshold, similar to GGIR.
  # vector magnitude
  vm <- sqrt(rowSums(data[, 1:3]^2))

  # 1 - NONWEAR BASED ON SD THRESHOLD -----------
  # sd crit
  vm <- as.numeric(slide(x = vm, width = epoch*sf, FUN = sd))
  vm2 <- ifelse(test = vm < sdThreshold, yes = 1, no = 0)

  # Duration (in epochs) of periods below SD threshold
  run <- rle(vm2)
  run <- rep(run$lengths, run$lengths)
  vm2 <- cbind(vm2, run)

  # SD below threshold for at least 30 minutes = nonwear
  nonwear <- rep(0, length(vm))
  epochs_in_30min <- 60 / epoch * 30
  e <- which(vm2[, 1] == 1 & vm2[, 2] >= epochs_in_30min)
  if (length(e) > 0) nonwear[e] <- 1

  # 2 - ADDITIONAL NONWEAR ------------------
  # if wear surrounded of nonwear -> nonwear
  check <- nonwear
  run <- rle(check)
  run <- as.matrix(list2DF(run))

  if (any(run[,2] == 0)) {

    # Create a table with row length equal to wear periods
    wear <- matrix(0, length(which(run[,2] == 0)), 3,
                  dimnames = list(1:length(which(run[,2] == 0)),
                                  c("before", "current", "after")))
    i <- 1; ii <- 1

    while (i <= nrow(run)) {
      if (run[i, 2] == 0) {

        # for each wear period...
        if (i > 1) wear[ii, 1] <- run[i - 1, 1] # before period
        wear[ii, 2] <- run[i, 1] # current period
        if (i < nrow(run)) wear[ii, 3] <- run[i + 1, 1] # after period

        # if wear period < 30 min and less than 30% of bordering nonwear, convert to nonwear
        nwBordering <- (wear[ii,2]/(wear[ii, 1] + wear[ii, 3]))
        if (wear[ii, 2] < epochs_in_30min & nwBordering < 0.3) run[i,2] <- 1

        # next iteration
        ii <- ii + 1
      }
      i <- i + 1
    }
    check <- rep(run[,2], run[,1])
    # sum original nonwear plus additional nonwear
    nonwear <- nonwear + check
    # any nonwear >= 1 is nonwear and make all values 1 to make binary outcome; 0/1
    nonwear[nonwear >= 1] <- 1
  }

  return(nonwear)

}
inbed = function(angle,
                 k = 60,
                 perc = 0.1,
                 inbedthreshold = 15,
                 bedblocksize = 30,
                 outofbedsize = 60,
                 ws3 = 5) {
  # Code in line with HASPT algorithm in GGIR.
  # medabsdi <- function(angle) {
  #   angvar <- stats::median(abs(diff(angle)))
  #   return(angvar)
  # }
  x <- slide(
    angle,
    width = k,
    # FUN = medabsdi,
    FUN = \(angle) stats::median(abs(diff(angle))),
    by = 1
  )
  nomov <- rep(0, length(x))
  inbedtime <- rep(NA, length(x))
  pp <- quantile(x, probs = c(perc)) * inbedthreshold

  if (pp == 0) pp <- .2
  nomov[which(x < pp)] <- 1
  nomov <- c(0, nomov, 0)
  s1 <- which(diff(nomov) == 1)
  e1 <- which(diff(nomov) == -1)
  bedblock <- which(
    (e1 - s1) > ((60 / ws3) * bedblocksize * 1)
  )

  if (length(bedblock) > 0) {
    s2 <- s1[bedblock]
    e2 <- e1[bedblock]

    for (j in 1:length(s2)) inbedtime[s2[j]:e2[j]] <- 1

    outofbed <- rep(0, length(inbedtime))
    outofbed[which(is.na(inbedtime) == TRUE)] <- 1
    outofbed <- c(0, outofbed, 0)
    s3 <- which(diff(outofbed) == 1)
    e3 <- which(diff(outofbed) == -1)
    outofbedblock <- which(
      (e3 - s3) < ((60 / ws3) * outofbedsize * 1)
    )

    if (length(outofbedblock) > 0) {
      s4 <- s3[outofbedblock]
      e4 <- e3[outofbedblock]
      if (length(s4) > 0) {
        for (j in 1:length(s4)) inbedtime[s4[j]:e4[j]] <- 1
      }
    }

    if (length(inbedtime) == (length(x) + 1)) {
      inbedtime <- inbedtime[1:(length(inbedtime) - 1)]
    }

    inbedtime2 <- rep(1, length(inbedtime))
    inbedtime2[which(is.na(inbedtime) == TRUE)] <- 0
    s5 <- which(diff(c(0, inbedtime2, 0)) == 1)
    e5 <- which(diff(c(0, inbedtime2, 0)) == -1)
    inbeddurations <- e5 - s5
    longestinbed <- which(inbeddurations == max(inbeddurations))
    lightsout <- s5[longestinbed] - 1
    lightson <- e5[longestinbed] - 1
    if(length(s5)>1){
      naplightsout <- s5[-longestinbed] - 1
      naplightson <- e5[-longestinbed] -1
    } else {
      naplightsout<-NA
      naplightson<-NA
    }
  } else {
    lightson <- c()
    lightsout <- c()
    tib.threshold <- c()
  }

  tib.threshold <- pp
  invisible(list(
    lightsout = lightsout,
    lightson = lightson,
    tib.threshold = tib.threshold,
    naplightsout = naplightsout,
    naplightson = naplightson
  ))
}

# Strip every extension; handles stacked ones like .csv.gz.
strip_all_ext <- function(x) {
  repeat {
    stripped <- tools::file_path_sans_ext(x)
    if (identical(stripped, x)) break
    x <- stripped
  }
  x
}
