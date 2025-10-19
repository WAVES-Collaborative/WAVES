# output      = output
# do_parallel = FALSE
# win = 15
# axis = "yaxis"
# Sed = 200
# Mod = 420
# Vig = 842
# Calibrate   = TRUE
# visual      = FALSE
# Guide       = FALSE
# sleep       = TRUE
# # dothis = dothis
# ndays = 12
# folder_name = "_test"
# file_name   = ""
# steps       = TRUE
# ellis       = TRUE
# mypath      = mypath
# input       = input

features <- function(output,
                     do_parallel=T,
                     win=15,
                     axis="yaxis",
                     Sed=200,
                     Mod=420,
                     Vig=842,
                     Calibrate=F,
                     visual=T,
                     Guide=T,
                     sleep=T,
                     dothis=dothis,
                     ndays=12,
                     folder_name="",
                     file_name="",
                     steps=F,
                     ellis=F,
                     mypath,
                     input) {

  #To do list: ndays utility, update Link to match gt3x read in, only call in functions when needed
  # custom folder names, sleep/wake, lag/lead models, timestamp, remove unused models
  list.of.packages <- c(
    "caret",
    "plyr",
    "dplyr",
    "e1071",
    "zoo",
    "data.table",
    "randomForest",
    "readxl",
    "Rcpp",
    "HMM",
    "rattle",
    "GENEAread",
    "signal",
    "GGIR",
    "GGIRread",
    "DescTools",
    "tools"
  )
  new.packages <- list.of.packages[
    !(list.of.packages %in% installed.packages()[,"Package"])
  ]
  if(length(new.packages)){
    print(paste("Installing dependancy package:",new.packages))
    install.packages(new.packages)
  }

  library(plyr)
  library(dplyr)
  library(e1071)
  library(data.table)
  library(randomForest)
  library(readxl)
  library(Rcpp)
  library(HMM)
  library(rattle)
  #library(GENEAread)
  library(signal)
  library(GGIR)
  library(GGIRread)
  #library(TLBC)
  library(DescTools)
  library(tools)
  library(caret)
  library(foreach)

  options(digits.secs=3)
  oldw <- getOption("warn")
  options(warn = -1)

  source(paste(mypath,"/center_radius.R",sep = ""))
  source(paste(mypath,"/choice.R",sep = ""))
  source(paste(mypath,"/cross corr.R",sep=""))
  source(paste(mypath,"/cwa.R",sep = ""))
  source(paste(mypath,"/detect_sleep_periods.R",sep = ""))
  source(paste(mypath,"/EllipsoidFit.R",sep = ""))
  source(paste(mypath,"/Ellis_feat_extraction.R",sep = ""))
  source(paste(mypath,"/EllisClassifier.R",sep = ""))
  source(paste(mypath,"/extract_features.r",sep = ""))
  source(paste(mypath,"/feature extraction.R",sep = ""))
  source(paste(mypath,"/freq domain.R",sep = ""))
  source(paste(mypath,"/genplot.R",sep = ""))
  source(paste(mypath,"/getbout.R",sep = ""))
  source(paste(mypath,"/GN function_20191024.R",sep = ""))
  source(paste(mypath,"/IntensityCutPoint.R",sep = ""))
  source(paste(mypath,"/lag.R",sep = ""))
  source(paste(mypath,"/mattcalibrate.R",sep = ""))
  source(paste(mypath,"/nonwear_1min.R",sep = ""))
  source(paste(mypath,"/note.R",sep = ""))
  source(paste(mypath,"/parse_txt.R",sep = ""))
  source(paste(mypath,"/partialdaysummary.R",sep = ""))
  # source(paste(mypath,"/partialdaysummary.R",sep = "")) maybe suppose to be ellis?
  source(paste(mypath,"/read_accel.R",sep = ""))
  source(paste(mypath,"/read_acceleration.R",sep = ""))
  source(paste(mypath,"/read_accelerationLink.R",sep = ""))
  source(paste(mypath,"/read_counts.R",sep = ""))
  source(paste(mypath,"/read_excel.R",sep = ""))
  source(paste(mypath,"/read_gt3x.R",sep = ""))
  source(paste(mypath,"/readax3.R",sep = ""))
  source(paste(mypath,"/readGA.R",sep = ""))
  source(paste(mypath,"/round_df.R",sep = ""))
  source(paste(mypath,"/shannon_entropy.r",sep = ""))
  source(paste(mypath,"/sleep detection.R",sep = ""))
  source(paste(mypath,"/slide.R",sep = ""))
  # source(paste(mypath,"/test.R",sep = ""))
  source(paste(mypath,"/tick.R",sep = ""))
  source(paste(mypath,"/time domain.R",sep = ""))
  source(paste(mypath,"/TrostAdultRFClassifier.R",sep = ""))
  source(paste(mypath,"/wrist_steps.R",sep = ""))

  is.wholenumber <- function(x,
                             tol = .Machine$double.eps^0.5) {
    abs(x - round(x)) < tol
  }

  CalibrateMethod <-
    "SphereFit"
  ifelse(
    !dir.exists(file.path(output, paste0("scored files", folder_name))),
    dir.create(file.path(output, paste0("scored files", folder_name))),
    FALSE
  ) #creates a file directory
  ifelse(
    !dir.exists(file.path(output, paste0("scored files", folder_name), "steps")),
    dir.create(file.path(output, paste0("scored files", folder_name), "steps")),
    FALSE
  )
  ifelse(
    !dir.exists(file.path(output, paste0("scored files", folder_name), "ellis")),
    dir.create(file.path(output, paste0("scored files", folder_name), "ellis")),
    FALSE
  )
  ifelse(
    !dir.exists(file.path(output, paste0("scored files", folder_name), "trost")),
    dir.create(file.path(output, paste0("scored files", folder_name), "trost")),
    FALSE
  )

  AG.file <-
    list.dirs(input,recursive=T)
  AG.file <- AG.file[
    which(AG.file%in%input)
  ]

  part <-
    matrix()
  part$ID <- list.files(
    AG.file,
    recursive  = FALSE,
    full.names = TRUE
  )
  part$ID <- part$ID[
    grep(".csv|.gt3x|.bin|.cwa|.rds", part$ID, fixed = FALSE)
  ]
  ct <- which(
    part$ID %in% part$ID[grep("sec.csv", part$ID, fixed = FALSE)]
  )
  if (length(ct) > 0) {
    part$ID <-
      part$ID[-ct]
  }

  if(do_parallel) {
    cores = parallel::detectCores()
    Ncores = cores[1]
    Ncores2use = max(1, Ncores-4)
    cl <- parallel::makeCluster(Ncores2use)
    doParallel::registerDoParallel(cl)
    `%myinfix%` = foreach::`%dopar%`
  }else {
    `%myinfix%` = foreach::`%do%`
  }

  errhand <-
    'stop'
  func <- c(
    'ActivityClassifier_Ellis',
    'ActivityClassifier_trost',
    'calibrate',
    'calibrateGN',
    'center_radius',
    'cross.corr',
    'cwa',
    'detect_sleep_periods',
    'EllipsoidFit',
    'Ellis.feat.extraction',
    'feature.extraction',
    'freq.domain',
    'features',
    'GenPlot',
    'getbout',
    'GN',
    'inbed',
    'IntensityCounts',
    'lag',
    'nonwear_vm',
    'parse_info_txt',
    'partialdaysummary',
    'read_acceleration',
    'read_accelerationLink',
    'read_activityC',
    'read_excel_allsheets',
    'read_gt3x',
    'read.accel',
    'read.counts',
    'readax3',
    'readGA',
    'round_df',
    'slide',
    'summary',
    'tick_to_posix',
    'time.domain',
    'summary',
    'shannon.entropy',
    'feature.extraction',
    'extract.features',
    'wrist_steps'
  )
  list.of.packages <- c(
    "plyr",
    "dplyr",
    "e1071",
    "data.table",
    "randomForest",
    "readxl",
    "Rcpp",
    "HMM",
    "GENEAread",
    "signal",
    "GGIR",
    "GGIRread",
    "DescTools",
    "tools",
    'caret'
  )
  packages2passon <-
    list.of.packages

  ##Check for already processed files
  ext <-
    list.files(file.path(output, paste0("scored files", folder_name),"trost")) |>
    basename() |>
    file_path_sans_ext()

  if (length(ext) > 0) {
    ext <-
      sub("_raw.*$", "", ext)
    ext <-
      grep(paste(ext,collapse = '|'),part$ID)
    part$ID <-
      part$ID[-ext]
  }

  cat(
    "\n\n-------",
    length(part$ID),
    "accelerometer files will be processed and scored",
    "-------\n\n"
  )

  # Loop ----
  output_list <- foreach::foreach(
    i         = seq_along(part$ID)[1:2],
    .packages = packages2passon,
    .export   = func,
    .errorhandling = errhand) %myinfix% {
      tryCatchResult <- tryCatch({

        AG <-
          part$ID[i]
        bindata <-
          tools::file_ext(AG)
        ID <-
          basename(AG)
        ID <-
          tools::file_path_sans_ext(ID)
        cat(paste("\nProcessing:",ID,"\n",sep=" "))

        # raw = AG
        readAX_jhm <- function(raw,
                               bindata) {
          I <-
            GGIR::g.inspectfile(raw)
          # Extract parameters for reading raw in chunks
          params <-
            GGIR::extract_params(params2check = "rawdata")
          readParams <- GGIR::get_nw_clip_block_params(
            monc = I$monc,
            dformat = I$dformc,
            sf = I$sf,
            params_rawdata = params$params_rawdata
          )
          isLastBlock <-
            FALSE
          blocknumber <-
            1
          iteration <-
            1
          PreviousLastValue <-
            c(0, 0, 1)
          PreviousLastTime <-
            NULL
          PreviousEndPage <-
            NULL
          S <-
            matrix(0,0,4) #dummy variable needed to cope with head-tailing succeeding blocks of data
          cat("\nReading data chunk:\n")

          while (isLastBlock == FALSE) {

            cat(blocknumber, " ")
            # 1 - read chunk
            accread <- GGIR::g.readaccfile(
              filename          = raw,
              blocksize         = readParams$blocksize,
              blocknumber       = blocknumber,
              filequality       = NULL,
              ws                = 3600,
              PreviousEndPage   = PreviousEndPage,
              inspectfileobject = I,
              PreviousLastValue = PreviousLastValue,
              PreviousLastTime  = PreviousLastTime
            )
            data <-
              accread$P$data
            blocknumber <-
              blocknumber + 1
            # PreviousLastTime = accread$PreviousLastTime; PreviousEndPage = accread$PreviousEndPage
            # isLastBlock = accread$isLastBlock; S = accread$S
            # remaining_epochs = accread$remaining_epochs; nHoursRead = accread$nHoursRead
            rm(accread); gc()

            temp_cols <-
              grep("temp|temperature", names(data), ignore.case = TRUE)
            # CHANGED BY JM: Removed "time"
            desired_cols <- c(
              # "time",
              "x", "y", "z"
            )
            all_cols <-
              c(desired_cols, names(data)[temp_cols])

            if (iteration == 1) {
              matt <-
                as.matrix(data[, all_cols])
            } else if (iteration > 1 & length(data) >= 1) {
              matt <-
                rbind(matt,
                      as.matrix(data[, all_cols]))
            }

            p <-
              nrow(data)

            if (length(p) == 0) p <- 0

            if (p < ((I$sf * 60 * 2) + 1)) { #last block
              isLastBlock <- TRUE
            }

            iteration <-
              iteration + 1

          }

          if (bindata == "gt3x") {
            a <-
              data.frame((I$header[[1]][6]))
            names(a)<-
              "start"
            a$start <-
              as.POSIXct(as.character(a$start),
                         format = "%Y-%m-%d %H:%M:%S")
            a <-
              a$start
            dat <-
              as.numeric(strptime(a, "%Y-%m-%d %H:%M:%OS"))
          } else if (bindata == "cwa") {
            a <-
              data.frame((I$header[[1]][3]))
            a <-
              a$start
            dat <-
              as.numeric(strptime(a, "%Y-%m-%d %H:%M:%OS"))
          } else if (bindata == "bin"){
            a <-
              data.frame((I$header[[1]][8]))
            a <-
              a[1,1]
            dat <-
              as.numeric(strptime(a, "%Y-%m-%d %H:%M:%OS"))
          } else if (I$monn == "actigraph" && bindata == "csv") {
            a <- paste0(
              I$header["Start Date", "value"],
              I$header["Start Time", "value"]
            )
            dat <-
              as.numeric(strptime(a, " %m/%d/%Y %H:%M:%OS"))
          }

          cat('\n')
          return(invisible(list(Fs = I$sf, data = matt, dat = dat)))

        }

        # read files ----
        hip.data <-
          readAX_jhm(AG, bindata)
        # Changed code to not include time from readAX_jhm.
        # hip.data$data <-
        #   hip.data$data[,2:4]
        # hip.data$Fs <-
        #   hip.data$Fs

        # calibrate ----
        if (Calibrate == FALSE) {

          ag <-
            "calibration = False"

        } else if (Calibrate == TRUE && CalibrateMethod == "SphereFit") {

          ag <- try(
            calibrateGN(raw = hip.data$data,
                        Fs  = hip.data$Fs),
            silent = TRUE
          )

          if (class(ag) == "try-error") {

            cat("Device Was Not Calibrated. Not Enough Orientation Changes")

          } else if (ag$vm.error.end >= ag$vm.error.st || ag$vm.error.end >= 50) {

            cat("Calibration Results Not Implemented Because Values Do Not Decrease Error")

          } else {

            # variables used to read data in 24 hr increment
            chunk_is_last <-
              FALSE
            chunk_begin <-
              1
            chunk_end <- chunk_length <-
              hip.data$Fs * 60 * 60 * 24
            chunk_n <-
              1
            nrow_data <-
              dim(hip.data$data)[1]

            while(!chunk_is_last){

              if (chunk_end >= nrow_data) {
                # if chunk is less than 24 hrs, set to end of data and make this
                # the last loop.
                chunk_end <-
                  nrow_data
                chunk_is_last <-
                  TRUE
              }

              cat(
                "\rCalibrating hours",
                round(chunk_begin / hip.data$Fs / 3600,
                      digits = 2),
                "to",
                round(chunk_end / hip.data$Fs / 3600,
                      digits = 2),
                "out of",
                round(nrow_data / hip.data$Fs / 3600,
                      digits = 2),
                "\r",
                sep = " "
              )

              ind_chunk <-
                chunk_begin:chunk_end
              hip.data$data[ind_chunk, "x"] <-
                ag$scale[1] * (hip.data$data[ind_chunk, "x"] - ag$offset[1])
              hip.data$data[ind_chunk, "y"]<-
                ag$scale[2] * (hip.data$data[ind_chunk, "y"] - ag$offset[2])
              hip.data$data[ind_chunk, "z"]<-
                ag$scale[3] * (hip.data$data[ind_chunk, "z"] - ag$offset[3])

              chunk_begin <-
                chunk_begin + chunk_length
              chunk_end <-
                chunk_begin + chunk_length - 1
              chunk_n <-
                chunk_n + 1

            }
          }
        }

        cat("\n")

        # steps ----
        if (steps == TRUE) {

          cat("steps...\n")
          wsteps <- wrist_steps(
            input_data = hip.data$data,
            fs         = hip.data$Fs,
            win        = 10
          )
          save(
            wsteps,
            file = file.path(
              output, paste0("scored files", folder_name), "steps",
              paste0(ID, "_steps", ".RData")
            )
          )

        }

        # Ellis RF/HMM ----
        if ( ellis == TRUE){

          cat("Ellis...\n")
          classifier <- ActivityClassifier_Ellis(
            raw        = hip.data$data,
            Fs         = hip.data$Fs,
            ID         = ID,
            mypath     = mypath,
            win        = 60,
            Classifier = "Ellis Wrist RF",
            sleep      = sleep,
            start.time = hip.data$dat
          )
          classifier <- list(
            Activity = classifier$Activity,
            FS       = hip.data$Fs
          )
          save(
            classifier,
            file = file.path(
              output , paste0("scored files", folder_name), "ellis",
              paste0(ID,"_raw.scored",".RData")
            )
          )
        }

        # Trost Adult RF Wrist ----
        cat("Trost...\n")
        classifier <- ActivityClassifier_trost(
          raw         = hip.data$data,
          Fs          = hip.data$Fs,
          ID          = ID,
          mypath      = mypath,
          win         = 10,
          sleep       = sleep,
          Classifier  = "Trost Adult Wrist RF",
          start.time  = hip.data$dat,
          # output      = output,
          folder_name = folder_name
        )
        classifier <- list(
          Activity = classifier$Activity,
          FS       = hip.data$Fs
        )
        save(
          classifier,
          file = file.path(
            output, paste0("scored files", folder_name), "trost",
            paste0(ID,"_raw.scored",".RData")
          )
        )

        cat("\nCompleted Activity Classification\n")
        rm(hip.data);rm(classifier);rm(wsteps)

      })
      return(tryCatchResult)
    }

  if(do_parallel) {
    parallel::stopCluster(cl)
  }

  # logged error and warning messages
  for (oli in 1:length(output_list)) {
    if (is.null(unlist(output_list[oli])) == FALSE) {
      cat(paste0("\nErrors and warnings for ",fnames[oli]))
      print(unlist(output_list[oli])) # print any error and warnings observed
    }
  }

  # unlink(file.path(paste(output, "/scored files", folder_name, "/steps", sep = "")), recursive = TRUE)

}
