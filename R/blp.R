# fpa_raw = vct_raw_poly[19]
secbysec_blp_poly <- function(fpa_raw) {

  key_behavior <- c(
    "SL- Sleep" 	                                                     = "SL-sleep",
    "PC- sleep"	                                                   	   = "SL-sleep",
    "PC- groom, health-related"	     	                                 = "PC-groom",
    "PC- other personal care" 	                                       = "PC-other",
    "HA- housework"		                                                 = "HA-housework",
    "HA- food prep and cleanup"		                                     = "HA-foodprep",
    "HA- interior maintenance, repair, & decoration"		               = "HA-interior",
    "HA- exterior maintenance"		                                     = "HA-exterior",
    "HA- exterior maintenance, repair, & decoration" 	                 = "HA-exterior",
    "HA- animals and pets"		                                         = "HA-animals",
    "HA- lawn, garden and houseplants"        	                       = "HA-lawn",
    "HA- household management/other household activities"	             = "HA-other",
    "CA- Caring for and Helping Children"	                             = "CA-children",
    "CA- Caring for and Helping Adults"	                               = "CA-adults",
    "WRK- general"	                                                   = "WRK-general", # combine with modifier_4 to get specific work types
    "WRK- screen based"		                                             = "WRK-screen",
    "EDU- taking class"		                                             = "EDU-class",
    "EDU- Taking Class, Research, Homework"		                         = "EDU-class",
    "EDU- taking class, research, homework"                            = "EDU-class",
    "EDU- Taking class, research, homework"		                         = "EDU-class",
    "EDU- Extracurricular"	                                           = "EDU-extra",
    "ORG- organizational civic,  volunteer,  and religious activities" = "ORG-volunteer",
    "ORG- organizational civic, volunteer, and religious activities"   = "ORG-volunteer",
    "PUR- purchasing goods and services"		                           = "PUR-shop",
    "EAT- eating and drinking,  waiting"	                             = "EAT-eating",
    "EAT- eating and drinking, waiting"	                               = "EAT-eating",
    "LES- socializing, communicating, leisure time not screen" 	       = "LES-nonscreen",
    "LES- socializing,  communicating,  leisure time not screen"	     = "LES-nonscreen",
    "LES- screen based leisure time (TV, video game, computer)"	       = "LES-screen",
    "LES- screen based leisure time (TV,  video game,  computer)"	     = "LES-screen",
    "EX- participating in sport,  exercise or recreation"            	 = "EX-sport",
    "EX- participating in sport, exercise or recreation"               = "EX-sport",
    "EX- Attending Sport, Exercise Recreation Event, or Performance"   = "EX-attend",
    "EX- attending sport, exercise recreation event, or performance"   = "EX-attend",
    "TRAV- Passenger (Car/Truck/Motorcycle)"		                       = "TRAV-passengercar",
    "TRAV- driver (car/truck/motorcycle)"		                           = "TRAV-driver",
    "TRAV- passenger bus or train"	                                   = "TRAV-passengerlarge",
    "TRAV- Passenger (Bus, Train, Tram, Plane, Boat, Ship)" 	         = "TRAV-passengerlarge",
    "TRAV- biking"	                                                   = "TRAV-bike",
    "TRAV- walking"		                                                 = "TRAV-walk",
    "OTHER- non codable" 	                                             = "OTHER-nocode"
  )
  key_posture <-  c(
    "SB- lying"		             = "SB-lying",
    "SB-sitting"	             = "SB-sit",
    "LA- kneeling/ squatting"	 = "LA-kneel",
    "LA- stretching"	         = "LA-stretch",
    "LA- stand"		             = "LA-stand",
    "LA- stand and move"		   = "LA-standmove",
    "WA- walk"	               = "WA-walk",
    "WA-walk with load"		     = "WA-walkload",
    "WA- running"	             = "WA-run",
    "SP- bike"	               = "SP-bike",
    "WA- ascend stairs"		     = "WA-ascend",
    "WA- descend stairs"		   = "WA-descend",
    "SP- muscle strengthening" = "SP-strength",
    "SP- other sport movement" = "SP-othersport",
    "private/not coded"	       = "PRV-private"
  )
  fnm_raw <-
    basename(fpa_raw)
  fnm_raw_split <- strsplit(
    fnm_raw,
    split = "\\."
  )[[1]]
  df_info <- tibble(
    study          = "BLP",
    subject        = fnm_raw_split[3],
    visit          = fnm_raw_split[4],
    file_source    = "CALPOLY",
    # schema         = df_cln$schema[1],
    coder_initials = fnm_raw_split[8]
  )

  df_raw <-
    readxl::read_xlsx(fpa_raw) |>
    rename_with(.cols = everything(),
                .fn = tolower)

  # Determine if if the modifier belongs to posture or activity schema. Do this
  # by finding the first time the modifier value shows up and then seeing if the
  # accompanying behavior falls under key_behavior or key_posture.
  vct_mod <- grep(
    x = names(df_raw),
    pattern = "modifier",
    value = TRUE
  )

  if (length(vct_mod) != 0) {

    for (i in seq_along(vct_mod)) {

      le_behavior <- df_raw$behavior[
        which(
          !is.na(df_raw[[vct_mod[i]]])
        )[1]
      ]

      if (le_behavior %in% names(key_behavior)) {

        names(vct_mod)[i] <-
          "modifier_activity"

      } else if (le_behavior %in% names(key_posture)) {

        le_modifier <-
          na.omit(df_raw[[vct_mod[i]]])[1]
        chk_intenisty <-
          !le_modifier %in% c("sedentary", "light", "moderate", "vigorous")

        if (chk_intenisty) {

          # if it is not an intensity, treat it as a comment.
          df_raw <-
            df_raw |>
            unite(comment,
                  comment, all_of(vct_mod[i]),
                  sep = "",
                  na.rm = TRUE)

        } else {

          names(vct_mod)[i] <-
            "modifier_posture"

        }
      } else {

        stop("FUUUUUUUUUUUUUUUU")

      }
    }

    vct_mod <- vct_mod[
      !is.na(names(vct_mod))
    ]
    df_act <-
      df_raw |>
      dplyr::filter(behavior %in% names(key_behavior)) |>
      select(!vct_mod[names(vct_mod) != "modifier_activity"])
    chk_mod <-
      names(vct_mod) %in% "modifier_activity"

    if (any(chk_mod)) {
      df_act <- unite(
        df_act,
        col = modifier,
        starts_with("modifier"),
        sep = "",
        na.rm = TRUE
      )
    } else {
      df_act$modifier <-
        NA_character_
    }

    df_pos <-
      df_raw |>
      dplyr::filter(behavior %in% names(key_posture)) |>
      select(!vct_mod[names(vct_mod) != "modifier_posture"])
    chk_mod <-
      names(vct_mod) %in% "modifier_posture"

    if (any(chk_mod)) {
      df_pos <- unite(
        df_pos,
        col = modifier,
        starts_with("modifier"),
        sep = "",
        na.rm = TRUE
      )
    } else {
      df_pos$modifier <-
        NA_character_
    }

    lst_raw <- list(
      act =
        df_act,
      pos =
        df_pos
    )

  } else {

    lst_raw <- list(
      act =
        df_raw |>
        dplyr::filter(behavior %in% names(key_behavior)) |>
        mutate(modifier = NA_character_),
      pos =
        df_raw |>
        dplyr::filter(behavior %in% names(key_posture)) |>
        mutate(modifier = NA_character_)
    )

  }


  lst_shp <-
    list()

  for (i in seq_along(lst_raw)) {

    type <-
      names(lst_raw)[i]
    df <-
      lst_raw[[i]]
    dtm_relative_hms_stop <-
      last(df$time_relative_hms)
    df <-
      df |>
      dplyr::filter(event_type != "State stop")
    vct_duration <-
      c(diff.POSIXt(df$time_relative_hms,
                    units = "secs"),
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = last(df$time_relative_hms),
                 units = "secs"))
    # TODO: Implement start stop
    vct_run <-
      vct_duration

    # Second-by-second
    if (!"comment" %in% names(df)) {

      df$comment <-
        NA_character_

    }

    df_shp <- tibble(
      study    = df_info$study,
      subject  = df_info$subject,
      visit    = df_info$visit,
      coder    = df_info$coder_initials,
      datetime = seq.POSIXt(from       = df$time_relative_hms[1],
                            to         = dtm_relative_hms_stop - 1,
                            # length.out = length(vct_run_seq)
                            by         = 1),
      date     = lubridate::as_date(datetime),
      time     = format(datetime,
                        "%H:%M:%S"),
      purrr::map_dfc(
        .x = select(df,
                    behavior, modifier, comment),
        .f = ~rep(.x,
                  times = vct_run)
      )
    )
      # Make datetime in UTC timezone, make mod_1 "dark/obscured/oof"
      # when behavior is "dark/obscured/oof".
      # mutate(
      #   datetime =
      #     datetime |>
      #     with_tz(tzone = "UTC"),
      #   mod_1    =
      #     fifelse(
      #       behavior == "dark/obscured/oof",
      #       yes = "dark/obscured/oof",
      #       no  = mod_1,
      #       na  = NA_character_
      #     )
      # )

    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                  SHAPE #4: SCHEMA SPECIFIC                ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    if (type == "act") {
      df_shp <-
        df_shp |>
        rename(activity         = modifier,
               comment.behavior = comment,
               coder.behavior   = coder) |>
        mutate(
          # Factorize.
          behavior = factor(
            behavior,
            levels = names(key_behavior),
            labels = key_behavior
          ),
          activity =
            activity |>
            recode(
              "FJ- Farm Jobs"                                               = "FJ-farm", # CHECK w/ KEADLE
              "GP- Mining and Logging"                                      = "GP-mining", # CHECK w/ KEADLE
              "GP- Construction"                                            = "GP-construction", # CHECK w/ KEADLE
              "GP- Manufacturing"                                           = "GP-manufacturing", # CHECK w/ KEADLE
              "SP- Trade, Retail, Transportation, and Utilities"            = "SP-util", # CHECK w/ KEADLE
              "SP- Office (business, professional services, finance, info)" = "SP-office",
              "SP- Education and Health Services"                           = "SP-edu",
              "SP- Leisure and hospitality"                                 = "SP-leisure", # CHECK w/ KEADLE
              "SP- Other Services"                                          = "SP-other" # CHECK w/ KEADLE
            )
        )
    } else if (type == "pos") {
      df_shp <-
        df_shp |>
        rename(posture         = behavior,
               `intensity.vig` = modifier,
               comment.posture = comment,
               coder.posture   = coder) |>
        mutate(
          # Add in intensity for postures that did not have any.
          # Factorize.
          posture = factor(
            posture,
            levels = names(key_posture),
            labels = key_posture
          ),
          `intensity.vig` = case_when(
            posture == "SB-lying"     ~ "sedentary",
            posture == "SB-sit"       ~ "sedentary",
            posture == "LA-kneel"     ~ "sedentary",
            posture == "LA-stand"     ~ "light",
            posture == "LA-standmove" ~ "light",
            .default = intensity.vig
          ),
          `intensity.vig` = factor(
            `intensity.vig`,
            levels = c("sedentary", "light", "moderate", "vigorous"),
          ),
          # Change mod-vig to mvpa.
          `intensity.mvpa` = fct_collapse(
            `intensity.vig`,
            mvpa = c("moderate", "vigorous")
          )
        )
    }

    lst_shp[[i]] <-
      df_shp |>
      rename_with(
        .cols = !study:time,
        .fn   = ~paste(
          .x,
          tolower(df_info$file_source),
          sep = "_"
        )
      )
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                 SHAPE #5: FLAC AIM SPECIFIC               ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # if (flac_aim == "AIM1" & df_info$schema == "Activity") {
    #
    #   df_shp <-
    #     df_shp |>
    #     mutate(across(
    #       .cols = starts_with(df_info$col_mod_1),
    #       .fns  = \(.x) factorize_flac("non-domestic",
    #                                    .source   = df_info$file_source,
    #                                    .variable = "ENVIRONMENT")
    #     )) |>
    #     as.data.table()
    #
    # }

  }

  df_write <- full_join(
    lst_shp[[1]],
    lst_shp[[2]],
    by = join_by(study, subject, visit, datetime,
                 date, time)
  )
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##                            WRITE                          ----
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  fnm_write <-
    df_info |>
    mutate(file_name = paste(
      study, subject, visit, coder_initials, sep = "_"
    )) |>
    pull(file_name) |>
    paste0(... = _, ".parquet")
  arrow::write_parquet(
    df_write,
    sink = file.path("data", "2_INTERIM", "BLP_NOLDUS_CALPOLY", fnm_write)
  )

}
# fpa_raw_act = vct_raw_wisc[13]
# fpa_raw_pos = vct_raw_wisc[14]
secbysec_blp_wisc <- function(fpa_raw_act,
                              fpa_raw_pos) {

  ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##                             INFO                           ----
  ##::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  fnm_raw <-
    basename(fpa_raw_act)
  fnm_raw_split <- strsplit(
    fnm_raw,
    split = "\\."
  )[[1]]
  df_info <- tibble(
    study          = "BLP",
    subject        = fnm_raw_split[3],
    visit          = fnm_raw_split[4],
    file_source    = "UWM",
    # schema         = df_cln$schema[1],
    coder_initials = fnm_raw_split[8]
  )

  df_act <-
    readxl::read_xlsx(fpa_raw_act) |>
    rename_with(.cols = everything(),
                .fn = tolower) |>
    rename(environment = modifier_1)
  chk_mod <- any(grepl(
   x       = names(df_act),
   pattern = "^modifier"
  ))

  if (chk_mod) {
    df_act <- unite(
      df_act,
      col   = modifier,
      starts_with("modifier"),
      sep   = "",
      na.rm = TRUE
    )
  } else {
    df_act$modifier <-
      NA_character_
  }

  lst_raw <- list(
    act =
      df_act,
    pos =
      readxl::read_xlsx(fpa_raw_pos) |>
      rename_with(.cols = everything(),
                  .fn = tolower) |>
      unite(col = modifier,
            starts_with("modifier"),
            sep = "",
            na.rm = TRUE)
  )
  lst_shp <-
    list()

  for (i in seq_along(lst_raw)) {
    type <-
      names(lst_raw)[i]
    switch(
      type,
      "act" = {
        df_info$col_annotation <- "behavior"
        df_info$col_mod_1 <- "activity"
        df_info$col_comment <- "comment.behavior"
      },
      "pos" = {
        df_info$col_annotation <- "posture"
        df_info$col_mod_1 <- "intensity.mvpa"
        df_info$col_comment <- "comment.posture"
      }
    )

    df <-
      lst_raw[[i]]
    dtm_relative_hms_stop <-
      last(df$time_relative_hms)
    df <-
      df |>
      dplyr::filter(
        !(event_type == "State stop" |
            behavior == "*General Placeholder*" |
            behavior == "*A Deviant*" |
            behavior == "*30 Sec Deviant*" |
            behavior == "*HQ Deviant*" |
            behavior == "*LQ Deviant*" |
            behavior == "*P Deviant*" |
            behavior == "*M Deviant*")
      )
    vct_duration <-
      c(diff.POSIXt(df$time_relative_hms,
                    units = "secs"),
        difftime(time1 = dtm_relative_hms_stop,
                 time2 = last(df$time_relative_hms),
                 units = "secs"))
    # TODO: Implement start stop
    vct_run <-
      vct_duration

    # Second-by-second
    if (!any(names(df) %in% "comment")) df$comment <- NA

    if (type == "act") {
      df_shp <- tibble(
        study    = df_info$study,
        subject  = df_info$subject,
        visit    = df_info$visit,
        coder    = df_info$coder_initials,
        datetime = seq.POSIXt(from       = df$time_relative_hms[1],
                              to         = dtm_relative_hms_stop - 1,
                              # length.out = length(vct_run_seq)
                              by         = 1),
        date     = lubridate::as_date(datetime),
        time     = format(datetime,
                          "%H:%M:%S"),
        purrr::map_dfc(
          .x = select(df,
                      behavior, environment, modifier, comment),
          .f = ~rep(.x,
                    times = vct_run)
        )
      )
    } else {
      df_shp <- tibble(
        study    = df_info$study,
        subject  = df_info$subject,
        visit    = df_info$visit,
        coder    = df_info$coder_initials,
        datetime = seq.POSIXt(from       = df$time_relative_hms[1],
                              to         = dtm_relative_hms_stop - 1,
                              # length.out = length(vct_run_seq)
                              by         = 1),
        date     = lubridate::as_date(datetime),
        time     = format(datetime,
                          "%H:%M:%S"),
        purrr::map_dfc(
          .x = select(df,
                      behavior, modifier, comment),
          .f = ~rep(.x,
                    times = vct_run)
        )
      )
    }
    # Make datetime in UTC timezone, make mod_1 "dark/obscured/oof"
    # when behavior is "dark/obscured/oof".
    # mutate(
    #   datetime =
    #     datetime |>
    #     with_tz(tzone = "UTC"),
    #   mod_1    =
    #     fifelse(
    #       behavior == "dark/obscured/oof",
    #       yes = "dark/obscured/oof",
    #       no  = mod_1,
    #       na  = NA_character_
    #     )
    # )

    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                  SHAPE #4: SCHEMA SPECIFIC                ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    if (type == "act") {

      df_shp <-
        df_shp |>
        rename("{df_info$col_annotation}" := behavior,
               "{df_info$col_mod_1}"      := modifier,
               "{df_info$col_comment}"    := comment,
               coder.behavior              = coder) |>
        mutate(
          environment = case_when(
            behavior == "[U] Dark/Obscured/OoF" ~ "Dark/Obscured/OoF",
            .default = environment
          ),
          # Factorize.
          behavior =
            behavior |>
            recode("[LQ] Caring Grooming - Self"   = "[LQ] Caring/Grooming - Self",
                   "[LQ] Cooking/Meal Preperation" = "[LQ] Cooking/Meal Preparation",
                   "[HQ] Caring Grooming - Self"   = "[HQ] Caring/Grooming - Self",
                   "[HQ] Cooking/Meal Preperation" = "[HQ] Cooking/Meal Preparation") |>
            sub(x = _,
                pattern = "\\[[A-Z]{1,2}\\] ",
                replacement = "") |>
            tolower(),
          activity =
            activity |>
            tolower(),
          environment =
            environment |>
            recode("Organizational/Civic/Religiious" = "Organizational/Civic/Religious") |>
            tolower() |>
            factor(levels = c("domestic",
                              "non-domestic",
                              "errands/shopping",
                              "occupation",
                              "organizational/civic/religious",
                              "social/leisure",
                              "dark/obscured/oof",
                              "uncoded"))
        )

    } else if (type == "pos") {

      df_shp <-
        df_shp |>
        rename("{df_info$col_annotation}" := behavior,
               "{df_info$col_mod_1}"      := modifier,
               "{df_info$col_comment}"    := comment,
               coder.posture               = coder) |>
        mutate(
          `intensity.mvpa` =
            case_when(
              posture == "Uncoded - Dark/Obscured/OoF" ~ "Dark/Obscured/OoF",
              `intensity.mvpa` == "mod-vig"            ~ "mvpa",
              .default = `intensity.mvpa`
            ) |>
            tolower(),
          # Factorize.
          posture =
            posture |>
            recode(
              "Uncoded - Dark/Obscured/OoF"         = "[U] Dark/Obscured/OoF",
              "[P] Other"                           = "[P] Other - Posture",
              "[P} Lying"                           = "[P] Lying",
              "[P] Crouching / Kneeling / Squating" = "[P] Crouching/Kneeling/Squatting",
              "[M] Other"                           = "[M] Other - Movement",
              "[M] Crouching / Squating"            = "[M] Crouching/Squatting"
            ) |>
            sub(x = _,
                pattern = "\\[[A-Z]{1,2}\\] ",
                replacement = "") |>
            tolower()
        )

    }


    lst_shp[[i]] <-
      df_shp |>
      rename_with(
        .cols = !study:time,
        .fn   = ~paste(
          .x,
          tolower(df_info$file_source),
          sep = "_"
        )
      )
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ##                 SHAPE #5: FLAC AIM SPECIFIC               ----
    ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    # if (flac_aim == "AIM1" & df_info$schema == "Activity") {
    #
    #   df_shp <-
    #     df_shp |>
    #     mutate(across(
    #       .cols = starts_with(df_info$col_mod_1),
    #       .fns  = \(.x) factorize_flac("non-domestic",
    #                                    .source   = df_info$file_source,
    #                                    .variable = "ENVIRONMENT")
    #     )) |>
    #     as.data.table()
    #
    # }

  }

  df_write <- full_join(
    lst_shp[[1]],
    lst_shp[[2]],
    by = join_by(study, subject, visit, datetime,
                 date, time)
  )
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ##                            WRITE                          ----
  ##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  fnm_write <-
    df_info |>
    mutate(file_name = paste(
      study, subject, visit, coder_initials, sep = "_"
    )) |>
    pull(file_name) |>
    paste0(... = _, ".parquet")
  arrow::write_parquet(
    df_write,
    sink = file.path("data", "2_INTERIM", "BLP_NOLDUS_UWM", fnm_write)
  )

}
# vct_behavior = df$behavior_calpoly
# vct_activity = df$activity_calpoly
collapse_behavior_calpoly <- function(vct_behavior,
                                      vct_activity) {

  vct_equal <- case_match(
    vct_behavior,
    "SL-sleep"            ~ "sleep",
    "PC-groom"            ~ "care personal",
    "PC-other"            ~ "other",
    "HA-housework"        ~ "housework",
    "HA-foodprep"         ~ "food prep",
    "HA-interior"         ~ "maintenance",
    "HA-exterior"         ~ "maintenance",
    "HA-animals"          ~ "care non-personal",
    "HA-lawn"             ~ "lawn/garden",
    "HA-other"            ~ "other housework",
    "CA-children"         ~ "care non-personal",
    "CA-adults"           ~ "care non-personal",
    "WRK-general"         ~ "occupation", # will be appended with vct_activity
    "WRK-screen"          ~ "occupation", # captured as office regardless?
    "EDU-class"           ~ "occupation general",
    "EDU-extra"           ~ "occupation general",
    "ORG-volunteer"       ~ "volunteer",
    "PUR-shop"            ~ "shopping/errands",
    "EAT-eating"          ~ "eating/drinking",
    "LES-nonscreen"       ~ "social/leisure",
    "LES-screen"          ~ "electronics",
    "EX-sport"            ~ "sport/exercise",
    "EX-attend"           ~ "social/leisure",
    "TRAV-passengercar"   ~ "travel passenger",
    "TRAV-driver"         ~ "travel driving",
    "TRAV-passengerlarge" ~ "travel public",
    "TRAV-bike"           ~ "travel active",
    "TRAV-walk"           ~ "travel active",
    "OTHER-nocode"        ~ "not coded",
    .default = "RECHECK CODE"
  )
  vct_equal <-
    case_match(
      vct_activity,
      "FJ-farm"          ~ " active",
      "GP-mining"        ~ " active",
      "GP-construction"  ~ " active",
      "GP-manufacturing" ~ " active",
      "SP-util"          ~ " general",
      "SP-office"        ~ " office",
      "SP-edu"           ~ " general", # Check w/ Keadle, sounds like it could easily be lumped with occupation office
      "SP-leisure"       ~ " general",
      "SP-other"         ~ " general",
      .default = ""
    ) |>
    paste0(vct_equal, ... = _)

}
# vct_behavior     = df$behavior_uwm
# vct_activity     = df$activity_uwm
# vct_environment  = df$environment_uwm
collapse_behavior_uwm <- function(vct_behavior,
                                  vct_activity,
                                  vct_environment) {

  c(# Household
    "care personal", #
    "care non-personal", #
    "food prep", #
    "housework", #
    "lawn/garden", #
    "maintenance", #
    "shopping/errands",
    "other housework",
    # Occupation
    "occupation active",
    "occupation office",
    "occupation general",
    "occupation general",
    "volunteer",
    # Leisure
    "eating/drinking", #
    "electronics", #
    "social/leisure", #
    "sport/exercise", #
    # Transportation
    "travel passenger", #
    "travel driving", #
    "travel public", #
    "travel active", #
    # Other
    "other",
    "sleep",
    # No PA Behavior
    "not coded",
    "RECHECK CODE"
  )

  vct_equal <-
    case_match(
      vct_environment,
      "errands/shopping" ~ "shopping/errands",
      "social/leisure"   ~ "social/leisure"
    )

  df_event <-
    tibble(
      behavior = vct_behavior,
      activity = vct_activity,
      environment = vct_environment
    ) |>
    mutate(
      equal = case_match(
        behavior,
        "sports/exercise"              ~ "sport/exercise",
        "eating/drinking"              ~ "eating/drinking",
        "transportation"               ~ "travel",
        "electronics"                  ~ "electronics",
        "other - manipulating objects" ~ "manipulating objects",
        "other - carrying load w/ ue"  ~ "carrying load",
        "other - pushing cart"         ~ "pushing cart",
        "talking - person"             ~ "talking",
        "talking - phone"              ~ "talking",
        "caring/grooming - adult"      ~ "care non-personal",
        "caring/grooming - animal/pet" ~ "care non-personal",
        "caring/grooming - child"      ~ "care non-personal",
        "caring/grooming - self"       ~ "care personal",
        "cleaning"                     ~ "housework",
        "c/f/r/m"                      ~ "maintenance",
        "cooking/meal preparation"     ~ "food prep",
        "laundry"                      ~ "housework",
        "lawn&garden"                  ~ "lawn/garden",
        "leisure based"                ~ "social/leisure",
        "only [p/m] code"              ~ "no PA behavior",
        "talking - researchers"        ~ "talking",
        "intermittent activity"        ~ "not coded",
        "dark/obscured/oof"            ~ "not coded",
        .default = "RECHECK CODE"
      ),
      # granular travel
      equal =
        case_match(
          activity,
          "driving automobile"           ~ " driving",
          "riding automobile"            ~ " passenger",
          "riding public transportation" ~ " public",
          "[m] travel"                   ~ " active",
          "other transportation"         ~ " other",
          .default = ""
        ) |>
        paste0(equal, ... = _),
      # Environment
      equal = case_match(
        vct_environment,
        "errands/shopping" ~ "shopping/errands",
        "social/leisure"   ~ "social/leisure",
        .default = equal
      ),
      event_behavior    = vctrs::vec_identify_runs(equal),
      event_environment = vctrs::vec_identify_runs(environment)
    )

  # Roll environment ----
  vct_env_extrapolate <- c(
    "occupation",
    "organizational/civic/religious"
  )
  vct_beh_occ_active <- c(
    "housework",
    "lawn/garden",
    "maintenance"
  )
  vct_beh_occ_office <- c(
    "electronics",
    "social/leisure"
  )

  # Roll no PA behavior and manipulating objects, pushing cart, carrying load ONCE before this for all household, leisure and transportation codes

  # Manipulating objects, carrying load and pushing cart...
  # if environment == "domestic", just call it housework??? other housework???

  # Step 1: determine if occupation is office (occupation_type)
  # environment == occupation & majority == "leisure based" | "electronics" ~ "office"
  # environment == occupation & majority == "talking" ~ "general"
  # environment == occupation & majority %in% household ~ "active"
  # environment == occupation & majority %in% c("manipulating objects", "carrying load") ~ "active"
  # Step 2: Anything that is not one of the codes above, call it the occupation type
  # If occupation is office, calpoly still calls food prep
  # environment == occupation & occupation_type == "office" & !behavior %in% c("housework", "food prep", "eating/drinking") ~ "occupation office",
  # environment == occupation & occupation_type == "general" & !behavior %in% c("housework", "food prep", "eating/drinking") ~ "occupation general",
  # environment == occupation & occupation_type == "active" & !behavior %in% c("talking", "leisure based", "electronics", "eating/drinking") ~ "occupation active"
  # environment == occupation & majority == leisure/electronics/talking & !behavior %in% household ~ "occupation

  # Unfortunately, there is no way to tell if talking in a volunteer location would be part of
  # volunteering or not. So would have to roll "LES-nonscreen" from calpoly side into volunteer if it is not in between any transportation.
  # It feels like a lot of what calpoly codes as LES-nonscreen is just no PA behavior, confirm this with
  # df |>
  #   count(behavior_calpoly, behavior_uwm)

  return(df_event$equal)

  # # Non occupational environment.
  # env_dom_nondom <-
  #   c(
  #     "domestic",                                                 # Noldus ENV
  #     "non-domestic"                                              # Noldus ENV
  #   )
  # # Occupational environment.
  # env_occupation <-
  #   c(
  #     "occupation"                                               # Noldus ENV
  #   )
  # env_org_civ_rel <-
  #   c(
  #     "organizational/civic/religious"                            # Noldus ENV
  #   )
  # # First go through and label activities that are not independent on
  # # environment or intention.
  # case_match(
  #   vct_behavior,
  #   "sports/exercise",
  #   "eating/drinking",
  #   "transportation",
  #   "electronics",
  #   "other - manipulating objects",
  #   "other - carrying load w/ ue",
  #   "other - pushing cart",
  #   "talking - person",
  #   "talking - phone",
  #   "caring/grooming - adult",
  #   "caring/grooming - animal/pet",
  #   "caring/grooming - child",
  #   "caring/grooming - self",
  #   "cleaning",
  #   "c/f/r/m",
  #   "cooking/meal preparation",
  #   "laundry" ,
  #   "lawn&garden",
  #   "leisure based",
  #   "only [p/m] code",
  #   "talking - researchers",
  #   "intermittent activity",
  #   "dark/obscured/oof"
  # )
  #
  #
  #
  # # Either Household or Occupation.
  # lump_hsh_or_occ <-
  #   c(
  #     "cleaning",                                                 # Noldus 15
  #     "c/f/r/m",                                                  # Noldus 16
  #     "cooking/meal preparation",                                 # Noldus 17
  #     "laundry",                                                  # Noldus 18
  #     "lawn&garden"                                               # Noldus 19
  #   )
  # # Either ADL or Occupation.
  # lump_crg_or_occ <-
  #   c(
  #     "caring/grooming - adult",                                  # Noldus 12
  #     "caring/grooming - animal/pet",                             # Noldus 13
  #     "caring/grooming - child"                                   # Noldus 14
  #   )
  # # All Transportation
  # lump_transportation <-
  #   c(
  #     "transportation"                                            # Noldus 8
  #   )
  # # All Leisure.
  # lump_leisure <-
  #   c(
  #     "eating/drinking"                                          # Noldus 2
  #   )
  # lump_crg_self <-
  #   c(
  #     "caring/grooming - self"                                    # Noldus 3
  #   )
  # # Either Leisure or Occupation.
  # lump_lei_or_occ <-
  #   c(
  #     "electronics",                                              # Noldus 4
  #     "talking - person",                                         # Noldus 5
  #     "leisure based"                                             # Noldus 7
  #   )
  # # All Leisure
  # lump_sport <-
  #   c(
  #     "sports/exercise"                                          # Noldus 1
  #   )
  # # All Other.
  # lump_other <-
  #   c("other - manipulating objects",                             # Noldus 9
  #     "other - carrying load w/ ue",                              # Noldus 10
  #     "other - pushing cart"                                     # Noldus 11
  #   )
  # # All NCA
  # lump_nca <-
  #   c(
  #     "only [p/m] code"                                           # Noldus 20
  #   )
  # lump_dontcare <-
  #   c(
  #     "talking - phone",                                          # Noldus 6
  #     "talking - researchers",                                    # Noldus 21
  #     "intermittent activity"                                    # Noldus 22
  #   )
  # # All Dark/Obscured/OOf
  # lump_dark <-
  #   c(
  #     "dark/obscured/oof"                                        # Noldus 23
  #   )
  # # All ADL
  # env_errands <-
  #   c(
  #     "errands/shopping"                                          # Noldus ENV
  #   )
  # env_social <-
  #   c(
  #     "social/leisure"                                            # Noldus ENV
  #   )
  # # Non occupational environment.
  # env_dom_nondom <-
  #   c(
  #     "domestic",                                                 # Noldus ENV
  #     "non-domestic"                                              # Noldus ENV
  #   )
  # # Occupational environment.
  # env_occupation <-
  #   c(
  #     "occupation"                                               # Noldus ENV
  #   )
  # env_org_civ_rel <-
  #   c(
  #     "organizational/civic/religious"                            # Noldus ENV
  #   )
  #   case_when(
  #     # Transportation
  #     vct_activity == "driving automobile"                                                        ~ "travel driving",
  #     vct_activity == "[m] travel"                                                                ~ "travel active",
  #     # TODO: riding public transportation
  #     vct_behavior %in% lump_transportation                                                       ~ "travel fudge",
  #     vct_behavior == "eating/drinking" & vct_environ %in% c(env_occupation, env_org_civ_rel)     ~ "occupation general",
  #     vct_behavior == "eating/drinking"                                                           ~ "eating/drinking",
  #     vct_behavior == "electronics" & !(vct_environ %in% c(env_occupation, env_org_civ_rel))      ~ "electronics",
  #     vct_behavior %in% lump_lei_or_occ & !(vct_environ %in% c(env_occupation, env_org_civ_rel))  ~ "social/leisure/nonscreen",
  #     vct_behavior %in% lump_sport                                         ~ "sport/exercise",
  #     vct_environ %in% env_errands                                         ~ "shopping",
  #     vct_environ %in% env_social                                          ~ "social/leisure/nonscreen",
  #     vct_environ %in% env_occupation                                      ~ "occupation general",
  #     vct_behavior %in% lump_hsh_or_occ & vct_environ %in% env_dom_nondom  ~ "housework",
  #     vct_behavior %in% lump_crg_or_occ & vct_environ %in% env_dom_nondom  ~ "care other",
  #     vct_behavior %in% lump_lei_or_occ & vct_environ %in% env_org_civ_rel ~ "occupation general",
  #     vct_behavior %in% lump_crg_or_occ & vct_environ %in% env_org_civ_rel ~ "occupation general",
  #     vct_behavior %in% lump_hsh_or_occ & vct_environ %in% env_org_civ_rel ~ "occupation general",
  #     vct_behavior %in% lump_nca        & vct_environ %in% env_org_civ_rel ~ "occupation general",
  #     # vct_behavior %in% lump_transportation                                    ~ 4L,
  #     # vct_behavior %in% lump_leisure                                           ~ 2L,
  #     # vct_behavior %in% lump_lei_or_occ & vct_environment %in% env_dom_nondom  ~ 2L,
  #     # vct_behavior %in% lump_sport                                             ~ 2L,
  #     # vct_behavior %in% lump_other      & vct_environment %in% env_occupation  ~ 3L,
  #     vct_behavior %in% lump_crg_self                                          ~ "care personal",
  #     vct_behavior %in% lump_other                                             ~ "ROLL ME",
  #     vct_behavior %in% lump_nca                                               ~ "ROLL ME NCA",
  #     vct_behavior %in% lump_dontcare                                          ~ "talking",
  #     vct_behavior %in% lump_dark                                              ~ "dark/obscured"
  #   )

}
# vct_behavior     = df_section$behavior_uwm
# vct_activity     = df_section$activity_uwm
# vct_environment  = df_section$environment_uwm
# This version includes rolling.
collapse_behavior_uwm2 <- function(vct_behavior,
                                   vct_activity,
                                   vct_environment) {

  df_init <- tibble(
    behavior      = vct_behavior,
    activity      = vct_activity,
    environment   = vct_environment,
    event_beh     = vctrs::vec_identify_runs(behavior),
    event_env     = vctrs::vec_identify_runs(environment)
  )
  df_collapse <-
    df_init |>
    group_by(event_beh) |>
    mutate(duration = n()) |>
    slice(1) |>
    ungroup()

  # 1st: rename "straightforward" codes ----
  # TODO It feels like a lot of what calpoly codes as LES-nonscreen is just "no PA
  # behavior", confirm this with keadle
  # df |>
  #   count(behavior_calpoly, behavior_uwm)
  df_collapse <-
    df_collapse |>
    mutate(
      collapse1 = case_match(
        behavior,
        "sports/exercise"              ~ "sport/exercise",
        "eating/drinking"              ~ "eating/drinking",
        "transportation"               ~ "travel",
        "electronics"                  ~ "electronics",
        # "other - manipulating objects" ~ "manipulating objects",
        # "other - carrying load w/ ue"  ~ "carrying load",
        # "other - pushing cart"         ~ "pushing cart",
        "talking - person"             ~ "talking",
        "talking - phone"              ~ "talking",
        "caring/grooming - adult"      ~ "care non-personal",
        "caring/grooming - animal/pet" ~ "care non-personal",
        "caring/grooming - child"      ~ "care non-personal",
        "caring/grooming - self"       ~ "care personal",
        "cleaning"                     ~ "housework",
        "c/f/r/m"                      ~ "maintenance",
        "cooking/meal preparation"     ~ "food prep",
        "laundry"                      ~ "housework",
        "lawn&garden"                  ~ "lawn/garden",
        "leisure based"                ~ "social/leisure",
        "only [p/m] code"              ~ "no PA behavior",
        "talking - researchers"        ~ "talking",
        "intermittent activity"        ~ "not coded",
        "dark/obscured/oof"            ~ "not coded",
        .default = behavior # "RECHECK CODE"
      ),
      # granular travel
      collapse1 =
        case_match(
          activity,
          "driving automobile"           ~ " driving",
          "riding automobile"            ~ " passenger",
          "riding public transportation" ~ " public",
          "[m] travel"                   ~ " active",
          "other transportation"         ~ " other",
          .default = ""
        ) |>
        paste0(collapse1, ... = _),
      # Environment
      collapse1 = case_match(
        environment,
        "errands/shopping" ~ "shopping/errands",
        "social/leisure"   ~ "social/leisure",
        .default = collapse1
      ),
      .before = behavior
    ) |>
    mutate(
      collapse4 = "",
      collapse3 = "",
      collapse2 = collapse1,
      .before = collapse1
    )
  # 2nd: Roll "other" UWM codes ----
  # Roll surrounding quality/general codes around manipulating objects,
  # pushing cart, and carrying load.
  vct_q <- c(
    "care non-personal",
    "care personal",
    "housework",
    "maintenance",
    "food prep",
    "lawn/garden"
  )
  vct_g <- c(
    "eating/drinking",
    "electronics",
    "social/leisure",
    "sport/exercise",
    "travel driving",
    "travel passenger",
    "travel public",
    "travel active",
    "travel other"
  )
  # vct_talk <-
  #   c("talking")
  # vct_t <-
  #   c("no PA behavior",
  #     "not coded")
  vct_other <-
    c("other - manipulating objects",
      "other - carrying load w/ ue",
      "other - pushing cart")
  vct_ind_roll <-
    which(df_collapse$collapse1 %in% vct_other)

  for (i in seq_along(vct_ind_roll)) {

    ind <-
      vct_ind_roll[i]

    behavior_previous <-
      df_collapse$collapse1[ind - 1]
    behavior_roll <-
      df_collapse$collapse1[ind]
    behavior_next <-
      df_collapse$collapse1[ind + 1]

    surrounded_by_q <-
      behavior_previous %in% vct_q & behavior_next %in% vct_q
    surrounded_by_g <-
      behavior_previous %in% vct_g & behavior_next %in% vct_g

    duration_next_gt <-
      df_collapse$duration[ind - 1] <
      df_collapse$duration[ind + 1]
    duration_previous_gt <-
      df_collapse$duration[ind - 1] >
      df_collapse$duration[ind + 1]
    duration_equal <-
      df_collapse$duration[ind - 1] ==
      df_collapse$duration[ind + 1]

    if (ind == 1) {

      # Only take into account behavior_next.
      df_collapse$collapse2[ind] <-
        case_when(
          behavior_next %in% vct_q    ~ behavior_next,
          behavior_next %in% vct_g    ~ behavior_next,
          .default = behavior_roll
        )

    } else if (ind == nrow(df_collapse)) {

      # Only take into account behavior_previous.
      df_collapse$collapse2[ind] <-
        case_when(
          behavior_previous %in% vct_q    ~ behavior_previous,
          behavior_previous %in% vct_g    ~ behavior_previous,
          .default = behavior_roll
        )

    } else {

      # All conditionals
      df_collapse$collapse2[ind] <-
        case_when(
          # surrounded_by_q & behavior_next == behavior_previous ~ behavior_next, # this isn't necessary, if this occurs then either of the two lines of code below will still be right.
          surrounded_by_q & duration_next_gt        ~ behavior_next,
          surrounded_by_q & duration_previous_gt    ~ behavior_previous,
          # surrounded_by_q & duration_equal          ~ ???,
          surrounded_by_g & duration_next_gt        ~ behavior_next,
          surrounded_by_g & duration_previous_gt    ~ behavior_previous,
          # surrounded_by_g & duration_equal          ~ ???,
          behavior_next %in% vct_q        ~ behavior_next,
          behavior_previous %in% vct_q    ~ behavior_previous,
          behavior_next %in% vct_g        ~ behavior_next,
          behavior_previous %in% vct_g    ~ behavior_previous,
          .default = behavior_roll
        )

    }
  }

  # 3rd: Occupation ----
  # Only do if the "occupation" or "organizational/civic/religious" environments
  # apear.
  chk_occ <-
    "occupation" %in% df_collapse$environment
  chk_vol <-
    "organizational/civic/religious" %in% df_collapse$environment

  if (!any(chk_occ, chk_vol)) {

    df_collapse$collapse3 <-
      df_collapse$collapse2

  } else {

    # Take care of "volunteer" since its pretty straightfoward.

    # Unfortunately, there is no way to tell if talking in a volunteer location
    # would be part of volunteering or not. So would have to change
    # "LES-nonscreen" from calpoly side into volunteer...if it is not in
    # between any transportation???

    # Don't overwrite "food prep", "eating/drinking", "sleep", "sport/exercise"
    # TODO: what about care personal and care non-personal? I assume any
    # "care non-personal" in a occupation environment is part of their occupational
    # activities.
    df_collapse <-
      df_collapse |>
      mutate(
        collapse3 = case_when(
          environment == "organizational/civic/religious" &
            !collapse2 %in% c("food prep",
                              "eating/drinking",
                              "sleep",
                              "sport/exercise") ~ "volunteer",
          .default = collapse2
        )
      )


    if (chk_occ) {

      df_occupation <-
        df_collapse |>
        mutate(
          type_occupation = case_match(
            collapse2,
            # office
            "electronics"          ~ "office",
            "social/leisure"       ~ "office",
            # active
            "housework"            ~ "active",
            "lawn/garden"          ~ "active",
            "maintenance"          ~ "active",
            "other housework"      ~ "active",
            "carrying load"        ~ "active",
            "pushing cart"         ~ "active",
            # general
            "care non-personal"    ~ "general",
            "talking"              ~ "general",
            "manipulating objects" ~ "general",
            .default = NA_character_
          )
        ) |>
        dplyr::filter(
          environment == "occupation",
          !is.na(type_occupation)
        ) |>
        summarise(
          duration = sum(duration),
          .by = c(event_env, environment, type_occupation)
        ) |>
        slice(
          which.max(duration),
          .by = event_env
        ) |>
        select(!duration)
      lst_collapse <- split.data.frame(
        df_collapse,
        df_collapse$event_env
      )

      for (i in seq_along(df_occupation$event_env)) {

        ind <-
          df_occupation$event_env[i]
        le_type <-
          df_occupation$type_occupation[i]
        lst_collapse[[ind]] <-
          lst_collapse[[ind]] |>
          mutate(
            collapse3 = ifelse(
              collapse3 %in% c("food prep",
                               "eating/drinking",
                               "sleep",
                               "sport/exercise"),
              yes = collapse3,
              no  = paste("occupation", le_type)
            )
          )

        # switch(
        #   le_type,
        #   "office" = {
        #     lst_collapse[[ind]] |>
        #       mutate(
        #         collapse3 = ifelse(
        #           collapse2 %in% c("food prep",
        #                                     "eating/drinking",
        #                                     "sleep",
        #                                     "sport/exercise"),
        #           yes = collapse2,
        #           no  = le_type
        #         )
        #       )
        #   }
        #
        # )
      }

      df_collapse <-
        bind_rows(lst_collapse)

    }
  }

  # 4th: Roll "other" again ----
  # Manipulating objects, carrying load and pushing cart...
  # if environment == "domestic", just call it housework??? other housework???
  # Same as 2nd step, except now if there is nothing to roll over the "other"
  # codes, just call it "housework" in a domestic environment and...hopefully
  # it doesn't occur in a non-domestic environment.

  chk_other <-
    any(vct_other %in% df_collapse$collapse3)

  if (!chk_other) {

    df_collapse$collapse4 <-
      df_collapse$collapse3

  } else {

    df_collapse <-
      df_collapse |>
      mutate(collapse4 = collapse3,
             event_beh3 = vctrs::vec_identify_runs(collapse3)) |>
      mutate(
        duration3 = sum(duration),
        .by = event_beh3
      )

    vct_ind_roll <-
      which(df_collapse$collapse3 %in% vct_other)

    for (i in seq_along(vct_ind_roll)) {

      ind <-
        vct_ind_roll[i]

      behavior_previous <-
        df_collapse$collapse3[ind - 1]
      behavior_roll <-
        df_collapse$collapse3[ind]
      behavior_next <-
        df_collapse$collapse3[ind + 1]

      surrounded_by_q <-
        behavior_previous %in% vct_q & behavior_next %in% vct_q
      surrounded_by_g <-
        behavior_previous %in% vct_g & behavior_next %in% vct_g

      duration_next_gt <-
        df_collapse$duration3[ind - 1] <
        df_collapse$duration3[ind + 1]
      duration_previous_gt <-
        df_collapse$duration3[ind - 1] >
        df_collapse$duration3[ind + 1]
      duration_equal <-
        df_collapse$duration3[ind - 1] ==
        df_collapse$duration3[ind + 1]

      if (ind == 1) {

        # Only take into account behavior_next.
        df_collapse$collapse4[ind] <-
          case_when(
            behavior_next %in% vct_q    ~ behavior_next,
            behavior_next %in% vct_g    ~ behavior_next,
            .default = behavior_roll
          )

      } else if (ind == nrow(df_collapse)) {

        # Only take into account behavior_previous.
        df_collapse$collapse4[ind] <-
          case_when(
            behavior_previous %in% vct_q    ~ behavior_previous,
            behavior_previous %in% vct_g    ~ behavior_previous,
            .default = behavior_roll
          )

      } else {

        # All conditionals
        df_collapse$collapse4[ind] <-
          case_when(
            # surrounded_by_q & behavior_next == behavior_previous ~ behavior_next, # this isn't necessary, if this occurs then either of the two lines of code below will still be right.
            surrounded_by_q & duration_next_gt        ~ behavior_next,
            surrounded_by_q & duration_previous_gt    ~ behavior_previous,
            # surrounded_by_q & duration3_equal          ~ ???,
            surrounded_by_g & duration_next_gt        ~ behavior_next,
            surrounded_by_g & duration_previous_gt    ~ behavior_previous,
            # surrounded_by_g & duration3_equal          ~ ???,
            behavior_next %in% vct_q        ~ behavior_next,
            behavior_previous %in% vct_q    ~ behavior_previous,
            behavior_next %in% vct_g        ~ behavior_next,
            behavior_previous %in% vct_g    ~ behavior_previous,
            .default = behavior_roll
          )

      }
    }

    # If the code is still "other" and in a domestic environment, then call it
    # "other housework".
    df_collapse <-
      df_collapse |>
      mutate(
        collapse4 = ifelse(
          collapse4 %in% vct_other & environment == "domestic",
          yes = "other housework",
          no  = collapse4
        )
      )

  }

  # Wrap up ----
  # Now left bind df_collapse to df using event column as joiner which
  # automatically "fills" the manip column.
  left_join(
    df_init,
    select(df_collapse,
           collapse4, event_beh),
    by = join_by(event_beh)
  ) |>
    pull(collapse4)

}
# vct_behavior = df$behavior_calpoly
# vct_activity = df$activity_calpoly
get_equal.behavior_calpoly <- function(vct_behavior,
                                       vct_activity) {

  vct_equal <- case_match(
    vct_behavior,
    "SL-sleep"            ~ "other",
    "PC-groom"            ~ "housework",
    "PC-other"            ~ "other",
    "HA-housework"        ~ "housework",
    "HA-foodprep"         ~ "housework",
    "HA-interior"         ~ "housework",
    "HA-exterior"         ~ "housework",
    "HA-animals"          ~ "housework",
    "HA-lawn"             ~ "housework",
    "HA-other"            ~ "housework",
    "CA-children"         ~ "housework",
    "CA-adults"           ~ "housework",
    "WRK-general"         ~ "occupation", # will be appended with vct_activity
    "WRK-screen"          ~ "occupation", # captured as office regardless?
    "EDU-class"           ~ "occupation general",
    "EDU-extra"           ~ "occupation general",
    "ORG-volunteer"       ~ "other",
    "PUR-shop"            ~ "shopping/errands",
    "EAT-eating"          ~ "leisure inactive",
    "LES-nonscreen"       ~ "leisure inactive",
    "LES-screen"          ~ "leisure inactive",
    "EX-sport"            ~ "leisure active",
    "EX-attend"           ~ "leisure inactive",
    "TRAV-passengercar"   ~ "travel passenger",
    "TRAV-driver"         ~ "travel driving",
    "TRAV-passengerlarge" ~ "travel passenger",
    "TRAV-bike"           ~ "travel active",
    "TRAV-walk"           ~ "travel active",
    "OTHER-nocode"        ~ "not coded",
    .default = "RECHECK CODE"
  )
  case_when(
    vct_equal == "occupation" & vct_activity == "FJ-farm"          ~ " active",
    vct_equal == "occupation" & vct_activity == "GP-mining"        ~ " active",
    vct_equal == "occupation" & vct_activity == "GP-construction"  ~ " active",
    vct_equal == "occupation" & vct_activity == "GP-manufacturing" ~ " active",
    vct_equal == "occupation" & vct_activity == "SP-util"          ~ " general",
    vct_equal == "occupation" & vct_activity == "SP-office"        ~ " general",
    vct_equal == "occupation" & vct_activity == "SP-edu"           ~ " general",
    vct_equal == "occupation" & vct_activity == "SP-leisure"       ~ " general",
    vct_equal == "occupation" & vct_activity == "SP-other"         ~ " general",
    vct_equal == "occupation" & vct_activity == "Other"         ~ " general",
    .default = ""
  ) |>
    paste0(vct_equal, ... = _) |>
    factor(levels = c(
      "housework",
      "shopping/errands",
      "leisure active",
      "leisure inactive",
      "occupation active",
      "occupation general",
      "travel active",
      "travel driving",
      "travel passenger",
      "other",
      "no PA behavior",
      "not coded"
    ))

}
# vct_behavior     = df_section$behavior_uwm
# vct_activity     = df_section$activity_uwm
# vct_environment  = df_section$environment_uwm
# vct_behavior     = df$behavior_uwm
# vct_activity     = df$activity_uwm
# vct_environment  = df$environment_uwm
get_equal.behavior_uwm <- function(vct_behavior,
                                   vct_activity,
                                   vct_environment) {

  df_init <- tibble(
    behavior      = vct_behavior,
    activity      = vct_activity,
    environment   = vct_environment,
    event_beh     = vctrs::vec_identify_runs(behavior),
    event_env     = vctrs::vec_identify_runs(environment)
  )
  df_collapse <-
    df_init |>
    group_by(event_beh) |>
    mutate(duration = n()) |>
    slice(1) |>
    ungroup()

  # Double-check "occupation" OR "organizational/civic/religious" appear. If both
  # appear within the same visit, then we are assuming the coder meant to annotate
  # one or the other, NOT BOTH.
  chk_both <- all(
    c("occupation", "organizational/civic/religious") %in% df_collapse$environment
  )

  if (chk_both) {

    # If both environments appear, call it all of the one that appears the
    # most.
    le_env <-
      df_collapse |>
      count(environment) |>
      slice(which.max(environment)) |>
      pull(environment)
    df_collapse <-
      df_collapse |>
      mutate(
        environment = case_when(
          environment %in% c("occupation",
                             "organizational/civic/religious") ~ le_env,
          .default = environment
        ),
        event_env = vctrs::vec_identify_runs(environment)
      )

  }

  # 1st: rename "straightforward" codes ----
  df_collapse <-
    df_collapse |>
    mutate(
      collapse1 = case_match(
        behavior,
        "sports/exercise"              ~ "leisure active",
        # "eating/drinking"              ~ "leisure inactive", # do this after occupation
        "transportation"               ~ "travel",
        # "electronics"                  ~ "leisure inactive", # do this after occupation
        # "other - manipulating objects" ~ "manipulating objects",
        # "other - carrying load w/ ue"  ~ "carrying load",
        # "other - pushing cart"         ~ "pushing cart",
        "talking - person"             ~ "talking",
        "talking - phone"              ~ "talking",
        "caring/grooming - adult"      ~ "care non-personal",
        "caring/grooming - animal/pet" ~ "care non-personal",
        "caring/grooming - child"      ~ "care non-personal",
        "caring/grooming - self"       ~ "care personal",
        "cleaning"                     ~ "housework",
        "c/f/r/m"                      ~ "housework",
        "cooking/meal preparation"     ~ "food prep",
        "laundry"                      ~ "housework",
        "lawn&garden"                  ~ "housework",
        "leisure based"                ~ "leisure inactive",
        "only [p/m] code"              ~ "no PA behavior",
        "talking - researchers"        ~ "talking",
        "intermittent activity"        ~ "other",
        "dark/obscured/oof"            ~ "not coded",
        .default = behavior # "RECHECK CODE"
      ),
      # granular travel
      collapse1 =
        case_match(
          activity,
          "driving automobile"           ~ " driving",
          "riding automobile"            ~ " passenger",
          "riding public transportation" ~ " passenger",
          "[m] travel"                   ~ " active",
          "other transportation"         ~ " other",
          .default = ""
        ) |>
        paste0(collapse1, ... = _),
      # Environment
      # collapse1 = case_when(
      #   environment == "errands/shopping"
      #   & behavior != "transportation" ~ "shopping/errands",
      #   environment == "social/leisure"
      #   & behavior != "transportation" ~ "leisure inactive",
      #   environment == "organizational/civic/religious"
      #   & behavior != "transportation" ~ "other",
      #   .default = collapse1
      # ),
      .before = behavior
    ) |>
    mutate(
      collapse7 = "",
      collapse6 = "",
      collapse5 = "",
      collapse4 = "",
      collapse3 = "",
      collapse2 = collapse1,
      .before = collapse1
    )

  # 2nd: Roll "other"/no behavior UWM codes ----
  # Roll surrounding quality/general codes around manipulating objects,
  # pushing cart, carrying load and no PA behavior.
  vct_q <- c(
    "care non-personal",
    "care personal",
    "food prep",
    "housework"
  )
  vct_g <- c(
    "eating/drinking",
    "electronics",
    "leisure active",
    "leisure inactive",
    "travel driving",
    "travel passenger",
    "travel active",
    "travel other"
  )
  # vct_t <-
  #   c("no PA behavior",
  #     "not coded")
  vct_other <-
    c("other - manipulating objects",
      "other - carrying load w/ ue",
      "other - pushing cart",
      "no PA behavior")
  vct_ind_roll <-
    which(df_collapse$collapse1 %in% vct_other)

  for (i in seq_along(vct_ind_roll)) {

    ind <-
      vct_ind_roll[i]

    behavior_previous <-
      df_collapse$collapse1[ind - 1]
    behavior_roll <-
      df_collapse$collapse1[ind]
    behavior_next <-
      df_collapse$collapse1[ind + 1]

    surrounded_by_q <-
      behavior_previous %in% vct_q & behavior_next %in% vct_q
    surrounded_by_g <-
      behavior_previous %in% vct_g & behavior_next %in% vct_g

    duration_next_gt <-
      df_collapse$duration[ind - 1] <
      df_collapse$duration[ind + 1]
    duration_previous_gt <-
      df_collapse$duration[ind - 1] >
      df_collapse$duration[ind + 1]
    duration_equal <-
      df_collapse$duration[ind - 1] ==
      df_collapse$duration[ind + 1]

    if (ind == 1) {

      # Only take into account behavior_next.
      df_collapse$collapse2[ind] <-
        case_when(
          behavior_next %in% vct_q    ~ behavior_next,
          behavior_next %in% vct_g    ~ behavior_next,
          .default = behavior_roll
        )

    } else if (ind == nrow(df_collapse)) {

      # Only take into account behavior_previous.
      df_collapse$collapse2[ind] <-
        case_when(
          behavior_previous %in% vct_q    ~ behavior_previous,
          behavior_previous %in% vct_g    ~ behavior_previous,
          .default = behavior_roll
        )

    } else {

      # All conditionals
      df_collapse$collapse2[ind] <-
        case_when(
          # surrounded_by_q & behavior_next == behavior_previous ~ behavior_next, # this isn't necessary, if this occurs then either of the two lines of code below will still be right.
          surrounded_by_q & duration_next_gt        ~ behavior_next,
          surrounded_by_q & duration_previous_gt    ~ behavior_previous,
          # surrounded_by_q & duration_equal          ~ ???,
          surrounded_by_g & duration_next_gt        ~ behavior_next,
          surrounded_by_g & duration_previous_gt    ~ behavior_previous,
          # surrounded_by_g & duration_equal          ~ ???,
          behavior_next %in% vct_q        ~ behavior_next,
          behavior_previous %in% vct_q    ~ behavior_previous,
          behavior_next %in% vct_g        ~ behavior_next,
          behavior_previous %in% vct_g    ~ behavior_previous,
          .default = behavior_roll
        )

    }
  }

  # 3rd: Environment ----
  chk_env <-any(
    c("errands/shopping",
      "occupation",
      "organizational/civic/religious",
      "social/leisure") %in%
    df_collapse$environment
  )

  if (!chk_env) {

    df_collapse$collapse3 <-
      df_collapse$collapse2

  } else {

    df_occupation <-
      df_collapse |>
      mutate(
        type_occupation = case_match(
          collapse2,
          # active
          "housework"            ~ "active",
          "leisure active"       ~ "active",
          "carrying load"        ~ "active",
          "pushing cart"         ~ "active",
          # general
          "care non-personal"    ~ "general",
          "electronics"          ~ "general",
          "leisure inactive"     ~ "general",
          "manipulating objects" ~ "general",
          "talking"              ~ "general",
          "no PA behavior"       ~ "general",
          .default = NA_character_
        )
      ) |>
      dplyr::filter(
        environment == "occupation",
        !is.na(type_occupation)
      ) |>
      summarise(
        duration = sum(duration),
        .by = c(event_env, environment, type_occupation, collapse2)
      ) |>
      slice(
        which.max(duration),
        .by = event_env
      ) |>
      select(!duration)

    if (nrow(df_occupation) != 0) {

      lst_collapse <- split.data.frame(
        df_collapse,
        df_collapse$event_env
      )

      for (i in seq_along(lst_collapse)) {

        if (lst_collapse[[i]]$environment[1] != "occupation") {

          lst_collapse[[i]] <-
            lst_collapse[[i]] |>
            mutate(
              collapse3 = collapse2
            )

        } else {

          ind <-
            df_occupation$event_env[i]
          le_type <-
            df_occupation$type_occupation[i]
          le_maj <-
            df_occupation$collapse2[i]

          # The two vectors below are codes to NOT roll over depending on type. The
          # majority code is removed from the appropriate vector.
          vct_dont_roll <- c(
            "care non-personal",
            "care personal",
            "eating/drinking",
            "housework",
            "food prep",
            "leisure active",
            "travel driving",
            "travel passenger",
            "travel active",
            "travel other"
          )
          vct_dont_roll <- vct_dont_roll[!grepl(
            x = vct_dont_roll,
            pattern = le_maj
          )]
          lst_collapse[[ind]] <-
            lst_collapse[[ind]] |>
            mutate(
              collapse3 = ifelse(
                collapse2 %in% vct_dont_roll,
                yes = collapse2,
                no  = paste("occupation", le_type)
              )
            )

        }


      }

      df_collapse <-
        bind_rows(lst_collapse)

    } else {

      df_collapse$collapse3 <-
        df_collapse$collapse2

    }

    vct_dont_roll <- c(
      "care non-personal",
      "care personal",
      "eating/drinking",
      "electronics",        # different
      "housework",
      "food prep",
      "leisure active",
      "occupation active",  # different
      "occupation general", # different
      "travel driving",
      "travel passenger",
      "travel active",
      "travel other"
    )
    df_collapse <-
      df_collapse |>
      mutate(
        collapse3 = case_when(
          environment == "errands/shopping"
          & !(collapse3 %in% vct_dont_roll) ~ "shopping/errands",
          environment == "social/leisure"
          & !(collapse3 %in% vct_dont_roll) ~ "leisure inactive",
          environment == "organizational/civic/religious"
          & !(collapse3 %in% vct_dont_roll) ~ "other",
          .default = collapse3
        )
      )

  }

  # 4th: Roll "other" again ----
  # Same as 2nd step, except now if there is nothing to roll over the "other"
  # codes, just call it "housework" in a domestic environment and...hopefully
  # it doesn't occur in a non-domestic environment...
  # Well it did occur in a non-domestic environment, just take care of it with
  # the new 6th step (final renaming will be 7th step).
  chk_other <-
    any(vct_other %in% df_collapse$collapse3)

  if (!chk_other) {

    df_collapse$collapse4 <-
      df_collapse$collapse3

  } else {

    df_collapse <-
      df_collapse |>
      mutate(collapse4 = collapse3,
             event_beh3 = vctrs::vec_identify_runs(collapse3)) |>
      mutate(
        duration3 = sum(duration),
        .by = event_beh3
      )

    vct_ind_roll <-
      which(df_collapse$collapse3 %in% vct_other)

    for (i in seq_along(vct_ind_roll)) {

      ind <-
        vct_ind_roll[i]

      behavior_previous <-
        df_collapse$collapse3[ind - 1]
      behavior_roll <-
        df_collapse$collapse3[ind]
      behavior_next <-
        df_collapse$collapse3[ind + 1]

      surrounded_by_q <-
        behavior_previous %in% vct_q & behavior_next %in% vct_q
      surrounded_by_g <-
        behavior_previous %in% vct_g & behavior_next %in% vct_g

      duration_next_gt <-
        df_collapse$duration3[ind - 1] <
        df_collapse$duration3[ind + 1]
      duration_previous_gt <-
        df_collapse$duration3[ind - 1] >
        df_collapse$duration3[ind + 1]
      duration_equal <-
        df_collapse$duration3[ind - 1] ==
        df_collapse$duration3[ind + 1]

      if (ind == 1) {

        # Only take into account behavior_next.
        df_collapse$collapse4[ind] <-
          case_when(
            behavior_next %in% vct_q    ~ behavior_next,
            behavior_next %in% vct_g    ~ behavior_next,
            .default = behavior_roll
          )

      } else if (ind == nrow(df_collapse)) {

        # Only take into account behavior_previous.
        df_collapse$collapse4[ind] <-
          case_when(
            behavior_previous %in% vct_q    ~ behavior_previous,
            behavior_previous %in% vct_g    ~ behavior_previous,
            .default = behavior_roll
          )

      } else {

        # All conditionals
        df_collapse$collapse4[ind] <-
          case_when(
            # surrounded_by_q & behavior_next == behavior_previous ~ behavior_next, # this isn't necessary, if this occurs then either of the two lines of code below will still be right.
            surrounded_by_q & duration_next_gt        ~ behavior_next,
            surrounded_by_q & duration_previous_gt    ~ behavior_previous,
            # surrounded_by_q & duration3_equal          ~ ???,
            surrounded_by_g & duration_next_gt        ~ behavior_next,
            surrounded_by_g & duration_previous_gt    ~ behavior_previous,
            # surrounded_by_g & duration3_equal          ~ ???,
            behavior_next %in% vct_q        ~ behavior_next,
            behavior_previous %in% vct_q    ~ behavior_previous,
            behavior_next %in% vct_g        ~ behavior_next,
            behavior_previous %in% vct_g    ~ behavior_previous,
            .default = behavior_roll
          )

      }
    }

    # If the code is still "other" and in a domestic environment, then call it
    # "other housework".
    df_collapse <-
      df_collapse |>
      mutate(
        collapse4 = ifelse(
          collapse4 %in% vct_other & environment == "domestic",
          yes = "housework",
          no  = collapse4
        )
      )

  }

  # 5th: calpoly rule of 45 seconds ----
  # CalPoly procedure: once a behavior is annotated, any "interrupting" behavior
  # different from the current behavior needs to occur for at least 45 seconds
  # in order for it to be applied as a new behavior. If not, then roll current
  # behavior forward.
  # The 5th step only rolls the previous code over "interrupting" behavior if
  # the previous code is at least 45 seconds.
  df_collapse <-
    df_collapse |>
    mutate(collapse5  = collapse4,
           event_beh4 = vctrs::vec_identify_runs(collapse4)) |>
    mutate(
      duration4 = sum(duration),
      .by = event_beh4
    )
  df_event <-
    df_collapse |>
    slice(1, .by = event_beh4)
  vct_ind_roll <- df_event$event_beh4[
    df_event$duration4 < 45
  ]

  # Keep indices within vct_ind_roll that have a previous behavior that occurs
  # >= 45 seconds.
  vct_ind_roll <- vct_ind_roll[
    df_event$duration4[vct_ind_roll - 1] >= 45
  ]

  if (length(vct_ind_roll) != 0) {

    df_event$collapse5[df_event$event_beh4 %in% vct_ind_roll] <-
      df_event$collapse5[df_event$event_beh4 %in% (vct_ind_roll - 1)]
    df_collapse <-
      left_join(
        select(df_collapse, !collapse5),
        select(df_event, collapse5, event_beh4),
        by = join_by(event_beh4)
      ) |>
      relocate(collapse5, .after = collapse6)

  }

  # 6th: Roll forward remaining "other"/"noPAbehavior" ----
  chk_other <-
    any(vct_other %in% df_collapse$collapse5)

  if (!chk_other) {

    df_collapse$collapse6 <-
      df_collapse$collapse5

  } else {

    df_collapse <-
      df_collapse |>
      mutate(collapse6  = collapse5,
             other_noPA = case_when(
               collapse5 %in% vct_other ~ "PLEASEROLLOVERME",
               .default = collapse5
             ),
             event_beh5 = vctrs::vec_identify_runs(other_noPA)
      )
    df_event <-
      df_collapse |>
      slice(1, .by = event_beh5)
    vct_ind_roll <- df_event$event_beh5[
      df_event$other_noPA == "PLEASEROLLOVERME"
    ]

    # Don't include "other"/"no PA behavior" if it happens in the very beginning
    # (i.e. event == 1) since there is nothing to roll forward.
    vct_ind_roll <- vct_ind_roll[
      vct_ind_roll != 1
    ]

    if (length(vct_ind_roll) == 0) {

      df_collapse$collapse6 <-
        df_collapse$collapse5

    } else {

      df_event$collapse6[df_event$event_beh5 %in% vct_ind_roll] <-
        df_event$collapse6[df_event$event_beh5 %in% (vct_ind_roll - 1)]
      df_collapse <-
        left_join(
          select(df_collapse, !collapse6),
          select(df_event, collapse6, event_beh5),
          by = join_by(event_beh5)
        ) |>
        relocate(collapse6, .after = collapse7)

    }
  }

  # 7th: Final renaming ----
  df_collapse <-
    df_collapse |>
    mutate(
      collapse7 = case_match(
        collapse6,
        "eating/drinking"   ~ "leisure inactive", # do this after occupation
        "electronics"       ~ "leisure inactive", # do this after occupation
        "talking"           ~ "leisure inactive",
        "care non-personal" ~ "housework",
        "care personal"     ~ "housework",
        "food prep"         ~ "housework",
        "travel other"      ~ "other",
        .default = collapse6
      )
    )

  # Wrap up ----
  # Now left bind df_collapse to df using event column as joiner which
  # automatically "fills" the manip column.
  left_join(
    df_init,
    select(df_collapse,
           collapse7, event_beh),
    by = join_by(event_beh)
  ) |>
    pull(collapse7) |>
    factor(levels = c(
      "housework",
      "shopping/errands",
      "leisure active",
      "leisure inactive",
      "occupation active",
      "occupation general",
      "travel active",
      "travel driving",
      "travel passenger",
      "other",
      "no PA behavior",
      "not coded"
    ))

}
get_broad.behavior = function(vct_equal.behavior) {

  case_match(
    vct_equal.behavior,
    "housework"          ~ "housework",
    "shopping/errands"   ~ "shopping/errands",
    "leisure active"     ~ "active time",
    "leisure inactive"   ~ "leisure inactive",
    "occupation active"  ~ "occupation/education",
    "occupation general" ~ "occupation/education",
    "travel active"      ~ "active time",
    "travel driving"     ~ "travel inactive",
    "travel passenger"   ~ "travel inactive",
    "other"              ~ "other",
    "no PA behavior"     ~ "no PA behavior",
    "not coded"          ~ "not coded",
    .default = "RECHECK R CODE/DATA"
  ) |>
    factor(levels = c(
      "housework",
      "shopping/errands",
      "active time",
      "leisure inactive",
      "occupation/education",
      "travel inactive",
      "other",
      "no PA behavior",
      "not coded",
      "RECHECK R CODE/DATA"
    ))

}
# vct_posture = df_section$posture_calpoly
get_equal.posture_calpoly <- function(vct_posture) {

  case_match(
    vct_posture,
    "SB-sit"        ~ "sitting",
    "SB-lying"      ~ "lying",
    "LA-kneel"      ~ "crouching/kneeling",
    "LA-stretch"    ~ "mixed movement",
    "LA-stand"      ~ "standing",
    "LA-standmove"  ~ "stationary",
    "WA-walk"       ~ "walking",
    "WA-walkload"   ~ "walking",
    "WA-run"        ~ "running",
    "SP-bike"       ~ "cycling",
    "WA-ascend"     ~ "ascending stairs",
    "WA-descend"    ~ "descending stairs",
    "SP-strength"   ~ "mixed movement",
    "SP-othersport" ~ "mixed movement",
    "PRV-private"   ~ "not coded",
    .default        = "RECHECK R CODE/DATA"
  ) |>
    factor(levels = c(
      "sitting",
      "lying",
      "crouching/kneeling",
      "standing",
      "stationary",
      "walking",
      "running",
      "ascending stairs",
      "descending stairs",
      "cycling",
      "mixed movement",
      "not coded",
      "RECHECK R CODE/DATA"
    ))

}
get_equal.posture_uwm <- function(vct_posture) {

  case_match(
    vct_posture,
    "sitting"                      ~ "sitting",
    "lying"                        ~ "lying",
    "crouching/kneeling/squatting" ~ "crouching/kneeling",
    "standing"                     ~ "standing",
    "other - posture"              ~ "stationary",
    "walking"                      ~ "walking",
    "stepping"                     ~ "mixed movement",
    "running"                      ~ "running",
    "ascending stairs"             ~ "ascending stairs",
    "descending stairs"            ~ "descending stairs",
    "crouching/squatting"          ~ "mixed movement",
    "cycling"                      ~ "cycling",
    "other - movement"             ~ "mixed movement",
    "intermittent movement"        ~ "mixed movement",
    "dark/obscured/oof"            ~ "not coded",
    .default                       = "RECHECK R CODE/DATA"
  ) |>
    factor(levels = c(
      "sitting",
      "lying",
      "crouching/kneeling",
      "standing",
      "stationary",
      "walking",
      "running",
      "ascending stairs",
      "descending stairs",
      "cycling",
      "mixed movement",
      "not coded",
      "RECHECK R CODE/DATA"
    ))

}
get_broad.posture <- function(vct_equal.posture) {

  case_match(
    vct_equal.posture,
    "sitting"            ~ "sedentary",
    "lying"              ~ "sedentary",
    "crouching/kneeling" ~ "stationary",
    "standing"           ~ "stationary",
    "stationary"         ~ "stationary",
    "walking"            ~ "walking",
    "running"            ~ "running",
    "ascending stairs"   ~ "mixed movement",
    "descending stairs"  ~ "mixed movement",
    "cycling"            ~ "cycling",
    "mixed movement"     ~ "mixed movement",
    "not coded"          ~ "not coded",
    .default             = "RECHECK R CODE/DATA"
  ) |>
    factor(levels = c(
      "sedentary",
      "stationary",
      "walking",
      "running",
      "cycling",
      "mixed movement",
      "not coded",
      "RECHECK R CODE/DATA"
    ))

}
get_sed.posture <- function(vct_equal.posture,
                            vct_equal.behavior) {

  vct_broad <-   case_match(
    vct_equal.posture,
    "sitting"            ~ "sitting",
    "lying"              ~ "lying",
    "crouching/kneeling" ~ "non-sedentary",
    "standing"           ~ "non-sedentary",
    "stationary"         ~ "non-sedentary",
    "walking"            ~ "non-sedentary",
    "running"            ~ "non-sedentary",
    "ascending stairs"   ~ "non-sedentary",
    "descending stairs"  ~ "non-sedentary",
    "cycling"            ~ "non-sedentary",
    "mixed movement"     ~ "non-sedentary",
    "not coded"          ~ "not coded",
    .default             = "RECHECK R CODE/DATA"
  )
  case_when(
    vct_broad %in% c("sitting", "lying")
    & vct_equal.behavior %in% c("travel driving",
                                "travel passenger") ~ "vehicle",
    .default = vct_broad
  ) |>
    factor(levels = c(
      "sitting",
      "lying",
      "vehicle",
      "non-sedentary",
      "not coded",
      "RECHECK R CODE/DATA"
    ))

}
