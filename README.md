
<!-- README.md is generated from README.Rmd. Please edit that file -->

# WAVES

Julian says helpme, my potato is a computer.

## Proposed Flow Diagram 

The proposed flow diagram outlines the process for how the validation data and data pipeline will work with the WAVES team and groups who have potentially available validation data. 

![Flow Diagram](WAVES Diagram.png)


__Full description of the flow diagram coming soon__

## General Pipeline

For the WAVES project, two “studies” are conceptualized to address the
primary purpose of the project.

Study \#1 is evaluating multiple wrist-worn methods against direct
observation with participants wearing wrist-worn accelerometers within
“visits” that have a duration less than a waking day. Study \#2 is
evaluating the same wrist-worn methods against a criterion method, but
now within the “field” where participants are asked to wear the
wrist-worn accelerometers for their waking day.

The data wrangling/processing pipeline is mostly be the same for both
studies, with more parts of GGIR used for Study \#2. The pipeline uses
the `targets` R package to run custom functions in parallel, as well as
helps keep track that all functions downstream from each other work and
are updated. The parallel processing is for when we share the repository
to study sites, the “making sure everything works downstream” is for
WAVES team members during testing the pipeline.

1.  Run GGIR on raw accelerometer data

    1.  Tested on RAW .gt3x files. Files will ideally be “clean”. That
        is, these files:

        1.  All have the same file naming scheme/format

        2.  Are named to have the correct participant ID

        3.  Are named to have the correct wear location or at least
            guaranteed all files in provided directories come from the
            same wear location.

        4.  Have followed all the initialization procedures provided
            from the data collection site.

    2.  Utilize “data/GGIR/config_WAVES_visit.csv” to have GGIR
        autocalibrate raw acceleration and produce metrics such as ENMO,
        ENMOa, and others needed for cutpoints.

2.  Read in raw accelerometer data separate from GGIR

    1.  Re-calibrate data using GGIR auto-calibration output.

3.  Apply methods to metric and visit data

    1.  Cutpoints to metric data from GGIR

    2.  Methods to raw parquet data

        1.  Montoye
        2.  ADEPT
        3.  SDT
        4.  Verisense
        5.  Trost
        6.  Ellis
        7.  oak

    3.  Python models to raw csv data (actinet , accelerometer,
        stepcount(?)

    4.  Variable names will be:

        1.  intensity\_\[METHOD\] (i.e. intensity_montoye.rf)

        2.  steps\_\[METHOD\] (i.e. steps_adept)

    5.  Output saved as:

        1.  data/2_INTERIM/OUTPUT-CUTPOINT_PARQUET

        2.  data/2_INTERIM/OUTPUT-MODEL_PARQUET

        3.  data/actinet

        4.  data/stepcount

        5.  data/walmsley

## Methods Implemented

| Processing                                                       | Method                                                                                                                                            | Output                           | In Pipeline? |
|------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------|--------------|
| Custom + actimetric                                              | Ellis 2016                                                                                                                                        | Posture/Movement -\> Intensity   | ✔️           |
| Custom + actimetric                                              | Trost Extended                                                                                                                                    | Posture/Movement -\> Intensity   | ✔️           |
| Custom (Adapt Lily Koff’s code / `adept` R package)              | ADEPT (Karas 2021)                                                                                                                                | Steps                            | ✔️           |
| Python `forest`                                                  | Oak (Straczkiewicz 2023)                                                                                                                          | Steps                            | ✔️           |
| Custom (Adapt Lily Koff’s code / muschelli’s `walking` R package | Step Detection Threshold (SDT, Ducharme 2021)                                                                                                     | Steps                            | ✔️           |
| Adapted from Muschelli’s `walking` R package                     | Verisense                                                                                                                                         | Steps                            | ✔️           |
| Python `stepcount`                                               | stepcount 3.16.2 (Chan 2023)                                                                                                                      | Steps                            | ✔️           |
| GGIR                                                             | Bakrania 2016                                                                                                                                     | Intensity (Sed only)             | ✔️           |
| GGIR                                                             | Esliger 2011                                                                                                                                      | Intensity                        | ✔️           |
| GGIR                                                             | Fraysse 2020                                                                                                                                      | Intensity                        | ✔️           |
| GGIR                                                             | Hildebrand 2014/2016 AG wrist                                                                                                                     | Intensity                        | ✔️           |
| GGIR                                                             | Mielke 2023 AG wrist                                                                                                                              | Intensity                        | ✔️           |
| GGIR                                                             | White 2016 ENMO linear                                                                                                                            | Intensity                        | ✔️           |
| GGIR                                                             | White 2016 ENMO polynomial                                                                                                                        | Intensity                        | ✔️           |
| GGIR                                                             | White 2016 HPFVM linear                                                                                                                           | Intensity                        | ✔️           |
| GGIR                                                             | White 2016 HPFVM polynomial                                                                                                                       | Intensity                        | ✔️           |
| GGIR + custom                                                    | Montoye 2018                                                                                                                                      | Intensity                        | ✔️           |
| Python `actinet`                                                 | OxWearables SSL (Yuan et al, 2024) OxWearables/ssl-wearables: Self-supervised learning for wearables using the UK-Biobank (\>700,000 person-days) | Posture (Extrapolate Intensity?) | ✔️           |
| Python `accelerometer`                                           | OxWearables Bio Bank Accelerometer Analysis                                                                                                       | Intensity                        | ✔️           |

## TODO

- Test RAW CSV files against OxWearable methods.

  - CSVs need to be pre-formatted to not include metadata within the
    first few lines

- In “config.qmd”, run WAVES functions against configuration files.

- Create “user-facing” readme with the following:

1.  Install R
2.  Install RStudio
3.  Install RTools
4.  Download WAVES repository.
5.  Open WAVES.RProj
6.  In Console, run renv::restore()
7.  Open “quarto/config.qmd” file and “knit” to create
    “summary_config.html” under reports.
    1.  This will make sure miniconda is setup
    2.  This will also run WAVES functions against
8.  Put in directory paths to raw accelerometer data within “INPUT”
    section.
9.  Change study_sampling_frequency and my_tz as needed.
10. Select all lines within “INPUT” section, then Ctrl+Enter.
11. In console, type `tar_make()` and press Enter.
12. Share `WAVES_OUTPUT` file(s) with WAVES working group, criterion
    data, and meta data

- Work on making a WAVES website using pkgdown? Doesn’t have to be fancy
  but I can see it being really helpful than a readme, or making a
  private youtube video?

- Create “technical-facing” readme

- Have site provide a key for breaking down ID?

  - The ID for now is the filename of the raw accelerometer data. FLAC
    UWM data follows a naming scheme of
    “\[STUDY\]\_\[MONITOR\]\_\[LOCATION\]\_\[SUBJECT\]\_\[VISIT\]RAW”
    where I think it would be nice to identify all data from a site with
    at least \[STUDY\] and \[SUBJECT\]. Maybe ask for a key to get this
    from ID/filenames? Or just ask in an email?
