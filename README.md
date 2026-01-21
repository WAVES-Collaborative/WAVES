
<!-- README.md is generated from README.Rmd. Please edit that file -->

# WAVES

Thank you for your collaboration in the Wrist Algorithm Verification and
Evaluation Study (WAVES). This repository contains all the code needed
to run multiple wrist algorithms against your raw data.

## Setup
## Proposed Flow Diagram 

The proposed flow diagram outlines the process for how the validation data and data pipeline will work with the WAVES team and groups who have potentially available validation data. 

![Flow Diagram](WAVES Diagram.png)


__Full description of the flow diagram coming soon__

## General Pipeline

1.  Install R

    1.  From [this link](https://cran.r-project.org/mirrors.html), click
        on the mirror from the location closest to you.

    2.  On the following page, download and install R for your operating
        system. So far, only Windows and macOS have been tested.

    3.  The code has been tested on v4.4.1 and v4.5.2. We recommend the
        latest v4.5.2. but earlier versions down to 4.4.0 should work.
        Just note you will get warning messages that the packages were
        developed for 4.5.2.

2.  Install RStudio

    1.  From [this link](https://posit.co/download/rstudio-desktop/),
        click on the “DOWNLOAD RSTUDIO DESKTOP FOR WINDOWS/MACOS”
    2.  A version from 2023 onwards should be okay.

3.  Install RTools

    1.  From [this
        link](https://cran.r-project.org/bin/windows/Rtools/), download
        the RTools version specific to the R version being used
        (i.e. Rtools 4.4 for R v4.4.1, RTools 4.5 for R v4.5.2)

4.  Download WAVES repository.

    1.  The location of the WAVES repository can be downloaded anywhere
        on your system, but it is generally recommended to keep it on
        the same drive of the raw accelerometer data.

## Instructions

1.  Open WAVES.RProj

    1.  This will automatically open the RStudio IDE.

2.  In Console, run `renv::restore()`

    1.  This command will download all the R software packages needed
        for the WAVES repository. It will take a while.

3.  Open “\_targets_config.R”

    1.  Navigate to the “Files” tab within Rstudio (located either in
        the bottom left or top left pane of the RStudio window IF you
        haven’t changed the pane layout in “Global Settings”). Click on
        “\_targets*\_*config.R”

    2.  Alternatively, open “\_targets*\_*config.R” with the system File
        Explorer. It should open the script within RStudio.

4.  Within “INPUT” section, change the following:

    1.  `n_workers`: The default is 3 workers, meaning 3 processes of
        the pipeline will run in parallel of each other. From testing, 3
        workers has been the most memory and time efficient for systems
        operating with ≤ 16gb RAM or ≤ 4 cores. If your operating system
        has more RAM and cores available, feel free to increase the
        number of workers, with the max being one less than the number
        of cores available of your system
        (`parallel::detectCores() - 1`)

5.  Run all code within the “INPUT” section (Line 8 - Line 26) and save
    the script.

6.  In Console, run `tar_make().`The “configuration” pipeline is now
    running, which will print messages out in the Console like the below
    image.

    ![](media/Screenshot%202026-01-17%20173935.png)

    The “configuration” pipeline is:

    1.  Downloading and installing non-R software

    2.  Running the code against “configuration” data included within
        the WAVES repository to ensure code is running properly

    3.  This will take awhile! On a potato computer, it took 15-24
        hours!

    4.  If the repository is being ran on a local computer, it is almost
        mandatory that no other work be done while the pipeline is
        running.

7.  Once the pipeline is complete the console should say “ended
    pipeline” with how long it took.

    ![](media/Screenshot%202026-01-20%20234701.png)

    Or it may error like so:

    ![](media/Screenshot%202026-01-20%20000733.png)

    At least `config_miniconda` should be completed, allowing you to
    move on to step 8.

8.  A “summary_miniconda.html” will have been created under the
    “reports” folder of the main WAVES repository. Within the html file:

    1.  Check Miniconda configuration is good.

        1.  All environments should say “Successfuly created”

        2.  All modules should say “Modules successfully installed.”

9.  If the config pipeline errored, please share a screenshot of the
    messages in the console (like the previous screenshot in this
    README, but also show the error message) to the WAVES data team.

10. If the pipeline successfully completed, a
    “summary_pipeline_config.html” file will have been created under the
    “reports” folder of the main WAVES repository. Open the file and
    ensure:

    1.  WAVES_10002 and WAVES_10003 do not go through the pipeline at
        all.

    2.  WAVES_10003 and WAVES_10004 completely go through the pipeline.

    3.  WAVES_10006 goes through everything except for stepcount,
        walmsley and actinet.

    4.  All summary metrics are highlighted green

11. If all conditions for step 10 are met, then the WAVES code is
    working properly on your computer! Woo.

12. Open the “\_targets.R” script

13. Change vct_raw_fpa to your path of raw files.

    1.  Suggest having it lead to directory of files, but then put
        “\[1\]” at the end to make sure pipeline works with one file.
        (image below)

    TODO IMAGE (use UWM computer for this part)

14. Change study_timezone, sample_frequency.

15. Change workers if necessary.

    1.  See Step 4.

16. Run code within “INPUT” section and save the “\_targets.R” script.

17. In Console, run “tar_make()”. For one file that is a whole day, it
    can take anywhere between 15-30 minutes on a potato computer. For
    one file that is a whole week, it may take up to 24 hours.

18. Once the pipeline has completed, a .html file should have been
    created under the “reports” folder called
    “summary_pipeline_main.html”. Open the file and double-check file
    went through entire pipeline successfully under the “By Major Steps”
    section.

19. If pipeline works successfully, close the
    “summary_pipeline_main.html” file and run pipeline on all files
    available by removing the the “\[1\]” at the end of the `file.path`
    function.

    1.  Make sure to rerun the code within “INPUT” section and save the
        “\_targets.R” script.

    2.  This WILL take multiple days if the repository is not being ran
        on a high performance cluster.

    3.  If the repository is being ran on a local computer, it is almost
        mandatory that no other work be done while the pipeline is
        running.

        1.  If the pipeline is interrupted due to an unexpected restart,
            the progress should be saved for major steps within the
            pipeline. Re-follow steps 12-13 the pipeline will pick up
            from the last major step.

20. Open “summary_pipeline_main.html” once again and check to see what
    files have made it through. If all files have made it through, or at
    least the file’s you would’ve expected to be successfully processed,
    share the “WAVES_MERGED_SITE.parquet” with WAVES data team, where
    “SITE” is renamed with the study acronym or the 3-4 letter
    abbreviation given to you by the WAVES study team.

## Notes

- Even if a pipeline finishes successfully, you may in the console
  “There were XX warnings (use warnings() to see them)”. This is normal
  if working on an R version that is not 4.5.2., as the messages will be
  warnings that the packages are meant to work on the latest R version.
  As long as the R version being used is R 4.4 or beyond, everything
  should still work. We have not tested the WAVES repository on R
  versions below 4.4.

- If working on a high performance cluster…TODO (work with Hayden)

- If the pipeline appears “stuck” after 24hrs, as in it looks like there
  has been no change in the processing time or the console messages have
  been the same for quite awhile, its most likely that your computer
  does not have enough computing resources to do parallel processing. In
  that case:

  - Stop the pipeline by clicking the “STOP” button that appears in the
    top right corner of the Console pane.

  ![](media/Screenshot%202026-01-17%20132513.png)

  - Change the `n_workers` object to 1, which will remove parallel
    processing.

## Methods Implemented

| Method | MVPA | SED | Steps |
|----|----|----|----|
| Bakrania 2016(Bakrania et al. 2016) | ❌ | ✔️ | ❌ |
| Ellis 2016(Ellis et al. 2016) Extended | ✔️️ | ✔️ | ❌ |
| Esliger 2011(Esliger et al. 2011) | ✔️ | ✔️ | ❌ |
| Fraysse 2020(Fraysse et al. 2021) | ✔️ | ✔️ | ❌ |
| Hildebrand 2014(Hildebrand et al. 2014)/2017(Hildebrand et al. 2017) | ✔️ | ✔️ | ❌ |
| Mielke 2023(Mielke et al. 2023) | ✔️ | ✔️ | ❌ |
| Montoye 2018(Montoye et al. 2018) | ✔️ | ✔️ | ❌ |
| Trost 2017(Pavey et al. 2017) Extended | ✔️ | ✔️ | ❌ |
| Walmsley 2022(Walmsley et al. 2022) (accelerometer v7.3.0(Doherty et al. 2025)) | ✔️ | ✔️ | ❌ |
| White 2016(White et al. 2016) ENMO linear | ✔️ | ✔️ | ❌ |
| White 2016(White et al. 2016) ENMO polynomial | ✔️ | ✔️ | ❌ |
| White 2016(White et al. 2016) HPFVM linear | ✔️ | ✔️ | ❌ |
| White 2016(White et al. 2016) HPFVM polynomial | ✔️ | ✔️ | ❌ |
| Yuan 2024(Yuan et al. 2024) (actinet) | ✔️ | ✔️ | ❌ |
| ADEPT(Karas et al. 2021) | ❌ | ❌ | ✔️ |
| Oak(Straczkiewicz, Huang, and Onnela 2023) | ❌ | ❌ | ✔️ |
| Small 2024(Small et al. 2024) (stepcount v3.17.1(Chan et al. 2026)) | ❌ | ❌ | ✔️ |
| Step Detection Threshold(Ducharme et al. 2021) (SDT) | ❌ | ❌ | ✔️ |
| Verisense (Original)(Gu et al. 2017) | ❌ | ❌ | ✔️ |
| Verisense (Revised)(Maylor et al. 2022) | ❌ | ❌ | ✔️ |

## TODO

- Test WAVES_10006 CSV file against OxWearable methods.

  - Stepcount
  - Walmsley
  - Actinet

## References for Methods

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-bakrania2016" class="csl-entry">

Bakrania, Kishan, Thomas Yates, Alex V. Rowlands, Dale W. Esliger, Sarah
Bunnewell, James Sanders, Melanie Davies, Kamlesh Khunti, and Charlotte
L. Edwardson. 2016. “Intensity Thresholds on Raw Acceleration Data:
Euclidean Norm Minus One (ENMO) and Mean Amplitude Deviation (MAD)
Approaches.” *PLOS ONE* 11 (10): e0164045.
<https://doi.org/10.1371/journal.pone.0164045>.

</div>

<div id="ref-chan2026" class="csl-entry">

Chan, Shing, Scott R Small, Aidan Acquah, Gert Mertes, and Aiden
Doherty. 2026. *Improved Step Counting via Foundation Models for
Wrist-Worn Accelerometers*. Zenodo.
<https://doi.org/10.5281/zenodo.18255030>.

</div>

<div id="ref-doherty2025" class="csl-entry">

Doherty, Aiden, Shing Chan, Hang Yuan, and Rosemary Walmsley. 2025.
*Accelerometer: A Python Toolkit for Extracting Physical Activity and
Behavior Metrics from Wearable Sensor Data*. Zenodo.
<https://doi.org/10.5281/zenodo.15874476>.

</div>

<div id="ref-ducharme2021" class="csl-entry">

Ducharme, Scott W., Jongil Lim, Michael A. Busa, Elroy J. Aguiar,
Christopher C. Moore, John M. Schuna, Tiago V. Barreira, John
Staudenmayer, Stuart R. Chipkin, and Catrine Tudor-Locke. 2021. “A
Transparent Method for Step Detection Using an Acceleration Threshold,”
October. <https://doi.org/10.1123/jmpb.2021-0011>.

</div>

<div id="ref-ellis2016" class="csl-entry">

Ellis, Katherine, Jacqueline Kerr, Suneeta Godbole, John Staudenmayer,
and Gert Lanckriet. 2016. “Hip and Wrist Accelerometer Algorithms for
Free-Living Behavior Classification.” *Medicine & Science in Sports &
Exercise* 48 (5): 933–40.
<https://doi.org/10.1249/MSS.0000000000000840>.

</div>

<div id="ref-esliger2011" class="csl-entry">

Esliger, Dale W., Ann V. Rowlands, Tina L. Hurst, Michael Catt, Peter
Murray, and Roger G. Eston. 2011. “Validation of the GENEA
Accelerometer.” *Medicine & Science in Sports & Exercise* 43 (6): 1085.
<https://doi.org/10.1249/MSS.0b013e31820513be>.

</div>

<div id="ref-fraysse2021" class="csl-entry">

Fraysse, François, Dannielle Post, Roger Eston, Daiki Kasai, Alex V.
Rowlands, and Gaynor Parfitt. 2021. “Physical Activity Intensity
Cut-Points for Wrist-Worn GENEActiv in Older Adults.” *Frontiers in
Sports and Active Living* 2 (January).
<https://doi.org/10.3389/fspor.2020.579278>.

</div>

<div id="ref-gu2017" class="csl-entry">

Gu, Fuqiang, Kourosh Khoshelham, Jianga Shang, Fangwen Yu, and Zhuo Wei.
2017. “Robust and Accurate Smartphone-Based Step Counting for Indoor
Localization.” *IEEE Sensors Journal* 17 (11): 3453–60.
<https://doi.org/10.1109/JSEN.2017.2685999>.

</div>

<div id="ref-hildebrand2017" class="csl-entry">

Hildebrand, Maria, Bjørge H. Hansen, Vincent T. van Hees, and Ulf
Ekelund. 2017. “Evaluation of Raw Acceleration Sedentary Thresholds in
Children and Adults.” *Scandinavian Journal of Medicine & Science in
Sports* 27 (12): 1814–23. <https://doi.org/10.1111/sms.12795>.

</div>

<div id="ref-hildebrand2014" class="csl-entry">

Hildebrand, Maria, Vincent T. Van Hees, Bjorge Hermann Hansen, and Ulf
Ekelund. 2014. “Age Group Comparability of Raw Accelerometer Output from
Wrist- and Hip-Worn Monitors.” *Medicine & Science in Sports & Exercise*
46 (9): 1816. <https://doi.org/10.1249/MSS.0000000000000289>.

</div>

<div id="ref-karas2021" class="csl-entry">

Karas, Marta, Marcin Stra Czkiewicz, William Fadel, Jaroslaw Harezlak,
Ciprian M. Crainiceanu, and Jacek K. Urbanek. 2021. “Adaptive empirical
pattern transformation (ADEPT) with application to walking stride
segmentation.” *Biostatistics (Oxford, England)* 22 (2): 331–47.
<https://doi.org/10.1093/biostatistics/kxz033>.

</div>

<div id="ref-maylor2022" class="csl-entry">

Maylor, Benjamin D., Charlotte L. Edwardson, Paddy C. Dempsey, Matthew
R. Patterson, Tatiana Plekhanova, Tom Yates, and Alex V. Rowlands. 2022.
“Stepping Towards More Intuitive Physical Activity Metrics with
Wrist-Worn Accelerometry: Validity of an Open-Source Step-Count
Algorithm.” *Sensors* 22 (24). <https://doi.org/10.3390/s22249984>.

</div>

<div id="ref-mielke2023" class="csl-entry">

Mielke, Gregore Iven, Márcio de Almeida Mendes, Ulf Ekelund, Alex V.
Rowlands, Felipe Fossati Reichert, and Inacio Crochemore-Silva. 2023.
“Absolute Intensity Thresholds for Tri-Axial Wrist and Waist
Accelerometer-Measured Movement Behaviors in Adults.” *Scandinavian
Journal of Medicine & Science in Sports* 33 (9): 1752–64.
<https://doi.org/10.1111/sms.14416>.

</div>

<div id="ref-montoye2018" class="csl-entry">

Montoye, Alexander H. K., Bradford S. Westgate, Morgan R. Fonley, and
Karin A. Pfeiffer. 2018. “Cross-validation and out-of-sample testing of
physical activity intensity predictions with a wrist-worn
accelerometer.” *Journal of Applied Physiology (Bethesda, Md.: 1985)*
124 (5): 1284–93. <https://doi.org/10.1152/japplphysiol.00760.2017>.

</div>

<div id="ref-pavey2017" class="csl-entry">

Pavey, Toby G., Nicholas D. Gilson, Sjaan R. Gomersall, Bronwyn Clark,
and Stewart G. Trost. 2017. “Field evaluation of a random forest
activity classifier for wrist-worn accelerometer data.” *Journal of
Science and Medicine in Sport* 20 (1): 75–80.
<https://doi.org/10.1016/j.jsams.2016.06.003>.

</div>

<div id="ref-small2024" class="csl-entry">

Small, Scott R., Shing Chan, Rosemary Walmsley, Lennart Von Fritsch,
Aidan Acquah, Gert Mertes, Benjamin G. Feakins, et al. 2024.
“Self-Supervised Machine Learning to Characterize Step Counts from
Wrist-Worn Accelerometers in the UK Biobank.” *Medicine & Science in
Sports & Exercise* 56 (10): 1945.
<https://doi.org/10.1249/MSS.0000000000003478>.

</div>

<div id="ref-straczkiewicz2023" class="csl-entry">

Straczkiewicz, Marcin, Emily J. Huang, and Jukka-Pekka Onnela. 2023. “A
“One-Size-Fits-Most” Walking Recognition Method for Smartphones,
Smartwatches, and Wearable Accelerometers.” *Npj Digital Medicine* 6
(1): 29. <https://doi.org/10.1038/s41746-022-00745-z>.

</div>

<div id="ref-walmsley2022" class="csl-entry">

Walmsley, Rosemary, Shing Chan, Karl Smith-Byrne, Rema Ramakrishnan,
Mark Woodward, Kazem Rahimi, Terence Dwyer, Derrick Bennett, and Aiden
Doherty. 2022. “Reallocation of Time Between Device-Measured Movement
Behaviours and Risk of Incident Cardiovascular Disease,” September.
<https://doi.org/10.1136/bjsports-2021-104050>.

</div>

<div id="ref-white2016" class="csl-entry">

White, Tom, Kate Westgate, Nicholas J. Wareham, and Soren Brage. 2016.
“Estimation of Physical Activity Energy Expenditure During Free-Living
from Wrist Accelerometry in UK Adults.” *PLOS ONE* 11 (12): e0167472.
<https://doi.org/10.1371/journal.pone.0167472>.

</div>

<div id="ref-yuan2024" class="csl-entry">

Yuan, Hang, Shing Chan, Andrew P. Creagh, Catherine Tong, Aidan Acquah,
David A. Clifton, and Aiden Doherty. 2024. “Self-Supervised Learning for
Human Activity Recognition Using 700,000 Person-Days of Wearable Data.”
*Npj Digital Medicine* 7 (1): 91.
<https://doi.org/10.1038/s41746-024-01062-3>.

</div>

</div>
