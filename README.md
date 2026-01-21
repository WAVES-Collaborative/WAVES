
<!-- README.md is generated from README.Rmd. Please edit that file -->

# WAVES

Thank you for your collaboration in the Wrist Algorithm Verification and
Evaluation Study (WAVES). This repository contains all the code needed
to run multiple wrist algorithms against your raw data.

## Proposed Flow Diagram

The proposed flow diagram outlines the process for how the validation
data and data pipeline will work with the WAVES team and groups who have
potentially available validation data.

<figure>
<img src="media/WAVES%20Diagram.png" alt="Flow Diagram" />
<figcaption aria-hidden="true">Flow Diagram</figcaption>
</figure>

**Full description of the flow diagram coming soon** \## Setup

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
| Bakrania 2016<sup>1</sup> | ❌ | ✔️ | ❌ |
| Ellis 2016<sup>2</sup> Extended | ✔️️ | ✔️ | ❌ |
| Esliger 2011<sup>3</sup> | ✔️ | ✔️ | ❌ |
| Fraysse 2020<sup>4</sup> | ✔️ | ✔️ | ❌ |
| Hildebrand 2014<sup>5</sup>/2017<sup>6</sup> | ✔️ | ✔️ | ❌ |
| Mielke 2023<sup>7</sup> | ✔️ | ✔️ | ❌ |
| Montoye 2018<sup>8</sup> | ✔️ | ✔️ | ❌ |
| Trost 2017<sup>9</sup> Extended | ✔️ | ✔️ | ❌ |
| Walmsley 2022<sup>10</sup> (accelerometer v7.3.0<sup>11</sup>) | ✔️ | ✔️ | ❌ |
| White 2016<sup>12</sup> ENMO linear | ✔️ | ✔️ | ❌ |
| White 2016<sup>12</sup> ENMO polynomial | ✔️ | ✔️ | ❌ |
| White 2016<sup>12</sup> HPFVM linear | ✔️ | ✔️ | ❌ |
| White 2016<sup>12</sup> HPFVM polynomial | ✔️ | ✔️ | ❌ |
| Yuan 2024<sup>13</sup> (actinet) | ✔️ | ✔️ | ❌ |
| Oak<sup>14</sup> | ❌ | ❌ | ✔️ |
| Small 2024<sup>15</sup> (stepcount v3.17.1<sup>16</sup>) | ❌ | ❌ | ✔️ |
| Step Detection Threshold<sup>17</sup> (SDT) | ❌ | ❌ | ✔️ |
| Verisense (Original)<sup>18</sup> | ❌ | ❌ | ✔️ |
| Verisense (Revised)<sup>19</sup> | ❌ | ❌ | ✔️ |

## TODO

- Test WAVES_10006 CSV file against OxWearable methods.

  - Stepcount
  - Walmsley
  - Actinet

## References for Methods

<div id="refs" class="references csl-bib-body">

<div id="ref-bakrania2016" class="csl-entry">

<span class="csl-left-margin">1.
</span><span class="csl-right-inline">Bakrania K, Yates T, Rowlands AV,
et al. [Intensity Thresholds on Raw Acceleration Data: Euclidean Norm
Minus One (ENMO) and Mean Amplitude Deviation (MAD)
Approaches](https://doi.org/10.1371/journal.pone.0164045). *PLOS ONE*
2016; 11: e0164045.</span>

</div>

<div id="ref-ellis2016" class="csl-entry">

<span class="csl-left-margin">2.
</span><span class="csl-right-inline">Ellis K, Kerr J, Godbole S, et al.
[Hip and wrist accelerometer algorithms for free-living behavior
classification](https://doi.org/10.1249/MSS.0000000000000840). *Medicine
& Science in Sports & Exercise* 2016; 48: 933–940.</span>

</div>

<div id="ref-esliger2011" class="csl-entry">

<span class="csl-left-margin">3.
</span><span class="csl-right-inline">Esliger DW, Rowlands AV, Hurst TL,
et al. [Validation of the GENEA
accelerometer](https://doi.org/10.1249/MSS.0b013e31820513be). *Medicine
& Science in Sports & Exercise* 2011; 43: 1085.</span>

</div>

<div id="ref-fraysse2021" class="csl-entry">

<span class="csl-left-margin">4.
</span><span class="csl-right-inline">Fraysse F, Post D, Eston R, et al.
Physical activity intensity cut-points for wrist-worn GENEActiv in older
adults. *Frontiers in Sports and Active Living*; 2. Epub ahead of print
15 January 2021. DOI:
[10.3389/fspor.2020.579278](https://doi.org/10.3389/fspor.2020.579278).</span>

</div>

<div id="ref-hildebrand2014" class="csl-entry">

<span class="csl-left-margin">5.
</span><span class="csl-right-inline">Hildebrand M, Van Hees VT, Hansen
BH, et al. [Age group comparability of raw accelerometer output from
wrist- and hip-worn
monitors](https://doi.org/10.1249/MSS.0000000000000289). *Medicine &
Science in Sports & Exercise* 2014; 46: 1816.</span>

</div>

<div id="ref-hildebrand2017" class="csl-entry">

<span class="csl-left-margin">6.
</span><span class="csl-right-inline">Hildebrand M, Hansen BH, Hees VT
van, et al. [Evaluation of raw acceleration sedentary thresholds in
children and adults](https://doi.org/10.1111/sms.12795). *Scandinavian
Journal of Medicine & Science in Sports* 2017; 27: 1814–1823.</span>

</div>

<div id="ref-mielke2023" class="csl-entry">

<span class="csl-left-margin">7.
</span><span class="csl-right-inline">Mielke GI, Almeida Mendes M de,
Ekelund U, et al. [Absolute intensity thresholds for tri-axial wrist and
waist accelerometer-measured movement behaviors in
adults](https://doi.org/10.1111/sms.14416). *Scandinavian Journal of
Medicine & Science in Sports* 2023; 33: 1752–1764.</span>

</div>

<div id="ref-montoye2018" class="csl-entry">

<span class="csl-left-margin">8.
</span><span class="csl-right-inline">Montoye AHK, Westgate BS, Fonley
MR, et al. [Cross-validation and out-of-sample testing of physical
activity intensity predictions with a wrist-worn
accelerometer](https://doi.org/10.1152/japplphysiol.00760.2017).
*Journal of Applied Physiology (Bethesda, Md: 1985)* 2018; 124:
1284–1293.</span>

</div>

<div id="ref-pavey2017" class="csl-entry">

<span class="csl-left-margin">9.
</span><span class="csl-right-inline">Pavey TG, Gilson ND, Gomersall SR,
et al. [Field evaluation of a random forest activity classifier for
wrist-worn accelerometer
data](https://doi.org/10.1016/j.jsams.2016.06.003). *Journal of Science
and Medicine in Sport* 2017; 20: 75–80.</span>

</div>

<div id="ref-walmsley2022" class="csl-entry">

<span class="csl-left-margin">10.
</span><span class="csl-right-inline">Walmsley R, Chan S, Smith-Byrne K,
et al. Reallocation of time between device-measured movement behaviours
and risk of incident cardiovascular disease. Epub ahead of print 1
September 2022. DOI:
[10.1136/bjsports-2021-104050](https://doi.org/10.1136/bjsports-2021-104050).</span>

</div>

<div id="ref-doherty2025" class="csl-entry">

<span class="csl-left-margin">11.
</span><span class="csl-right-inline">Doherty A, Chan S, Yuan H, et al.
*Accelerometer: A python toolkit for extracting physical activity and
behavior metrics from wearable sensor data*. Zenodo. Epub ahead of print
13 July 2025. DOI:
[10.5281/zenodo.15874476](https://doi.org/10.5281/zenodo.15874476).</span>

</div>

<div id="ref-white2016" class="csl-entry">

<span class="csl-left-margin">12.
</span><span class="csl-right-inline">White T, Westgate K, Wareham NJ,
et al. [Estimation of Physical Activity Energy Expenditure during
Free-Living from Wrist Accelerometry in UK
Adults](https://doi.org/10.1371/journal.pone.0167472). *PLOS ONE* 2016;
11: e0167472.</span>

</div>

<div id="ref-yuan2024" class="csl-entry">

<span class="csl-left-margin">13.
</span><span class="csl-right-inline">Yuan H, Chan S, Creagh AP, et al.
[Self-supervised learning for human activity recognition using 700,000
person-days of wearable
data](https://doi.org/10.1038/s41746-024-01062-3). *npj Digital
Medicine* 2024; 7: 91.</span>

</div>

<div id="ref-straczkiewicz2023" class="csl-entry">

<span class="csl-left-margin">14.
</span><span class="csl-right-inline">Straczkiewicz M, Huang EJ, Onnela
J-P. [A “one-size-fits-most” walking recognition method for smartphones,
smartwatches, and wearable
accelerometers](https://doi.org/10.1038/s41746-022-00745-z). *npj
Digital Medicine* 2023; 6: 29.</span>

</div>

<div id="ref-small2024" class="csl-entry">

<span class="csl-left-margin">15.
</span><span class="csl-right-inline">Small SR, Chan S, Walmsley R, et
al. [Self-supervised machine learning to characterize step counts from
wrist-worn accelerometers in the UK
biobank](https://doi.org/10.1249/MSS.0000000000003478). *Medicine &
Science in Sports & Exercise* 2024; 56: 1945.</span>

</div>

<div id="ref-chan2026" class="csl-entry">

<span class="csl-left-margin">16.
</span><span class="csl-right-inline">Chan S, Small SR, Acquah A, et al.
*Improved step counting via foundation models for wrist-worn
accelerometers*. Zenodo. Epub ahead of print 15 January 2026. DOI:
[10.5281/zenodo.18255030](https://doi.org/10.5281/zenodo.18255030).</span>

</div>

<div id="ref-ducharme2021" class="csl-entry">

<span class="csl-left-margin">17.
</span><span class="csl-right-inline">Ducharme SW, Lim J, Busa MA, et
al. A Transparent Method for Step Detection Using an Acceleration
Threshold. Epub ahead of print 25 October 2021. DOI:
[10.1123/jmpb.2021-0011](https://doi.org/10.1123/jmpb.2021-0011).</span>

</div>

<div id="ref-gu2017" class="csl-entry">

<span class="csl-left-margin">18.
</span><span class="csl-right-inline">Gu F, Khoshelham K, Shang J, et
al. [Robust and accurate smartphone-based step counting for indoor
localization](https://doi.org/10.1109/JSEN.2017.2685999). *IEEE Sensors
Journal* 2017; 17: 3453–3460.</span>

</div>

<div id="ref-maylor2022" class="csl-entry">

<span class="csl-left-margin">19.
</span><span class="csl-right-inline">Maylor BD, Edwardson CL, Dempsey
PC, et al. Stepping towards More Intuitive Physical Activity Metrics
with Wrist-Worn Accelerometry: Validity of an Open-Source Step-Count
Algorithm. *Sensors*; 22. Epub ahead of print 18 December 2022. DOI:
[10.3390/s22249984](https://doi.org/10.3390/s22249984).</span>

</div>

</div>
