
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
<img src="man/figures/WAVES%20Diagram.png" alt="Flow Diagram" />
<figcaption aria-hidden="true">Flow Diagram</figcaption>
</figure>

**Full description of the flow diagram coming soon**

## Preliminary Setup

For the following steps, administrator access should not be needed.

1.  Install R

    1.  From this [CRAN mirrors
        webpage](https://cran.r-project.org/mirrors.html), click on the
        mirror from the location closest to you.

    2.  On the following page, download and install R for your operating
        system. So far, only Windows and macOS have been tested.

    3.  The code has been tested on v4.4.1 and v4.5.2. We recommend the
        latest v4.5.2. but earlier versions down to 4.4.0 should work.
        Just note you will get warning messages that the packages were
        developed for 4.5.2.

2.  Install RStudio

    1.  From this [POSIT RStudio
        webpage](https://posit.co/download/rstudio-desktop/), click on
        the “DOWNLOAD RSTUDIO DESKTOP FOR WINDOWS/MACOS”
    2.  A version from 2023 onwards should be okay.

3.  Install compilation tools (platform-specific)

    **Windows:**

    1.  From the [CRAN RTools
        webpage](https://cran.r-project.org/bin/windows/Rtools/),
        download the RTools version specific to the R version being used
        (i.e. Rtools 4.4 for R v4.4.1, RTools 4.5 for R v4.5.2)

    **macOS:**

    Several R packages need to be compiled from source. Install the
    following dependencies via [Homebrew](https://brew.sh/) by running
    the following command within the system shell (Terminal):

        brew install cmake gcc gettext

    Then create `~/.R/Makevars` so R can find the installed libraries by
    running the following code within the system shell (Terminal):

        mkdir -p ~/.R
        cat > ~/.R/Makevars << 'EOF'
        CPPFLAGS += -I/opt/homebrew/opt/gettext/include
        LDFLAGS += -L/opt/homebrew/opt/gettext/lib -lintl -L/opt/homebrew/opt/gcc/lib/gcc/current
        FLIBS = -L/opt/homebrew/opt/gcc/lib/gcc/current -lgfortran -lquadmath
        FC = /opt/homebrew/bin/gfortran
        F77 = /opt/homebrew/bin/gfortran
        EOF

    Additionally, the `arrow` R package requires setting an environment
    variable before running `renv::restore()`. Run the following within
    the system shell (Terminal):

        export LIBARROW_BINARY=true

    This tells the package to download a pre-built Arrow C++ library
    instead of compiling against the system version.

4.  Install Git by going to [git install
    webpage](https://git-scm.com/install/). There, select your operating
    system and follow the instructions on the webpage.

    1.  In the git setup wizard, select all the default options.

5.  Open the RStudio application. In the Navigation bar at the top left
    corner, click File -\> New Project…

    ![](man/figures/rstudio_new_project.png)

6.  In the New Project wizard pop-up, click “Version Control”.

    ![](man/figures/rstudio_version_control.png)

7.  Then, click “Git”.

    ![](man/figures/rstudio_version_control2.png)

8.  Go to the WAVES github page, Click on the green “Code” button, and
    click the “Copy to Clipboard” button.

    ![](man/figures/github_link.png)

9.  Go back to the RStudio New Project pop-up and paste the WAVES Github
    link in the Repository URL section. The “Project directory name”
    will automatically appear as WAVES.

    ![](man/figures/rstudio_version_control3.png)

10. Choose location of WAVES repository in the “Create project as
    subdirectory of” section.

    1.  The location of the WAVES repository can generally be downloaded
        anywhere on your system. However, it is ***HIGHLY*** recommended
        to:
        1.  Download the repository on a mapped network drive or on the
            actual computer system itself, ***NOT*** on the cloud such
            as OneDrive.
        2.  Not save it directly under a directory with the following
            names: `bash`, `data`, `logs`, `media`, `models`, `quarto`,
            `R`, `renv` or `reports.`
        3.  Keep it on the same drive where the raw accelerometer data
            is located.

11. Click the “Create Project” button.

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
| White 2016<sup>12</sup> ENMO and HPFVM | ✔️ | ✔️ | ❌ |
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
et al. [Intensity thresholds on raw acceleration data: Euclidean norm
minus one (ENMO) and mean amplitude deviation (MAD)
approaches](https://doi.org/10.1371/journal.pone.0164045). *PLOS ONE*
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
et al. [Estimation of physical activity energy expenditure during
free-living from wrist accelerometry in UK
adults](https://doi.org/10.1371/journal.pone.0167472). *PLOS ONE* 2016;
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
al. A transparent method for step detection using an acceleration
threshold. Epub ahead of print 25 October 2021. DOI:
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
PC, et al. Stepping towards more intuitive physical activity metrics
with wrist-worn accelerometry: Validity of an open-source step-count
algorithm. *Sensors*; 22. Epub ahead of print 18 December 2022. DOI:
[10.3390/s22249984](https://doi.org/10.3390/s22249984).</span>

</div>

</div>
