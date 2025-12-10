
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

    1.  Tested only on RAW CSV exported from ActiLife. Files will
        ideally be files that are “clean”. That is, these files:

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

    2.  Save visit data into parquet file format under
        “data/2_INTERIM/OUTPUT-RAW_PARQUET”

3.  Apply methods to metric and visit data

    1.  Cutpoints to metric data from GGIR

    2.  Methods to raw parquet data

        1.  Montoye
        2.  ADEPT
        3.  SDT
        4.  Verisense
        5.  Trost
        6.  Ellis

    3.  Python models to raw csv data (actinet , accelerometer,
        stepcount(?), forest/oak(?))

    4.  Variable names will be:

        1.  intensity\_\[METHOD\] (i.e. intensity_montoye.rf)

        2.  steps\_\[METHOD\] (i.e. steps_adept)

    5.  Output saved as:

        1.  data/2_INTERIM/OUTPUT-CUTPOINT_PARQUET

        2.  data/2_INTERIM/OUTPUT-MODEL_PARQUET

        3.  ??data/oxwearables/actinet??;
            ??data/oxwearables/accelerometer??,
            ??data/oxwearables/stepcount??, ??data/forest??

## Methods Implemented

<table style="width:99%;">
<colgroup>
<col style="width: 34%" />
<col style="width: 48%" />
<col style="width: 11%" />
<col style="width: 5%" />
</colgroup>
<thead>
<tr class="header">
<th>Processing</th>
<th>Method</th>
<th>Output</th>
<th>In Pipeline?</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Custom + actimetric</td>
<td>Ellis 2016</td>
<td>Posture/Movement -&gt; Intensity</td>
<td>✔️ |</td>
</tr>
<tr class="even">
<td>Custom + actimetric</td>
<td>Trost Extended</td>
<td>Posture/Movement -&gt; Intensity</td>
<td>✔️ |</td>
</tr>
<tr class="odd">
<td>Custom + actimetric (??)</td>
<td>SydneyGroup Model (??)</td>
<td>??</td>
<td>❌</td>
</tr>
<tr class="even">
<td>Custom (Adapt Lily Koff’s code / <code>adept</code> R package)</td>
<td>ADEPT (Karas 2021)</td>
<td>Steps</td>
<td>✔️ |</td>
</tr>
<tr class="odd">
<td><p>Custom (Adapt Lily Koffs’s code / muschelli’s
<code>walking</code> R package</p>
<p>OR</p>
<p>Python <code>forest</code></p></td>
<td>Oak (Straczkiewicz 2023)</td>
<td>Steps</td>
<td>❌</td>
</tr>
<tr class="even">
<td>Custom (Adapt Lily Koff’s code / muschelli’s <code>walking</code> R
package</td>
<td>Step Detection Threshold (SDT, Ducharme 2021)</td>
<td>Steps</td>
<td>✔️ |</td>
</tr>
<tr class="odd">
<td>Muschelli’s <code>walking</code> R package</td>
<td>Verisense</td>
<td>Steps</td>
<td>✔️ |</td>
</tr>
<tr class="even">
<td><p>Custom (Adapt Lily Koff’s code / <code>stepcount</code> R package
that is a wrapper around OxWearables stepcount</p>
<p>OR</p>
<p>Python <code>stepcount</code></p></td>
<td>stepcount 3.16.2 (Chan 2023)</td>
<td>Steps</td>
<td>❌</td>
</tr>
<tr class="odd">
<td>GGIR</td>
<td>Bakrania 2016</td>
<td>Intensity (Sed only)</td>
<td>(In Progress)</td>
</tr>
<tr class="even">
<td>GGIR</td>
<td>Esliger 2011</td>
<td>Intensity</td>
<td>✔️ |</td>
</tr>
<tr class="odd">
<td>GGIR</td>
<td>Fraysse 2020</td>
<td>Intensity</td>
<td>✔️ |</td>
</tr>
<tr class="even">
<td>GGIR</td>
<td>Hildebrand 2014/2016 AG wrist</td>
<td>Intensity</td>
<td>✔️ |</td>
</tr>
<tr class="odd">
<td>GGIR</td>
<td>Mielke 2023 AG wrist</td>
<td>Intensity</td>
<td>✔️ |</td>
</tr>
<tr class="even">
<td>GGIR</td>
<td>White 2016 ENMO linear</td>
<td>Intensity</td>
<td>✔️ |</td>
</tr>
<tr class="odd">
<td>GGIR</td>
<td>White 2016 ENMO polynomial</td>
<td>Intensity</td>
<td>✔️ |</td>
</tr>
<tr class="even">
<td>GGIR</td>
<td>White 2016 HPFVM linear</td>
<td>Intensity</td>
<td>✔️ |</td>
</tr>
<tr class="odd">
<td>GGIR</td>
<td>White 2016 HPFVM polynomial</td>
<td>Intensity</td>
<td>✔️ |</td>
</tr>
<tr class="even">
<td>GGIR + custom</td>
<td>Montoye 2018 (get metrics from <code>myfun</code> that will also get
Verisense steps)</td>
<td>Intensity</td>
<td>✔️ |</td>
</tr>
<tr class="odd">
<td>Python <code>actinet</code></td>
<td>OxWearables SSL (Yuan et al, 2024) OxWearables/ssl-wearables:
Self-supervised learning for wearables using the UK-Biobank (&gt;700,000
person-days)</td>
<td>Posture (Extrapolate Intensity?)</td>
<td>❌</td>
</tr>
<tr class="even">
<td>Python <code>accelerometer</code></td>
<td>OxWearables Bio Bank Accelerometer Analysis</td>
<td>Intensity</td>
<td>❌</td>
</tr>
</tbody>
</table>

## TODO

- Implement Python methods

  - I added a “src” directory to save any python scripts, but this can
    easily be changed

- Have a function to convert criterion data into a parquet for sending
  to JM?

  - My idea is to have site users provide a file path to the csv that
    holds the DO/activPAL/WearableCamera/whatever criterion measures for
    ALL visit/field data where the function just simply converts it to
    parquet file format to send to JM, along with
    “WAVES_OUTPUT-\[VISIT/FIELD\].parquet” and Python output

- Control/checks if field and visit data have same filenames?

  - The whole reason why there is visit and field distinctions is due to
    the FLAC UWM dataset (Strath) has both DO “visit” data and 7-day
    “field” data where participants where also asked to wear activpal.
    Visit and 7day data are named distinctly from each other, and will
    not overlap. If another site that contributes to WAVES also have
    visit and field data, the filenames should also be
    different…hopefully.

- Create “user-facing” readme with the following:

1.  Install R
2.  Install RStudio
3.  Install RTools
4.  Download WAVES repository.
5.  Open WAVES.RProj
6.  In Console, run renv::restore()
7.  In console, type `tar_make()` and make sure pipeline works against
    test data (TODO).
8.  Put in directory paths to raw accelerometer data within “INPUT”
    section.
9.  Change study_sampling_frequency and my_tz as needed.
10. Select all lines within “INPUT” section, then Ctrl+Enter.
11. In console, type `tar_make()` and press Enter.
12. Share `WAVES_OUTPUT` file(s) with WAVES working group, criterion
    data, and demographic data

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
