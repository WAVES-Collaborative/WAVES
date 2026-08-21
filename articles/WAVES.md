# Introduction to WAVES

Before continuing, please ensure you have set up your computer according
to the “Preliminary Setup” section of the README.

## Configuration Pipeline

### renv

1.  If you are continuing from “Preliminary Steps” in the README, then
    go to Step 2.

    1.  If not, Open your FIle Explorer application and navigate to
        where you downloaded the WAVES repository.

    2.  In the WAVES repository, open the “WAVES.RProj” file. This will
        automatically open the RStudio IDE.

2.  Click anywhere within the **Console** pane. The blinking cursor
    should now appear in the **Console** pane.

3.  Type
    [`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)
    in the Console pane and then press the “Enter” key. This will run
    the
    [`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)
    function.

4.  In the **Console** pane, you will see a bunch of text appear. At the
    bottom it will ask if “Do you want to proceed?”. Type “Y” and press
    the “Enter” key.

    ![](images/renv_restore_proceed.png)

    1.  This will download all the R software packages needed for the
        WAVES repository. It will take a while.

### \_targets_config.R

5.  Navigate to the **Files** pane within Rstudio (located either in the
    bottom right area of the RStudio window *if* you haven’t changed the
    pane layout in “Global Settings”). Click on “\_targets*\_*config.R”
    to open it. This will open the “\_targets_config.R” script within
    the **Source** pane in the top left area of your RStudio window.

    ![](images/files_pane.png)

    1.  Alternatively, open “\_targets*\_*config.R” script with the
        system File Explorer. It should open within the **Source** pane
        of RStudio.

6.  Within the “INPUT” section of the “\_targets_config.R” script, check
    the following:

    ![](images/section_input.png)

    1.  `RETICULATE_MINICONDA_PATH`: The path to an existing conda
        installation or where you want a new minconda distribution to be
        installed.
        1.  If this is changed, please ensure there are no spaces within
            the file path
        2.  **NOTE**: that if the WAVES directory is located under
            another directory with the names `bash`, `data`, `logs`,
            `media`, `models`, `quarto`, `R`, `renv` or `reports`, the
            config pipeline will install miniconda underneath the
            corresponding folder *in the WAVES directory*.
            1.  For example, if the WAVES directory was installed under
                “/data/martinez/”, then miniconda will be installed in
                “/data/martinez/WAVES/data/martinez/r-miniconda”
    2.  `n_workers`: The default is 2 workers, meaning 2 processes of
        the pipeline will run in parallel of each other.
        1.  `n_workers` should always be at least 2!
        2.  If your operating system has more RAM and cores available,
            feel free to increase the number of workers, with the max
            being one less than the number of cores available of your
            system (`future::availableCores() - 1`). In testing, each
            worker typically requires 2-3GB of memory.

### Running the pipeline

7.  If any changes were made to the script, save the script with the
    keyboard shortcut “Ctrl + s”.

8.  Run all the code within the “INPUT” section (Line 8 - Line 17) by
    highlighting these lines and then pressing the “Enter” key. Save the
    script with the keyboard shortcut “Ctrl + s”.

9.  Bring the blinking cursor to the **Console** pane and run
    `tar_make()`. The “configuration” pipeline is now running, which
    will print messages out in the Console like the below image.

    ![](images/tar_make.png)

    The “configuration” pipeline is:

    1.  Downloading and installing non-R software

    2.  Running the code against “configuration” data included within
        the WAVES repository to ensure code is running properly

    3.  This will take awhile! On a potato computer, it took 15-24
        hours!

    4.  If the repository is being ran on a local computer, it is almost
        mandatory that no other work be done while the pipeline is
        running.

10. Once the pipeline is complete the console should say “ended
    pipeline” with how long it took.

    ![](images/tar_make2.png)

    Or it may error like so:

    ![](images/tar_make3.png)

    At least `miniconda_summary_file` should be completed, allowing you
    to move on to step 11.

11. A “summary_miniconda_config.html” will have been created under the
    “reports” folder of the main WAVES repository. Open the .html file
    and check:

    1.  The Miniconda configuration is good, where status is not
        “Unsuccessful installation”.

    2.  No packages/modules are highlighted red for each environment.

        1.  For a environment, the Modules message may say “Modules
            installated do not completely match WAVES configuration”.
            This is expected with slight changes in package/module
            versions within each environment, and are highlighted
            yellow. This shouldn’t impact pipeline processes or
            computations, but are still noted to assist with diagnosing
            potential problems.

    3.  If any red appears, please open a Github issue and attach the
        .pdf of “summary_miniconda_config” or screenshots of which
        methods/installations are red.

12. If the config pipeline errored, please post on issue on Github
    following the convention set forth under Posting an Issue on GitHub
    TODO article.

13. If the pipeline successfully completed, a
    “summary_pipeline_config.html” file will have been created under the
    “reports” folder of the main WAVES repository. Open the file and
    follow the directions stated within the report.

14. If the “summary_pipeline_config.html” indicates no warnings or
    errors, then the WAVES code is working properly on your computer!
    Woo.

15. If the “summary_pipeline_config” report is red for any reason,
    please post the issue with the title as “Config - Summary Report -
    \[Quick Description\]” with the .pdf or screenshots attached to the
    issue.

## YAML File (TODO)

## Main Pipeline (TODO)

1.  Open “\_targets.R”

    1.  Navigate to the “Files” tab within Rstudio (located either in
        the bottom left or top left pane of the RStudio window IF you
        haven’t changed the pane layout in “Global Settings”). Click on
        “\_targets.R”

    2.  Alternatively, open “\_targets.R” with the system File Explorer.
        It should open the script within RStudio.

2.  Within “INPUT” section, check the following:

    1.  `RETICULATE_MINICONDA_PATH`: The path to the conda installation
        specified within the configuration pipeline.
    2.  `study_timezone`: Change from
        [`Sys.timezone()`](https://rdrr.io/r/base/timezones.html) if
        data was collected in another timezone. Supply it as
        country/city (e.g. `America/Los Angeles`, `Europe/London`, etc.)
    3.  `sampling_frequency`: The sampling frequency set for
        accelerometers during data collection. If using .bin/.cwa/.gt3x
        data then this doesn’t matter, but does for .csv data.
    4.  `n_workers`: The default is 2 workers, meaning 2 processes of
        the pipeline will run in parallel of each other.
        1.  `n_workers` should always be at least 2!
        2.  If your operating system has more RAM and cores available,
            feel free to increase the number of workers, with the max
            being one less than the number of cores available of your
            system (`future::availableCores() - 1`)
    5.  `vct_raw_fpa`: Change vct_raw_fpa such that it creates a
        character vector that points towards your raw files.
        1.  We provide an example of how to do so using the `list.files`
            function, where the `file.path` function is used to list
            multiple directories at once.

        2.  We suggest putting a “\[1\]” at the end of `list.files` to
            make sure pipeline works with one file. (image below)

            ![](images/vct_raw_fpa.png)

3.  Save the “\_targets.R” script.

4.  Run the code within “INPUT” section.

5.  In Console, run “tar_make()”. For one file that is a whole day, it
    can take anywhere between 15-30 minutes on a potato computer. For
    one file that is a whole week, it may take up to 24 hours.

6.  Once the pipeline has completed, a .html file should have been
    created under the “reports” folder called
    “summary_pipeline_main.html”. Open the file and double-check file
    went through entire pipeline successfully under the “By Major Steps”
    section.

7.  If pipeline works successfully, close the
    “summary_pipeline_main.html” file and run pipeline on all files
    available by removing the the “\[1\]” at the end of the `list.files`
    function.

    1.  Make sure to save the “\_targets.R” script and rerun the code
        within “INPUT” section.

    2.  This WILL take multiple days if the repository is not being ran
        on a high performance cluster.

    3.  If the repository is being ran on a local computer, it is almost
        mandatory that no other work be done while the pipeline is
        running.

        1.  If the pipeline is interrupted due to an unexpected restart,
            the progress should be saved for major steps within the
            pipeline. Re-follow steps 12-13 the pipeline will pick up
            from the last major step.

8.  Open “summary_pipeline_main.html” once again and check to see what
    files have made it through. If all files have made it through, or at
    least the file’s you would’ve expected to be successfully processed,
    share the “3_MERGED” folder with WAVES data team, where “3_MERGED”
    is renamed with the study acronym.

## Notes

- Even if a pipeline finishes successfully, you may see in the console
  “There were XX warnings (use warnings() to see them)”. This is normal
  if working on an R version that is not 4.5.2., as the messages will be
  warnings that the packages are meant to work on the latest R version.
  As long as the R version being used is R 4.4 or beyond, everything
  should still work. We have not tested the WAVES repository on R
  versions below 4.4.

- If working on a high performance cluster…TODO (work with Hayden, Ben
  on swarm and batch scripts)

- If the pipeline appears “stuck” after 24hrs, as in it looks like there
  has been no change in the processing time or the console messages have
  been the same for quite awhile, its most likely that your computer
  does not have enough computing resources to do parallel processing. In
  that case:

  - Stop the pipeline by clicking the “STOP” button that appears in the
    top right corner of the Console pane.

  ![](images/console_stop.png)

  - Change the `n_workers` object to 1, which will remove parallel
    processing.

- **Resetting conda environments:** Once the WAVES repository has been
  installed, major version changes to the pipeline may result in prior
  conda environment installations to be outdated. Unfortunately,
  re-running the configuration pipeline by itself may not be sufficient,
  which will require removing the existing environments entirely before
  rerunning the configuration pipeline. To do so, first run the code
  within the INPUT section of `_targets_config.R`. Then, run the
  following code within the R console:

      library(reticulate)
      conda_remove("WHO_WAVES_stepcount")
      conda_remove("WHO_WAVES_accelerometer")
      conda_remove("WHO_WAVES_actinet")
      conda_remove("WHO_WAVES_oak_1.0")
      conda_remove("WHO_WAVES_oak_pre")

  After removal, re-run the config pipeline (`tar_make()`) and it will
  recreate the missing environments.

## Posting an Issue on GitHub

After navigating to the Issues tab of WAVES, please use the following
naming convention for the title of the issue:

    [Pipeline] - [Target/Report] - [Quick Description]

where:

- Pipeline: `Config` or `Main`

  - If the error occurred while running the `Config` pipeline or `Main`
    pipeline.

- Target/Report: any of the targets within a pipeline or a report such
  as `summary_pipeline_main` or `summary_miniconda`

  - A “target” is another name for a step within the pipeline.

  - The target that errored is usually what appears after an ❌. So in
    Step 7 of the Configuration pipeline instructions, the target was
    `fpa_merged`

  - The following is a list of targets within the pipelines where errors
    may occur:

    - vct_raw

    - vct_raw_type

    - vct_basic

    - lst_out.cut

    - vct_ox_input

    - vct_ox_step

    - vct_ox_wlms

    - vct_ox_acti

    - lst_ox

    - vct_cal

    - lst_out.raw

    - lst_out.oak.pre

    - fpa_merged

    - pipeline_summary

  - The below only appear within the `config` pipeline:

    - lst_miniconda

    - minconda_summary

- Quick Description: The error itself if it is ≤ 10 words or a summary

Within the description of the issue, please either:

- screenshot your console that includes the pipeline output and the
  error itself (example below)

![](images/example_issue_screenshot.png)

- Copy the output from the console into a codeblock

  - With the description box highlighted, enter a forward slash `/` and
    then select “Code Block”

  ![](images/example_issue_pasting_code.png)

  - For language, select “R”

  - Within the code block, paste the console output

Additionally, if the error occured at `lst_out.raw` , `vct_ox_step` ,
`vct_ox_wlms` , `vct_ox_acti` `lst_out.raw` or `lst_out.oak.pre` , then
please also attach the “summary_miniconda.html” report.
