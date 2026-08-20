# Reference Data Codebook

Last Update: May 7, 2026

``` r

cat("poop")
#> poop
```

**Purpose**: Describe the variables for meta data and ground truth
format for studies contributing data to the WAVES analysis. Designed to
align with analysis plan below.

Of note, the expectation is for WAVES studies to map their existing
labeled data to this codebook where possible. The expectation is not to
relabel data for this analysis. In the excel appendix, studies should
provide their labels, operational definitions and mappings.

## Analysis Plan - Aim 1 (primary reference video recorded DO)

Performance evaluation of three core metrics (sedentary time, step
counting, moderate-vigorous physical activity) compared to a primary
reference measure collected for a fixed amount of time, at least 1-hour
hours within a free-living environment (i.e., naturalistic conditions,
not laboratory or scripted).

Identify primary outcome of interest and propose a priori sensitivity
analyses to examine accuracy across different domains and movement that
are important for public health and/or may have a unique movement signal
that should be considered.

1.  Sedentary TIme
    1.  **\*Total sedentary time**: Comparison of algorithms and
        ground-truth across all labeled data in final datasets.
    2.  **Broad domain**: Stratified by 5-class consistent with GPAQ
        where possible (household, occupation, travel, leisure, other)
    3.  **Sedentary types**: Stratified by 4-class: non-sedentary,
        sitting/reclining, lying, sedentary driving.
2.  Step Counting
    1.  **\*Total steps**: Comparison of algorithms and ground-truth
        across all labeled data in final datasets.
    2.  **Broad domain**: Stratified by 5-class consistent with GPAQ
        where possible (household, occupation, travel, leisure, other).
    3.  **Whole-body movement**: Stratified by 5-class (sedentary, mixed
        movement, walking, running, biking).
3.  Moderate-vigorous physical activity (MVPA)
    1.  **\*Total MVPA**: Comparison of algorithms and ground-truth
        across all labeled data in final datasets.
    2.  **Broad domain**: Stratified by 5-class consistent with GPAQ
        where possible (household, occupation, travel, leisure, other)
    3.  **Whole-body movement**: Stratified by 5-class (sedentary, mixed
        movement, walking, running, biking).

*\*Indicates primary analysis*

## Analysis Plan – Aim 2 (Day-level secondary comparisons)

Performance evaluation of three core metrics (sedentary time, step
counting, MVPA) compared to a field-based secondary reference measure
collected over one or more 24-hour periods in free-living conditions.

**Study Requirements**:

- Protocol that includes minimum of 16-hour wear protocol for wrist worn
  device and field-based reference measure.

- Simultaneous raw wrist-worn device AND a reference measure traceable
  to a primary reference method with acceptable accuracy and precision
  in field-based testing

## Metadata

Notes:

- • Variables indicated as **required** must be included for a study’s
  data to be used in this analysis.

### Data Dictionary

[TABLE]

### Example

| site | pid   | age | bmi  | gender | device | sampling | location |
|------|-------|-----|------|--------|--------|----------|----------|
| CP   | CP001 | 21  | 23.4 | F      | AG3X   | 30       | non_dom  |

## *Primary Reference: Video-Recorded Direct Observation*

Notes:

- Reference measure for sedentary time, step counts and MVPA.

- Row unit: one record per 1-second epoch of direct observation.

- Variables listed as required must be provided for the study’s data to
  be included in the analysis.

- The data provided to the team should be “clean” with all rows removed
  for observations that were non-codable/missing.

- intensity3_do and intensity4_do are MET-based classifications and
  should be internally consistent:

  - “mvpa” in intensity3_do corresponds to “moderate” or “vigorous” in
    intensity4_do.

- Sedtype_do provides a sedentary subtype classification and should
  align with:

  - posture_do = “sedentary”

  - intensity_do = “sedentary”

  - “non-sed” in Sedtype_do corresponds to all non-sedentary
    posture/intensity combinations.

- A study must have sedentary time, MVPA or steps labeled in a way
  consistent with operational definitions to be included in the
  analysis. The study does not need to have all three to be included,
  should provide NAs for metrics that aren’t labeled.

### Data Dictionary

| **variable** | definition | type | levels | labels/notes | missingness |
|----|----|----|----|----|----|
| **site** | Study site identifier | character (nominal) | CP | California Polytechnic State University | none (required for each included file) |
|  |  |  | UWM | University of Wisconsin–Milwaukee |  |
| **pid** | Unique participant identifier | alphanumeric | \- | unique identifier variable that identifies participant and can be used to link with device data | none |
| **observation** | Observation session identifier | numeric (count) | \- | If a participant is only observed once, then default to 1 | none |
| **datetime** | Timestamp of observation in UTC timezone | POSIXct | \- | Timestamp in coordinated universal time (UTC), YYYY-mm-dd HH:MM:SS format | none |
| **date** | Local calendar date at the observation site | date | \- | In year-month-day format (i.e. YYYY-MM-DD) | none |
| **time** | Local wall-clock time at the observation site | character | \- | In 24-hour hour:minute:second format (i.e. HH:MM:SS) | none |
| **domain_do** | Behavioral domain classification | character (nominal) | leisure | Discretionary time activities including social activities, sports, fitness, recreation, and screen-based leisure activities | allowed |
|  |  |  | household | Personal care, housework, lawn and garden work, exterior maintenance |  |
|  |  |  | transportation | Traveling to and from places (e.g., work, shopping, place of worship). Includes driving or riding in a car, public motorized transport (bus, train), and active transport |  |
|  |  |  | occupation | Work or school-related activities including paid or unpaid work, study, and seeking employment |  |
|  |  |  | other | Behaviors not categorized above (e.g., purchasing goods, volunteering, other uncategorized activities) |  |
| **posture_do** | Observed posture or movement type | character (nominal) | sedentary | Sitting or lying | allowed |
|  |  |  | mixed_movement | Includes standing, stand_move, sport_movement, stretching, crouching/kneeling/squatting, ascending/descending stairs |  |
|  |  |  | walking | L-R-L-R pattern including walking with load |  |
|  |  |  | running | Running |  |
|  |  |  | biking | Cycling |  |
| **sedtype_do** | Sedentary subtype classification | character (nominal) | non_sed | All non-sedentary/active behaviors | allowed |
|  |  |  | sitting | sitting/reclining not in a motor vehicle as a driver/passenger |  |
|  |  |  | lying | lying |  |
|  |  |  | vehicle | Driving a personal motor vehicle or as a passenger in any land-based motor vehicle |  |
| intensity3_do | 3-level intensity classification based on METs | character (ordinal) | sedentary | sedentary posture with low energy expenditure | allowed |
|  |  |  | light | non sedentary posture 1.50 - 2.99 METS |  |
|  |  |  | mvpa | 3.00+ METS |  |
| intensity4_do | 4-level intensity classification based on METs | character (ordinal) | sedentary | sedentary posture with low energy expenditure | allowed |
|  |  |  | light | non sedentary posture 1.50 - 2.99 METS |  |
|  |  |  | moderate | 3.00 - 5.99 METS |  |
|  |  |  | vigorous | 6.00+ METS |  |
| steps_do | Number of steps recorded during the 1-second epoch | count | \- | \- | allowed |

### Example

| site | pid | observation | datetime | date | time | domain_do | posture_do | sedtype_do | intensity3_do | intensity4_do | steps_do |
|----|----|----|----|----|----|----|----|----|----|----|----|
| CP | CP001 | 1 | 1990-01-01T18:36:10 | 1990-01-01 | 10:42:10 | transportation | walking | non_sed | mvpa | moderate | 2 |

## Thigh-worn device data dictionary
