# Reasoning experiment data

This dataset contains the results of the main reasoning experiment. It
contains the following columns:

- An encrypted unique participant `id` (that also allows to join the
  data with the survey data)

- `language` (the language the experiment was conducted in, FR or EN)

- `group` (aphantasia, hypophantasia, typical, hyperphantasia or
  no_vviq)

- `expe_phase` (the phase of the experiment)

- `trial_number` (the presentation order of the problems)

- `problem` (the problem presented to the participant)

- `category` (the category of the problem, visual, spatial or control)

- `premise_*_rt` (the reaction time of the participant for each premise)

- `conclusion_rt` (the reaction time of the participant for the
  conclusion, i.e., the response screen)

- `rt_total` (the total time spent on the trial)

- `response` (the participant's response, TRUE or FALSE)

- `correct_response` (the correct response to the problem, TRUE or
  FALSE)

- `accuracy` (the accuracy of the participant's response, TRUE or FALSE)

## Usage

``` r
experiment_data
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
3699 rows and 15 columns.

## Source

Data collected through an online experiment
