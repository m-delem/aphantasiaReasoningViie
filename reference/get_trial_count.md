# Get the count of trials per participant and category

Get the count of trials per participant and category

## Usage

``` r
get_trial_count(df)
```

## Arguments

- df:

  A data frame containing trial data with columns for id, group, and
  category.

## Value

A data frame summarizing the number of trials per participant and
category, including total trials and a flag for low trial counts.

## Examples

``` r
df <- get_clean_data()$df_expe
get_trial_count(df) |> head()
#> # A tibble: 6 × 7
#> # Rowwise: 
#>   id                      group Control Spatial Visual n_trials_total low_trials
#>   <fct>                   <chr>   <int>   <int>  <int>          <int> <lgl>     
#> 1 acdn247721443631359lzxb Typi…       9       9      9             27 FALSE     
#> 2 ahos206230340719944kiwl Apha…       9       9      9             27 FALSE     
#> 3 anoo201523848333505mpxv Typi…       9       9      9             27 FALSE     
#> 4 arje91119258110263fkhy  Typi…       9       9      9             27 FALSE     
#> 5 auzb748856118756901vqiz Typi…       9       9      9             27 FALSE     
#> 6 azcj317771758245897ccrd Hypo…       9       9      9             27 FALSE     

df_rt <- filter_trials_on_rt(df)
# Allows to identify and examine the participants that lost a lot of trials
get_trial_count(df_rt) |> head()
#> # A tibble: 6 × 7
#> # Rowwise: 
#>   id                      group Control Spatial Visual n_trials_total low_trials
#>   <fct>                   <chr>   <int>   <int>  <int>          <int> <lgl>     
#> 1 ynhf865603843332103uxhx Typi…       2       6      4             12 TRUE      
#> 2 bbwg95779316229529nvnw  Typi…       5       4      4             13 FALSE     
#> 3 bwba621197531274841rnng Hype…       5       3      5             13 FALSE     
#> 4 ckmw15672323159356eazf  Apha…       5       5      4             14 FALSE     
#> 5 rxgq435410820299509eabx Typi…       4       5      5             14 FALSE     
#> 6 tqnu59278134349817sjwu  Typi…       4       5      5             14 FALSE     
```
