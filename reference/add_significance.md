# Add significance label and line to a plot

Add significance label and line to a plot

## Usage

``` r
add_significance(
  df,
  size_star = 2.5,
  lw = 0.2,
  color = "black",
  linetype = "solid"
)
```

## Arguments

- df:

  A dataframe containing one column per variable in the desired
  aesthetics (x, y, colour, etc.) and the following columns:

  - `x_star`: x position of the star label

  - `y_star`: y position of the star label

  - `stars`: the star label (e.g., "*", "**", "***")

  - `x_line`: x position of the start of the line

  - `x_line_end`: x position of the end of the line

  - `y_line`: y position of the line

- size_star:

  Size of the star label. Default is 2.5.

- lw:

  Line width of the significance line. Default is 0.2.

- color:

  Color of the star label and line. Default is "black".

- linetype:

  Line type of the significance line. Default is "solid".

## Value

A list of ggplot2 layers that can be added to a ggplot object.

## Examples

``` r
group_effect <-
 tibble::tibble(
    x_star     = 1.5,
    y_star     = 1.08,
    stars      = "**",
    x_line     = x_star - 0.5,
    x_line_end = x_star + 0.5,
    y_line     = 1.05
  )

ggplot2::ggplot() +
  ggplot2::scale_x_discrete(limits = factor(c(1, 2))) +
  ggplot2::scale_y_continuous(limits = c(0, 1.1)) +
  ggplot2::labs(x = NULL, y = NULL) +
  add_significance(group_effect, size_star = 4)
```
