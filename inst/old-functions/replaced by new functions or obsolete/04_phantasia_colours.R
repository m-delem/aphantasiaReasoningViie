#' Custom colour scales for the variables in this project
#' 
#' @description 
#' This function eases the use of custom colour scales for the variables in this
#' project by mapping friendly colours to specific values of the variables. Each 
#' VVIQ group is associated to an Okabe-Ito colour, and the strategy/style 
#' modalities are associated to a colour from Matplotlib's "Set1", which was 
#' used by Reeder & Pounder (2024) for their strategy questionnaire.
#'
#' @param type The type of scale to create. Can be either "colour", "fill" or 
#'             c("colour", "fill") to cover the two scales. 
#'             Default is c("colour", "fill").
#' @param ... Additional arguments passed to the `scale_discrete_manual()` 
#' function.
#'
#' @returns A ggplot2 scale object with the specified colours.
scale_discrete_phantasia <- function(type = c("colour", "fill"), ...) {
  ggplot2::scale_discrete_manual(
    aesthetics = type,
    values = c(
      # Phantasia sub-groups
      aphantasia     = palette.colors()[3] |> as.character(),
      Aphantasia     = palette.colors()[3] |> as.character(),  
      hypophantasia  = palette.colors()[4] |> as.character(), 
      Hypophantasia  = palette.colors()[4] |> as.character(), 
      typical        = palette.colors()[2] |> as.character(), 
      Typical        = palette.colors()[2] |> as.character(), 
      hyperphantasia = palette.colors()[8] |> as.character(),
      Hyperphantasia = palette.colors()[8] |> as.character(),
      # OSIVQ profiles
      Visualiser  = "#1b6096",
      Spatialiser = "#318f2c",
      Verbaliser  = "#793183",
      # Experiment strategies
      visual       = "#1b6096", 
      Visual       = "#1b6096", 
      verbal       = "#793183", 
      Verbal       = "#793183", 
      control      = "#793183",
      Control      = "#793183", 
      semantic     = "#db6100", 
      Semantic     = "#db6100", 
      spatial      = "#318f2c", 
      Spatial      = "#318f2c", 
      sensorimotor = "#d4629e",
      Sensorimotor = "#d4629e"
    )
  )
}
