#' Write information about the sample on the plots
#' 
#' @description 
#' This function formats the number of participants in the sample or in each 
#' group inside a call object that can be displayed nicely on ggplot2 figures.
#' It started as a personal fancy to learn how to put text in subscript on
#' ggplots with plotmath, but it ended up being pretty useful.
#'
#' @param df       A dataframe with id and Group columns
#' @param n_groups The number of groups to display. Must be either 0, 2 or 4.
#' @param type  A string indicating the type of information to display. Can be
#' either "groups" or "clusters".
#' @param prefix   A string. Text to add before the number of participants
#' @param suffix   A string. Text to add after the number of participants
#' @param ...      Other arguments passed to the function. Not used.
#'
#' @returns A call with the number of participants in each group that can be 
#' used in ggplot2 text elements (such as the title or subtitle).
#' @export
write_sample_info <- function(
    df, 
    n_groups = 2, 
    type   = "groups",
    prefix = "",
    suffix = "",
    ...
) {
  df <- 
    df |> 
    dplyr::select(id, Group, contains("cluster")) |> 
    dplyr::distinct() |> 
    dplyr::ungroup() |> 
    dplyr::mutate(Group = stringr::str_to_lower(Group))
  
  if (n_groups == 0) {
    txt <- 
      bquote(
        .(prefix)*
          "N"[Total] == 
          .(nrow(df))*
          .(suffix)
      )
  } else if (n_groups == 2) {
    txt <- 
      bquote(
        .(prefix)*
          "N"[Aphant.] == 
          .(nrow(dplyr::filter(df, Group == "aphantasia"))) ~
          ", N"[Typical.] == .(nrow(dplyr::filter(df, Group == "typical")))*
          .(suffix)
      )
  } else if (n_groups == 3) {
    txt <- 
      bquote(
        .(prefix)*
          "N"[Aphant.] == 
          .(nrow(dplyr::filter(df, Group == "aphantasia"))) ~
          ", N"[Hypophant.] == 
          .(nrow(dplyr::filter(df, Group == "hypophantasia"))) ~
          ", N"[Typical.] == .(nrow(dplyr::filter(df, Group == "typical")))*
          .(suffix)
      )
  } else if (n_groups == 4) {
    txt <- 
      bquote(
        .(prefix)*
          "N"[Aphant.] == 
          .(nrow(dplyr::filter(df, Group == "aphantasia"))) ~
          ", N"[Hypophant.] == 
          .(nrow(dplyr::filter(df, Group == "hypophantasia"))) ~
          ", N"[Typical.] == 
          .(nrow(dplyr::filter(df, Group == "typical"))) ~
          ", N"[Hyperphant.] == 
          .(nrow(dplyr::filter(df, Group == "hyperphantasia")))*
          .(suffix)
      )
  } else stop(glue::glue_col(
    "n_groups must be either '{blue 0}', '{cyan 2}', '{blue 3}' or '{green 4}'"
  ))
  
  if (type == "clusters") {
    txt <- 
      bquote(
        .(prefix)*
          "N"[Visualisers] == 
          .(nrow(dplyr::filter(df, cluster == "Visualiser"))) ~
          ", N"[Spatialisers] == 
          .(nrow(dplyr::filter(df, cluster == "Spatialiser"))) ~
          ", N"[Verbalisers] == 
          .(nrow(dplyr::filter(df, cluster == "Verbaliser")))*
          .(suffix)
      )
  }
  
  return(txt)
}
