#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom grDevices palette.colors
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom stats confint
#' @importFrom stats contr.treatment
#' @importFrom stats contrasts<-
#' @importFrom stats formula
#' @importFrom stats median
#' @importFrom stats rbinom
#' @importFrom stats sd
## usethis namespace: end
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(glue::glue_col(
    "{blue
    Welcome to {cyan aphantasiaReasoningViie}.
    }
    ",
    .literal = TRUE))
}

# To avoid check NOTE on package sub-dependencies not called
ignore_unused_imports <- function() {
  crayon::underline # for glue_col
  rlang::expr
  sessioninfo::session_info
  dplyr::mutate
  stringr::str_detect
  return(NULL)
}
