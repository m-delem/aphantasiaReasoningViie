#' @keywords internal
"_PACKAGE"

## usethis namespace: start
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
  return(NULL)
}
