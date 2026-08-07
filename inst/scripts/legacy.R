#' Legacy-compatible name for the section reader
#'
#' This wrapper preserves the name and argument used in the original
#' aPRoVAR script. New package code should use
#' [read_nirvana_sections()] instead.
#'
#' @param file.path A path to a JSON or gzip-compressed JSON file.
#' @param verbose Whether to report reading progress.
#' @param encoding Character encoding passed to the input connection.
#'
#' @return The same three-element list returned by
#'   [read_nirvana_sections()].
#'
#' @examples
#' example_file <- system.file(
#'     "extdata", "nirvana-example.json", package = "GenVarJSON"
#' )
#' result <- read_anno_json_ultra_fast_v2(
#'     example_file,
#'     verbose = FALSE
#' )
#'
#' @export
read_anno_json_ultra_fast_v2 <- function(file.path, verbose = TRUE,
                                         encoding = "UTF-8") {
  read_ica_sections(
    path = file.path,
    verbose = verbose,
    encoding = encoding
  )
}
