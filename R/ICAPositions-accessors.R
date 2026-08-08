#' Access position-level ICA annotations
#'
#' Returns the position-level annotation table stored in an
#' `ICAPositions` object.
#'
#' @param x An `ICAPositions` object.
#'
#' @return An [S4Vectors::DataFrame()] containing position-level ICA
#'   annotations.
#'
#' @examples
#' example_file <- system.file(
#'     "extdata",
#'     "ica-example.json.gz",
#'     package = "AnnoJSON"
#' )
#'
#' parsed <- read_ica_json(
#'     example_file,
#'     progress = FALSE,
#'     verbose = FALSE
#' )
#'
#' object <- ICAPositions(
#'     positions = parsed$tbl_positions,
#'     header = parsed$header
#' )
#'
#' positions <- position_data(object)
#' positions
#'
#' @export
methods::setGeneric(
  "position_data",
  function(x) {
    methods::standardGeneric("position_data")
  }
)

#' @rdname position_data
#' @export
methods::setMethod(
  "position_data",
  signature(x = "ICAPositions"),
  function(x) {
    x@positions
  }
)


#' Access the parsed ICA header
#'
#' Returns the parsed header stored in an `ICAPositions` object. The header
#' contains information such as the annotator version, creation time, genome
#' assembly, schema version, data sources, and sample identifiers.
#'
#' @param x An `ICAPositions` object.
#'
#' @return A list containing the parsed ICA header.
#'
#' @examples
#' example_file <- system.file(
#'     "extdata",
#'     "ica-example.json.gz",
#'     package = "AnnoJSON"
#' )
#'
#' parsed <- read_ica_json(
#'     example_file,
#'     progress = FALSE,
#'     verbose = FALSE
#' )
#'
#' object <- ICAPositions(
#'     positions = parsed$tbl_positions,
#'     header = parsed$header
#' )
#'
#' header <- ica_header(object)
#' names(header)
#'
#' @export
methods::setGeneric(
  "ica_header",
  function(x) {
    methods::standardGeneric("ica_header")
  }
)

#' @rdname ica_header
#' @export
methods::setMethod(
  "ica_header",
  signature(x = "ICAPositions"),
  function(x) {
    x@header
  }
)

#' Access ICAPositions object metadata
#'
#' Returns the object-level metadata stored in an `ICAPositions` object.
#' These metadata are separate from the original ICA header and may contain
#' provenance, processing, or package-specific information.
#'
#' @param x An `ICAPositions` object.
#'
#' @return A list containing object-level metadata.
#'
#' @examples
#' example_file <- system.file(
#'     "extdata",
#'     "ica-example.json.gz",
#'     package = "AnnoJSON"
#' )
#'
#' parsed <- read_ica_json(
#'     example_file,
#'     progress = FALSE,
#'     verbose = FALSE
#' )
#'
#' object <- ICAPositions(
#'     positions = parsed$tbl_positions,
#'     header = parsed$header,
#'     metadata = list(source = "ICA JSON")
#' )
#'
#' metadata <- ica_metadata(object)
#' metadata
#'
#' @export
methods::setGeneric(
  "ica_metadata",
  function(x) {
    methods::standardGeneric("ica_metadata")
  }
)

#' @rdname ica_metadata
#' @export
methods::setMethod(
  "ica_metadata",
  signature(x = "ICAPositions"),
  function(x) {
    x@metadata
  }
)


