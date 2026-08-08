#' Construct an ICAPositions object
#'
#' Creates an `ICAPositions` object from the position-level table generated
#' from an ICA JSON annotation file. Base data frames and tibbles are
#' converted to [S4Vectors::DataFrame()] objects.
#'
#' The ten core ICA columns are placed first and in their standard order.
#' Additional columns are preserved after the core columns.
#'
#' @param positions A [S4Vectors::DataFrame()], data frame, or tibble
#'   containing position-level ICA annotations.
#' @param header A named list containing the parsed ICA header, typically
#'   returned by [parse_ica_header()].
#' @param metadata A named list containing provenance or other object-level
#'   metadata.
#'
#' @return An `ICAPositions` object.
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
#' ica_positions <- ICAPositions(
#'     positions = parsed$tbl_positions,
#'     header = parsed$header
#' )
#'
#' ica_positions
#'
#' @importFrom S4Vectors DataFrame
#'
#' @export
ICAPositions <- function(positions, header = list(),
                         metadata = list()) {
  if (methods::is(positions, "DataFrame")) {
    position_data <- positions
  } else if (is.data.frame(positions)) {
    position_data <- S4Vectors::DataFrame(
      positions,
      check.names = FALSE
    )
  } else {
    stop(
      paste0(
        "`positions` must be a `DataFrame`, data frame, ",
        "or tibble."
      ),
      call. = FALSE
    )
  }

  if (!is.list(header) || is.data.frame(header)) {
    stop(
      "`header` must be a list.",
      call. = FALSE
    )
  }

  if (!is.list(metadata) || is.data.frame(metadata)) {
    stop(
      "`metadata` must be a list.",
      call. = FALSE
    )
  }

  position_names <- names(position_data)

  can_reorder <- (
    anyDuplicated(position_names) == 0L &&
      all(.ICA_POSITION_COLUMNS %in% position_names)
  )

  if (can_reorder) {
    additional_columns <- setdiff(
      position_names,
      .ICA_POSITION_COLUMNS
    )

    position_data <- position_data[
      ,
      c(
        .ICA_POSITION_COLUMNS,
        additional_columns
      ),
      drop = FALSE
    ]
  }

  object <- methods::new(
    "ICAPositions",
    positions = position_data,
    header = header,
    metadata = metadata
  )

  methods::validObject(object)

  object
}
