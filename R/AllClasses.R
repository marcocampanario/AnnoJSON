#' Required columns for ICAPositions objects
#'
#' @noRd
.ICA_POSITION_COLUMNS <- c(
  "chromosome",
  "position",
  "refAllele",
  "altAlleles",
  "vcfInfo",
  "quality",
  "filters",
  "cytogeneticBand",
  "samples",
  "variants"
)

#' ICA position-level annotations
#'
#' Stores position-level Illumina Connected Annotations records together
#' with the corresponding file header and object-level metadata.
#'
#' @slot positions A `S4Vectors::DataFrame` containing position-level annotations.
#' @slot header The parsed ICA header.
#' @slot metadata Additional object-level metadata.
#'
#' @importClassesFrom S4Vectors DataFrame
#' @importFrom S4Vectors DataFrame
#'
#' @exportClass ICAPositions
methods::setClass(
  "ICAPositions",
  slots = c(
    positions = "DataFrame",
    header = "list",
    metadata = "list"
  ),
  prototype = list(
    positions = S4Vectors::DataFrame(),
    header = list(),
    metadata = list()
  )
)

#' Required columns for ICAPositions objects
#'
#' @noRd
.ICA_POSITION_COLUMNS <- c(
  "chromosome",
  "position",
  "refAllele",
  "altAlleles",
  "vcfInfo",
  "quality",
  "filters",
  "cytogeneticBand",
  "samples",
  "variants"
)

#' Validate an ICAPositions object
#'
#' Performs shallow structural validation without inspecting individual
#' elements within nested annotation columns.
#'
#' @param object An `ICAPositions` object.
#'
#' @return `TRUE` when valid, or a character vector describing validation
#'   failures.
#'
#' @noRd
.validate_ICAPositions <- function(object) {
  messages <- character()
  positions <- object@positions
  column_names <- names(positions)

  missing_columns <- setdiff(
    .ICA_POSITION_COLUMNS,
    column_names
  )

  if (length(missing_columns) > 0L) {
    messages <- c(
      messages,
      paste0(
        "The `positions` slot is missing required column(s): ",
        paste(missing_columns, collapse = ", "),
        "."
      )
    )
  }

  duplicated_columns <- unique(
    column_names[
      duplicated(column_names) &
        column_names %in% .ICA_POSITION_COLUMNS
    ]
  )

  if (length(duplicated_columns) > 0L) {
    messages <- c(
      messages,
      paste0(
        "The `positions` slot contains duplicated required ",
        "column name(s): ",
        paste(duplicated_columns, collapse = ", "),
        "."
      )
    )
  }

  present_columns <- intersect(
    .ICA_POSITION_COLUMNS,
    column_names
  )

  is_list_column <- function(x) {
    is.list(x) || methods::is(x, "List")
  }

  validators <- list(
    chromosome = is.character,
    position = is.integer,
    refAllele = is.character,
    altAlleles = is_list_column,
    vcfInfo = is_list_column,
    quality = is.numeric,
    filters = is.character,
    cytogeneticBand = is.character,
    samples = is_list_column,
    variants = is_list_column
  )

  expected_types <- c(
    chromosome = "a character vector",
    position = "an integer vector",
    refAllele = "a character vector",
    altAlleles = "a list-like column",
    vcfInfo = "a list-like column",
    quality = "a numeric vector",
    filters = "a character vector",
    cytogeneticBand = "a character vector",
    samples = "a list-like column",
    variants = "a list-like column"
  )

  for (column in present_columns) {
    value <- positions[[column]]

    if (!validators[[column]](value)) {
      messages <- c(
        messages,
        sprintf(
          "`positions$%s` must be %s.",
          column,
          expected_types[[column]]
        )
      )
    }
  }

  number_of_records <- nrow(positions)

  record_lengths <- vapply(
    present_columns,
    function(column) {
      NROW(positions[[column]])
    },
    integer(1L)
  )

  invalid_lengths <- names(record_lengths)[
    record_lengths != number_of_records
  ]

  if (length(invalid_lengths) > 0L) {
    messages <- c(
      messages,
      paste0(
        "The following column(s) do not contain one element per ",
        "position record: ",
        paste(invalid_lengths, collapse = ", "),
        "."
      )
    )
  }

  if ("position" %in% present_columns) {
    position <- positions[["position"]]

    if (
      is.numeric(position) &&
      any(!is.na(position) & position <= 0)
    ) {
      messages <- c(
        messages,
        "`positions$position` must contain positive values or NA."
      )
    }
  }

  validate_named_list <- function(value, slot_name) {
    slot_messages <- character()

    if (!is.list(value) || is.data.frame(value)) {
      return(
        sprintf(
          "The `%s` slot must be a list.",
          slot_name
        )
      )
    }

    if (length(value) == 0L) {
      return(slot_messages)
    }

    value_names <- names(value)

    if (
      is.null(value_names) ||
      length(value_names) != length(value) ||
      anyNA(value_names) ||
      any(!nzchar(value_names))
    ) {
      slot_messages <- c(
        slot_messages,
        sprintf(
          "All elements of the `%s` slot must be named.",
          slot_name
        )
      )
    } else if (anyDuplicated(value_names) > 0L) {
      slot_messages <- c(
        slot_messages,
        sprintf(
          "Names in the `%s` slot must be unique.",
          slot_name
        )
      )
    }

    slot_messages
  }

  messages <- c(
    messages,
    validate_named_list(object@header, "header"),
    validate_named_list(object@metadata, "metadata")
  )

  if (length(messages) == 0L) {
    TRUE
  } else {
    unique(messages)
  }
}

methods::setValidity(
  "ICAPositions",
  .validate_ICAPositions
)
