#' Display an ICAPositions object
#'
#' Displays a compact summary of an `ICAPositions` object without printing
#' the complete position-level table or its nested annotation columns.
#'
#' @param object An `ICAPositions` object.
#'
#' @return `object`, invisibly.
#'
#' @export
methods::setMethod(
  "show",
  signature(object = "ICAPositions"),
  function(object) {
    positions <- object@positions
    header <- object@header

    number_of_records <- nrow(positions)
    number_of_columns <- ncol(positions)

    additional_columns <- setdiff(
      names(positions),
      .ICA_POSITION_COLUMNS
    )

    genome_assembly <- .as_character_scalar(
      header$genomeAssembly,
      default = "Not available"
    )

    annotation_version <- .as_character_scalar(
      header$annotator,
      default = "Not available"
    )

    number_of_data_sources <- if (
      is.null(header$dataSources)
    ) {
      NA_integer_
    } else {
      NROW(header$dataSources)
    }

    number_of_samples <- if (
      is.null(header$samples) ||
      length(header$samples) == 0L
    ) {
      NA_integer_
    } else {
      NROW(header$samples)
    }

    format_count <- function(x) {
      if (length(x) == 0L || is.na(x)) {
        return("Not available")
      }

      format(
        x,
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
      )
    }

    cat(
      "ICAPositions object\n",
      "  Records: ", format_count(number_of_records), "\n",
      "  Genome assembly: ", genome_assembly, "\n",
      "  Annotator: ", annotation_version, "\n",
      "  Data sources: ", format_count(number_of_data_sources), "\n",
      "  Samples: ", format_count(number_of_samples), "\n",
      "  Position columns: ", format_count(number_of_columns), "\n",
      "  Additional columns: ",
      format_count(length(additional_columns)),
      "\n",
      sep = ""
    )

    invisible(object)
  }
)

#' Replacement method for ICA positions data
#'
#' Replaces the position-level data stored in an `ICAPositions` object.
#' The resulting object is validated before it is returned.
#'
#' @param x An `ICAPositions` object.
#' @param value A [S4Vectors::DataFrame()] containing the required ICA
#'   position columns. Additional columns are permitted.
#'
#' @return The modified `ICAPositions` object.
#'
#' @examples
#' positions <- position_data(ica_object)
#' positions$source_row <- seq_len(nrow(positions))
#' position_data(ica_object) <- positions
#'
#' @export
methods::setGeneric(
  "position_data<-",
  function(x, value) {
    methods::standardGeneric("position_data<-")
  }
)

#' @rdname position_data-set
#' @export
methods::setReplaceMethod(
  "position_data",
  signature(
    x = "ICAPositions",
    value = "DataFrame"
  ),
  function(x, value) {
    x@positions <- value
    methods::validObject(x)
    x
  }
)
