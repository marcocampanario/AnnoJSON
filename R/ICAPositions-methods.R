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
      header$nirvanaVersion,
      default = "Not available"
    )

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
      "  ICA version: ", annotation_version, "\n",
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
