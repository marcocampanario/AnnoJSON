#' Parse a single ICA JSON record
#'
#' Converts a character string containing one JSON record into its
#' corresponding R representation using [jsonlite::fromJSON()]. The
#' simplification and flattening behavior is controlled by the arguments
#' passed to the JSON parser.
#'
#' @param record A character scalar containing one valid JSON record.
#' @param simplify_vector A logical scalar indicating whether JSON arrays
#'   should be simplified to atomic vectors, matrices, or data frames when
#'   possible.
#' @param flatten A logical scalar indicating whether nested data frames
#'   should be flattened.
#'
#' @return The R object produced by [jsonlite::fromJSON()]. Its structure
#'   depends on the contents of `record` and the selected parsing options.
#'
#' @importFrom jsonlite fromJSON
#'
#' @noRd
.parse_one_json_record <- function(record, simplify_vector, flatten) {
    jsonlite::fromJSON(
        record,
        simplifyVector = simplify_vector,
        flatten = flatten
    )
}

#' Check compatibility between legacy and BiocParallel arguments
#'
#' Prevents ambiguous calls combining `workers` or `progress` with an
#' explicit `BPPARAM`.
#'
#' @param BPPARAM A BiocParallel parameter object or `NULL`.
#' @param workers_supplied Whether `workers` was explicitly supplied.
#' @param progress_supplied Whether `progress` was explicitly supplied.
#'
#' @return `NULL`, invisibly.
#'
#' @noRd
.check_parallel_arguments <- function(BPPARAM,
                                      workers_supplied,
                                      progress_supplied) {
  if (
    !is.null(BPPARAM) &&
    (workers_supplied || progress_supplied)
  ) {
    stop(
      paste0(
        "Use either `workers` and `progress`, or `BPPARAM`; ",
        "do not combine both interfaces."
      ),
      call. = FALSE
    )
  }

  invisible(NULL)
}

#' Resolve the BiocParallel backend
#'
#' Constructs a serial or socket-based backend from the legacy arguments,
#' unless an explicit BiocParallel parameter object is supplied.
#'
#' @param BPPARAM A BiocParallel parameter object or `NULL`.
#' @param workers Number of workers used when `BPPARAM` is `NULL`.
#' @param progress Whether to enable the backend progress bar.
#'
#' @return A `BiocParallelParam` object.
#'
#' @importFrom BiocParallel SerialParam SnowParam
#' @importFrom methods is
#'
#' @noRd
.resolve_ica_bpparam <- function(
    BPPARAM = NULL,
    workers = 1L,
    progress = interactive()
) {
  if (!is.null(BPPARAM)) {
    if (!methods::is(BPPARAM, "BiocParallelParam")) {
      stop(
        "`BPPARAM` must be a BiocParallelParam object.",
        call. = FALSE
      )
    }

    return(BPPARAM)
  }

  workers <- .validate_positive_integer(workers, "workers")
  .assert_flag(progress, "progress")

  if (workers == 1L) {
    return(
      BiocParallel::SerialParam(
        progressbar = progress
      )
    )
  }

  BiocParallel::SnowParam(
    workers = workers,
    type = "SOCK",
    progressbar = progress,
    stop.on.error = TRUE
  )
}

#' Parse ICA JSON records using BiocParallel
#'
#' @param records A character vector of JSON records.
#' @param simplify_vector Whether to simplify JSON vectors.
#' @param flatten Whether to flatten nested data frames.
#' @param BPPARAM A BiocParallel parameter object.
#'
#' @return A list in the same order as `records`.
#'
#' @importFrom BiocParallel bplapply
#'
#' @noRd
.parse_records_biocparallel <- function(
    records,
    simplify_vector,
    flatten,
    BPPARAM
) {
  BiocParallel::bplapply(
    X = records,
    FUN = .parse_one_json_record,
    simplify_vector = simplify_vector,
    flatten = flatten,
    BPPARAM = BPPARAM
  )
}

#' Parse position- or gene-level ICA JSON records
#'
#' Converts a character vector containing one JSON object per element into
#' a list of R objects. Parsing is performed through BiocParallel while
#' preserving input order.
#'
#' The legacy `workers` and `progress` arguments construct either a
#' [BiocParallel::SerialParam()] or a socket-based
#' [BiocParallel::SnowParam()]. Alternatively, users may supply an explicit
#' BiocParallel backend through `BPPARAM`.
#'
#' @param records A character vector of JSON records, such as the
#'   `positions` or `genes` element returned by [read_ica_sections()].
#' @param workers Number of R processes when `BPPARAM` is `NULL`. A value of
#'   `1L` selects sequential evaluation; larger values create a socket
#'   backend.
#' @param progress Whether to enable the backend progress bar when
#'   `BPPARAM` is `NULL`.
#' @param simplifyVector Passed to [jsonlite::fromJSON()].
#' @param flatten Passed to [jsonlite::fromJSON()].
#' @param BPPARAM An optional `BiocParallelParam` object. When supplied,
#'   `workers` and `progress` must be omitted.
#'
#' @return A list containing one parsed object per input record, in the
#'   same order as `records`.
#'
#' @examples
#' example_file <- system.file(
#'     "extdata", "ica-example.json.gz", package = "AnnoJSON"
#' )
#' sections <- read_ica_sections(example_file, verbose = FALSE)
#' positions <- parse_ica_records(
#'     sections$positions,
#'     workers = 1L,
#'     progress = TRUE
#' )
#'
#' @export
parse_ica_records <- function(
    records,
    workers = 1L,
    progress = interactive(),
    simplifyVector = TRUE,
    flatten = FALSE,
    BPPARAM = NULL
) {
  workers_supplied <- !missing(workers)
  progress_supplied <- !missing(progress)

  if (!is.character(records)) {
    stop(
      "`records` must be a character vector.",
      call. = FALSE
    )
  }

  .assert_flag(simplifyVector, "simplifyVector")
  .assert_flag(flatten, "flatten")

  .check_parallel_arguments(
    BPPARAM = BPPARAM,
    workers_supplied = workers_supplied,
    progress_supplied = progress_supplied
  )

  BPPARAM <- .resolve_ica_bpparam(
    BPPARAM = BPPARAM,
    workers = workers,
    progress = progress
  )

  if (length(records) == 0L) {
    return(list())
  }

  .parse_records_biocparallel(
    records = records,
    simplify_vector = simplifyVector,
    flatten = flatten,
    BPPARAM = BPPARAM
  )
}

#' Parse the ICA header
#'
#' @param header The JSON header string returned by
#'   [read_ica_sections()].
#' @param simplifyVector Passed to [jsonlite::fromJSON()].
#' @param flatten Passed to [jsonlite::fromJSON()].
#'
#' @return The parsed header as produced by [jsonlite::fromJSON()].
#'
#' @examples
#' example_file <- system.file(
#'     "extdata", "ica-example.json.gz", package = "AnnoJSON"
#' )
#' sections <- read_ica_sections(example_file, verbose = FALSE)
#' header <- parse_ica_header(sections$header)
#'
#' @importFrom jsonlite fromJSON
#'
#' @export
parse_ica_header <- function(header, simplifyVector = TRUE,
                                 flatten = FALSE) {
    .assert_scalar_character(header, "header")
    .assert_flag(simplifyVector, "simplifyVector")
    .assert_flag(flatten, "flatten")

    jsonlite::fromJSON(
        header,
        simplifyVector = simplifyVector,
        flatten = flatten
    )
}

#' List unique top-level fields in parsed ICA records
#'
#' @param records A list of parsed position or gene records.
#'
#' @return A character vector containing field names in first-occurrence
#'   order.
#'
#' @examples
#' example_file <- system.file(
#'     "extdata", "ica-example.json.gz", package = "AnnoJSON"
#' )
#' sections <- read_ica_sections(example_file, verbose = FALSE)
#' genes <- parse_ica_records(
#'     sections$genes,
#'     progress = FALSE
#' )
#' ica_unique_fields(genes)
#'
#' @export
ica_unique_fields <- function(records) {
    if (!is.list(records)) {
        stop("`records` must be a list.", call. = FALSE)
    }

    unique(unlist(lapply(records, names), use.names = FALSE))
}

