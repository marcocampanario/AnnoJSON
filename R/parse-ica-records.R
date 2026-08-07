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

#' Parse ICA JSON records sequentially
#'
#' Parses a character vector of JSON records sequentially using
#' `.parse_one_json_record()`. Records are processed in their original order
#' and returned as a list with one parsed R object per input record.
#'
#' When `progress` is `TRUE`, parsing is wrapped in
#' [progressr::with_progress()] and the progress state is updated after each
#' record.
#'
#' @param records A character vector containing one JSON record per element.
#' @param simplify_vector A logical scalar passed to
#'   `.parse_one_json_record()` to control JSON simplification.
#' @param flatten A logical scalar passed to `.parse_one_json_record()` to
#'   control the flattening of nested data frames.
#' @param progress A logical scalar indicating whether parsing progress should
#'   be reported through `progressr`.
#'
#' @return A list containing one parsed R object per element of `records`, in
#'   the same order as the input.
#'
#' @importFrom progressr progressor
#' @importFrom progressr with_progress
#'
#' @noRd
.parse_records_sequential <- function(records, simplify_vector, flatten,
                                      progress) {
    parse_record <- function(record) {
        .parse_one_json_record(record, simplify_vector, flatten)
    }

    if (!progress) {
        return(lapply(records, parse_record))
    }

    progressr::with_progress({
        progressor <- progressr::progressor(along = records)
        lapply(records, function(record) {
            value <- parse_record(record)
            progressor()
            value
        })
    })
}

#' Parse ICA JSON records in parallel
#'
#' Parses a character vector of JSON records in parallel using independent R
#' sessions created with [future::multisession()]. Records are distributed
#' across the requested number of workers and parsed with
#' `.parse_one_json_record()`.
#'
#' The previously active `future` plan is restored when the function exits,
#' including when an error occurs during parsing. Parsed records are returned
#' in the same order as the input.
#'
#' @param records A character vector containing one JSON record per element.
#' @param workers A positive integer specifying the number of parallel R
#'   processes.
#' @param simplify_vector A logical scalar passed to
#'   `.parse_one_json_record()` to control JSON simplification.
#' @param flatten A logical scalar passed to `.parse_one_json_record()` to
#'   control the flattening of nested data frames.
#' @param progress A logical scalar indicating whether parsing progress should
#'   be reported.
#'
#' @return A list containing one parsed R object per element of `records`, in
#'   the same order as the input.
#'
#' @importFrom future multisession
#' @importFrom future plan
#' @importFrom furrr future_map
#' @importFrom furrr furrr_options
#' @importFrom progressr with_progress
#'
#' @noRd
.parse_records_parallel <- function(records, workers, simplify_vector,
                                    flatten, progress) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)

    future::plan(future::multisession, workers = workers)
    options <- furrr::furrr_options(
        packages = "jsonlite",
        seed = TRUE
    )

    parse_records <- function() {
        furrr::future_map(
            .x = records,
            .f = .parse_one_json_record,
            simplify_vector = simplify_vector,
            flatten = flatten,
            .options = options,
            .progress = progress
        )
    }

    if (progress) {
        progressr::with_progress(parse_records())
    } else {
        parse_records()
    }
}

#' Parse position- or gene-level ICA JSON records
#'
#' Converts a character vector containing one JSON object per element into
#' a list of R objects. A single worker uses sequential parsing. Two or more
#' workers use an isolated `future::multisession` plan, and the previous
#' future plan is restored when parsing finishes.
#'
#' @param records A character vector of JSON records, such as the
#'   `positions` or `genes` element returned by
#'   [read_ica_sections()].
#' @param workers Number of R processes. Use `1L` for sequential parsing.
#' @param progress Whether to display parsing progress.
#' @param simplifyVector Passed to [jsonlite::fromJSON()].
#' @param flatten Passed to [jsonlite::fromJSON()].
#'
#' @return A list with one parsed R object per input record and in the same
#'   order as `records`.
#'
#' @examples
#' example_file <- system.file(
#'     "extdata", "ica-example.json.gz", package = "AnnoJSON"
#' )
#' sections <- read_ica_sections(example_file, verbose = FALSE)
#' positions <- parse_ica_records(
#'     sections$positions,
#'     workers = 1L,
#'     progress = FALSE
#' )
#'
#' @export
parse_ica_records <- function(records, workers = 1L,
                                  progress = interactive(),
                                  simplifyVector = TRUE,
                                  flatten = FALSE) {
    if (!is.character(records)) {
        stop("`records` must be a character vector.", call. = FALSE)
    }

    workers <- .validate_positive_integer(workers, "workers")
    .assert_flag(progress, "progress")
    .assert_flag(simplifyVector, "simplifyVector")
    .assert_flag(flatten, "flatten")

    if (length(records) == 0L) {
        return(list())
    }

    if (workers == 1L) {
        .parse_records_sequential(
            records = records,
            simplify_vector = simplifyVector,
            flatten = flatten,
            progress = progress
        )
    } else {
        .parse_records_parallel(
            records = records,
            workers = workers,
            simplify_vector = simplifyVector,
            flatten = flatten,
            progress = progress
        )
    }
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

