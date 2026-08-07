#' Read and parse a complete Nirvana JSON annotation file
#'
#' Runs the section reader, JSON parsers, and tibble constructors in one
#' call. The returned names deliberately match the central objects of the
#' original aPRoVAR `read_json.R` workflow.
#'
#' @details
#' The input file is first divided into its `header`, `positions`, and optional
#' `genes` sections using [read_ica_sections()]. The individual JSON records
#' are then parsed with [parse_ica_records()].
#'
#' When `workers` is greater than one, position and gene records are parsed
#' using independent R sessions. Parallel execution requires the `AnnoJSON`
#' package and its dependencies to be available to the worker processes.
#'
#' Files without a top-level gene section produce an empty `genes` list and
#' an empty `tbl_genes` tibble with the complete gene-column schema.
#'
#' @param path A character scalar containing the path to an uncompressed or
#'   gzip-compressed ICA JSON annotation file.
#' @param workers A positive integer specifying the number of R processes used
#'   to parse position and gene records. Use `1L` for sequential parsing.
#' @param progress A logical scalar indicating whether parsing progress should
#'   be displayed.
#' @param simplifyVector A logical scalar passed to [jsonlite::fromJSON()] to
#'   control the simplification of JSON arrays.
#' @param flatten A logical scalar passed to [jsonlite::fromJSON()] to control
#'   the flattening of nested data frames.
#' @param verbose A logical scalar indicating whether section-reading messages
#'   should be displayed.
#' @param encoding A character scalar specifying the encoding used to read the
#'   input file. The default is `"UTF-8"`.
#'
#' @return A plain list with exactly five elements: `header`,
#'   `positions`, `genes`, `tbl_positions`, and `tbl_genes`.
#'
#' @examples
#' example_file <- system.file(
#'     "extdata", "ica-example.json.gz", package = "AnnoJSON"
#' )
#' parsed <- read_ica_json(
#'     example_file,
#'     workers = 1L,
#'     progress = FALSE,
#'     verbose = FALSE
#' )
#' names(parsed)
#'
#' @seealso
#' [read_ica_sections()], [parse_ica_header()], [parse_ica_records()],
#' [ica_positions_tibble()], and [ica_genes_tibble()].
#'
#' @export
read_ica_json <- function(path, workers = 1L,
                              progress = interactive(),
                              simplifyVector = TRUE,
                              flatten = FALSE,
                              verbose = TRUE,
                              encoding = "UTF-8") {
    sections <- read_ica_sections(
        path = path,
        verbose = verbose,
        encoding = encoding
    )

    header <- parse_ica_header(
        sections$header,
        simplifyVector = simplifyVector,
        flatten = flatten
    )
    positions <- parse_ica_records(
        sections$positions,
        workers = workers,
        progress = progress,
        simplifyVector = simplifyVector,
        flatten = flatten
    )
    genes <- parse_ica_records(
        sections$genes,
        workers = workers,
        progress = progress,
        simplifyVector = simplifyVector,
        flatten = flatten
    )

    tbl_positions <- ica_positions_tibble(positions)
    tbl_genes <- ica_genes_tibble(genes)

    list(
        header = header,
        positions = positions,
        genes = genes,
        tbl_positions = tbl_positions,
        tbl_genes = tbl_genes
    )
}

