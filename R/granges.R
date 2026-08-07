#' Convert the position tibble to genomic ranges
#'
#' Provides Bioconductor interoperability without changing
#' `tbl_positions`. Records without a chromosome or position cannot be
#' represented as genomic ranges and are omitted with a warning.
#'
#' @param tbl_positions A tibble returned by
#'   [ica_positions_tibble()].
#'
#' @return A [GenomicRanges::GRanges()] object. All original columns except
#'   `chromosome` and `position` are retained as metadata columns, together
#'   with `source_row`.
#'
#' @examples
#' example_file <- system.file(
#'     "extdata", "ica-example.json.gz", package = "AnnoJSON"
#' )
#' result <- read_ica_json(
#'     example_file,
#'     progress = FALSE,
#'     verbose = FALSE
#' )
#' ranges <- ica_positions_granges(result$tbl_positions)
#' ranges
#'
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom S4Vectors DataFrame
#' @importFrom S4Vectors mcols
#'
#' @export
ica_positions_granges <- function(tbl_positions) {
    if (!is.data.frame(tbl_positions)) {
        stop("`tbl_positions` must be a data-frame-like object.", call. = FALSE)
    }

    required <- c("chromosome", "position")
    if (!all(required %in% names(tbl_positions))) {
        stop(
            "`tbl_positions` must contain `chromosome` and `position`.",
            call. = FALSE
        )
    }

    valid <- !is.na(tbl_positions$chromosome) &
        nzchar(as.character(tbl_positions$chromosome)) &
        !is.na(tbl_positions$position)

    if (any(!valid)) {
        warning(
            sum(!valid),
            " record(s) without a genomic coordinate were omitted.",
            call. = FALSE
        )
    }

    data <- tbl_positions[valid, , drop = FALSE]
    source_row <- which(valid)

    ranges <- GenomicRanges::GRanges(
        seqnames = as.character(data$chromosome),
        ranges = IRanges::IRanges(
            start = as.integer(data$position),
            width = 1L
        ),
        strand = "*"
    )

    metadata_names <- setdiff(
        names(data),
        c("chromosome", "position")
    )
    metadata <- S4Vectors::DataFrame(data, check.names = FALSE)
    metadata <- metadata[, metadata_names, drop = FALSE]
    metadata$source_row <- source_row
    metadata <- metadata[
        ,
        c("source_row", metadata_names),
        drop = FALSE
    ]
    S4Vectors::mcols(ranges) <- metadata
    ranges
}
