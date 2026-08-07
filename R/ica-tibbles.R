#' Convert parsed ICA positions to the positions tibble
#'
#' Creates the same ten columns, in the same order and with the same names,
#' as `tbl_positions` in the original `read_json.R` workflow. Nested
#' annotations remain list-columns.
#'
#' @details
#' Scalar fields are converted to typed atomic columns. Missing scalar values
#' are represented by the corresponding typed `NA`.
#'
#' The `altAlleles`, `vcfInfo`, `samples`, and `variants` fields are retained
#' as list-columns. When `vcfInfo` is absent from a record, the corresponding
#' list-column element is `NULL`.
#'
#' @param positions A list of parsed position records.
#'
#' @return A tibble with columns `chromosome`, `position`, `refAllele`,
#'   `altAlleles`, `vcfInfo`, `quality`, `filters`, `cytogeneticBand`,
#'   `samples`, and `variants`.
#'
#' @examples
#' example_file <- system.file(
#'     "extdata", "ica-example.json.gz", package = "AnnoJSON"
#' )
#' sections <- read_ica_sections(example_file, verbose = FALSE)
#' positions <- parse_ica_records(
#'     sections$positions,
#'     progress = FALSE
#' )
#' tbl_positions <- ica_positions_tibble(positions)
#'
#' @seealso [read_ica_sections()], [parse_ica_records()]
#'
#' @importFrom tibble tibble
#'
#' @export
ica_positions_tibble <- function(positions) {
    if (!is.list(positions)) {
        stop("`positions` must be a list of parsed records.", call. = FALSE)
    }

    tibble::tibble(
        chromosome = vapply(
            positions,
            function(record) {
                .as_character_scalar(
                    .record_field(record, "chromosome")
                )
            },
            character(1L)
        ),
        position = vapply(
            positions,
            function(record) {
                .as_integer_scalar(.record_field(record, "position"))
            },
            integer(1L)
        ),
        refAllele = vapply(
            positions,
            function(record) {
                .as_character_scalar(.record_field(record, "refAllele"))
            },
            character(1L)
        ),
        altAlleles = lapply(
            positions,
            .record_field,
            field = "altAlleles",
            default = list(NULL)
        ),
        vcfInfo = lapply(
            positions,
            .record_field,
            field = "vcfInfo",
            default = NULL
        ),
        quality = vapply(
            positions,
            function(record) {
                .as_double_scalar(.record_field(record, "quality"))
            },
            numeric(1L)
        ),
        filters = vapply(
            positions,
            function(record) {
                .as_character_scalar(.record_field(record, "filters"))
            },
            character(1L)
        ),
        cytogeneticBand = vapply(
            positions,
            function(record) {
                .as_character_scalar(
                    .record_field(record, "cytogeneticBand")
                )
            },
            character(1L)
        ),
        samples = lapply(
            positions,
            .record_field,
            field = "samples",
            default = NULL
        ),
        variants = lapply(
            positions,
            .record_field,
            field = "variants",
            default = NULL
        )
    )
}

#' Convert parsed ICA genes to the gene tibble
#'
#' Creates the same nine columns, in the same order and with the same names,
#' as `tbl_genes` in the original `read_json.R` workflow. Nested
#' annotations remain list-columns.
#'
#' @details
#' Scalar identifiers are converted to typed atomic columns. Missing scalar
#' values are represented by the corresponding typed `NA`.
#'
#' The `gnomAD`, `omim`, `clingenGeneValidity`,
#' `clingenDosageSensitivityMap`, and `cosmic` fields are retained as
#' list-columns. When any of these fields is absent or `NULL`, its
#' corresponding list-column element is represented by `list(NULL)`.
#'
#' @param genes A list of parsed gene records.
#'
#' @return A tibble with columns `name`, `ensemblGeneId`, `hgncId`,
#'   `ncbiGeneId`, `gnomAD`, `omim`, `clingenGeneValidity`,
#'   `clingenDosageSensitivityMap`, and `cosmic`.
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
#' tbl_genes <- ica_genes_tibble(genes)
#'
#' @importFrom tibble tibble
#'
#' @export
ica_genes_tibble <- function(genes) {
    if (!is.list(genes)) {
        stop("`genes` must be a list of parsed records.", call. = FALSE)
    }

    tibble::tibble(
        name = vapply(
            genes,
            function(record) {
                .as_character_scalar(.record_field(record, "name"))
            },
            character(1L)
        ),
        ensemblGeneId = vapply(
            genes,
            function(record) {
                .as_character_scalar(
                    .record_field(record, "ensemblGeneId")
                )
            },
            character(1L)
        ),
        hgncId = vapply(
            genes,
            function(record) {
                .as_integer_scalar(.record_field(record, "hgncId"))
            },
            integer(1L)
        ),
        ncbiGeneId = vapply(
            genes,
            function(record) {
                .as_character_scalar(
                    .record_field(record, "ncbiGeneId")
                )
            },
            character(1L)
        ),
        gnomAD = lapply(
            genes,
            .record_field,
            field = "gnomAD",
            default = list(NULL)
        ),
        omim = lapply(
            genes,
            .record_field,
            field = "omim",
            default = list(NULL)
        ),
        clingenGeneValidity = lapply(
            genes,
            .record_field,
            field = "clingenGeneValidity",
            default = list(NULL)
        ),
        clingenDosageSensitivityMap = lapply(
            genes,
            .record_field,
            field = "clingenDosageSensitivityMap",
            default = list(NULL)
        ),
        cosmic = lapply(
            genes,
            .record_field,
            field = "cosmic",
            default = list(NULL)
        )
    )
}

