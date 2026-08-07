#' Read the line-oriented sections of a Nirvana JSON file
#'
#' Reads a Illumina Connected Annotations JSON file without
#' parsing every record.
#' The function produces a three-element structure.
#'
#' @param path A path to a JSON or gzip-compressed JSON file.
#' @param verbose Whether to report reading progress.
#' @param encoding Character encoding passed to the input connection.
#'
#' @return A plain list with exactly three elements:
#'   `header`, `positions`, and `genes`. The header is one JSON string;
#'   positions and genes are character vectors containing one JSON record
#'   per element. If the input has no gene section, `genes` is
#'   `character(0)`.
#'
#' @examples
#' example_file <- system.file(
#'     "extdata", "nirvana-example.json", package = "GenVarJSON"
#' )
#' sections <- read_ica_sections(example_file, verbose = FALSE)
#' names(sections)
#' lengths(sections)
#'
#' @export
read_ica_sections <- function(path, verbose = TRUE,
                                  encoding = "UTF-8") {
    .assert_scalar_character(path, "path")
    .assert_flag(verbose, "verbose")
    .assert_scalar_character(encoding, "encoding")

    if (!file.exists(path)) {
        stop(sprintf("Input file does not exist: %s", path), call. = FALSE)
    }

    .message_if(verbose, "Reading all file lines...")
    connection <- .open_text_input(path, encoding = encoding)
    on.exit(close(connection), add = TRUE)
    all_lines <- readLines(connection, warn = FALSE)

    while (
        length(all_lines) > 0L &&
            !nzchar(trimws(all_lines[[length(all_lines)]]))
    ) {
        all_lines <- all_lines[-length(all_lines)]
    }

    if (length(all_lines) < 2L) {
        stop("The input file is empty or incomplete.", call. = FALSE)
    }

    .message_if(verbose, "Extracting sections...")
    gene_section_index <- which(trimws(all_lines) == "],\"genes\":[")

    if (length(gene_section_index) > 1L) {
        stop(
            "More than one top-level gene-section separator was found.",
            call. = FALSE
        )
    }

    if (length(gene_section_index) == 1L) {
        .message_if(
            verbose,
            "Section 'genes' found. Splitting the file..."
        )
        position_lines <- .safe_slice(
            all_lines,
            2L,
            gene_section_index - 1L
        )
        gene_lines <- .safe_slice(
            all_lines,
            gene_section_index + 1L,
            length(all_lines) - 1L
        )
    } else {
        .message_if(
            verbose,
            paste0(
                "Section 'genes' not found; assuming that the file ",
                "contains positions only."
            )
        )
        position_lines <- .safe_slice(
            all_lines,
            2L,
            length(all_lines) - 1L
        )
        gene_lines <- character()
    }

    header <- .extract_header_json(all_lines[[1L]])
    rm(all_lines)
    positions <- .remove_terminal_commas(position_lines)
    genes <- .remove_terminal_commas(gene_lines)

    .message_if(verbose, "Processing completed.")
    .message_if(verbose, "Number of positions: ", length(positions))
    .message_if(verbose, "Number of genes: ", length(genes))

    list(
        header = header,
        positions = positions,
        genes = genes
    )
}
