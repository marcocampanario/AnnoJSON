#' Write an R object with optional parallel gzip compression
#'
#' Uses `pigz` when it is installed and requested. Otherwise, it falls back
#' to R's standard gzip compression. A temporary file is completed before
#' the requested output path is replaced.
#'
#' @details
#' The compression backend is selected as follows:
#' - Linux or macOS with `pigz` available and `use_pigz = TRUE`: parallel
#'   compression with `pigz`.
#' - Windows: standard R gzip compression.
#' - `pigz` unavailable: standard R gzip compression.
#' - `use_pigz = FALSE`: standard R gzip compression.
#' The `threads` argument is used only when compression is performed with
#' `pigz`. It has no effect on the standard R gzip fallback.
#'
#' @param object An R object.
#' @param path Output path.
#' @param threads Number of compression threads passed to `pigz`.
#' @param overwrite Whether an existing file may be replaced.
#' @param use_pigz Whether to use `pigz` when it is available.
#'
#' @return The normalized output path, invisibly.
#'
#' @examples
#' output <- tempfile(fileext = ".rds.gz")
#' write_rds_parallel(
#'     list(value = 1:3),
#'     output,
#'     threads = 1L,
#'     use_pigz = FALSE
#' )
#' read_rds_parallel(output, use_pigz = FALSE)
#' unlink(output)
#'
#' @export
write_rds_parallel <- function(object, path, threads = 1L,
                               overwrite = FALSE, use_pigz = TRUE) {
    .assert_scalar_character(path, "path")
    threads <- .validate_positive_integer(threads, "threads")
    .assert_flag(overwrite, "overwrite")
    .assert_flag(use_pigz, "use_pigz")

    output_directory <- dirname(path)
    if (!dir.exists(output_directory)) {
        stop(
            sprintf("Output directory does not exist: %s", output_directory),
            call. = FALSE
        )
    }

    path <- file.path(
        normalizePath(output_directory, mustWork = TRUE),
        basename(path)
    )

    if (file.exists(path) && !overwrite) {
        stop(
            sprintf("Output file already exists: %s", path),
            call. = FALSE
        )
    }

    temporary_path <- tempfile(
        pattern = paste0(".", basename(path), "-"),
        tmpdir = dirname(path)
    )
    on.exit(unlink(temporary_path), add = TRUE)

    pigz <- Sys.which("pigz")
    can_use_pigz <- use_pigz &&
        nzchar(pigz) &&
        .Platform$OS.type != "windows"

    if (can_use_pigz) {
        command <- sprintf(
            "%s -p %d -c > %s",
            shQuote(pigz),
            threads,
            shQuote(temporary_path)
        )
        connection <- pipe(command, open = "wb")
        on.exit({
            if (!is.null(connection)) {
                try(close(connection), silent = TRUE)
            }
        }, add = TRUE)

        saveRDS(object, file = connection, compress = FALSE)
        close(connection)
        connection <- NULL
    } else {
        saveRDS(object, file = temporary_path, compress = "gzip")
    }

    .atomic_move(temporary_path, path, overwrite = overwrite)
    invisible(path)
}

#' Read an RDS file with optional parallel gzip decompression
#'
#' @details
#' The decompression backend is selected as follows:
#'
#' - Linux or macOS, gzip-compressed input, `pigz` available, and
#'   `use_pigz = TRUE`: decompression through `pigz`.
#' - Windows: direct reading with [base::readRDS()].
#' - Uncompressed input: direct reading with [base::readRDS()].
#' - `pigz` unavailable: direct reading with [base::readRDS()].
#' - `use_pigz = FALSE`: direct reading with [base::readRDS()].
#'
#' @param path Path to an RDS or gzip-compressed RDS file.
#' @param use_pigz Whether to use `pigz` when it is available.
#'
#' @return The R object stored in `path`.
#'
#' @examples
#' output <- tempfile(fileext = ".rds.gz")
#' saveRDS(list(value = 1:3), output)
#' read_rds_parallel(output, use_pigz = FALSE)
#' unlink(output)
#'
#' @seealso [write_rds_parallel()]
#'
#' @export
read_rds_parallel <- function(path, use_pigz = TRUE) {
    .assert_scalar_character(path, "path")
    .assert_flag(use_pigz, "use_pigz")

    if (!file.exists(path)) {
        stop(sprintf("Input file does not exist: %s", path), call. = FALSE)
    }

    path <- normalizePath(path, mustWork = TRUE)
    pigz <- Sys.which("pigz")
    can_use_pigz <- use_pigz &&
        .is_gzip_file(path) &&
        nzchar(pigz) &&
        .Platform$OS.type != "windows"

    if (!can_use_pigz) {
        return(readRDS(path))
    }

    command <- sprintf(
        "%s -dc -- %s",
        shQuote(pigz),
        shQuote(path)
    )
    connection <- pipe(command, open = "rb")
    on.exit({
        if (!is.null(connection)) {
            try(close(connection), silent = TRUE)
        }
    }, add = TRUE)
    object <- readRDS(connection)
    close(connection)
    connection <- NULL
    object
}

#' Write the five parsed ICA outputs
#'
#' Writes the parsed header, position list, gene list, position tibble, and
#' gene tibble with predictable names. These files correspond to the final
#' save blocks in the original `read_json.R` script.
#'
#' @details
#' The output directory must already exist. The following files are created:
#'
#' - `<prefix>_header.rds`
#' - `<prefix>_positions_list.rds.gz`
#' - `<prefix>_genes_list.rds.gz`
#' - `<prefix>_tbl_positions.rds.gz`
#' - `<prefix>_tbl_genes.rds.gz`
#'
#' All objects are written with [write_rds_parallel()]. When `pigz` is
#' available, supported, and enabled, `threads` determines the number of
#' compression threads. Otherwise, standard R gzip compression is used and
#' `threads` has no effect.
#'
#' Before writing any object, the function checks whether any destination
#' file already exists. If `overwrite` is `FALSE`, no files are written and
#' an informative error is raised.
#'
#' @param result The five-element result returned by
#'   [read_ica_json()].
#' @param output_dir Existing output directory.
#' @param prefix Filename prefix.
#' @param threads Number of compression threads.
#' @param overwrite Whether existing files may be replaced.
#' @param use_pigz Whether to use `pigz` when it is available.
#'
#' @return A named character vector of output paths, invisibly.
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
#' output_dir <- tempfile()
#' dir.create(output_dir)
#' files <- write_ica_results(
#'     result,
#'     output_dir,
#'     prefix = "example",
#'     use_pigz = FALSE
#' )
#' unlink(output_dir, recursive = TRUE)
#'
#' @export
write_ica_results <- function(result, output_dir, prefix = "ica",
                                  threads = 1L, overwrite = FALSE,
                                  use_pigz = TRUE) {
    required <- c(
        "header",
        "positions",
        "genes",
        "tbl_positions",
        "tbl_genes"
    )

    if (!is.list(result) || !all(required %in% names(result))) {
        stop(
            paste0(
                "`result` must contain: ",
                paste(required, collapse = ", "),
                "."
            ),
            call. = FALSE
        )
    }

    .assert_scalar_character(output_dir, "output_dir")
    .assert_scalar_character(prefix, "prefix")
    threads <- .validate_positive_integer(threads, "threads")
    .assert_flag(overwrite, "overwrite")
    .assert_flag(use_pigz, "use_pigz")

    if (!dir.exists(output_dir)) {
        stop(
            sprintf("Output directory does not exist: %s", output_dir),
            call. = FALSE
        )
    }

    if (
        !identical(prefix, basename(prefix)) ||
            prefix %in% c(".", "..")
    ) {
        stop(
            "`prefix` must be a filename prefix without path separators.",
            call. = FALSE
        )
    }

    output_dir <- normalizePath(output_dir, mustWork = TRUE)
    paths <- c(
        header = file.path(
            output_dir,
            paste0(prefix, "_header.rds")
        ),
        positions = file.path(
            output_dir,
            paste0(prefix, "_positions_list.rds.gz")
        ),
        genes = file.path(
            output_dir,
            paste0(prefix, "_genes_list.rds.gz")
        ),
        tbl_positions = file.path(
            output_dir,
            paste0(prefix, "_tbl_positions.rds.gz")
        ),
        tbl_genes = file.path(
            output_dir,
            paste0(prefix, "_tbl_genes.rds.gz")
        )
    )

    existing <- paths[file.exists(paths)]
    if (length(existing) > 0L && !overwrite) {
        stop(
            paste0(
                "Output file(s) already exist: ",
                paste(existing, collapse = ", ")
            ),
            call. = FALSE
        )
    }

    for (name in names(paths)) {
        write_rds_parallel(
            object = result[[name]],
            path = paths[[name]],
            threads = threads,
            overwrite = overwrite,
            use_pigz = use_pigz
        )
    }

    invisible(paths)
}
