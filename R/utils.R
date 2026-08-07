#' Validate a non-empty character scalar
#'
#' Checks whether an object is a character vector of length one containing
#' a non-missing and non-empty value. If the validation fails, an informative
#' error is raised using the supplied argument name.
#'
#' @param x The object to validate.
#' @param argument A character scalar containing the argument name to include
#'   in the error message.
#'
#' @return `x`, invisibly, when the validation succeeds.
#'
#' @noRd
.assert_scalar_character <- function(x, argument) {
    valid <- is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
    if (!valid) {
        stop(
            sprintf("`%s` must be one non-empty character value.", argument),
            call. = FALSE
        )
    }
    invisible(x)
}

#' Validate a logical flag
#'
#' Checks whether an object is a non-missing logical vector of length one.
#' If validation fails, an informative error is raised using the supplied
#' argument name.
#'
#' @param x The object to validate.
#' @param argument A character scalar containing the argument name to include
#'   in the error message.
#'
#' @return `x`, invisibly, when validation succeeds.
#'
#' @noRd
.assert_flag <- function(x, argument) {
    valid <- is.logical(x) && length(x) == 1L && !is.na(x)
    if (!valid) {
        stop(
            sprintf("`%s` must be either TRUE or FALSE.", argument),
            call. = FALSE
        )
    }
    invisible(x)
}

#' Validate and coerce a positive integer
#'
#' Checks whether an object is a finite, non-missing, integer-valued numeric
#' scalar greater than or equal to one. If validation fails, an informative
#' error is raised using the supplied argument name.
#'
#' @param x The object to validate.
#' @param argument A character scalar containing the argument name to include
#'   in the error message.
#'
#' @return A positive integer scalar obtained by coercing `x` to integer.
#'
#' @noRd
.validate_positive_integer <- function(x, argument) {
    valid <- is.numeric(x) &&
        length(x) == 1L &&
        !is.na(x) &&
        is.finite(x) &&
        x >= 1 &&
        x == floor(x)

    if (!valid) {
        stop(
            sprintf("`%s` must be one positive integer.", argument),
            call. = FALSE
        )
    }

    as.integer(x)
}

#' Emit a message conditionally
#'
#' Produces a diagnostic message only when `verbose` is exactly `TRUE`.
#' In our package, all verboses are true, so... there will be messages XD
#' Additional arguments are passed directly to [message()].
#'
#' @param verbose A logical flag indicating whether the message should be
#'   displayed.
#' @param ... Arguments passed to [message()].
#'
#' @return `NULL`, invisibly.
#'
#' @noRd
.message_if <- function(verbose, ...) {
    if (isTRUE(verbose)) {
        message(...)
    }
    invisible(NULL)
}

#' Extract a bounded slice from a character vector
#'
#' Extracts an inclusive range of elements from a character vector while
#' restricting the requested indices to the valid boundaries of the vector.
#' If the vector is empty or the requested interval does not overlap its
#' valid indices, an empty character vector is returned.
#'
#' @param x A character vector from which elements are extracted.
#' @param first A numeric scalar specifying the first index of the requested
#'   interval.
#' @param last A numeric scalar specifying the last index of the requested
#'   interval.
#'
#' @return A character vector containing the elements between the bounded
#'   `first` and `last` indices, inclusive. Returns `character(0)` when no
#'   valid interval is available.
#'
#' @noRd
.safe_slice <- function(x, first, last) {
    if (length(x) == 0L || first > last || first > length(x) || last < 1L) {
        return(character())
    }

    first <- max(1L, as.integer(first))
    last <- min(length(x), as.integer(last))
    x[seq.int(first, last)]
}

#' Remove terminal commas from JSON record strings
#'
#' Removes a comma occurring at the end of each character string, together
#' with any whitespace following the comma. Strings without a terminal comma
#' are returned unchanged.
#'
#' @param x A character vector containing JSON record strings.
#'
#' @return A character vector of the same length as `x`, with terminal commas
#'   and their trailing whitespace removed.
#'
#' @noRd
.remove_terminal_commas <- function(x) {
    sub(",[[:space:]]*$", "", x)
}

#' Open a text connection to a compressed or uncompressed file
#'
#' Opens a file for reading in text mode. Gzip compression is detected by
#' `.is_gzip_file()`, independently of the filename extension. A gzip
#' connection is created for compressed files, whereas a standard file
#' connection is created for uncompressed files.
#'
#' @param path A character scalar containing the path to the input file.
#' @param encoding A character scalar specifying the text encoding used by
#'   the connection. The default is `"UTF-8"`.
#'
#' @return An open text-mode connection created by [gzfile()] or
#'   [file()]. The caller is responsible for closing the connection.
#'
#' @noRd
.open_text_input <- function(path, encoding = "UTF-8") {
    if (.is_gzip_file(path)) {
        gzfile(path, open = "rt", encoding = encoding)
    } else {
        file(path, open = "rt", encoding = encoding)
    }
}

#' Detect gzip compression from a file signature
#'
#' Determines whether a file is gzip-compressed by reading its first two
#' bytes and comparing them with the gzip magic-number signature:
#' hexadecimal `1F 8B`, corresponding to decimal values `31` and `139`.
#' Detection is therefore independent of the filename extension.
#'
#' @param path A character scalar containing the path to the input file.
#'
#' @return A logical scalar: `TRUE` when the file has a gzip signature and
#'   `FALSE` otherwise.
#'
#' @noRd
.is_gzip_file <- function(path) {
    connection <- file(path, open = "rb")
    on.exit(close(connection), add = TRUE)
    magic <- readBin(connection, what = "raw", n = 2L)

    length(magic) == 2L &&
        identical(as.integer(magic), c(31L, 139L))
}

#' Extract the header JSON object from the first ICA line
#'
#' Removes surrounding whitespace and an optional Unicode byte order mark
#' from the first line of a ICA annotation file. The function verifies
#' that the line starts with the top-level `header` field and ends by opening
#' the `positions` array, then extracts the enclosed header JSON object.
#'
#' @param header_line A character scalar containing the first line of a
#'   ICA JSON annotation file.
#'
#' @return A character scalar containing only the header JSON object,
#'   without the surrounding top-level field names.
#'
#' @noRd
.extract_header_json <- function(header_line) {
    header_line <- trimws(header_line)
    header_line <- sub("^\ufeff", "", header_line)
    prefix <- "{\"header\":"
    suffix <- ",\"positions\":["

    valid <- startsWith(header_line, prefix) &&
        endsWith(header_line, suffix)

    if (!valid) {
        stop(
            paste0(
                "The first line does not have the expected ICA structure: ",
                "'{\"header\":...,\"positions\":['."
            ),
            call. = FALSE
        )
    }

    first <- nchar(prefix) + 1L
    last <- nchar(header_line) - nchar(suffix)
    substr(header_line, first, last)
}

#' Convert nested values to a character scalar
#'
#' Flattens an object recursively, converts its non-missing elements to
#' character values, and combines multiple values into a single character
#' scalar. Missing values are removed before concatenation.
#'
#' If `x` is `NULL`, has length zero, or contains only missing values, the
#' value supplied in `default` is returned.
#'
#' @param x An object whose elements should be converted to a character
#'   scalar. Nested objects are flattened recursively.
#' @param default A character scalar returned when `x` is `NULL`, empty, or
#'   contains no non-missing values. The default is `NA_character_`.
#' @param collapse A character scalar used to separate multiple values. The
#'   default is `";"`.
#'
#' @return A character scalar containing the non-missing values of `x`
#'   separated by `collapse`, or `default` when no values are available.
#'
#' @noRd
.as_character_scalar <- function(x, default = NA_character_,
                                 collapse = ";") {
    if (is.null(x) || length(x) == 0L) {
        return(default)
    }

    value <- as.character(unlist(x, recursive = TRUE, use.names = FALSE))
    value <- value[!is.na(value)]

    if (length(value) == 0L) {
        default
    } else {
        paste(value, collapse = collapse)
    }
}

#' Convert the first nested value to an integer scalar
#'
#' Flattens an object recursively and attempts to coerce its first element to
#' an integer scalar. Warnings produced during coercion are suppressed.
#'
#' If `x` is `NULL`, empty, has no elements after flattening, or its first
#' element cannot be converted to a non-missing integer, the value supplied
#' in `default` is returned.
#'
#' @param x An object whose first flattened element should be converted to an
#'   integer scalar.
#' @param default An integer scalar returned when no suitable value is
#'   available. The default is `NA_integer_`.
#'
#' @return The first flattened element of `x`, coerced to an integer scalar,
#'   or `default` when conversion does not produce a non-missing value.
#'
#' @noRd
.as_integer_scalar <- function(x, default = NA_integer_) {
    if (is.null(x) || length(x) == 0L) {
        return(default)
    }

    value <- unlist(x, recursive = TRUE, use.names = FALSE)
    if (length(value) == 0L) {
        return(default)
    }

    value <- suppressWarnings(
        as.integer(value[[1L]])
    )

    if (length(value) == 0L || is.na(value)) default else value
}

#' Convert the first nested value to a double scalar
#'
#' Flattens an object recursively and attempts to coerce its first element to
#' a double-precision numeric scalar. Warnings produced during coercion are
#' suppressed.
#'
#' If `x` is `NULL`, empty, has no elements after flattening, or its first
#' element cannot be converted to a non-missing numeric value, the value
#' supplied in `default` is returned.
#'
#' @param x An object whose first flattened element should be converted to a
#'   double scalar.
#' @param default A double scalar returned when no suitable value is
#'   available. The default is `NA_real_`.
#'
#' @return The first flattened element of `x`, coerced to a double scalar, or
#'   `default` when conversion does not produce a non-missing numeric value.
#'
#' @noRd
.as_double_scalar <- function(x, default = NA_real_) {
    if (is.null(x) || length(x) == 0L) {
        return(default)
    }

    value <- unlist(x, recursive = TRUE, use.names = FALSE)
    if (length(value) == 0L) {
        return(default)
    }

    value <- suppressWarnings(
        as.numeric(value[[1L]])
    )

    if (length(value) == 0L || is.na(value)) default else value
}

#' Extract a field from a parsed ICA record
#'
#' Retrieves a named field from a parsed ICA record represented as a list.
#' If `record` is not a list, has no names, does not contain the requested
#' field, or contains `NULL` for that field, the supplied default value is
#' returned.
#'
#' @param record A named list representing a parsed ICA position or gene
#'   record.
#' @param field A character scalar containing the name of the field to
#'   retrieve.
#' @param default An object returned when the requested field is unavailable
#'   or contains `NULL`. The default is `NULL`.
#'
#' @return The value stored in `record[[field]]`, or `default` when no
#'   non-`NULL` value is available.
#'
#' @noRd
.record_field <- function(record, field, default = NULL) {
    if (
        !is.list(record) ||
            is.null(names(record)) ||
            !(field %in% names(record))
    ) {
        return(default)
    }

    value <- record[[field]]
    if (is.null(value)) default else value
}

#' Move a completed file to its final output path
#'
#' Moves a source file to a destination path using [file.rename()].
#' If renaming fails, for example when the source and destination are on
#' different file systems, the function falls back to copying the source
#' file and subsequently removing it.
#'
#' If the destination already exists, it is replaced only when `overwrite`
#' is `TRUE`. An informative error is raised when the destination exists and
#' replacement is not permitted, or when neither renaming nor copying
#' succeeds.
#'
#' @param from A character scalar containing the path of the completed source
#'   file.
#' @param to A character scalar containing the final destination path.
#' @param overwrite A logical scalar indicating whether an existing
#'   destination file may be replaced.
#'
#' @return The destination path `to`, invisibly.
#'
#' @noRd
.atomic_move <- function(from, to, overwrite) {
    if (file.exists(to)) {
        if (!overwrite) {
            stop(
                sprintf("Output file already exists: %s", to),
                call. = FALSE
            )
        }
        unlink(to)
    }

    moved <- file.rename(from, to)
    if (!moved) {
        copied <- file.copy(from, to, overwrite = overwrite)
        if (!copied) {
            stop(
                sprintf("Could not move the completed output to: %s", to),
                call. = FALSE
            )
        }
        unlink(from)
    }

    invisible(to)
}
