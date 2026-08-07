test_that("standard gzip fallback round-trips R objects", {
    path <- tempfile(fileext = ".rds.gz")
    on.exit(unlink(path), add = TRUE)
    object <- list(a = 1:3, b = "value")

    write_rds_parallel(
        object,
        path,
        use_pigz = FALSE
    )

    expect_identical(
        read_rds_parallel(path, use_pigz = FALSE),
        object
    )
})

test_that("an uncompressed RDS is not sent to pigz", {
    path <- tempfile(fileext = ".rds")
    on.exit(unlink(path), add = TRUE)
    object <- list(a = 1:3, b = "value")
    saveRDS(object, path, compress = FALSE)

    expect_identical(
        read_rds_parallel(path, use_pigz = TRUE),
        object
    )
})

test_that("position tibble converts to GRanges without changing rows", {
    result <- read_ica_json(
        example_ica_file(),
        progress = FALSE,
        verbose = FALSE
    )
    ranges <- ica_positions_granges(result$tbl_positions)

    expect_s4_class(ranges, "GRanges")
    expect_identical(length(ranges), nrow(result$tbl_positions))
    expect_identical(
        as.integer(IRanges::start(ranges)),
        result$tbl_positions$position
    )
    expect_identical(
        as.integer(S4Vectors::mcols(ranges)$source_row),
        seq_len(nrow(result$tbl_positions))
    )
})
