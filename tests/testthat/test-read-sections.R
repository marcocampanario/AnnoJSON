test_that("section reader preserves the original three-element structure", {
    sections <- read_ica_sections(
        example_ica_file(),
        verbose = FALSE
    )

    expect_identical(names(sections), c("header", "positions", "genes"))
    expect_type(sections$header, "character")
    expect_length(sections$header, 1L)
    expect_length(sections$positions, 2L)
    expect_length(sections$genes, 2L)
    expect_false(any(endsWith(sections$positions, ",")))
    expect_false(any(endsWith(sections$genes, ",")))
})

test_that("files without a gene section return an empty character vector", {
    sections <- read_ica_sections(
        example_ica_file_without_genes(),
        verbose = FALSE
    )

    expect_length(sections$positions, 1L)
    expect_identical(sections$genes, character())
})

test_that("gzip-compressed JSON input has the same section structure", {
    gzip_path <- tempfile(fileext = ".json.gz")
    on.exit(unlink(gzip_path), add = TRUE)

    input_lines <- readLines(
        example_ica_file(),
        warn = FALSE
    )
    connection <- gzfile(gzip_path, open = "wt")
    writeLines(input_lines, connection)
    close(connection)

    uncompressed <- read_ica_sections(
        example_ica_file(),
        verbose = FALSE
    )
    compressed <- read_ica_sections(
        gzip_path,
        verbose = FALSE
    )

    expect_identical(compressed, uncompressed)
})
