test_that("header and records are parsed without reordering", {
    sections <- read_ica_sections(
        example_ica_file(),
        verbose = FALSE
    )
    header <- parse_ica_header(sections$header)
    positions <- parse_ica_records(
        sections$positions,
        workers = 1L,
        progress = FALSE
    )

    expect_identical(header$genomeAssembly, "GRCh38")
    expect_length(positions, 2L)
    expect_identical(positions[[1L]]$position, 69805L)
    expect_identical(positions[[2L]]$position, 100L)
    expect_equal(
        positions[[1L]]$variants$gnomad$allAf,
        0.000118
    )
    expect_equal(
        positions[[1L]]$variants[["gnomad-exome"]]$allAf,
        0.000197
    )
})

test_that("unique fields retain first-occurrence order", {
    sections <- read_ica_sections(
        example_ica_file(),
        verbose = FALSE
    )
    genes <- parse_ica_records(
        sections$genes,
        progress = FALSE
    )

    fields <- ica_unique_fields(genes)
    expect_identical(
        fields[seq_len(4L)],
        c("name", "ensemblGeneId", "hgncId", "ncbiGeneId")
    )
})

