test_that("position tibble has the original schema", {
    result <- read_ica_json(
        example_ica_file(),
        progress = FALSE,
        verbose = FALSE
    )
    tbl_positions <- result$tbl_positions

    expect_s3_class(tbl_positions, "tbl_df")
    expect_identical(
        names(tbl_positions),
        c(
            "chromosome",
            "position",
            "refAllele",
            "altAlleles",
            "vcfInfo",
            "quality",
            "filters",
            "cytogeneticBand",
            "samples",
            "variants"
        )
    )
    expect_identical(tbl_positions$position, c(69805L, 100L))
    expect_identical(tbl_positions$filters[[2L]], "LowQual;StrandBias")
})

test_that("gene tibble has the original schema and nested defaults", {
    result <- read_ica_json(
        example_ica_file(),
        progress = FALSE,
        verbose = FALSE
    )
    tbl_genes <- result$tbl_genes

    expect_s3_class(tbl_genes, "tbl_df")
    expect_identical(
        names(tbl_genes),
        c(
            "name",
            "ensemblGeneId",
            "hgncId",
            "ncbiGeneId",
            "gnomAD",
            "omim",
            "clingenGeneValidity",
            "clingenDosageSensitivityMap",
            "cosmic"
        )
    )
    expect_identical(tbl_genes$name, c("OR4F5", "GENE2"))
    expect_identical(tbl_genes$hgncId, c(14825L, 2L))
    expect_identical(tbl_genes$gnomAD[[2L]], list(NULL))
})

test_that("a positions-only file creates an empty typed gene tibble", {
    result <- read_ica_json(
        example_ica_file_without_genes(),
        progress = FALSE,
        verbose = FALSE
    )

    expect_identical(nrow(result$tbl_genes), 0L)
    expect_length(names(result$tbl_genes), 9L)
})

test_that("complete reader returns exactly the five central objects", {
    result <- read_ica_json(
        example_ica_file(),
        progress = FALSE,
        verbose = FALSE
    )

    expect_identical(
        names(result),
        c(
            "header",
            "positions",
            "genes",
            "tbl_positions",
            "tbl_genes"
        )
    )
})

test_that("missing vcfInfo fields are represented by NULL", {
  positions <- list(
    list(
      chromosome = "chr1",
      position = 100L,
      refAllele = "A"
    )
  )

  result <- ica_positions_tibble(positions)

  expect_length(result$vcfInfo, 1L)
  expect_null(result$vcfInfo[[1L]])
})
