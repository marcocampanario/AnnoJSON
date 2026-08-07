test_that("legacy sequential and SerialParam parsing are identical", {
  sections <- read_ica_sections(
    example_ica_file(),
    verbose = FALSE
  )

  legacy <- parse_ica_records(
    sections$positions,
    workers = 1L,
    progress = FALSE
  )

  serial <- parse_ica_records(
    sections$positions,
    BPPARAM = BiocParallel::SerialParam(
      progressbar = FALSE
    )
  )

  expect_identical(serial, legacy)
})

test_that("SOCK parsing preserves values and input order", {
  sections <- read_ica_sections(
    example_ica_file(),
    verbose = FALSE
  )

  serial <- parse_ica_records(
    sections$positions,
    BPPARAM = BiocParallel::SerialParam(
      progressbar = FALSE
    )
  )

  param <- BiocParallel::SnowParam(
    workers = 2L,
    type = "SOCK",
    tasks = 2L,
    progressbar = FALSE,
    stop.on.error = TRUE
  )

  on.exit({
    if (BiocParallel::bpisup(param)) {
      BiocParallel::bpstop(param)
    }
  }, add = TRUE)

  parallel <- parse_ica_records(
    sections$positions,
    BPPARAM = param
  )

  expect_identical(parallel, serial)
  expect_identical(
    vapply(parallel, `[[`, integer(1L), "position"),
    vapply(serial, `[[`, integer(1L), "position")
  )
})

test_that("complete ICA results are backend-independent", {
  serial <- read_ica_json(
    example_ica_file(),
    verbose = FALSE,
    BPPARAM = BiocParallel::SerialParam(
      progressbar = FALSE
    )
  )

  param <- BiocParallel::SnowParam(
    workers = 2L,
    type = "SOCK",
    progressbar = FALSE,
    stop.on.error = TRUE
  )

  on.exit({
    if (BiocParallel::bpisup(param)) {
      BiocParallel::bpstop(param)
    }
  }, add = TRUE)

  parallel <- read_ica_json(
    example_ica_file(),
    verbose = FALSE,
    BPPARAM = param
  )

  expect_identical(parallel, serial)
  expect_identical(
    names(parallel),
    c(
      "header",
      "positions",
      "genes",
      "tbl_positions",
      "tbl_genes"
    )
  )
})

test_that("parallel arguments cannot be ambiguous", {
  expect_error(
    parse_ica_records(
      "{}",
      workers = 2L,
      BPPARAM = BiocParallel::SerialParam()
    ),
    "Use either `workers` and `progress`, or `BPPARAM`"
  )
})

test_that("invalid BPPARAM objects are rejected", {
  expect_error(
    parse_ica_records(
      "{}",
      BPPARAM = list()
    ),
    "`BPPARAM` must be a BiocParallelParam object"
  )
})

test_that("empty input does not start a backend", {
  param <- BiocParallel::SnowParam(
    workers = 2L,
    type = "SOCK",
    progressbar = FALSE
  )

  result <- parse_ica_records(
    character(),
    BPPARAM = param
  )

  expect_identical(result, list())
  expect_false(BiocParallel::bpisup(param))
})

test_that("JSON parsing errors propagate through BiocParallel", {
  expect_error(
    parse_ica_records(
      '{"incomplete":',
      BPPARAM = BiocParallel::SerialParam(
        progressbar = FALSE
      )
    )
  )
})
