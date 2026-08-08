.make_ica_test_fixture <- local({
  cache <- NULL

  function() {
    if (!is.null(cache)) {
      return(cache)
    }

    path <- system.file(
      "extdata",
      "subset_100_af.json.gz",
      package = "AnnoJSON"
    )

    if (!nzchar(path)) {
      stop(
        "Test file 'subset_100_af.json.gz' was not found.",
        call. = FALSE
      )
    }

    parsed <- read_ica_json(
      path,
      verbose = FALSE,
      BPPARAM = BiocParallel::SerialParam(
        progressbar = FALSE
      )
    )

    positions <- if (
      methods::is(parsed$tbl_positions, "DataFrame")
    ) {
      parsed$tbl_positions
    } else {
      S4Vectors::DataFrame(
        parsed$tbl_positions,
        check.names = FALSE
      )
    }

    metadata <- list(
      source_file = basename(path),
      purpose = "package testing"
    )

    object <- ICAPositions(
      positions = positions,
      header = parsed$header,
      metadata = metadata
    )

    cache <<- list(
      path = path,
      parsed = parsed,
      positions = positions,
      metadata = metadata,
      object = object
    )

    cache
  }
})


testthat::test_that(
  "ICAPositions constructor creates a valid object",
  {
    fixture <- .make_ica_test_fixture()
    object <- fixture$object

    testthat::expect_s4_class(
      object,
      "ICAPositions"
    )
    testthat::expect_true(
      methods::is(position_data(object), "DataFrame")
    )
    testthat::expect_identical(
      nrow(position_data(object)),
      100L
    )
    testthat::expect_identical(
      methods::validObject(object, test = TRUE),
      TRUE
    )
  }
)


testthat::test_that(
  "show displays a compact ICAPositions summary",
  {
    object <- .make_ica_test_fixture()$object
    header <- ica_header(object)

    output <- paste(
      capture.output(methods::show(object)),
      collapse = "\n"
    )

    testthat::expect_match(
      output,
      "ICAPositions object"
    )
    testthat::expect_match(
      output,
      "Records:[[:space:]]*100"
    )
    testthat::expect_match(
      output,
      paste0(
        "Data sources:[[:space:]]*",
        NROW(header$dataSources)
      )
    )
    testthat::expect_match(
      output,
      "Position columns:[[:space:]]*10"
    )
    testthat::expect_true(
      grepl(
        header$genomeAssembly,
        output,
        fixed = TRUE
      )
    )
    testthat::expect_true(
      grepl(
        header$annotator,
        output,
        fixed = TRUE
      )
    )
  }
)


testthat::test_that(
  "position_data returns the ICA position DataFrame",
  {
    fixture <- .make_ica_test_fixture()

    observed <- position_data(fixture$object)

    testthat::expect_true(
      methods::is(observed, "DataFrame")
    )
    testthat::expect_identical(
      observed,
      fixture$positions
    )
    testthat::expect_identical(
      names(observed),
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
  }
)


testthat::test_that(
  "ica_header returns the parsed ICA header",
  {
    fixture <- .make_ica_test_fixture()

    testthat::expect_identical(
      ica_header(fixture$object),
      fixture$parsed$header
    )
  }
)


testthat::test_that(
  "ica_metadata returns the object metadata",
  {
    fixture <- .make_ica_test_fixture()

    testthat::expect_identical(
      ica_metadata(fixture$object),
      fixture$metadata
    )
  }
)


testthat::test_that(
  "position_data replacement updates position data",
  {
    object <- .make_ica_test_fixture()$object

    original_header <- ica_header(object)
    original_metadata <- ica_metadata(object)

    modified <- position_data(object)
    modified$analysis_flag <- rep(
      FALSE,
      nrow(modified)
    )

    testthat::expect_no_error({
      position_data(object) <- modified
    })

    testthat::expect_identical(
      position_data(object),
      modified
    )
    testthat::expect_true(
      "analysis_flag" %in% names(position_data(object))
    )
    testthat::expect_identical(
      ica_header(object),
      original_header
    )
    testthat::expect_identical(
      ica_metadata(object),
      original_metadata
    )
    testthat::expect_identical(
      methods::validObject(object, test = TRUE),
      TRUE
    )
  }
)


testthat::test_that(
  "position_data replacement invokes object validity",
  {
    fixture <- .make_ica_test_fixture()

    missing_column <- fixture$positions[
      ,
      names(fixture$positions) != "chromosome",
      drop = FALSE
    ]

    testthat::expect_error({
      object <- fixture$object
      position_data(object) <- missing_column
    })

    wrong_type <- fixture$positions
    wrong_type$position <- as.character(
      wrong_type$position
    )

    testthat::expect_error({
      object <- fixture$object
      position_data(object) <- wrong_type
    })

    invalid_position <- fixture$positions
    invalid_position$position[[1L]] <- 0L

    testthat::expect_error({
      object <- fixture$object
      position_data(object) <- invalid_position
    })
  }
)


testthat::test_that(
  "position_data replacement requires a DataFrame",
  {
    object <- .make_ica_test_fixture()$object

    testthat::expect_error({
      position_data(object) <- data.frame(
        chromosome = "chr1"
      )
    })
  }
)


testthat::test_that(
  "ICAPositions requires list header and metadata",
  {
    fixture <- .make_ica_test_fixture()

    testthat::expect_error(
      ICAPositions(
        positions = fixture$positions,
        header = "invalid header",
        metadata = list()
      )
    )

    testthat::expect_error(
      ICAPositions(
        positions = fixture$positions,
        header = fixture$parsed$header,
        metadata = "invalid metadata"
      )
    )
  }
)
