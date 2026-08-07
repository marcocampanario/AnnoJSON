library(devtools)
load_all('.')

## Checking the content of our package
load_all('.')
document()
run_examples()
test()

check('.')
# ... We had to manually add few packages in Imports section of DESCRIPTION to solve the ERROR "checking package dependencies"

## How to create a `se` object (SummarizedExperiment)


## Example variables

bw_file <- system.file("extdata", "Scc1-vs-input_subset.bw", package = "examplebioc")
peak_file <- system.file("extdata", "Scc1-peaks.narrowPeak", package = "examplebioc")
width <- 2000


## How I can interact with `se` object
library(SummarizedExperiment)
colData(se)
rowData(se)
assays(se)


## DEFINE the AggregatedCoverage CLASS

setClass(
    "AggregatedCoverage",
    slots = list(
        features = "GRanges",
        width = "numeric"
    ),
    contains = c("SummarizedExperiment")
)


is(ac, 'AggregatedCoverage')
is(ac, 'SummarizedExperiment')
is(ac, 'Annotated')

colData(ac)


setMethod(
    "colData",
    signature = "AggregatedCoverage",
    function(x) {"Please leave me alone"}
)


## Create a constructor for AggregatedCoverage objects

AggregatedCoverage <- function(bw_file, peak_file, width) {

    ac <- new(
        "AggregatedCoverage", {

            cData <- data.frame(
                file = c(bw_file),
                assay = c("Scc1_ChipSeq"),
                date = c('2026-07-30'),
                creator = c('JacquesSerizay'),
                treatment = c('mock'),
                background = c('wild-type')
            )
            rData <- data.frame(
                position = seq(-width/2, {+width/2}-1)
            )


            df <- importFiles(bw_file, peak_file, width = width) |>
                filterGRanges() |>
                computeCoverage()

            ass <- list(
                mean = matrix(df$mean, nrow = width),
                lowCI = matrix(df$ci_low, width),
                upCI = matrix(df$ci_high, width)
            )

            SummarizedExperiment::SummarizedExperiment(
                colData = cData,
                rowData = rData,
                assays = ass
            )
        }
    )
    return(ac)
}

ac <- AggregatedCoverage(bw_file, peak_file, width)
dim(ac)
ac2 <- AggregatedCoverage(bw_file, peak_file, width = 10000)

AggregatedCoverage(bw_file, peak_file, width = 10000) |> plot()

## Populate R files with our super-nice implementaion of AC class
use_r("AllClasses")
use_r("AllMethods")
use_r("AggregatedCoverage")


