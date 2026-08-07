# GenVarJSON

`GenVarJSON` is a Bioconductor-oriented R package for importing the
line-oriented JSON output produced by Nirvana or Illumina Connected
Annotations. It refactors the aPRoVAR `read_json.R` script into reusable
functions while preserving the original data structures.

The package name is provisional and should be confirmed before Bioconductor
submission.

## Preserved structures

| Original object | Package output | Structure |
|---|---|---|
| `resultados_ultra_v2` | `read_nirvana_sections()` | List with `header`, `positions`, and `genes` |
| `header` | `parse_nirvana_header()` | Parsed header object |
| `positions_list` | `parse_nirvana_records()` | Ordered list of position records |
| `genes_list` | `parse_nirvana_records()` | Ordered list of gene records |
| `tbl_inicial` | `nirvana_positions_tibble()` | Ten original position columns |
| `tbl_genes` | `nirvana_genes_tibble()` | Nine original gene columns |

No join, key normalization, or row reordering is performed. Position and gene
records remain in input order.

## One-call workflow

```r
library(GenVarJSON)

result <- read_nirvana_json(
    "annotations.json.gz",
    workers = 8L
)

header <- result$header
positions_list <- result$positions
genes_list <- result$genes
tbl_inicial <- result$tbl_inicial
tbl_genes <- result$tbl_genes
```

`read_nirvana_json()` returns exactly these five named elements:

```r
c("header", "positions", "genes", "tbl_inicial", "tbl_genes")
```

## Block-by-block workflow

This form follows the original script:

```r
resultados_ultra_v2 <- read_anno_json_ultra_fast_v2(
    "annotations.json.gz"
)

positions_list <- parse_nirvana_records(
    resultados_ultra_v2$positions,
    workers = 8L
)

genes_list <- parse_nirvana_records(
    resultados_ultra_v2$genes,
    workers = 8L
)

header <- parse_nirvana_header(resultados_ultra_v2$header)
tbl_inicial <- nirvana_positions_tibble(positions_list)
tbl_genes <- nirvana_genes_tibble(genes_list)
```

A complete compatibility script is installed at:

```r
system.file("scripts", "read_json.R", package = "GenVarJSON")
```

## Output schemas

`tbl_inicial` contains, in order:

```text
chromosome, position, refAllele, altAlleles, vcfInfo, quality,
filters, cytogeneticBand, samples, variants
```

`tbl_genes` contains, in order:

```text
name, ensemblGeneId, hgncId, ncbiGeneId, gnomAD, omim,
clingenGeneValidity, clingenDosageSensitivityMap, cosmic
```

Nested annotation objects, including both `gnomad` and `gnomad-exome`
variant databases, are retained inside the `variants` list-column.

## Bioconductor interoperability

The original tibble remains unchanged. A separate conversion function creates
a `GRanges` object when a Bioconductor genomic-range representation is useful:

```r
ranges <- nirvana_positions_granges(tbl_inicial)
```

## Compressed RDS files

```r
write_rds_parallel(
    tbl_inicial,
    "tbl_inicial.rds.gz",
    threads = 8L
)

tbl_inicial <- read_rds_parallel("tbl_inicial.rds.gz")
```

If `pigz` is unavailable, these functions automatically use standard R gzip
compression. Writing is performed through a temporary file to avoid leaving a
partially written result at the requested path.

## Development checks

```r
devtools::document()
devtools::test()
devtools::check()
BiocCheck::BiocCheck(".")
```

Before submission, replace the maintainer email placeholder and add the final
repository URLs to `DESCRIPTION`.
