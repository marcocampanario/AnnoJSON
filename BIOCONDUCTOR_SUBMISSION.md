# Bioconductor submission checklist

## Required metadata

- [ ] Confirm the final package name and verify that it has never been used
  on Bioconductor or CRAN.
- [ ] Replace `REPLACE_WITH_ACTIVE_EMAIL@example.org` in `DESCRIPTION`.
- [ ] Add the final repository URL and issue-tracker URL to `DESCRIPTION`.
- [ ] Confirm author roles and add ORCID identifiers when available.

## Package checks

- [ ] Run `devtools::document()` with the target R and Bioconductor versions.
- [ ] Run `R CMD build GenVarJSON`.
- [ ] Run `R CMD check GenVarJSON_0.99.0.tar.gz`.
- [ ] Run `BiocCheck::BiocCheck("GenVarJSON_0.99.0.tar.gz")`.
- [ ] Run checks on Linux, macOS, and Windows when possible.
- [ ] Review peak memory use with a representative input.

## Scientific validation

- [ ] Compare the five central outputs with the original `read_json.R`
  workflow on the complete aPRoVAR JSON file.
- [ ] Confirm exact row counts and row order for `positions` and
  `tbl_inicial`.
- [ ] Confirm exact row counts and row order for `genes` and `tbl_genes`.
- [ ] Confirm that nested `gnomad` and `gnomad-exome` annotations remain
  available in the `variants` list-column.
- [ ] Test one file without a top-level `genes` section.

## Documentation

- [ ] Add the final repository URL.
- [ ] Add a package citation.
- [ ] Add the complete aPRoVAR use case to the vignette if it can be
  distributed.
- [ ] Confirm that all examples finish within Bioconductor time limits.
