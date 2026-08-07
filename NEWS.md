# GenVarJSON 0.99.0

- Initial Bioconductor submission version.
- Refactored the aPRoVAR `read_json.R` workflow into documented functions.
- Preserved the original `header`, `positions`, `genes`, `tbl_inicial`, and
  `tbl_genes` data structures.
- Added sequential and multisession JSON parsing.
- Added optional parallel RDS compression with `pigz`.
- Added conversion of position records to `GRanges`.

