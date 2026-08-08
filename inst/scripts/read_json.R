##### read_json.R #####
# Alysson Henrique Urbanski & Marco Antonio Campanário @ Fiocruz Paraná,
# 2026 Jul.
# Function-based parsing and processing of ICA JSON annotations.
#
# This script keeps the block order and central object names used in the
# original aPRoVAR workflow. Run it after installing AnnoJSON:
#
# Rscript read_json.R input.json.gz output_directory output_prefix [workers]

library(AnnoJSON)

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) < 3L) {
    stop(
        paste(
            "Usage: Rscript read_json.R",
            "input.json.gz output_directory output_prefix [workers]"
        ),
        call. = FALSE
    )
}

json_file <- arguments[[1L]]
output_directory <- arguments[[2L]]
output_prefix <- arguments[[3L]]
workers <- if (length(arguments) >= 4L) {
    as.integer(arguments[[4L]])
} else {
    1L
}

# -------------------------------------------------------------------------
# 1. Read the header, position records, and gene records as JSON strings.
# -------------------------------------------------------------------------

json_file <- system.file("extdata", "ica-example.json.gz", package = "AnnoJSON")
output_directory <- "C:/Users/Marco Campanário/Desktop/AnnoJSON_07082026/AnnoJSON/AnnoJSON/inst/extdata/"
output_prefix <- "TOYDATA-P"

execution_time <- system.time({
    parsed_json <- read_ica_sections(json_file)
}, gcFirst = TRUE)

print(execution_time)

raw_sections_file <- file.path(
    output_directory,
    paste0(output_prefix, "_sections.rds.gz")
)

write_rds_parallel(
    parsed_json,
    raw_sections_file,
    threads = workers
)

# -------------------------------------------------------------------------
# 2. Read the saved section object.
# -------------------------------------------------------------------------

parsed_json_read <- read_rds_parallel(raw_sections_file)

# -------------------------------------------------------------------------
# 3. Parse position records.
# -------------------------------------------------------------------------

positions_list <- parse_ica_records(
    parsed_json_read$positions,
    workers = workers,
    progress = interactive()
)

# -------------------------------------------------------------------------
# 4. Parse gene records.
# -------------------------------------------------------------------------

genes_list <- parse_ica_records(
    parsed_json_read$genes,
    workers = workers,
    progress = interactive()
)

# -------------------------------------------------------------------------
# 5. Parse the header.
# -------------------------------------------------------------------------

header <- parse_ica_header(parsed_json_read$header)

# -------------------------------------------------------------------------
# 6. Inspect the top-level fields.
# -------------------------------------------------------------------------

all_names <- lapply(positions_list, names)
all_names_genes <- lapply(genes_list, names)
unique_names <- ica_unique_fields(positions_list)
unique_names_genes <- ica_unique_fields(genes_list)

print("Unique fields found in the position list:")
print(unique_names)
print("Unique fields found in the gene list:")
print(unique_names_genes)

# -------------------------------------------------------------------------
# 7. Create the same two tibbles as the original workflow.
# -------------------------------------------------------------------------

tbl_positions <- ica_positions_tibble(positions_list)
tbl_genes <- ica_genes_tibble(genes_list)

# -------------------------------------------------------------------------
# 8. Save the parsed objects.
# -------------------------------------------------------------------------

parsed_results <- list(
    header = header,
    positions = positions_list,
    genes = genes_list,
    tbl_positions = tbl_positions,
    tbl_genes = tbl_genes
)

output_files <- write_ica_results(
    parsed_results,
    output_dir = output_directory,
    prefix = output_prefix,
    threads = workers
)

print(output_files)

# -------------------------------------------------------------------------
# X. Now that you get the logic, just run the master function!
# -------------------------------------------------------------------------

example_file <- system.file("extdata",
                            "subset_100_af.json.gz",
                            package = "AnnoJSON")

parsed_results_master <- read_ica_json(
      example_file,
      workers = 1L,
      progress = T,
      verbose = T
  )

output_files_master <- write_ica_results(
  parsed_results,
  output_dir = output_directory,
  prefix = "TOYDATA2",
  threads = workers,
  overwrite = T
)

ICAPositions_object <- ICAPositions(
  positions = parsed_results_master$tbl_positions,
  header = parsed_results_master$header,
  metadata = list(
    source = "ICA JSON",
    package = "AnnoJSON"
  )
)

show(ICAPositions_object)
positions <- position_data(ICAPositions_object)
header <- ica_header(ICAPositions_object)
metadata <- ica_metadata(ICAPositions_object)

# Replacement method
positions$source_row <- seq_len(nrow(positions))
position_data(ICAPositions_object) <- positions
