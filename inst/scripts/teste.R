library(AnnoJSON)

output_directory <- "C:/Users/r_packages/Desktop/R_analyses/AnnoJSON/inst/extdata/"
output_prefix <- "TOYDATA"
workers <- 2

json_file <- system.file("extdata", "subset_100_af.json.gz", package = "AnnoJSON")
plain_nirvana_list <- read_nirvana_sections(json_file)

raw_sections_file <- file.path(output_directory, paste0(output_prefix, "_sections.rds.gz"))

write_rds_parallel(
  plain_nirvana_list,
  raw_sections_file,
  threads = workers
)

raw_sections_file_read <- read_rds_parallel(raw_sections_file)

positions_list <- parse_nirvana_records(
  raw_sections_file_read$positions,
  workers = workers,
  progress = interactive()
)

genes_list <- parse_nirvana_records(
  raw_sections_file_read$genes,
  workers = workers,
  progress = interactive()
)

header <- parse_nirvana_header(raw_sections_file_read$header)

all_names <- lapply(positions_list, names)
all_names_genes <- lapply(genes_list, names)
unique_names <- nirvana_unique_fields(positions_list)
unique_names_genes <- nirvana_unique_fields(genes_list)

print("Unique fields found in the position list:")
print(unique_names)
print("Unique fields found in the gene list:")
print(unique_names_genes)

tbl_positions <- nirvana_positions_tibble(positions_list)
tbl_genes <- nirvana_genes_tibble(genes_list)

parsed_results <- list(
  header = header,
  positions = positions_list,
  genes = genes_list,
  tbl_positions = tbl_positions,
  tbl_genes = tbl_genes
)

output_files <- write_nirvana_results(
  parsed_results,
  output_dir = output_directory,
  prefix = output_prefix,
  threads = workers
)

print(output_files)


ranges <- nirvana_positions_granges(parsed_results$tbl_positions)
