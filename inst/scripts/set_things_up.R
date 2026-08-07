install.packages("remotes")
remotes::install_cran(
  c(
    "available",
    "devtools",
    "rcmdcheck",
    "knitr",
    "pkgdown",
    "RefManageR",
    "rmarkdown",
    "rstudioapi",
    "sessioninfo",
    "styler",
    "usethis",
    "gert"
  )
)
remotes::install_cran("BiocManager")
BiocManager::install(c("BiocStyle", "BiocCheck", "biocthis"), force = T)
install.packages("gh")

##

devtools::load_all('.')
devtools::document()
devtools::test()
devtools::install(
  ".",
  upgrade = FALSE
)
library(AnnoJSON)
