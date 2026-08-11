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

##

usethis::git_sitrep()
gert::git_config_global_set("user.name", "Marco")
gert ::git_config_global_set("user.email", "macscampanario@gmail.com")
usethis ::git_vaccinate()
usethis ::use_git()
usethis ::use_git_ignore("*.Rproj")
# Don’t forget to make an initial commit!

gh::gh_whoami()
usethis::gh_token_help()
#Create a GH PAT
usethis ::create_github_token()
gitcreds ::gitcreds_set()
#Paste your GitHub token, not your password!
#Add an SSH key to GitHub
usethis ::use_github()
credentials::ssh_setup_github()
