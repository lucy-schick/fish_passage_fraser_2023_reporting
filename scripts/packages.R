# install.packages('pak')

pkgs_cran <- c(
  'tidyverse',
  'knitr',
  'bookdown',
  'rmarkdown',
  'pagedown',
  'RPostgres',
  'sf',
  "ggdark",
  "kableExtra"
)

pkgs_gh <- c(
  "newgraphenvironment/fpr",
  "newgraphenvironment/ngr",
  "newgraphenvironment/staticimports",
  "lucy-schick/fishbc@updated_data",
  "poissonconsulting/readwritesqlite" #https://github.com/poissonconsulting/readwritesqlite/issues/47

)

pkgs_all <- c(pkgs_cran,
              pkgs_gh)


# install or upgrade all the packages with pak
# install or upgrade all the packages with pak
if(params$update_packages){
  lapply(pkgs_all, pak::pkg_install, ask = FALSE)
}

# load all the packages
pkgs_ld <- c(pkgs_cran,
             basename(pkgs_gh))

lapply(pkgs_ld,
       require,
       character.only = TRUE)
