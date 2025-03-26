# Package names
packages <- c("reticulate","tidyverse","DT","data.table","kableExtra","knitr","scales","stats","caret","pROC")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))