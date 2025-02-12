# Package names
packages <- c("tidyverse","tidyquant","tidymodels","xts","glue","scales","downloadthis","ggthemes","highcharter")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))