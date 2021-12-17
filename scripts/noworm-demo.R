## Define the required packages (these are all on CRAN)

pkg_req <- c("cimir", "conflicted", "dplyr", "DT", "googlesheets4", "leaflet", "lubridate",
             "plotly", "sf", "shiny", "shinybusy", "shinyhelper", "shinyjs", "tidyr", "units",
             "remotes")

## See what's missing:

(pkg_missing <- setdiff(pkg_req, rownames(installed.packages())))

## Install missing packages

install.packages(pkg_missing, dependencies = TRUE)

## install.packages(pkg_req, dependencies = TRUE)

## Install degday from GitHub

remotes::install_github("ucanr-igis/degday")

## Run the Navel Orangeworm Degree Day app

shiny::runUrl( "<the weblink>")
