# Script to deploy shiny app to shinyapps.io

devtools::install_github("us-amlr/vvipr", ref = "shiny_func")
rsconnect::deployApp("inst/shiny", appName = "vvipr")
