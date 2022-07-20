# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
pkgload::load_all(export_all = T, helpers = FALSE, attach_testthat = FALSE)

# install package from github if necessary
# remotes::install_github("us-amlr/vvipr", ref = "shiny_func", force = TRUE)

vvipr::vvipr()
