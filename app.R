# Launch the ShinyApp (Do not remove this comment)

# To deploy:
# No need to run the uncommented lines in this script. Rather, 
# 1) install package from github if necessary:
#   remotes::install_github("us-amlr/vvipr", force = TRUE)
# 2) run: rsconnect::deployApp()
#   Or use the blue button on top of this file. Keep all files selected.

pkgload::load_all(export_all = T, helpers = FALSE, attach_testthat = FALSE)

vvipr::vvipr()
