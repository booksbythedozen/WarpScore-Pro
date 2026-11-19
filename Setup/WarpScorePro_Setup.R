## WarpScorePro_Setup.R

# A script to assist users with installation of all R packages required
# to use the downloaded version of WarpScorePro.

# After running this script once per machine, you can use and modify the
# WarpScorePro scripts to generate your own dymamic html reports.

if (!file.exists("renv.lock")) {
  stop("Can't find renv.lock. Open the project folder in RStudio and try again.")
}

if (!file.exists("renv/activate.R")) {
  stop("Can't find renv/activate.R. Make sure the project's renv files were included.")
}

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

renv::restore(prompt = FALSE)

message("Package restore complete. Restart RStudio, then knit the report.")

