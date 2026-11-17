## NormBroadcaster.R

# Define custom functions
source("./Scripts/make_normative_csv.R")

# The batch function handles reading, grid generation, verification, and export.
output_paths <- process_normative_batch()
print(output_paths)


# Can be run from a terminal with Rscript, without opening RStudio. First
# navigate to the project folder so the script’s relative paths resolve
# correctly, then run:
# Rscript ./Scripts/NormBroadcaster.R

# On Windows PowerShell, the command is:
# Rscript .\Scripts\NormBroadcaster.R

# R must be installed, and Rscript must be available on your system’s PATH.

# To use as Shiny app:
# Install Shiny once if needed with install.packages("shiny"). Then, from the
# project’s top-level folder, run 
# shiny::runApp(".")
# in R, or open terminal, navigate to project folder, and launch with: 
# Rscript -e "shiny::runApp('.')"