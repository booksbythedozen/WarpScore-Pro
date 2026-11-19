## WarpScorePro_renv.locker.R

# **** For developer use ****

# create a renv.lock file that records the project’s package versions; users can
# then restore that environment with one command. renv is designed to create a
# project-specific library and restore packages from a lockfile.

library(renv)

# Step 1) initialize renv, 
renv::init()

# Step 2) knit the report successfully

# Step 3) Check that all packages needed by the Rmd and its scripts are installed and detected. 

# Step 4) Save the environment
renv::snapshot()

# Step 5) Include renv.lock in the files you share. renv scans project files for
# dependencies, but check the lockfile after a successful knit
