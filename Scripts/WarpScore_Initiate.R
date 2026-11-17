# WarpScore_Initiate.R
# WarpScore neuropsychology data processing protocol.
# Bryce P Mulligan, PhD, CPsych
# 19 January 2024
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# This script performs all of the initial operations required for running the
# digital scoring protocol for a single patient. In particular, this script:
## clears the R environment; 
## loads required packages and functions; 
## creates an "empty container" dataframe to compile the results of scoring; and
## loads patient and normative data.
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# remove all elements for a clean start (¡caution!)

rm(list=ls(all=TRUE))  # clears the R environment
# dev.off()              # clears all graphics plots
# cat("\014")            # clears R terminal
# ------------------------------------------------------------------------------
# Upgrade R (run periodically)

# library (installr) # for updating R
# updateR()

# ------------------------------------------------------------------------------
# Install packages (only needs to be run once per machine or to update)

# install.packages("singcar") # Bayesian single-case neuropsychology

# Rittmo, J. Ö., & McIntosh, R. D. (2021). 
# singcar: Comparing single cases to small samples in R. 
# Journal of Open Source Software, 6(68), 3887.
# ------------------------------------------------------------------------------
# Load packages from library

library(readr)      # to read in data files
library(dplyr)      # a grammar of data manipulation
library(singcar)    # single-case neuropsychology functions
library(lattice)    # high-level graphics functions
library(ggplot2)    # powerful, flexible graphics functions
library(tibble)     # provides an alternative to dataframes, the 'tbl_df' class (the 'tibble')
# ------------------------------------------------------------------------------
# Define functions
source("./Scripts/BTDfun.R")
# source("./Scripts/RawMatch_fun.R") # Coming in 2029!!
# ------------------------------------------------------------------------------
# Create empty dataframe container for results
BTDdf <- data.frame("TestScore" = character(),     
                    "df" = numeric(),
                    "p.value" = numeric(),
                    "L.PI" = numeric(),
                    "H.PI" = numeric(),
                    "Input" = character())
# ------------------------------------------------------------------------------
### Identify patient data

# # For importing already-transformed profile sheet (variables in colunms, first row=variable names, second row=variable values)
# file.list <- list.files("./PatientData", pattern="*.csv", full.names=TRUE) # creates a list of file names
# suppressMessages(                         # suppresses messages from read_csv output
#   df.list <- lapply(file.list, read_csv)  # reads all files in list
#   )
# PatientData <- as.data.frame(df.list)     # converts to dataframe

# For importing untransformed profile sheet
file.list <- list.files(
  "./PatientData",
  pattern = "\\.csv$",
  full.names = TRUE
)

PatientData <- suppressMessages(
  lapply(file.list, function(file) {
    
    dat <- read_csv(
      file,
      col_names = FALSE,
      show_col_types = FALSE
    )
    
    # Keep only columns 2 and 3
    dat <- dat[, c(2, 3)]
    
    # Rename them for clarity
    names(dat) <- c("Variable", "Value")
    
    # Remove rows without a variable name
    dat <- dat %>%
      filter(!is.na(Variable), Variable != "")
    
    # Convert the variable/value pairs into one row
    as_tibble_row(
      setNames(as.list(dat$Value), dat$Variable)
    )
  }) %>%
    bind_rows()
)

# Convert numeric-looking values to numeric values
PatientData <- PatientData %>%
  mutate(
    across(
      everything(),
      ~ type.convert(as.character(.x), as.is = TRUE)
    )
  )


# ------------------------------------------------------------------------------
### Build normative datasets

## 1: Dataframe for use in BTDfun
#For raw scores with corresponding means/standard deviations for estimating base rate

# Patient values to be used for selecting rows from the normative database
PatientAge <- PatientData$Age
PatientEducation <- PatientData$Education

# Creates a list of *.csv file names in normative database
file.list <- list.files("./NormGrids",pattern="*.csv", full.names=TRUE) 
suppressMessages(                         # suppresses messages from read_csv output
  df.list <- lapply(file.list, read_csv)  # reads all files in list
  )
# Select the rows matched for Age and Education from each listed dataframe
row.list <- lapply(df.list, function(x) x[x$Age == PatientAge &
                                          x$Education == PatientEducation, ])
# Bind rows in list into single dataframe
dsNorms <- data.frame(do.call("rbind", row.list))
# lapply(row.list, names) # run to troubleshoot if there is a "names do not match" error
# print(dsNorms)

# ------------------------------------------------------------------------------
# Diagnostics 

# # Age and Education should have 0 variance or else print a warning message
# AllClearA <- "|~|~¡Great news: there is no variance in the age/education values included in the normative dataset. Proceed with glee!~|~|"
# WarnMsgA <- "There is variance in the age/education values included in the normative dataset.
# The normative dataset you just constructed may contain errors. Proceed with caution!"
# ifelse( (var(dsNorms$Age)+var(dsNorms$Education))>0, warning(WarnMsgA), print(AllClearA))
# 
# # Each test/score name should appear only once in the normative dataset
# AllClearB <- "|~|~¡Great news: each test/score name appears only once in the normative dataset. Proceed with glee!~|~|"
# WarnMsgB <- "***Some test/score names appear more than once in the normative dataset***
# Proceeding with scoring will result in errors. Halt! Don't panic! Seek assistance!"
# ifelse( (sum(duplicated(dsNorms$Score)))>0, stop(WarnMsgB), print(AllClearB))


# sum(duplicated(dsNorms$Score))

## 2: RawMatch_fun
#For raw scores that are matched to corresponding standardized scores
############ COMING SOON(ish)!!

