# TOHDigitalNeuropsychScoring_Alpha00.R
# Ottawa Bayesian Neuropsychology (OBN) protocol.
# Bryce P Mulligan, PhD, CPsych
# 4 July 2023
# ------------------------------------------------------------------------------
## @knitr CleanStart
# remove all elements for a clean start
# rm(list=ls(all=TRUE))
# cat("\014")
# ------------------------------------------------------------------------------
## @knitr InstallPackage
# source("./Scripts/Utility/InstallPackages.R")
# install.packages("ggthemes")
# ------------------------------------------------------------------------------
## @knitr WarpScore_Initiate
# Initiate scoring protocol
source("./Scripts/WarpScore_Initiate.R") # loads functions, data, etc
# ------------------------------------------------------------------------------
## @knitr WarpScore_Verify_Patient
# Verify patient/client descriptives
# print(PatientData)
cols <- c("Initials", "ID", "AxDate", "Age", "Sex", "Education", "Occupation")  # choose the subset you want
df_sub <- PatientData[, cols, drop = FALSE]

t_df_sub <- t(as.matrix(df_sub))
colnames(t_df_sub) <- ""

print(t_df_sub)
# ------------------------------------------------------------------------------
## @knitr WarpScore_Verify_Norms

# show the normative data
# print(dsNorms)

## Truncate comments before printing
shorten <- function(text, max_chars = 30) {
  if (is.na(text)) {
    return(NA_character_)
  }
  
  if (nchar(text) > max_chars) {
    paste0(substr(text, 1, max_chars), "...")
  } else {
    text
  }
}

comments_short <- vapply(dsNorms$Comment, shorten, character(1), max_chars = 43)

dsNorms_Trunc <- dsNorms
dsNorms_Trunc$Comment <- comments_short

# shorten this variable name for better legibility
names(dsNorms_Trunc)[names(dsNorms_Trunc) == "Education"] <- "Edu"
# print(dsNorms_Trunc)

# ensure comments left-side aligned
print(as.data.frame(dsNorms_Trunc), right = FALSE) 

# There should be no missing values in the normative set or else print a warning message
AllClearNA <- "Great news! There are no missing values in the normative dataset."
WarnMsgNA <- "There are missing values in the normative dataset. It is likely that there is no normative data available for the specified age/education.
Or, the normative dataset you just constructed may contain errors. 
Proceed with caution!"

if ( any(is.na(dsNorms$Nmean)) ) {
  warning(WarnMsgNA)   
} else {
  print(AllClearNA)
}

# Age and Education should have 0 variance or else print a warning message
AllClearAE <- "Great news! There is no variance in the age/education values included in the normative dataset."
WarnMsgAE <- "There is variance in the age/education values included in the normative dataset.
The normative dataset you just constructed may contain errors. 
Proceed with caution!"

if ( (var(dsNorms$Age) + var(dsNorms$Education)) > 0 ) {
  warning(WarnMsgAE)
} else {
  print(AllClearAE)
}

# Each test/score name should appear only once in the normative dataset
AllClearD <- "Great news! Each test/score name appears only once in the normative dataset."
WarnMsgD <- "***Some test/score names appear more than once in the normative dataset***
Proceeding with scoring will result in errors. Halt! Don't panic! Seek assistance!"

if ( any(duplicated(dsNorms$Score)) ) {
  stop(WarnMsgD)  # or use warning(WarnMsgNA) if you prefer non-fatal
} else {
  print(AllClearD)
}

# ------------------------------------------------------------------------------
## @knitr WarpScore_Score
### Identify individual test scores to compute/compile Bayesian tests of discrepancy (BTD)
BTDfun("TrailsA", "greater") # higher score is worse
BTDfun("TrailsB", "greater") # higher score is worse

BTDfun("AnimalFluency", "less") # lower score is worse
BTDfun("PhonemicFluency", "less") # lower score is worse

BTDfun("VisName_Correct", "less") # lower score is worse
BTDfun("AudName_Correct", "less") # lower score is worse

BTDfun("CCC_9s", "less") # lower score is worse
BTDfun("CCC_18s", "less") # lower score is worse
BTDfun("CCC_36s", "less") # lower score is worse

BTDfun("SDMT_writ", "less") # lower score is worse
BTDfun("SDMT_oral", "less") # lower score is worse

BTDfun("PCL_5", "greater") # higher score is worse
BTDfun("SHAI_14", "greater") # higher score is worse
BTDfun("PSS_4", "greater") # higher score is worse
BTDfun("FSS", "greater") # higher score is worse
BTDfun("EROS", "less") # lower score is worse
BTDfun("ECog_Self", "greater") # higher score is worse
BTDfun("ECog_Inform", "greater") # higher score is worse



# ------------------------------------------------------------------------------
## @knitr WarpScore_OutputGraph
# Generate analysis outputs
source("./Scripts/WarpScorePro_Output.R") # generates/saves tables, graphs, etc
# ------------------------------------------------------------------------------
## @knitr WarpScore_OutputTable
# print(BTDdf)
rownames(BTDdf) <- NULL  # drops the row names
# print(BTDdf)
# Round numeric columns to three decimal places while keeping Input as text
BTDdf_rounded <- BTDdf
numeric_cols <- vapply(BTDdf_rounded, is.numeric, logical(1))

BTDdf_rounded[numeric_cols] <- lapply(
  BTDdf_rounded[numeric_cols],
  round,
  digits = 2
)

print(BTDdf_rounded)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------