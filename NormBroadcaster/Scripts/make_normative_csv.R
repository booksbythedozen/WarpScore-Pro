## make_normative_csv.R

# -----------------------------
# Import CSV or Excel input
# -----------------------------

read_normative_input <- function(path = NULL,
                                 input_dir = "./InputData") {
  if (is.null(path)) {
    csv_files <- list.files(
      path = input_dir,
      pattern = "\\.csv$",
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    if (length(csv_files) == 0) {
      stop("No CSV file found in: ", input_dir, call. = FALSE)
    }
    
    if (length(csv_files) > 1) {
      stop(
        "Expected exactly one CSV file in ", input_dir,
        ", but found ", length(csv_files), ".",
        call. = FALSE
      )
    }
    
    path <- csv_files[[1]]
  }
  
  if (!file.exists(path)) {
    stop("Input file does not exist: ", path, call. = FALSE)
  }
  
  extension <- tolower(tools::file_ext(path))
  
  switch(
    extension,
    csv  = read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    xlsx = readxl::read_excel(path),
    xls  = readxl::read_excel(path),
    stop("Unsupported input file type: .", extension, call. = FALSE)
  )
}


# -----------------------------
# Validate and standardize input
# -----------------------------

validate_normative_input <- function(dat) {
  required <- c(
    "Score",
    "age_min", "age_max",
    "education_min", "education_max",
    "Stratification",
    "Nmean", "Nsd", "Nn",
    "Source", "Comment"
  )
  
  missing_columns <- setdiff(required, names(dat))
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  
  # Reject empty input file
  if (nrow(dat) == 0) {
    stop("Input contains no data rows.")
  }
  
  # Convert text fields consistently.
  for (column in c("Score", "Source", "Comment")) {
    dat[[column]] <- as.character(dat[[column]])
  }
  
  # Convert numeric fields, but report nonblank values that cannot be parsed.
  numeric_columns <- c(
    "age_min", "age_max",
    "education_min", "education_max",
    "Nmean", "Nsd", "Nn"
  )
  
  for (column in numeric_columns) {
    original <- dat[[column]]
    converted <- suppressWarnings(as.numeric(as.character(original)))
    
    bad_value <- !is.na(original) &
      trimws(as.character(original)) != "" &
      is.na(converted)
    
    if (any(bad_value)) {
      stop("Column '", column, "' contains nonnumeric values.")
    }
    
    dat[[column]] <- converted
  }
  
  # Treat blank text cells as missing.
  for (column in c("Score", "Source", "Comment")) {
    dat[[column]][
      !is.na(dat[[column]]) & trimws(dat[[column]]) == ""
    ] <- NA_character_
  }
  
  if (any(is.na(dat$Score))) {
    stop("Score cannot be blank.")
  }
  
  range_columns <- c(
    "age_min", "age_max",
    "education_min", "education_max"
  )
  
  if (any(!complete.cases(dat[range_columns]))) {
    stop("Every input row must specify all four range endpoints.")
  }
  
  if (any(!is.finite(as.matrix(dat[range_columns])))) {
    stop("Age and education range endpoints must be finite numbers.")
  }
  
  if (any(dat$age_min > dat$age_max)) {
    stop("Some rows have age_min greater than age_max.")
  }
  
  if (any(dat$education_min > dat$education_max)) {
    stop("Some rows have education_min greater than education_max.")
  }
  
  if (any(
    dat$age_min < 16 |
    dat$age_max > 100
  )) {
    stop("Age ranges must be within 16 to 100.")
  }
  
  if (any(
    dat$education_min < 0 |
    dat$education_max > 22
  )) {
    stop("Education ranges must be within 0 to 22.")
  }
  
  if (any(
    dat$age_min != floor(dat$age_min) |
    dat$age_max != floor(dat$age_max) |
    dat$education_min != floor(dat$education_min) |
    dat$education_max != floor(dat$education_max)
  )) {
    stop("Age and education range endpoints must be whole numbers.")
  }
  
  # A row must have either all three statistics or none of them.
  statistic_columns <- c("Nmean", "Nsd", "Nn")
  statistic_count <- rowSums(!is.na(dat[statistic_columns]))
  
  if (any(statistic_count != 0 & statistic_count != 3)) {
    stop(
      "Each row must contain Nmean, Nsd, and Nn together, ",
      "or leave all three blank."
    )
  }
  
  if (any(!is.na(dat$Nmean) & !is.finite(dat$Nmean))) {
    stop("Nmean must be finite.")
  }
  
  if (any(!is.na(dat$Nsd) & !is.finite(dat$Nsd))) {
    stop("Nsd must be finite.")
  }
  
  if (any(!is.na(dat$Nsd) & dat$Nsd < 0)) {
    stop("Nsd cannot be negative.")
  }
  
  if (any(
    !is.na(dat$Nn) &
    (dat$Nn <= 0 | dat$Nn != floor(dat$Nn))
  )) {
    stop("Nn must be a positive whole number.")
  }
  
  rows_with_statistics <- statistic_count == 3
  
  dat$Stratification <- tolower(trimws(as.character(dat$Stratification)))
  dat$Stratification[dat$Stratification == ""] <- NA_character_
  
  allowed_stratification <- c(
    "overall", "age", "education", "age_education"
  )
  
  bad_stratification <- rows_with_statistics &
    (is.na(dat$Stratification) |
       !dat$Stratification %in% allowed_stratification)
  
  if (any(bad_stratification)) {
    stop(
      "Rows with statistics need Stratification set to one of: ",
      paste(allowed_stratification, collapse = ", "),
      "."
    )
  }
  
  
  if (any(rows_with_statistics & is.na(dat$Source))) {
    stop("Every row containing statistics must have a Source.")
  }
  
  dat
}


# -----------------------------
# Check whether one input row
# is more specific than another
# -----------------------------

row_is_strictly_more_specific <- function(a, b) {
  # Row a is more specific than row b if a's age interval and
  # education interval are both contained within b's intervals,
  # and at least one interval is strictly narrower.
  contained <-
    a$age_min >= b$age_min &&
    a$age_max <= b$age_max &&
    a$education_min >= b$education_min &&
    a$education_max <= b$education_max
  
  strictly_narrower <-
    a$age_min > b$age_min ||
    a$age_max < b$age_max ||
    a$education_min > b$education_min ||
    a$education_max < b$education_max
  
  contained && strictly_narrower
}


# -----------------------------
# Choose the best matching row
# -----------------------------

choose_matching_row <- function(age, education, source_rows, score) {
  matches <- which(
    source_rows$age_min <= age &
      source_rows$age_max >= age &
      source_rows$education_min <= education &
      source_rows$education_max >= education
  )
  
  if (length(matches) == 0) {
    return(NA_integer_)
  }
  
  # Keep rows that are not strictly less specific than another match.
  best <- matches[
    !vapply(
      matches,
      function(i) {
        any(vapply(
          setdiff(matches, i),
          function(j) {
            row_is_strictly_more_specific(
              source_rows[j, , drop = FALSE],
              source_rows[i, , drop = FALSE]
            )
          },
          logical(1)
        ))
      },
      logical(1)
    )
  ]
  
  if (length(best) != 1) {
    input_rows <- paste(best, collapse = ", ")
    
    stop(
      "Ambiguous input for Score='", score,
      "', Age=", age,
      ", Education=", education,
      ". Competing input rows: ", input_rows,
      ". Make their ranges more specific or resolve the overlap."
    )
  }
  
  best
}


# -----------------------------
# Comment helper
# -----------------------------

# -----------------------------
# Comment helper
# -----------------------------

make_scope_comment <- function(row) {
  paste0(
    "Age: ", row$age_min, "-", row$age_max,
    "; Education: ", row$education_min, "-", row$education_max
  )
}

# -----------------------------
# Expand onto the requested grid
# -----------------------------

make_normative_grid <- function(
    dat,
    ages = 16:100,
    education = 0:22,
    missing_comment =
      "No normative data for this age/education") {
  
  dat <- validate_normative_input(dat)
  
  if (
    length(ages) == 0 ||
    anyNA(ages) ||
    any(ages != floor(ages)) ||
    any(ages < 16 | ages > 100) ||
    anyDuplicated(ages)
  ) {
    stop("ages must be unique whole numbers from 16 to 100.")
  }
  
  if (
    length(education) == 0 ||
    anyNA(education) ||
    any(education != floor(education)) ||
    any(education < 0 | education > 22) ||
    anyDuplicated(education)
  ) {
    stop("education must be unique whole numbers from 0 to 22.")
  }
  
  scores <- unique(dat$Score)
  results <- vector("list", length(scores))
  
  for (score_index in seq_along(scores)) {
    score <- scores[score_index]
    source_rows <- dat[dat$Score == score, , drop = FALSE]
    
    grid <- expand.grid(
      Age = ages,
      Education = education,
      KEEP.OUT.ATTRS = FALSE
    )
    
    result <- data.frame(
      Score = score,
      Age = grid$Age,
      Education = grid$Education,
      Nmean = NA_real_,
      Nsd = NA_real_,
      Nn = NA_real_,
      Source = NA_character_,
      Comment = NA_character_,
      stringsAsFactors = FALSE
    )
    
    for (i in seq_len(nrow(result))) {
      selected_index <- choose_matching_row(
        age = result$Age[i],
        education = result$Education[i],
        source_rows = source_rows,
        score = score
      )
      
      if (is.na(selected_index)) {
        result$Comment[i] <- missing_comment
        next
      }
      
      selected <- source_rows[selected_index, , drop = FALSE]
      
      result$Nmean[i] <- selected$Nmean
      result$Nsd[i] <- selected$Nsd
      result$Nn[i] <- selected$Nn
      result$Source[i] <- selected$Source
      
      # An explicit row with no statistics marks the cell as unavailable.
      if (is.na(selected$Nmean)) {
        comment <- selected$Comment
        
        if (is.na(comment) || trimws(comment) == "") {
          comment <- missing_comment
        }
        
        result$Comment[i] <- comment
        next
      }
      
      # Replace the old scope-comment block with this:
      scope_comment <- make_scope_comment(selected)
      extra_comment <- selected$Comment
      
      if (is.na(extra_comment) || trimws(extra_comment) == "") {
        result$Comment[i] <- scope_comment
      } else {
        result$Comment[i] <- paste(
          scope_comment,
          extra_comment,
          sep = " | "
        )
      }
    } # closes: for (i in seq_len(nrow(result)))
    
    results[[score_index]] <- result
  } # closes: for (score_index in seq_along(scores))
  
  output <- do.call(rbind, results)
  rownames(output) <- NULL
  
  # Keep the requested output column order.
  output <- output[
    c(
      "Score", "Age", "Education",
      "Nmean", "Nsd", "Nn",
      "Source", "Comment"
    )
  ]
  
  output
}


# -----------------------------
# Export output
# -----------------------------

write_normative_csv <- function(output, output_dir = "./OutputData") {
  required_output <- c(
    "Score", "Age", "Education",
    "Nmean", "Nsd", "Nn",
    "Source", "Comment"
  )
  
  if (!all(required_output %in% names(output))) {
    stop("Output does not have the required normative CSV columns.")
  }
  
  scores <- unique(as.character(output$Score))
  
  if (anyNA(scores) || any(trimws(scores) == "")) {
    stop("Score values cannot be blank.")
  }
  
  if (length(scores) != 1) {
    stop("Automatic filename requires exactly one unique Score value.")
  }
  
  filename_score <- gsub("[^A-Za-z0-9._-]+", "_", scores)
  filename <- paste0("NormGrid_", filename_score, ".csv")
  path <- file.path(output_dir, filename)
  
  # Create the output folder if it doesn't already exist.
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!dir.exists(output_dir)) {
    stop("Could not create output directory: ", output_dir)
  }
  
  write.csv(
    output[required_output],
    file = path,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  
  invisible(path)
}


# -----------------------------
# Verify output
# -----------------------------

# With the default arguments, it expects ages 16–100 and education values 0–22. It checks that:
## The number of rows equals 1,955 × the number of unique scores.
## Every Age and Education value is in the requested ranges.
## There are no duplicate Score–Age–Education combinations.
## No non-missing Nsd is negative.
## Every non-missing Nn is a positive whole number.

verify_normative_grid <- function(output, ages = 16:100, education = 0:22) {
  expected_rows <- length(unique(output$Score)) *
    length(ages) *
    length(education)
  
  stopifnot(nrow(output) == expected_rows)
  stopifnot(all(output$Age %in% ages))
  stopifnot(all(output$Education %in% education))
  stopifnot(!anyDuplicated(output[c("Score", "Age", "Education")]))
  stopifnot(all(is.na(output$Nsd) | output$Nsd >= 0))
  stopifnot(all(
    is.na(output$Nn) |
      (output$Nn > 0 & output$Nn == floor(output$Nn))
  ))
  
  message(
    paste(
      "FYI: This step verifies the output's structure and some basic value constraints,",
      "not the substantive accuracy of the norms.",
      "In other words, this function does not check whether the",
      "means, standard deviations, or sample sizes match the source",
      "publication, or whether the source data were entered correctly."
    )
  )
  
  invisible(TRUE)
}


# -----------------------------
# Batch processing
# -----------------------------
# finds every CSV, processes each file independently, verifies its grid, and
# writes one CSV per Score. It can also split a file containing multiple scores
# into separate output CSVs.

# The batch function handles reading, grid generation, verification, and export. 

process_normative_batch <- function(
    input_dir = "./InputData",
    output_dir = "./OutputData") {
  
  input_files <- list.files(
    path = input_dir,
    pattern = "\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(input_files) == 0) {
    stop("No CSV files found in: ", input_dir, call. = FALSE)
  }
  
  outputs <- list()
  output_filenames <- character()
  
  # Process and validate everything before writing any files.
  for (input_file in input_files) {
    dat <- read_normative_input(path = input_file)
    grid <- make_normative_grid(dat)
    verify_normative_grid(grid)
    
    for (score in unique(as.character(grid$Score))) {
      score_grid <- grid[
        as.character(grid$Score) == score,
        ,
        drop = FALSE
      ]
      
      safe_score <- gsub("[^A-Za-z0-9._-]+", "_", score)
      filename <- paste0("NormGrid_", safe_score, ".csv")
      
      if (filename %in% output_filenames) {
        stop(
          "More than one input would create the output file '",
          filename,
          "'. Check for repeated Score values or filename collisions.",
          call. = FALSE
        )
      }
      
      output_filenames <- c(output_filenames, filename)
      outputs[[length(outputs) + 1]] <- score_grid
    }
  }
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!dir.exists(output_dir)) {
    stop("Could not create output directory: ", output_dir, call. = FALSE)
  }
  
  output_paths <- vapply(
    outputs,
    write_normative_csv,
    character(1),
    output_dir = output_dir
  )
  
  names(output_paths) <- output_filenames
  output_paths
}
