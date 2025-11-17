#' Read All Supported Data Files in a Directory with Progress
#'
#' This function reads all supported tabular data files from a specified directory
#' into R and returns them as a named list of data frames. It supports multiple
#' file formats including CSV, TSV, TXT, Excel (XLS/XLSX), JSON, RDS, SPSS (SAV),
#' SAS (SAS7BDAT), Stata (DTA), and Parquet.
#' A progress bar is displayed in the console for user feedback.
#'
#' The function is cross-platform and works on Windows, macOS, and Linux.
#' It can optionally read files recursively from subdirectories.
#'
#' @param input_dir Character. Path to the input directory containing data files.
#' @param recursive Logical. If TRUE, subdirectories will also be scanned recursively.
#'   Default is FALSE.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{data_list}{A named list of data frames. Each element corresponds to a file.}
#'   \item{summary}{A data frame summarizing the processing of each file, including
#'     file name, type, status (Success, Failed, Skipped), and reason for failures.}
#' }
#'
#' @details
#' This function automatically checks for required packages (`readxl`, `data.table`,
#' `jsonlite`, `haven`, `arrow`, `tools`, `dplyr`, `progress`) and installs them
#' if missing. Files are read using appropriate functions for their type. Errors are
#' handled gracefully, with failed or unsupported files logged in the summary table.
#'
#' Supported file formats:
#' \itemize{
#'   \item CSV (`.csv`)
#'   \item TSV, TXT (`.tsv`, `.txt`)
#'   \item Excel (`.xls`, `.xlsx`)
#'   \item JSON (`.json`)
#'   \item RDS (`.rds`)
#'   \item SPSS (`.sav`)
#'   \item SAS (`.sas7bdat`)
#'   \item Stata (`.dta`)
#'   \item Parquet (`.parquet`)
#' }
#'
#' @examples
#' \dontrun{
#' # Read all supported files from a directory (non-recursive)
#' results <- read_any_directory_with_progress("~/Documents/my_data")
#'
#' # Access loaded datasets
#' names(results$data_list)
#' head(results$data_list[["sales_data"]])
#'
#' # View summary of files processed
#' View(results$summary)
#'
#' # Recursive mode: include subdirectories
#' results_recursive <- read_any_directory_with_progress("~/Documents/my_data", recursive = TRUE)
#' }
#'
#' @export
read_any_directory_with_progress <- function(input_dir, recursive = FALSE) {
  #------------------------------
  # Step 1: Check & install required packages
  #------------------------------
  required_packages <- c(
    "readxl", "data.table", "jsonlite", "haven", "arrow", "tools", "dplyr", "progress"
  )

  not_installed <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  if (length(not_installed) > 0) {
    message("Installing missing packages: ", paste(not_installed, collapse = ", "))
    install.packages(not_installed)
  }

  lapply(required_packages, require, character.only = TRUE)

  #------------------------------
  # Step 2: Validate directory
  #------------------------------
  if (!dir.exists(input_dir)) stop("Input directory does not exist.")

  #------------------------------
  # Step 3: List all files
  #------------------------------
  all_files <- list.files(input_dir, full.names = TRUE, recursive = recursive)
  if (length(all_files) == 0) stop("No files found in directory.")

  #------------------------------
  # Step 4: Initialize results
  #------------------------------
  results_list <- list()
  summary_log <- data.frame(
    file_name = character(),
    file_type = character(),
    status = character(),
    reason = character(),
    stringsAsFactors = FALSE
  )

  #------------------------------
  # Step 5: Initialize progress bar
  #------------------------------
  pb <- progress::progress_bar$new(
    format = "Loading files [:bar] :percent | :current/:total | Elapsed: :elapsed",
    total = length(all_files),
    clear = FALSE,
    width = 70
  )

  #------------------------------
  # Step 6: Loop through files
  #------------------------------
  for (file in all_files) {
    pb$tick()

    ext <- tolower(tools::file_ext(file))
    base <- tools::file_path_sans_ext(basename(file))
    status <- "Success"
    reason <- ""

    tryCatch({
      if (ext == "csv") {
        data <- data.table::fread(file, encoding = "UTF-8", data.table = FALSE)
      } else if (ext %in% c("tsv", "txt")) {
        data <- data.table::fread(file, sep = "\t", encoding = "UTF-8", data.table = FALSE)
      } else if (ext %in% c("xls", "xlsx")) {
        data <- readxl::read_excel(file)
      } else if (ext == "json") {
        data <- jsonlite::fromJSON(file)
        if (!is.data.frame(data)) data <- as.data.frame(data)
      } else if (ext == "rds") {
        data <- readRDS(file)
      } else if (ext == "sav") {
        data <- haven::read_sav(file)
      } else if (ext == "sas7bdat") {
        data <- haven::read_sas(file)
      } else if (ext == "dta") {
        data <- haven::read_dta(file)
      } else if (ext == "parquet") {
        data <- arrow::read_parquet(file)
      } else {
        status <- "Skipped"
        reason <- paste("Unsupported file type:", ext)
        data <- NULL
      }

      if (!is.null(data)) results_list[[base]] <- data
    },
    error = function(e) {
      status <<- "Failed"
      reason <<- e$message
    })

    # Log summary
    summary_log <- rbind(
      summary_log,
      data.frame(
        file_name = basename(file),
        file_type = ext,
        status = status,
        reason = reason,
        stringsAsFactors = FALSE
      )
    )
  }

  #------------------------------
  # Step 7: Print summary counts
  #------------------------------
  message("\n???? Summary of Files Processed:")
  print(
    summary_log %>%
      dplyr::group_by(status) %>%
      dplyr::summarise(count = dplyr::n())
  )

  #------------------------------
  # Step 8: Return results
  #------------------------------
  return(list(
    data_list = results_list,
    summary = summary_log
  ))
}
