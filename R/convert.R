#' Convert any file to csv format
#'
#' @param input_dir directory address to folder containing files to convert to CSV format
#' @param output_dir text in quotes for directory to hold converted CSV files
#'
#' @returns CSV files
#' @export
#'
#' @examples
#' \dontrun{convert_any_to_csv_iso(getwd())}
#-----------------------------------------------------------
# Function: convert_any_to_csv_iso
# Purpose:  Convert any readable data file in a directory to .csv (Latin-1 encoding),
#           track process details, show progress, and visualize results.
#-----------------------------------------------------------

convert_any_to_csv_iso <- function(input_dir, output_dir = NULL) {
  #------------------------------
  # Step 1: Define and install required packages
  #------------------------------
  required_packages <- c("readxl", "dplyr", "ggplot2", "tools", "progress", "data.table")

  not_installed <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  if (length(not_installed) > 0) {
    message("Installing missing packages: ", paste(not_installed, collapse = ", "))
    install.packages(not_installed)
  }

  lapply(required_packages, require, character.only = TRUE)

  #------------------------------
  # Step 2: Validate input directory
  #------------------------------
  if (!dir.exists(input_dir)) stop("Input directory does not exist.")

  #------------------------------
  # Step 3: Create output directory
  #------------------------------
  if (is.null(output_dir)) {
    output_dir <- file.path(input_dir, "converted_csv")
  }
  if (!dir.exists(output_dir)) dir.create(output_dir)

  #------------------------------
  # Step 4: List all files in directory
  #------------------------------
  all_files <- list.files(input_dir, full.names = TRUE)
  if (length(all_files) == 0) stop("No files found in input directory.")

  #------------------------------
  # Step 5: Prepare results storage
  #------------------------------
  results <- data.frame(
    file_name = character(),
    input_size_kb = numeric(),
    output_size_kb = numeric(),
    time_seconds = numeric(),
    status = character(),
    reason = character(),
    stringsAsFactors = FALSE
  )

  #------------------------------
  # Step 6: Initialize progress bar
  #------------------------------
  pb <- progress::progress_bar$new(
    format = "Processing [:bar] :percent | :current/:total files | Elapsed: :elapsed",
    total = length(all_files),
    clear = FALSE,
    width = 70
  )

  #------------------------------
  # Step 7: Loop through each file
  #------------------------------
  for (file in all_files) {
    pb$tick()  # Advance progress bar
    start_time <- Sys.time()

    file_ext <- tolower(file_ext(file))
    csv_name <- paste0(file_path_sans_ext(basename(file)), ".csv")
    csv_path <- file.path(output_dir, csv_name)

    status <- "Success"
    reason <- ""
    success <- FALSE

    #------------------------------
    # Step 8: Try reading the file based on extension
    #------------------------------
    tryCatch({
      if (file_ext %in% c("xls", "xlsx")) {
        data <- readxl::read_excel(file)
        success <- TRUE
      } else if (file_ext %in% c("csv")) {
        data <- data.table::fread(file)
        success <- TRUE
      } else if (file_ext %in% c("tsv", "txt")) {
        data <- data.table::fread(file, sep = "\t")
        success <- TRUE
      } else if (file_ext %in% c("json")) {
        if ("jsonlite" %in% installed.packages()[, "Package"]) {
          data <- jsonlite::fromJSON(file)
          if (is.data.frame(data)) success <- TRUE else success <- FALSE
        } else {
          status <- "Failed"
          reason <- "JSON file - install jsonlite for support"
        }
      } else {
        status <- "Failed"
        reason <- paste("Unsupported file type:", file_ext)
      }

      #------------------------------
      # Step 9: Convert character columns to ISO8859-1
      #------------------------------
      if (success) {
        if (nrow(data) > 0 && ncol(data) > 0) {
          # Convert character columns safely to Latin-1
          data[] <- lapply(data, function(x) {
            if (is.character(x)) {
              iconv(x, from = "", to = "ISO8859-1", sub = "byte")
            } else x
          })

          # Write CSV with Latin-1 encoding
          write.csv(data, csv_path, row.names = FALSE, fileEncoding = "ISO8859-1")
        } else {
          status <- "Failed"
          reason <- "File contained no readable data"
        }
      } else {
        if (reason == "") reason <- "File type not supported or unreadable"
      }

      end_time <- Sys.time()

      results <- rbind(
        results,
        data.frame(
          file_name = basename(file),
          input_size_kb = file.info(file)$size / 1024,
          output_size_kb = ifelse(file.exists(csv_path), file.info(csv_path)$size / 1024, NA),
          time_seconds = as.numeric(difftime(end_time, start_time, units = "secs")),
          status = status,
          reason = reason,
          stringsAsFactors = FALSE
        )
      )
    },
    #------------------------------
    # Step 10: Handle read/convert errors
    #------------------------------
    error = function(e) {
      results <<- rbind(
        results,
        data.frame(
          file_name = basename(file),
          input_size_kb = file.info(file)$size / 1024,
          output_size_kb = NA,
          time_seconds = NA,
          status = "Failed",
          reason = iconv(e$message, from = "", to = "ISO8859-1", sub = "byte"),
          stringsAsFactors = FALSE
        )
      )
    })
  }

  #------------------------------
  # Step 11: Compute summary statistics
  #------------------------------
  summary_stats <- results %>%
    dplyr::filter(status == "Success") %>%
    dplyr::summarise(
      total_files_converted = n(),
      avg_input_size_kb = mean(input_size_kb, na.rm = TRUE),
      avg_output_size_kb = mean(output_size_kb, na.rm = TRUE),
      total_time_seconds = sum(time_seconds, na.rm = TRUE),
      avg_time_seconds = mean(time_seconds, na.rm = TRUE)
    )

  #------------------------------
  # Step 12: Display results
  #------------------------------
  message("\n??? Conversion process complete!")
  print(summary_stats)

  #------------------------------
  # Step 13: Visualization with ISO8859-1 fonts
  #------------------------------
  if (any(results$status == "Success")) {
    plot <- results %>%
      dplyr::filter(status == "Success") %>%
      ggplot(aes(x = reorder(file_name, time_seconds), y = time_seconds)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      theme_minimal(base_size = 13, base_family = "sans") +  # ISO8859-1 compatible font
      labs(
        title = iconv("Conversion Time per File", to = "ISO8859-1"),
        x = iconv("File Name", to = "ISO8859-1"),
        y = iconv("Time (seconds)", to = "ISO8859-1")
      )
    print(plot)
  } else {
    plot <- NULL
    message("No successful conversions to plot.")
  }

  #------------------------------
  # Step 14: Return results
  #------------------------------
  return(list(summary = summary_stats, details = results, plot = plot))
}
