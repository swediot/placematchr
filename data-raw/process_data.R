
library(readxl)
library(dplyr)
library(stringr)
library(stringdist)
library(tidyr)
library(data.table)

# Source the normalization functions
source("../R/normalize_city.R")

# --- LAU Processing Functions ---
process_lau_and_nuts <- function(file, sheet, country) {
  message(sprintf("Processing %s LAU & NUTS...", country))
  
  if (!file.exists(file)) {
    stop(sprintf("File %s not found!", file))
  }
  
  # Read sheet safely
  raw <- tryCatch({
    read_excel(file, sheet = sheet)
  }, error = function(e) {
    warning(sprintf("Could not read sheet %s: %s", sheet, e$message))
    return(NULL)
  })
  
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  
  df <- as.data.frame(raw)
  
  # Normalize column names slightly because they vary by sheet version sometimes
  # We look for standard columns: LAU CODE, LAU NAME LATIN (or National if Latin missing), NUTS 3 CODE, POPULATION
  
  cols <- colnames(df)
  # Helper to find valid col
  find_col <- function(patterns) {
    for (p in patterns) {
      m <- grep(p, cols, ignore.case=TRUE)
      if (length(m) > 0) {
          # Check if col is mostly NAs
          vals <- df[[m[1]]]
          if (sum(!is.na(vals)) > 0) return(m[1])
      }
    }
    return(NULL)
  }
  
  col_lau_code <- find_col(c("LAU CODE", "LAU_CODE"))
  col_lau_name <- find_col(c("LAU NAME LATIN", "LAU NAME NATIONAL", "LAU NAME"))
  col_nuts <- find_col(c("NUTS 3 CODE", "NUTS_3_CODE"))
  col_pop <- find_col(c("POPULATION", "POP"))
  
  if (is.null(col_lau_code) || is.null(col_lau_name) || is.null(col_nuts)) {
    warning(sprintf("Skipping %s: Missing critical columns. Found: %s", country, paste(colnames(df), collapse=", ")))
    return(NULL)
  }
  
  df$lau_code <- df[[col_lau_code]]
  df$lau_name <- as.character(df[[col_lau_name]])
  df$nuts_3_id <- df[[col_nuts]]
  
  if (!is.null(col_pop)) {
    df$population <- as.numeric(as.character(df[[col_pop]]))
  } else {
    df$population <- 0 # Default if missing
  }
  
  # Normalize
  df$city_clean <- normalize_city(df$lau_name, country = country)
  
  # 1. Create LAU Dataset
  lau_out <- df %>%
    filter(!is.na(city_clean) & city_clean != "") %>%
    select(lau_code, lau_name, nuts_3_id, city_clean, population) %>%
    distinct()
    
  # 2. Synthesize NUTS Dataset (Step 1 Targets)
  
  # Get unique NUTS IDs and key info
  nuts_candidates <- df[!is.na(df$nuts_3_id), c("nuts_3_id", "lau_name", "population")]
  
  # For each NUTS ID, find the row with max population using base R
  nuts_candidates <- nuts_candidates[order(nuts_candidates$nuts_3_id, -as.numeric(nuts_candidates$population)), ]
  nuts_unique <- nuts_candidates[!duplicated(nuts_candidates$nuts_3_id), ]
  
  nuts_out <- nuts_unique %>%
    select(nuts_3_id, lau_name) %>%
    rename(nuts_label = lau_name)
    
  # Normalize separately
  nuts_out$city_clean <- normalize_city(nuts_out$nuts_label, country = country)
  nuts_out$priority <- 1
  
  nuts_out <- nuts_out[nuts_out$city_clean != "", ]
  
  return(list(lau = lau_out, nuts = nuts_out))
}

# --- Main Execution ---
lau_file <- "NUTS_localadministrativeunits.xlsx"

# Get all country sheets
# Exclude metadata sheets
all_sheets <- readxl::excel_sheets(lau_file)
skip_sheets <- c("File_info", "Overview", "Overview_Population")
country_sheets <- setdiff(all_sheets, skip_sheets)

message(sprintf("Found %d countries to process: %s", length(country_sheets), paste(country_sheets, collapse=", ")))

for (country in country_sheets) {
  res <- process_lau_and_nuts(lau_file, country, country)
  
  if (!is.null(res)) {
    # Dynamically assign variables
    assign(paste0("lau_", tolower(country)), res$lau)
    assign(paste0("nuts_", tolower(country)), res$nuts)
    
    # Save
    f_lau <- sprintf("../data/lau_%s.rda", tolower(country))
    f_nuts <- sprintf("../data/nuts_%s.rda", tolower(country))
    
    save(list = paste0("lau_", tolower(country)), file = f_lau, compress = "bzip2")
    save(list = paste0("nuts_", tolower(country)), file = f_nuts, compress = "bzip2")
  }
}

message("Done processing all countries.")
