
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
  
  raw <- read_excel(file, sheet = sheet)
  df <- as.data.frame(raw)
  
  # Standardize cols
  df$lau_code <- df[["LAU CODE"]]
  df$lau_name <- as.character(df[["LAU NAME NATIONAL"]])
  df$nuts_3_id <- df[["NUTS 3 CODE"]]
  df$population <- as.numeric(df[["POPULATION"]])
  
  # Normalize
  df$city_clean <- normalize_city(df$lau_name, country = country)
  
  # 1. Create LAU Dataset
  lau_out <- df %>%
    filter(!is.na(city_clean) & city_clean != "") %>%
    select(lau_code, lau_name, nuts_3_id, city_clean, population) %>%
    distinct()
    
  # 2. Synthesize NUTS Dataset (Step 1 Targets)
  # Fix: Use simpler aggregation to avoid vctrs/rlang version issues with slice_max
  
  # Get unique NUTS IDs and key info
  nuts_candidates <- df[!is.na(df$nuts_3_id), c("nuts_3_id", "lau_name", "population")]
  
  # For each NUTS ID, find the row with max population
  # Using base R 'ave' or 'order'
  nuts_candidates <- nuts_candidates[order(nuts_candidates$nuts_3_id, -nuts_candidates$population), ]
  nuts_unique <- nuts_candidates[!duplicated(nuts_candidates$nuts_3_id), ]
  
  nuts_out <- nuts_unique %>%
    select(nuts_3_id, lau_name) %>%
    rename(nuts_label = lau_name)
    
  # Normalize separately to avoid dplyr mutate errors if any
  nuts_out$city_clean <- normalize_city(nuts_out$nuts_label, country = country)
  nuts_out$priority <- 1
  
  nuts_out <- nuts_out[nuts_out$city_clean != "", ]
  
  return(list(lau = lau_out, nuts = nuts_out))
}

# --- Main Execution ---
lau_file <- "NUTS_localadministrativeunits.xlsx"

# DE
de_data <- process_lau_and_nuts(lau_file, "DE", "DE")
lau_de <- de_data$lau
nuts_de <- de_data$nuts

# CH
ch_data <- process_lau_and_nuts(lau_file, "CH", "CH")
lau_ch <- ch_data$lau
nuts_ch <- ch_data$nuts

# --- Saving ---
message("Saving embedded datasets...")
save(lau_de, file = "../data/lau_de.rda", compress = "bzip2")
save(lau_ch, file = "../data/lau_ch.rda", compress = "bzip2")
save(nuts_de, file = "../data/nuts_de.rda", compress = "bzip2")
save(nuts_ch, file = "../data/nuts_ch.rda", compress = "bzip2")
message("Done.")
