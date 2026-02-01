#' Match City Names to NUTS Regions
#'
#' Matches a vector of city names to NUTS 3 regions using a cascading logic:
#' 1. Exact match against NUTS Region label (if available).
#' 2. Exact match against LAU list.
#' 3. Fuzzy match against LAU list.
#' 4. Fuzzy match against NUTS Region label (if available).
#'
#' @param x Character vector of city names.
#' @param country Character string "DE" or "CH".
#' @param fuzzy Logical, whether to perform fuzzy matching.
#' @param threshold Numeric, similarity threshold for fuzzy matching (0-1).
#' @return A data frame with columns: original, city_clean, nuts_3_id, lau_name, match_type, similarity.
#' @importFrom stringdist stringsim
#' @importFrom dplyr filter select mutate bind_rows left_join everything
#' @export
match_city <- function(x, country = "DE", fuzzy = TRUE, threshold = 0.95) {
  
  # Normalize input
  normalized <- normalize_city(x, country = country)
  input_df <- data.frame(
    original = x,
    city_clean = normalized,
    stringsAsFactors = FALSE
  )
  
  # Filter out empty or invalid clean names
  valid_idxs <- which(input_df$city_clean != "" & !is.na(input_df$city_clean) & !grepl("Do not match", input_df$city_clean))
  valid_inputs <- input_df[valid_idxs, ]
  
  # Initialize results container
  results <- cm_results(original = character(), city_clean=character(), nuts_3_id=character(), 
                        lau_name = character(), match_type=character(), similarity=numeric())
  
  # Identify initially unmatched
  unmatched <- valid_inputs
  
  # Helper to update unmatched
  update_unmatched <- function(curr_unmatched, matches) {
      if (nrow(matches) > 0) {
          curr_unmatched[!curr_unmatched$original %in% matches$original, ]
      } else {
          curr_unmatched
      }
  }

  # --- LOAD DATA ---
  nuts_data <- NULL
  lau_data <- NULL
  
  if (country == "DE") {
      # Try find NUTS data in package or global (for check script)
      if (exists("nuts_de") && is.data.frame(get("nuts_de"))) {
           nuts_data <- get("nuts_de")
      } else if (exists("nuts_de", where = as.environment("package:placematchr"))) {
           nuts_data <- placematchr::nuts_de
      }
      
      # LAU data
      if (exists("lau_de") && is.data.frame(get("lau_de"))) {
           lau_data <- get("lau_de")
      } else {
           lau_data <- placematchr::lau_de
      }
      
      # DE Order: Exact NUTS -> Exact LAU -> Fuzzy LAU -> Fuzzy NUTS

  } else if (country == "CH") {
      if (exists("nuts_ch") && is.data.frame(get("nuts_ch"))) {
           nuts_data <- get("nuts_ch")
      } else if (exists("nuts_ch", where = as.environment("package:placematchr"))) {
           nuts_data <- placematchr::nuts_ch
      }
      
      if (exists("lau_ch") && is.data.frame(get("lau_ch"))) {
           lau_data <- get("lau_ch")
      } else {
           lau_data <- placematchr::lau_ch
      }
      
      # CH Order: Exact LAU -> Fuzzy LAU -> Exact NUTS -> Fuzzy NUTS
      # We handle this by ordering the steps below conditionally.
  }
  
  
  # --- STEP DEFINITIONS ---
  
  do_exact_nuts <- function(inputs, ref) {
      msg <- merge(inputs, ref, by = "city_clean", all.x = FALSE)
      if (nrow(msg) > 0) {
          # Prioritize if multiple? The script used slice_min on priority.
          # Here we verify if priority col exists
          if ("priority" %in% names(msg)) {
              msg <- msg[order(msg$priority), ]
              msg <- msg[!duplicated(msg$original), ]
          }
          
          data.frame(
              original = msg$original,
              city_clean = msg$city_clean,
              nuts_3_id = msg$nuts_3_id,
              lau_name = if ("nuts_label" %in% names(msg)) msg$nuts_label else NA, # Use NUTS label as name
              match_type = "Exact (NUTS)",
              similarity = 1.0,
              stringsAsFactors = FALSE
          )
      } else {
          cm_results()
      }
  }
  
  do_exact_lau <- function(inputs, ref) {
      msg <- merge(inputs, ref, by = "city_clean", all.x = FALSE)
      if (nrow(msg) > 0) {
          data.frame(
              original = msg$original,
              city_clean = msg$city_clean,
              nuts_3_id = msg$nuts_3_id,
              lau_name = msg$lau_name,
              match_type = "Exact (LAU)",
              similarity = 1.0,
              stringsAsFactors = FALSE
          )
      } else {
          cm_results()
      }
  }
  
  do_fuzzy <- function(inputs, ref, target_col, id_col, label_col, type_name) {
      if (!fuzzy || nrow(inputs) == 0) return(cm_results())
      
      unique_queries <- unique(inputs$city_clean)
      res_list <- list()
      
      for (q in unique_queries) {
          sims <- stringdist::stringsim(q, ref[[target_col]], method = "jw")
          best_idx <- which.max(sims)
          
          if (length(best_idx) > 0 && sims[best_idx] >= threshold) {
              row <- ref[best_idx, , drop=FALSE]
              res_df <- data.frame(
                  original = inputs$original[inputs$city_clean == q], # Map back to all originals with this clean name
                  city_clean = q,
                  nuts_3_id = row[[id_col]],
                  lau_name = row[[label_col]],
                  match_type = type_name,
                  similarity = sims[best_idx],
                  stringsAsFactors = FALSE
              )
              res_list[[length(res_list) + 1]] <- res_df
          }
      }
      
      if (length(res_list) > 0) do.call(rbind, res_list) else cm_results()
  }

  do_fuzzy_lau <- function(inputs, ref) {
      do_fuzzy(inputs, ref, "city_clean", "nuts_3_id", "lau_name", "Fuzzy (LAU)")
  }
  
  do_fuzzy_nuts <- function(inputs, ref) {
      do_fuzzy(inputs, ref, "city_clean", "nuts_3_id", "nuts_label", "Fuzzy (NUTS)")
  }

  # --- EXECUTE CASCADE ---
  
  matches_s1 <- cm_results()
  matches_s2 <- cm_results()
  matches_s3 <- cm_results()
  matches_s4 <- cm_results()
  
  if (country == "DE") {
      # 1. Exact NUTS
      if (!is.null(nuts_data) && nrow(unmatched) > 0) {
          matches_s1 <- do_exact_nuts(unmatched, nuts_data)
          unmatched <- update_unmatched(unmatched, matches_s1)
      }
      
      # 2. Exact LAU
      if (!is.null(lau_data) && nrow(unmatched) > 0) {
          matches_s2 <- do_exact_lau(unmatched, lau_data)
          unmatched <- update_unmatched(unmatched, matches_s2)
      }
      
      # 3. Fuzzy LAU
      if (!is.null(lau_data) && nrow(unmatched) > 0) {
          matches_s3 <- do_fuzzy_lau(unmatched, lau_data)
          unmatched <- update_unmatched(unmatched, matches_s3)
      }
      
      # 4. Fuzzy NUTS
      if (!is.null(nuts_data) && nrow(unmatched) > 0) {
          matches_s4 <- do_fuzzy_nuts(unmatched, nuts_data)
          unmatched <- update_unmatched(unmatched, matches_s4)
      }
      
  } else { # CH
      # 1. Exact LAU
      if (!is.null(lau_data) && nrow(unmatched) > 0) {
          matches_s1 <- do_exact_lau(unmatched, lau_data)
          unmatched <- update_unmatched(unmatched, matches_s1)
      }
      
      # 2. Fuzzy LAU
      if (!is.null(lau_data) && nrow(unmatched) > 0) {
          matches_s2 <- do_fuzzy_lau(unmatched, lau_data)
          unmatched <- update_unmatched(unmatched, matches_s2)
      }
      
      # 3. Exact NUTS
      if (!is.null(nuts_data) && nrow(unmatched) > 0) {
          matches_s3 <- do_exact_nuts(unmatched, nuts_data)
          unmatched <- update_unmatched(unmatched, matches_s3)
      }
      
      # 4. Fuzzy NUTS
      if (!is.null(nuts_data) && nrow(unmatched) > 0) {
          matches_s4 <- do_fuzzy_nuts(unmatched, nuts_data)
          unmatched <- update_unmatched(unmatched, matches_s4)
      }
  }
  
  # Combine all matches
  final_matches <- rbind(matches_s1, matches_s2, matches_s3, matches_s4)
  
  # Add unmatched inputs as NAs
  if (nrow(unmatched) > 0) {
      unmatched_rows <- data.frame(
          original = unmatched$original,
          city_clean = unmatched$city_clean,
          nuts_3_id = NA,
          lau_name = NA,
          match_type = NA,
          similarity = NA,
          stringsAsFactors = FALSE
      )
      final_matches <- rbind(final_matches, unmatched_rows)
  }
  
  # Restore order and structure
  # We join original x to preserve full list
  full_results <- data.frame(original = x, stringsAsFactors = FALSE)
  # Join. Note: if one original had multiple matches (e.g. multiple LAUs with same cleaned name),
  # this will multiply rows. This is expected behavior of the package as per user requirement to "match".
  # However, if we want to deduplicate best match:
  # The cascade prioritized inputs. 
  # S1 matches are best. S2 matches are second best. 
  # But within S2 (Exact LAU), "Berlin" might match 10 "Berlin" entries.
  # We keep all of them? 
  # Previous script (Step 129) used `lau_exact <- ... left_join(lau)`. That produces multiple rows.
  # So we probably should return multiple rows.
  
  out <- merge(full_results, final_matches, by = "original", all.x = TRUE)
  
  # Fill clean name for unmatched if missing
  missing_clean <- is.na(out$city_clean)
  if (any(missing_clean)) {
      out$city_clean[missing_clean] <- normalize_city(out$original[missing_clean], country=country)
  }
  
  return(out)
}

# Helper for empty DF
cm_results <- function(original=character(), city_clean=character(), nuts_3_id=character(), 
                       lau_name=character(), match_type=character(), similarity=numeric()) {
    data.frame(original=original, city_clean=city_clean, nuts_3_id=nuts_3_id, 
               lau_name=lau_name, match_type=match_type, similarity=similarity, stringsAsFactors=FALSE)
}
