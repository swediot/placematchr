#' Match City Names to NUTS Regions
#'
#' Matches a vector of city names to NUTS 3 regions using a cascading logic for any supported country.
#'
#' @param x Character vector of city names.
#' @param country Character string of two-letter country code (e.g. "DE", "FR").
#' @param fuzzy Logical, whether to perform fuzzy matching.
#' @param threshold Numeric, similarity threshold for fuzzy matching (0-1).
#' @return A data frame with columns: original, city_clean, nuts_3_id, lau_name, match_type, similarity.
#' @examples
#' # Match German cities
#' cities <- c("Berlin", "Munich", "Hamburg")
#' match_city(cities, country = "DE")
#'
#' # Match with exact matching only (no fuzzy)
#' match_city(c("Frankfurt am Main"), country = "DE", fuzzy = FALSE)
#' @importFrom stringdist stringsim
#' @importFrom dplyr filter select mutate bind_rows left_join everything
#' @export
match_city <- function(x, country = "DE", fuzzy = TRUE, threshold = 0.95) {
    country <- toupper(country)

    # Normalize input
    normalized <- normalize_city(x, country = country)
    input_df <- data.frame(
        original = x,
        city_clean = normalized,
        stringsAsFactors = FALSE
    )

    # Filter out empty or invalid clean names
    valid_idxs <- which(input_df$city_clean != "" & !is.na(input_df$city_clean))
    valid_inputs <- input_df[valid_idxs, ]

    # Helper for empty DF
    cm_results <- function() {
        data.frame(
            original = character(), city_clean = character(), nuts_3_id = character(),
            lau_name = character(), match_type = character(), similarity = numeric(), stringsAsFactors = FALSE
        )
    }

    # Helper to update unmatched
    update_unmatched <- function(curr_unmatched, matches) {
        if (nrow(matches) > 0) {
            curr_unmatched[!curr_unmatched$original %in% matches$original, ]
        } else {
            curr_unmatched
        }
    }

    # --- LOAD DATA DYNAMICALLY ---
    nuts_name <- paste0("nuts_", tolower(country))
    lau_name <- paste0("lau_", tolower(country))

    nuts_data <- NULL
    lau_data <- NULL

    # Try global env first (e.g. verification scripts or devtools::load_all)
    if (exists(nuts_name) && is.data.frame(get(nuts_name))) nuts_data <- get(nuts_name)
    if (exists(lau_name) && is.data.frame(get(lau_name))) lau_data <- get(lau_name)

    # Try package env (safely)
    if (is.null(nuts_data) && "package:placematchr" %in% search() && exists(nuts_name, where = as.environment("package:placematchr"))) {
        nuts_data <- get(nuts_name, pos = "package:placematchr")
    }
    if (is.null(lau_data) && "package:placematchr" %in% search() && exists(lau_name, where = as.environment("package:placematchr"))) {
        lau_data <- get(lau_name, pos = "package:placematchr")
    }

    # If still null, try lazy loading might require data() call if installed
    if (is.null(nuts_data)) try(utils::data(list = nuts_name, package = "placematchr", envir = environment()), silent = TRUE)
    if (is.null(lau_data)) try(utils::data(list = lau_name, package = "placematchr", envir = environment()), silent = TRUE)

    # Check local environment after try(data())
    if (is.null(nuts_data) && exists(nuts_name, envir = environment())) nuts_data <- get(nuts_name, envir = environment())
    if (is.null(lau_data) && exists(lau_name, envir = environment())) lau_data <- get(lau_name, envir = environment())

    if (is.null(nuts_data) && is.null(lau_data)) {
        warning("No NUTS or LAU data found for country: ", country)
        # Return empty results for all
        res <- cm_results()
        res <- res[rep(1, nrow(input_df)), ]
        res[] <- NA
        res$original <- x
        res$city_clean <- normalized
        return(res)
    }


    # --- MATCHING FUNCTIONS ---
    do_exact_nums <- function(inputs, ref, type_name, label_col) {
        msg <- merge(inputs, ref, by = "city_clean", all.x = FALSE)
        if (nrow(msg) > 0) {
            data.frame(
                original = msg$original,
                city_clean = msg$city_clean,
                nuts_3_id = msg$nuts_3_id,
                lau_name = msg[[label_col]],
                match_type = type_name,
                similarity = 1.0,
                stringsAsFactors = FALSE
            )
        } else {
            cm_results()
        }
    }

    do_fuzzy <- function(inputs, ref, label_col, type_name) {
        if (!fuzzy || nrow(inputs) == 0) {
            return(cm_results())
        }
        unique_queries <- unique(inputs$city_clean)
        res_list <- list()

        for (q in unique_queries) {
            # stringdist for this batch
            sims <- stringdist::stringsim(q, ref$city_clean, method = "jw")
            best_idx <- which.max(sims)

            if (length(best_idx) > 0 && sims[best_idx] >= threshold) {
                row <- ref[best_idx, , drop = FALSE]
                res_df <- data.frame(
                    original = inputs$original[inputs$city_clean == q],
                    city_clean = q,
                    nuts_3_id = row$nuts_3_id,
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

    # --- CASCADE EXECUTION ---

    unmatched <- valid_inputs
    matches_s1 <- cm_results()
    matches_s2 <- cm_results()
    matches_s3 <- cm_results()
    matches_s4 <- cm_results()

    # Standard Logic for ALL countries:
    # 1. Exact NUTS
    if (!is.null(nuts_data) && nrow(unmatched) > 0) {
        matches_s1 <- do_exact_nums(unmatched, nuts_data, "Exact (NUTS)", "nuts_label")
        unmatched <- update_unmatched(unmatched, matches_s1)
    }

    # 2. Exact LAU
    if (!is.null(lau_data) && nrow(unmatched) > 0) {
        matches_s2 <- do_exact_nums(unmatched, lau_data, "Exact (LAU)", "lau_name")
        unmatched <- update_unmatched(unmatched, matches_s2)
    }

    # 3. Fuzzy LAU
    if (!is.null(lau_data) && nrow(unmatched) > 0) {
        matches_s3 <- do_fuzzy(unmatched, lau_data, "lau_name", "Fuzzy (LAU)")
        unmatched <- update_unmatched(unmatched, matches_s3)
    }

    # 4. Fuzzy NUTS
    if (!is.null(nuts_data) && nrow(unmatched) > 0) {
        matches_s4 <- do_fuzzy(unmatched, nuts_data, "nuts_label", "Fuzzy (NUTS)")
        unmatched <- update_unmatched(unmatched, matches_s4)
    }

    # Combine
    final_matches <- rbind(matches_s1, matches_s2, matches_s3, matches_s4)

    # Handle completely unmatched
    if (nrow(unmatched) > 0) {
        unmatched_rows <- data.frame(
            original = unmatched$original,
            city_clean = unmatched$city_clean,
            nuts_3_id = NA, lau_name = NA, match_type = NA, similarity = NA, stringsAsFactors = FALSE
        )
        final_matches <- rbind(final_matches, unmatched_rows)
    }

    # Merge back to original list order
    full_results <- data.frame(original = x, stringsAsFactors = FALSE)
    out <- merge(full_results, final_matches, by = "original", all.x = TRUE)

    # Fill NA cleans
    missing_clean <- is.na(out$city_clean)
    if (any(missing_clean)) out$city_clean[missing_clean] <- normalize_city(out$original[missing_clean], country = country)

    return(out)
}
