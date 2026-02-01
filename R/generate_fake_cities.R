#' Generate Fake City Data
#' 
#' Generates a vector of fake city names for testing, including common variations and noise.
#' @param n Integer, matching number of cities to generate.
#' @param country "DE" or "CH".
#' @return Character vector of city names.
#' @export
generate_fake_cities <- function(n = 10, country = "DE") {
  if (country == "DE") {
      candidates <- c("Berlin", "München", "Hamburg", "Koeln", "Frankfurt am Main", "Stuttgart", "Düsseldorf", 
                     "Leipzig", "Dortmund", "Essen", "Bremen", "Hannover", "Nürnberg", "Dresden", 
                     "Bochum", "Wuppertal", "Bielefeld", "Bonn", "Münster", "Karlsruhe")
      noisy <- c("Berlin (West)", "Muenchen ", "hamburg", "Koeln-Ehrenfeld", "Frankfurt/Main", "Stuttgart-Mitte",
                 "Duesseldorf", "Leipzig.", "Dortmund, Stadt", "Essen/Ruhr", "Munich", "Cologne", "Frankfurt a.M.")
      pool <- c(candidates, noisy)
  } else {
       candidates <- c("Zürich", "Genf", "Basel", "Bern", "Lausanne", "Winterthur", "Luzern", "St. Gallen", "Lugano", "Biel/Bienne")
       noisy <- c("Zuerich", "Geneve", "Basel-Stadt", "Bern (BE)", "Lausanne, VD", "Winterthur 1", "Luzern.", "Sankt Gallen", "Lugano TI", "Biel", "Zurich", "Geneva")
       pool <- c(candidates, noisy)
  }
  
  sample(pool, n, replace = TRUE)
}
