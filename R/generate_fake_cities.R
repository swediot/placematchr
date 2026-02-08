#' Generate Fake City Data
#'
#' Generates a vector of fake city names for testing, including common variations and noise.
#' @param n Integer, matching number of cities to generate.
#' @param country "DE" or "CH".
#' @return Character vector of city names.
#' @examples
#' # Generate 5 fake German cities
#' generate_fake_cities(5, country = "DE")
#'
#' # Generate 3 fake Swiss cities
#' generate_fake_cities(3, country = "CH")
#' @export
generate_fake_cities <- function(n = 10, country = "DE") {
  if (country == "DE") {
    candidates <- c(
      "Berlin", "M\u00FCnchen", "Hamburg", "Koeln", "Frankfurt am Main", "Stuttgart", "D\u00FCsseldorf",
      "Leipzig", "Dortmund", "Essen", "Bremen", "Hannover", "N\u00FCrnberg", "Dresden",
      "Bochum", "Wuppertal", "Bielefeld", "Bonn", "M\u00FCnster", "Karlsruhe"
    )
    noisy <- c(
      "Berlin (West)", "Muenchen ", "hamburg", "Koeln-Ehrenfeld", "Frankfurt/Main", "Stuttgart-Mitte",
      "Duesseldorf", "Leipzig.", "Dortmund, Stadt", "Essen/Ruhr", "Munich", "Cologne", "Frankfurt a.M."
    )
    pool <- c(candidates, noisy)
  } else {
    candidates <- c("Z\u00FCrich", "Genf", "Basel", "Bern", "Lausanne", "Winterthur", "Luzern", "St. Gallen", "Lugano", "Biel/Bienne")
    noisy <- c("Zuerich", "Geneve", "Basel-Stadt", "Bern (BE)", "Lausanne, VD", "Winterthur 1", "Luzern.", "Sankt Gallen", "Lugano TI", "Biel", "Zurich", "Geneva")

    pool <- c(candidates, noisy)
  }

  sample(pool, n, replace = TRUE)
}
