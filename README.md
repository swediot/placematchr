
# placematchr: Robust City Normalization and NUTS Matching for Europe

`placematchr` is an R package designed to normalize city names and map them to standard **NUTS 3** and **LAU** (Local Administrative Units) codes across **32 European countries**.

It is widely used to harmonize messy geographical data (survey responses, address lists) with official regional identifiers for analysis.

## Key Features

*   **Broad Coverage**: Supports all EU/EEA countries
*   **English Exonym Support**: Handles common English names for major cities (e.g., "Munich" matches "München", "Prague" matches "Praha", "Florence" matches "Firenze", "Cologne" matches "Köln").
*   **Robust Suburb Handling**:
    *   Normalizes suburb suffixes (e.g., "Garching b. München" -> "Garching", "Champs-sur-Marne" -> "Champs").
    *   Correctly disambiguates complex cases like "Frankfurt (Oder)" vs "Frankfurt am Main".
    *   Handles article inversions in Spanish/French (e.g., "Rozas, Las" -> "Las Rozas").
*   **Cascading Matching Logic**:
    1.  **Exact NUTS Match**: Checks if the normalized input matches a NUTS 3 region name directly (e.g., "Berlin").
    2.  **Exact LAU Match**: Checks if it matches a Local Administrative Unit (e.g., "Garching").
    3.  **Fuzzy LAU Match**: Fuzzy string matching for slight variations (e.g., typos).
    4.  **Fuzzy NUTS Match**: Fallback to fuzzy NUTS identification.
*   **Zero-Config Data**: All necessary geographical data (NUTS/LAU tables) is processed and bundled with the package.

## Usage

### Basic Matching
```r
library(placematchr)

# Match a single city in Germany
match_city("Munich", country = "DE")
# Returns data frame with NUTS_ID: DE212, Name: München, Landeshauptstadt

# Match a list of cities in Italy
cities <- c("Rome", "Milan", "Naples", "Venice")
match_city(cities, country = "IT")
```

### Handling Suburbs and Variations
```r
# Input: "Garching b. München" (Suburb)
# Result: Matches "Garching" (LAU). Mapped to Munich District (NUTS).
match_city("Garching b. München", country = "DE")

# Input: "Champs-sur-Marne" (French Suburb)
# Result: Matches "Champs" (LAU).
match_city("Champs-sur-Marne", country = "FR")
```

## Supported Countries
BE, BG, CZ, DK, DE, EE, IE, EL, ES, FR, HR, IT, CY, LV, LT, LU, HU, MT, NL, AT, PL, PT, RO, SI, SK, FI, SE, LI, NO, CH, MK, TR, UK.

## License
MIT
