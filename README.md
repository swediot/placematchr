# placematchr: City to NUTS Region Matching

`placematchr` is an R package designed to normalize city names and map them to NUTS 3 regions for **Germany (DE)** and **Switzerland (CH)**. 

It solves the common problem of inconsistent city naming in survey or administrative data (e.g., "Frankfurt (Oder)" vs "Frankfurt an der Oder", or "München" vs "Munich") by using a **cascading matching strategy** backed by official LAU (Local Administrative Units) data.

## Features

- **Robust Normalization**: Country-specific cleaning rules handling umlauts, dialects, suffixes, and common variations (e.g. "Frankfurt a.M." -> "frankfurt am main").
- **Cascading Logic**: Matches are prioritized to ensuring the highest quality:
    1.  **Exact NUTS Region Match**: Direct match to a major NUTS region name (e.g., "Berlin", "München").
    2.  **Exact LAU Match**: Match to a specific municipality (LAU), mapped back to its parent NUTS region.
    3.  **Fuzzy LAU Match**: String distance matching to the nearest municipality.
    4.  **Fuzzy NUTS Match**: String distance matching to the nearest NUTS region name.
- **Single Data Source**: Self-contained datasets (synthesized from official `NUTS_localadministrativeunits.xlsx`, downloaded from the [European Commission](https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts)), requiring no external file inputs during runtime.

## Usage

### 1. Matching Cities to NUTS Regions

The main function `match_city` automates the entire process:

```r
library(placematchr)

cities <- c("Berlin", "Frankfurt (Oder)", "Überlingen", "Munich (typo)")
results <- match_city(cities, country = "DE")

print(results)
#             original        city_clean                 lau_name nuts_3_id   match_type
# 1             Berlin            berlin            Berlin, Stadt     DE300 Exact (NUTS)
# 2   Frankfurt (Oder)    frankfurt oder  Frankfurt (Oder), Stadt     DE403 Exact (NUTS)
# 3         Überlingen       ueberlingen        Überlingen, Stadt     DE147  Exact (LAU)
# 4      Munich (typo)          muenchen München, Landeshauptstadt    DE212  Fuzzy (NUTS)
```

### 2. Normalization Only

If you only want to clean names without matching:

```r
normalize_city("München", country = "DE")
#> [1] "muenchen"

normalize_city("Zürich", country = "CH")
#> [1] "zuerich"
```

## Data Sources

This package bundles processed data derived from the official EU `NUTS_localadministrativeunits.xlsx` file.
- **`lau_de` / `lau_ch`**: Full lists of municipalities with their parent NUTS 3 codes.
- **`nuts_de` / `nuts_ch`**: Synthesized list of NUTS 3 regions and their representative main cities.

## License

MIT
