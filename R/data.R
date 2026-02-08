#' Local Administrative Units (LAU) Crosswalks
#'
#' Datasets containing mappings from city names to LAU codes and NUTS 3 regions for various countries.
#' The data handles string normalization and matches cities to their respective statistical regions.
#'
#' @format Data frames with varying columns depending on the country, typically including:
#' \describe{
#'   \item{lau_id}{Local Administrative Unit code}
#'   \item{lau_name}{Name of the Local Administrative Unit}
#'   \item{nuts_3_id}{NUTS 3 region code}
#'   \item{population}{Population (if available)}
#' }
#' @source Eurostat and national statistical institutes.
#' @name lau_data
NULL

#' @rdname lau_data
"lau_at"
#' @rdname lau_data
"lau_be"
#' @rdname lau_data
"lau_bg"
#' @rdname lau_data
"lau_ch"
#' @rdname lau_data
"lau_cy"
#' @rdname lau_data
"lau_cz"
#' @rdname lau_data
"lau_de"
#' @rdname lau_data
"lau_dk"
#' @rdname lau_data
"lau_ee"
#' @rdname lau_data
"lau_el"
#' @rdname lau_data
"lau_es"
#' @rdname lau_data
"lau_fi"
#' @rdname lau_data
"lau_fr"
#' @rdname lau_data
"lau_hr"
#' @rdname lau_data
"lau_hu"
#' @rdname lau_data
"lau_ie"
#' @rdname lau_data
"lau_it"
#' @rdname lau_data
"lau_li"
#' @rdname lau_data
"lau_lt"
#' @rdname lau_data
"lau_lu"
#' @rdname lau_data
"lau_lv"
#' @rdname lau_data
"lau_mk"
#' @rdname lau_data
"lau_mt"
#' @rdname lau_data
"lau_nl"
#' @rdname lau_data
"lau_no"
#' @rdname lau_data
"lau_pl"
#' @rdname lau_data
"lau_pt"
#' @rdname lau_data
"lau_ro"
#' @rdname lau_data
"lau_se"
#' @rdname lau_data
"lau_si"
#' @rdname lau_data
"lau_sk"
#' @rdname lau_data
"lau_tr"


#' NUTS 3 Region Metadata
#'
#' Metadata for NUTS 3 regions for various countries, used for hierarchical matching.
#'
#' @format Data frames with columns:
#' \describe{
#'   \item{nuts_3_id}{NUTS 3 region code}
#'   \item{nuts_3_name}{Name of the NUTS 3 region}
#' }
#' @source Eurostat
#' @name nuts_data
NULL

#' @rdname nuts_data
"nuts_at"
#' @rdname nuts_data
"nuts_be"
#' @rdname nuts_data
"nuts_bg"
#' @rdname nuts_data
"nuts_ch"
#' @rdname nuts_data
"nuts_cy"
#' @rdname nuts_data
"nuts_cz"
#' @rdname nuts_data
"nuts_de"
#' @rdname nuts_data
"nuts_dk"
#' @rdname nuts_data
"nuts_ee"
#' @rdname nuts_data
"nuts_el"
#' @rdname nuts_data
"nuts_es"
#' @rdname nuts_data
"nuts_fi"
#' @rdname nuts_data
"nuts_fr"
#' @rdname nuts_data
"nuts_hr"
#' @rdname nuts_data
"nuts_hu"
#' @rdname nuts_data
"nuts_ie"
#' @rdname nuts_data
"nuts_it"
#' @rdname nuts_data
"nuts_li"
#' @rdname nuts_data
"nuts_lt"
#' @rdname nuts_data
"nuts_lu"
#' @rdname nuts_data
"nuts_lv"
#' @rdname nuts_data
"nuts_mk"
#' @rdname nuts_data
"nuts_mt"
#' @rdname nuts_data
"nuts_nl"
#' @rdname nuts_data
"nuts_no"
#' @rdname nuts_data
"nuts_pl"
#' @rdname nuts_data
"nuts_pt"
#' @rdname nuts_data
"nuts_ro"
#' @rdname nuts_data
"nuts_se"
#' @rdname nuts_data
"nuts_si"
#' @rdname nuts_data
"nuts_sk"
#' @rdname nuts_data
"nuts_tr"
