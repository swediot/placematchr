#' Normalize City Names
#'
#' Normalizes city names for EEA countries using comprehensive rules tailored to each language/region.
#'
#' @param x Character vector of city names.
#' @param country Character string of the ISO 2-character country code (e.g. "DE", "FR", "PL").
#' @return Character vector of normalized names.
#' @examples
#' # Normalize German city names
#' # Normalize German city names
#' normalize_city(c("M\u00FCnchen", "K\u00F6ln", "Frankfurt a.M."), country = "DE")
#'
#' # Normalize Swiss city names
#' normalize_city(c("Z\u00FCrich", "Gen\u00E8ve", "Basel-Stadt"), country = "CH")
#' @export
normalize_city <- function(x, country = "DE") {
  country <- toupper(country)

  if (country == "DE") {
    return(normalize_city_de(x))
  }
  if (country == "CH") {
    return(normalize_city_ch(x))
  }
  if (country == "AT") {
    return(normalize_city_at(x))
  }
  if (country == "LI") {
    return(normalize_city_de(x))
  }

  if (country %in% c("FR", "BE", "LU")) {
    return(normalize_city_fr_base(x, country))
  }
  if (country %in% c("IT")) {
    return(normalize_city_it(x))
  }
  if (country %in% c("ES")) {
    return(normalize_city_es(x))
  }
  if (country %in% c("PT")) {
    return(normalize_city_pt(x))
  }

  if (country %in% c("NL")) {
    return(normalize_city_nl(x))
  }

  if (country %in% c("DK", "NO", "SE")) {
    return(normalize_city_scandi(x, country))
  }
  if (country %in% c("FI")) {
    return(normalize_city_fi(x))
  }
  if (country %in% c("IS")) {
    return(normalize_city_is(x))
  }

  if (country %in% c("PL")) {
    return(normalize_city_pl(x))
  }
  if (country %in% c("CZ", "SK")) {
    return(normalize_city_cz_sk(x, country))
  }
  if (country %in% c("HU")) {
    return(normalize_city_hu(x))
  }
  if (country %in% c("RO", "MD")) {
    return(normalize_city_ro(x))
  }

  if (country %in% c("HR", "SI", "BG", "MK")) {
    return(normalize_city_slavic_south(x, country))
  }
  if (country %in% c("EE", "LV", "LT")) {
    return(normalize_city_baltic(x, country))
  }
  if (country %in% c("EL", "CY")) {
    return(normalize_city_greek(x))
  }

  return(normalize_city_generic(x))
}

# --- UTILITIES ---

clean_basic <- function(x) {
  x <- enc2utf8(x)
  x <- tolower(x)
  x <- trimws(x)
  # Basic Latin replacements
  x <- gsub("\u00E0|\u00E1|\u00E2|\u00E3|\u00E5|\u0101|\u0103|\u0105", "a", x)
  x <- gsub("\u00E8|\u00E9|\u00EA|\u00EB|\u0113|\u0119|\u011B", "e", x)
  x <- gsub("\u00EC|\u00ED|\u00EE|\u00EF|\u012B", "i", x)
  x <- gsub("\u00F2|\u00F3|\u00F4|\u00F5|\u00F8|\u014D", "o", x)
  x <- gsub("\u00F9|\u00FA|\u00FB|\u00FC|\u016B|\u016F", "u", x)
  x <- gsub("\u00FD|\u00FF", "y", x)
  x <- gsub("\u00E7|\u0107|\u010D", "c", x)
  x <- gsub("\u00F1|\u0144|\u0148", "n", x)
  x <- gsub("\u015B|\u0161|\u015F", "s", x)
  x <- gsub("\u017A|\u017C|\u017E", "z", x)
  x <- gsub("\u0142", "l", x)
  x <- gsub("\u0111", "d", x)

  x <- gsub("\\(.*?\\)", "", x, perl = TRUE)
  x <- gsub("\\[.*?\\]", "", x, perl = TRUE)
  # NOTE: We do NOT remove numbers here anymore to allow "Paris 1er" logic.
  x <- gsub("(?:(?![0-9/&-])[[:punct:]])+", " ", x, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  x <- trimws(x)
  return(x)
}


# --- GERMAN (DE) ---
normalize_city_de <- function(x) {
  x <- enc2utf8(x)

  # 0. Critical Pre-Clean Overrides
  # Must handle "Frankfurt (Oder)" before parens are stripped by clean_basic
  x <- gsub("frankfurt\\s*\\(oder\\)", "frankfurt oder", x, ignore.case = TRUE)
  x <- gsub("frankfurt\\s*an\\s*der\\s*oder", "frankfurt oder", x, ignore.case = TRUE)

  # Exonyms (English -> German)
  x <- gsub("^munich.*", "muenchen", x, ignore.case = TRUE)
  x <- gsub("^cologne.*", "koeln", x, ignore.case = TRUE)
  x <- gsub("^nuremberg.*", "nuernberg", x, ignore.case = TRUE)
  x <- gsub("^hanover.*", "hannover", x, ignore.case = TRUE)
  x <- gsub("^brunswick.*", "braunschweig", x, ignore.case = TRUE)
  x <- gsub("^frankfort.*", "frankfurt", x, ignore.case = TRUE)

  # Umlaut Standardization (Crucial for matching u/ue variants)
  # Umlaut Standardization (Crucial for matching u/ue variants)
  x <- gsub("\u00E4", "ae", x)
  x <- gsub("\u00F6", "oe", x)
  x <- gsub("\u00FC", "ue", x)
  x <- gsub("\u00DF", "ss", x)

  # Handle "b." and "b.Name" (no space)
  # Replace "b." followed by anything with "bei " to standardize
  x <- gsub("\\bb\\.", "bei ", x, perl = TRUE)

  x <- tolower(x)
  x <- clean_basic(x)

  # 1. Abbreviations
  x <- gsub("\\bffm\\b", "frankfurt am main", x, perl = TRUE)

  # 3. Admin & Noise
  # "St", "Stadt", "Mkt" etc.
  x <- gsub("\\b(landkreis|kreis|stadt|gem\\.|gemeinde|markt|samtgemeinde|verbandsgemeinde|amt|bezirk|freie|hansestadt|landeshauptstadt)\\b", "", x, perl = TRUE)
  x <- gsub("\\b(bad )", "bad ", x)
  x <- gsub("\\bD\\.", "dorf ", x, ignore.case = TRUE)
  x <- gsub("\\bB\\.", "bach ", x, ignore.case = TRUE)
  x <- gsub("\\bst\\.?\\s+", "sankt ", x, perl = TRUE)

  # Suburb Suffix Stripping

  orig_x <- x
  x <- gsub("\\b(bei|am|an|im|in|vor|ob|der|die|das|und)\\b.*", "", x, perl = TRUE)

  # Restore criticals (Frankfurt)
  # If it became "frankfurt" but was originally "frankfurt oder", restore it.
  mask_oder <- grepl("frankfurt.*oder", orig_x)
  x[mask_oder] <- "frankfurt oder"

  mask_main <- grepl("frankfurt.*main", orig_x) | (x == "frankfurt" & !mask_oder)
  x[mask_main] <- "frankfurt am main"

  # 4. Major Cities & Districts
  # Berlin
  x <- gsub("^(?:berlin[ -]?)?(?:mitte|friedrichshain|kreuzberg|pankow|charlottenburg|wilmersdorf|spandau|steglitz|zehlendorf|tempelhof|schoeneberg|neukoelln|treptow|koepenick|marzahn|hellersdorf|reinickendorf|lichtenberg)$", "berlin", x, perl = TRUE)

  # Hamburg
  x <- gsub("^hamburg[ -]?(mitte|altona|eimsbuettel|nord|wandsbek|bergedorf|harburg)$", "hamburg", x, perl = TRUE)

  # Munich (M\u00FCnchen)
  x <- gsub("^muenchen[ -]?(altstadt|lehel|ludwigsvorstadt|isarvorstadt|maxvorstadt|schwabing|au|haidhausen|sendling|binn|moosach|milbertshofen|am hart|schwabing|freimann|bogenhausen|perlach|trudering|riem|ramersdorf|obergiesing|fasangarten|untergiesing|harlaching|thalkirchen|obersendling|forstenried|fuerstenried|solln|hadern|pasing|obermenzing|aubing|lochhausen|langwied|allach|untermenzing|feldmoching|hasenbergl|laim)$", "muenchen", x, perl = TRUE)

  # Cologne (K\u00F6ln)
  x <- gsub("^koeln[ -]?(innenstadt|rodenkirchen|lindenthal|ehrenfeld|nippes|chorweiler|porz|kalk|muelheim)$", "koeln", x, perl = TRUE)

  # Frankfurt am Main override
  x <- gsub("^(?:frankfurt[ -]?)?(?:mitte|innenstadt|bornheim|westend|nordend|ostend|sachsenhausen|gallus|bockenheim|riedberg|hoechst|niederrad|schwanheim)$", "frankfurt am main", x, perl = TRUE)

  # Stuttgart
  x <- gsub("^stuttgart[ -]?(mitte|nord|ost|sued|west|bad cannstatt|birkach|botnang|degerloch|feuerbach|hedelfingen|moehringen|muehlhausen|muenster|obertuerkheim|plieningen|sillenbuch|stammheim|untertuerkheim|vaihingen|wangen|weilimdorf|zuffenhausen)$", "stuttgart", x, perl = TRUE)

  # D\u00FCsseldorf
  x <- gsub("^duesseldorf[ -]?(stadtbezirk \\d+|altstadt|carlstadt|derendorf|golzheim|pemelfort|unterbilk|bilk|oberkassel|heerdt|lierenfeld|eller|benrath|wersten)$", "duesseldorf", x, perl = TRUE)

  # Dortmund
  x <- gsub("^dortmund[ -]?(innenstadt|nord|ost|west|eving|scharnhorst|brackel|aplerbeck|hoerde|hombruch|luetgendortmund|huckarde|mengede)$", "dortmund", x, perl = TRUE)

  # Essen
  x <- gsub("^essen[ -]?(stadtbezirk \\d+|mitte|nord|west|sued|ost|borbeck|altenessen|karnap|kray|steele)$", "essen", x, perl = TRUE)

  # Leipzig
  x <- gsub("^leipzig[ -]?(mitte|nord|ost|sued|west|alt-west|nordost|nordwest|suedost|suedwest)$", "leipzig", x, perl = TRUE)

  x <- gsub("[0-9]+", "", x, perl = TRUE)
  x <- trimws(x)
  return(x)
}


# --- AUSTRIA (AT) ---
normalize_city_at <- function(x) {
  x <- gsub("^vienna.*", "wien", x, ignore.case = TRUE) # Exonym
  x <- normalize_city_de(x)

  # "innere stadt" might become "innere" due to DE 'stadt' removal
  vienna_districts <- c("innere( stadt)?", "leopoldstadt", "landstrasse", "wieden", "margareten", "mariahilf", "neubau", "josefstadt", "alsergrund", "favoriten", "simmering", "meidling", "hietzing", "penzing", "rudolfsheim-fuenfhaus", "ottakring", "hernals", "waehring", "doebling", "brigittenau", "floridsdorf", "donaustadt", "liesing")
  regex_vienna <- paste0("^(?:wien[ -]?)?(?:", paste(vienna_districts, collapse = "|"), ")$")
  x <- gsub(regex_vienna, "wien", x, perl = TRUE)

  graz_districts <- c("innere( stadt)?", "st leonhard", "geidorf", "lend", "gries", "jakomini", "liebenau", "st peter", "waltendorf", "ries", "mariatrost", "andritz", "goesting", "eggneberg", "wetzelsdorf", "strassgang", "puntigam")
  regex_graz <- paste0("^(?:graz[ -]?)?(?:", paste(graz_districts, collapse = "|"), ")$")
  x <- gsub(regex_graz, "graz", x, perl = TRUE)

  # M\u00F6dling bei Wien
  x <- gsub("^moedling.*", "moedling", x, perl = TRUE)
  # Klosterneuburg
  x <- gsub("^klosterneuburg.*", "klosterneuburg", x, perl = TRUE)

  x <- gsub("[0-9]+", "", x, perl = TRUE)
  return(trimws(x))
}


# --- SWITZERLAND (CH) ---
normalize_city_ch <- function(x) {
  x <- gsub("\u00E4", "ae", x)
  x <- gsub("\u00F6", "oe", x)
  x <- gsub("\u00FC", "ue", x)
  x <- clean_basic(x)
  x <- gsub("\\b(gemeinde|commune|comune|vischnanca)\\b", "", x, perl = TRUE)
  x <- gsub("\\b(bei|am|an|sur|scuol)\\b", "", x, perl = TRUE)
  x <- gsub("\\bst\\.?\\s+", "sankt ", x, perl = TRUE)
  x <- gsub("\\bsaint[ -]", "sankt ", x, perl = TRUE)

  x <- gsub("^zuerich[ -]?(kreis \\d+|city|altstadt|wiedikon|aussersihl|industriequartier|unterstrass|oberstrass|fluntern|hottingen|hirslanden|riesbach|seefeld|muhlebach|weetzikon|witikon|schwamendingen|oerlikon|seebach|affoltern|leimbach)$", "zuerich", x, perl = TRUE)
  x <- gsub("^zurich[ -]?(kreis \\d+|city|altstadt|wiedikon|aussersihl|industriequartier|unterstrass|oberstrass|fluntern|hottingen|hirslanden|riesbach|seefeld|muhlebach|weetzikon|witikon|schwamendingen|oerlikon|seebach|affoltern|leimbach)$", "zuerich", x, perl = TRUE)

  x <- gsub("^geneve[ -]?(cite|plainpalais|eaux-vives|petit-saconnex)$", "geneve", x, perl = TRUE)

  x <- gsub("^basel[ -]?(grossbasel|kleinbasel|vorstaedte|kleinhueningen|kleinhuningen)$", "basel", x, perl = TRUE)

  x <- gsub("^bern[ -]?(innere stadt|laenggasse|felsenau|mattenhof|monbijou|kirchenfeld|schosshalde|breitenrain|lorraine|buempliz|bumpliz|oberbottigen)$", "bern", x, perl = TRUE)

  x <- gsub("[0-9]+", "", x, perl = TRUE)
  x <- trimws(x)
  return(x)
}


# --- FRENCH (FR, BE, LU) ---
normalize_city_fr_base <- function(x, country) {
  x <- clean_basic(x)

  x <- gsub("\\b[ld]'", "", x, perl = TRUE)
  x <- gsub("-", " ", x)
  x <- gsub("\\bst[ .-]?", "saint ", x, perl = TRUE)
  x <- gsub("\\bsainte[ .-]?", "sainte ", x, perl = TRUE)

  x <- gsub("\\b(commune|ville|arrondissement|cedex)\\b", "", x, perl = TRUE)
  # Removing suffix locators
  # E.g. "Champs sur Marne" -> "Champs". Both input and reference LAU will be reduced.
  # This matches the 'make it robust' requirement.
  x <- gsub("\\b(sur|les|le|la|de|du|des|en|aux)\\b.*", "", x, perl = TRUE)

  if (country == "FR") {
    # Exonyms for FR
    x <- gsub("^dunkirk.*", "dunkerque", x, perl = TRUE)

    x <- gsub("^paris.*", "paris", x, perl = TRUE)
    x <- gsub("^lyon.*", "lyon", x, perl = TRUE)
    x <- gsub("^marseille.*", "marseille", x, perl = TRUE)
    x <- gsub("^toulouse.*", "toulouse", x, perl = TRUE)
    x <- gsub("^nice.*", "nice", x, perl = TRUE)
    x <- gsub("^nantes.*", "nantes", x, perl = TRUE)
  }

  if (country == "BE") {
    # Exonyms for BE
    x <- gsub("^brussels.*", "bruxelles", x, perl = TRUE)
    x <- gsub("^antwerp.*", "antwerpen", x, perl = TRUE)
    x <- gsub("^ghent.*", "gent", x, perl = TRUE)
    x <- gsub("^bruges.*", "brugge", x, perl = TRUE)
    x <- gsub("^liege.*", "liege", x, perl = TRUE)
    x <- gsub("^louvain.*", "leuven", x, perl = TRUE)

    x <- gsub("^bruxelles.*", "bruxelles", x, perl = TRUE)
  }

  x <- gsub("[0-9]+", "", x, perl = TRUE)
  x <- trimws(x)
  return(x)
}


# --- ITALIAN (IT) ---
normalize_city_it <- function(x) {
  x <- clean_basic(x)

  # Exonyms
  x <- gsub("^rome.*", "roma", x, perl = TRUE)
  x <- gsub("^milan.*", "milano", x, perl = TRUE)
  x <- gsub("^naples.*", "napoli", x, perl = TRUE)
  x <- gsub("^turin.*", "torino", x, perl = TRUE)
  x <- gsub("^florence.*", "firenze", x, perl = TRUE)
  x <- gsub("^venice.*", "venezia", x, perl = TRUE)
  x <- gsub("^genoa.*", "genova", x, perl = TRUE)
  x <- gsub("^padua.*", "padova", x, perl = TRUE)

  x <- gsub("\\b(santa|santo)\\b", "san", x, perl = TRUE)
  x <- gsub("\\bs\\.\\s*", "san ", x, perl = TRUE)
  x <- gsub("\\b(comune di|citta di|provincia di|municipio)\\b", "", x, perl = TRUE)
  x <- gsub("\\b(di|del|della|dei|dalle|da|su)\\b.*", "", x, perl = TRUE)

  x <- gsub("^roma.*", "roma", x, perl = TRUE)
  x <- gsub("^milano.*", "milano", x, perl = TRUE)
  x <- gsub("^napoli.*", "napoli", x, perl = TRUE)
  x <- gsub("^torino.*", "torino", x, perl = TRUE)

  x <- gsub("[0-9]+", "", x, perl = TRUE)
  x <- trimws(x)
  return(x)
}


# --- SPANISH (ES) ---
normalize_city_es <- function(x) {
  # Inversion BEFORE clean_basic to preserve commas
  # e.g. "Rozas de Madrid, Las"
  x <- gsub("^(.*),\\s*(el|la|los|las|as|os)$", "\\2 \\1", x, ignore.case = TRUE, perl = TRUE)

  x <- clean_basic(x)

  # Exonyms
  x <- gsub("^seville.*", "sevilla", x, perl = TRUE)
  x <- gsub("^saragossa.*", "zaragoza", x, perl = TRUE)
  x <- gsub("^majorca.*", "mallorca", x, perl = TRUE)
  x <- gsub("^pampeluna.*", "pamplona", x, perl = TRUE)

  # Catalan L' handling
  x <- gsub("\\b[ld]'", "", x, perl = TRUE)

  x <- gsub("\\b(san|santa|santo)\\b", "san", x, perl = TRUE)
  x <- gsub("\\b(municipio|ciudad|ayuntamiento|distrito)\\b", "", x, perl = TRUE)
  x <- gsub("\\b(de|la|el|los|las|del)\\b", "", x, perl = TRUE)

  # Suburb Suffixes
  x <- gsub("\\b(de madrid|de barcelona|de valencia|de sevilla|de llobregat)\\b", "", x, perl = TRUE)

  x <- gsub("^madrid.*", "madrid", x, perl = TRUE)
  x <- gsub("^barcelona.*", "barcelona", x, perl = TRUE)
  x <- gsub("^valencia.*", "valencia", x, perl = TRUE)
  x <- gsub("^sevilla.*", "sevilla", x, perl = TRUE)

  x <- gsub("[0-9]+", "", x, perl = TRUE)
  x <- trimws(x)
  return(x)
}


# --- POLISH (PL) ---
normalize_city_pl <- function(x) {
  x <- clean_basic(x)
  # Exonyms
  x <- gsub("^warsow.*", "warszawa", x, perl = TRUE)
  x <- gsub("^warsaw.*", "warszawa", x, perl = TRUE)
  x <- gsub("^cracow.*", "krakow", x, perl = TRUE)

  x <- gsub("\\b(m\\.|miasto|gmina|powiat|wojewodztwo|dzielnica)\\b", "", x, perl = TRUE)

  # "Stare miasto" -> "stare" after "miasto" removal.
  x <- gsub("^warszawa.*", "warszawa", x, perl = TRUE)
  x <- gsub("^krakow.*", "krakow", x, perl = TRUE)
  x <- gsub("^lodz.*", "lodz", x, perl = TRUE)
  x <- gsub("^wroclaw.*", "wroclaw", x, perl = TRUE)

  x <- gsub("[0-9]+", "", x, perl = TRUE)
  x <- trimws(x)
  return(x)
}

# --- DUTCH (NL) ---
normalize_city_nl <- function(x) {
  x <- clean_basic(x)
  x <- gsub("\\b(gemeente|stad|stadsdeel)\\b", "", x, perl = TRUE)
  x <- gsub("\\b(aan de|aan den|op)\\b", "", x, perl = TRUE)
  x <- gsub("\\b(s[- ]?gravenhage)\\b", "den haag", x, perl = TRUE)

  x <- gsub("^amsterdam.*", "amsterdam", x, perl = TRUE)
  x <- gsub("^rotterdam.*", "rotterdam", x, perl = TRUE)
  x <- gsub("^den haag.*", "den haag", x, perl = TRUE)

  return(trimws(x))
}

# --- SCANDINAVIAN (DK, NO, SE) ---
normalize_city_scandi <- function(x, country) {
  x <- enc2utf8(x)
  x <- tolower(x)

  # Exonyms (Pre-clean)
  if (country == "DK") x <- gsub("^copenhagen", "koebenhavn", x, ignore.case = TRUE)
  if (country == "SE") x <- gsub("^gothenburg", "goeteborg", x, ignore.case = TRUE)

  x <- gsub("\u00E5", "aa", x)
  x <- gsub("\u00E4|\u00E6", "ae", x)
  x <- gsub("\u00F6|\u00F8", "oe", x)
  x <- clean_basic(x)
  x <- gsub("\\b(kommune|kommun|stad|by|stadsdelsomraade)\\b", "", x, perl = TRUE)

  if (country == "DK") {
    x <- gsub("koe?benhavn.*", "koebenhavn", x, perl = TRUE)
    x <- gsub("^aarhus[ -]?(midt|nord|syd|vest|oe)$", "aarhus", x, perl = TRUE)
  }

  if (country == "SE") {
    x <- gsub("^stockholm.*", "stockholm", x, perl = TRUE)
    x <- gsub("^goeteborg.*", "goeteborg", x, perl = TRUE)
    x <- gsub("^malmoe.*", "malmoe", x, perl = TRUE)
  }

  if (country == "NO") {
    x <- gsub("^oslo.*", "oslo", x, perl = TRUE)
  }

  x <- gsub("[0-9]+", "", x, perl = TRUE)
  x <- trimws(x)
  return(x)
}


# --- Others (stubs using generic) ---
normalize_city_fi <- function(x) {
  x <- gsub("\u00E4", "ae", x)
  x <- gsub("\u00F6", "oe", x)
  x <- clean_basic(x)
  x <- gsub("\\b(kaupunki|kunta|l\u00E4\u00E4ni)\\b", "", x, perl = TRUE)
  x <- gsub("^helsinki.*", "helsinki", x, perl = TRUE)
  x <- gsub("[0-9]+", "", x, perl = TRUE)
  return(trimws(x))
}

normalize_city_pt <- function(x) {
  x <- clean_basic(x)
  x <- gsub("^lisbon.*", "lisboa", x, perl = TRUE) # Exonym
  x <- gsub("\\b(municipio|vila|cidade|freguesia|uniao)\\b", "", x, perl = TRUE)
  x <- gsub("\\b(de|da|do|dos|das)\\b", "", x, perl = TRUE)
  x <- gsub("^lisboa.*", "lisboa", x, perl = TRUE)
  x <- gsub("^porto.*", "porto", x, perl = TRUE)
  x <- gsub("[0-9]+", "", x, perl = TRUE)
  return(trimws(x))
}

normalize_city_cz_sk <- function(x, country) {
  x <- clean_basic(x)
  x <- gsub("\\b(mesto|obec|okres)\\b", "", x, perl = TRUE)
  if (country == "CZ") {
    x <- gsub("^prague.*", "praha", x, perl = TRUE) # Exonym
    x <- gsub("^praha.*", "praha", x, perl = TRUE)
    x <- gsub("^brno.*", "brno", x, perl = TRUE)
    x <- gsub("^ostrava.*", "ostrava", x, perl = TRUE)
  }
  if (country == "SK") x <- gsub("bratislava.*", "bratislava", x, perl = TRUE)
  x <- gsub("[0-9]+", "", x, perl = TRUE)
  return(trimws(x))
}

normalize_city_hu <- function(x) {
  x <- clean_basic(x)
  x <- gsub("\\b(megye|varos|kozseg|keruelet)\\b", "", x, perl = TRUE)
  x <- gsub("^budapest.*", "budapest", x, perl = TRUE)
  x <- gsub("[0-9]+", "", x, perl = TRUE)
  return(trimws(x))
}

normalize_city_ro <- function(x) {
  x <- clean_basic(x)
  x <- gsub("^bucharest.*", "bucuresti", x, perl = TRUE) # Exonym
  x <- gsub("\\b(municipiul|orasul|judetul|comuna|sector)\\b", "", x, perl = TRUE)
  x <- gsub("^bucuresti.*", "bucuresti", x, perl = TRUE)
  x <- gsub("[0-9]+", "", x, perl = TRUE)
  return(trimws(x))
}

normalize_city_slavic_south <- function(x, country) {
  x <- clean_basic(x)
  x <- gsub("\\b(grad|opcina|obcina)\\b", "", x, perl = TRUE)
  if (country == "HR") x <- gsub("^zagreb.*", "zagreb", x, perl = TRUE)
  if (country == "SI") x <- gsub("^ljubljana.*", "ljubljana", x, perl = TRUE)
  if (country == "BG") x <- gsub("^sofia.*", "sofia", x, perl = TRUE)
  x <- gsub("[0-9]+", "", x, perl = TRUE)
  return(trimws(x))
}

normalize_city_baltic <- function(x, country) {
  x <- clean_basic(x)
  if (country == "EE") {
    x <- gsub("\\b(vald|linn)\\b", "", x, perl = TRUE)
    x <- gsub("^tallinn.*", "tallinn", x, perl = TRUE)
  }
  if (country == "LV") {
    x <- gsub("\\b(pilseta|novads|pagasts)\\b", "", x, perl = TRUE)
    x <- gsub("^riga.*", "riga", x, perl = TRUE)
  }
  if (country == "LT") {
    x <- gsub("\\b(miestas|savivaldybe|rajono)\\b", "", x, perl = TRUE)
    x <- gsub("^vilnius.*", "vilnius", x, perl = TRUE)
    x <- gsub("^kaunas.*", "kaunas", x, perl = TRUE)
  }
  x <- gsub("[0-9]+", "", x, perl = TRUE)
  return(trimws(x))
}

# --- GREEK (EL/GR) ---
normalize_city_greek <- function(x) {
  x <- clean_basic(x)
  x <- gsub("^athens", "athina", x, perl = TRUE) # Exonym

  x <- gsub("\\b(dimos|koinotita)\\b", "", x, perl = TRUE)
  x <- gsub("^athina.*", "athina", x, perl = TRUE)
  x <- gsub("^thessaloniki.*", "thessaloniki", x, perl = TRUE)
  x <- gsub("[0-9]+", "", x, perl = TRUE)
  return(trimws(x))
}

normalize_city_is <- function(x) {
  x <- gsub("\u00F0", "d", x)
  x <- gsub("\u00FE", "th", x)
  x <- gsub("\u00E6", "ae", x)
  x <- gsub("\u00F6", "oe", x)
  x <- clean_basic(x)
  x <- gsub("reykjavik.*", "reykjavik", x, perl = TRUE)
  x <- gsub("[0-9]+", "", x, perl = TRUE)
  return(trimws(x))
}

normalize_city_generic <- function(x) {
  x <- clean_basic(x)
  x <- gsub("[0-9]+", "", x, perl = TRUE)
  return(trimws(x))
}
