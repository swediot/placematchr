#' Normalize City Names
#'
#' Normalizes city names for Germany (DE) and Switzerland (CH) using comprehensive rules
#' extracted from project scripts.
#'
#' @param x Character vector of city names.
#' @param country Character string "DE" or "CH".
#' @return Character vector of normalized names.
#' @export
normalize_city <- function(x, country = "DE") {
  if (country == "DE") {
    return(normalize_city_de(x))
  } else if (country == "CH") {
    return(normalize_city_ch(x))
  } else {
    stop("Country must be 'DE' or 'CH'")
  }
}
normalize_city_de <- function(x) {
  stopifnot(is.character(x))
  x <- iconv(x, to = "UTF-8")
  x <- tolower(x)
  x <- trimws(x)
  
  # Common abbreviations and special cases
  x <- gsub("\\bffm\\b", "frankfurt am main", x, perl = TRUE)
  x <- gsub("frankfurt\\s*\\(oder\\)", "frankfurt oder", x, ignore.case=TRUE) # Protect before bracket removal
  x <- gsub("frankfurt\\s*an\\s*der\\s*oder", "frankfurt oder", x, ignore.case=TRUE)
  
  # Umlaut handling
  x <- gsub("ä", "ae", x)
  x <- gsub("ö", "oe", x)
  x <- gsub("ü", "ue", x)
  x <- gsub("ß", "ss", x)
  
  # Accents
  x <- gsub("à|á|â|ã|å|ā", "a", x)
  x <- gsub("è|é|ê|ë|ē", "e", x)
  x <- gsub("ì|í|î|ï|ī", "i", x)
  x <- gsub("ò|ó|ô|õ|ö|ø|ō", "o", x)
  x <- gsub("ù|ú|û|ü|ū", "u", x)
  x <- gsub("ç", "c", x)
  x <- gsub("ñ", "n", x)
  
  
  # Noise removal (Corrected to use the pipe operator '|>')
  x <- gsub("\\b\\S*(straße|strasse)\\S*\\b", "", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\b\\S*(str.)\\S*\\b", "", x, ignore.case = TRUE, perl = TRUE)

  # Removal of content in brackets
  x <- gsub("\\(.*?\\)", "", x, perl = TRUE)
  x <- gsub("\\[.*?\\]", "", x, perl = TRUE)
  x <- gsub("\\{.*?\\}", "", x, perl = TRUE)

  
  x <- gsub("[0-9]+", "", x, perl = TRUE)
  x <- gsub("(?:(?![/&])[[:punct:]])+", " ", x, perl = TRUE)
  x <- gsub("/", " / ", x, perl = TRUE)
  
  # Remove everything after 'auch', 'und', '/', or '&'
  x <- gsub("(auch|und| /|&)\\s+.*", "", x, ignore.case = TRUE, perl = TRUE)
  
  # Remove the standalone 'auch', 'und', '/', and '&'
  x <- gsub("\\b(auch|und)\\b| /|&", "", x, ignore.case = TRUE, perl = TRUE)
  
  x <- gsub("\\b(homeoffice|standort|standorte|geschlossen|umgezogen|etc|verlegt|bundesweit|deutschlandweit|deutschland|straße|all|alland|alle|büros|büro|ort|orte|allgemein|sitz|landeshauptstadt|stadt|Kupferstadt|Hanse- und Universitätsstadt|Universitätsstadt|Hansestadt|Raiffeisengemeinde|region|selb|goldcoats|europaweit)\\b",
            "", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\s+", " ", x, perl = TRUE)
  
  # Remove common region/office keywords (case-insensitive with (?i))
  x <- gsub("(?i)\\b(raum|region|gebiet|ueberregional|bundesgebiet|weltweit|international)\\b", "", x, perl=TRUE)
  x <- gsub("(?i)\\b(keine angabe|keine angaben|spielt keine rolle)\\b", "", x, perl=TRUE)
  x <- gsub("(?i)\\b(home office|head office|hauptsitz|hauptverwaltung|niederlassung|filiale|filialen|zentrale)\\b", "", x, perl=TRUE)
  
  # Remove "bei" (and abbreviation "b.") as standalone words
  x <- gsub("(?i)\\bbei\\b|\\bb\\.?\\b", "", x, perl=TRUE)
  
  # Remove leading single-letter codes like "X-"
  x <- gsub("^[A-Za-z]-\\s*", "", x)
  
  # Remove state abbreviations if present as standalone (example for NRW, BW, BY, etc.)
  x <- gsub("(?i)\\b(NRW|Bayern|BY|BW|Hessen|Hessen)\\b", "", x, perl=TRUE)
  
  
  x <- trimws(x)

  x <- gsub("st ", "sankt ", x)
  
  # --- Extra normalization helpers ---
  x <- gsub("\\bst\\.?\\s+", "sankt ", x, perl=TRUE)
  x <- gsub("\\bfrankfurt[ /-]?m\\b", "frankfurt am main", x, perl=TRUE)
  x <- gsub("\\bfrankfurt[ /-]?a\\.? ?m\\.?\\b", "frankfurt am main", x, perl=TRUE)
  x <- gsub("\\bkoeln[ /-]?bonn\\b|\\bcologne[ /-]?bonn\\b", "koeln", x, perl=TRUE)
  x <- gsub("\\bmunich\\b", "muenchen", x, perl=TRUE)
  x <- gsub("\\bcologne\\b", "koeln", x, perl=TRUE)
  x <- gsub("\\bhanover\\b", "hannover", x, perl=TRUE)
  
  # Strip administrative wrappers seen inside names
  x <- gsub("\\b(landkreis|kreis|stadtbezirk|bezirk|amt|verbandsgemeinde|samtgemeinde|verwaltungsverband|verwaltungsgemeinschaft|kreisfreie stadt|gemeindefreies gebiet)\\b", "", x, perl=TRUE)
  
  # Berlin boroughs -> berlin
  # We require that if a borough match is found, it accounts for the whole string or isn't followed by distinguishing location chars like "/"
  # Negative lookahead for / or - (unless standard separators) is tricky in one go. 
  # Simpler: Don't match if it looks like "Lichtenberg/"
  # Berlin boroughs -> berlin
  # Group 1: Unique boroughs (can stand alone)
  x <- gsub("^(?:berlin[-, ]*)?(charlottenburg|wilmersdorf|pankow|friedrichshain|kreuzberg|tempelhof|sch[oö]neberg|spandau|steglitz|zehlendorf|treptow|k[oö]penick|marzahn|hellersdorf|reinickendorf|neukoelln|neukölln)$", "berlin", x, ignore.case=TRUE, perl=TRUE)
  
  # Group 2: Ambiguous boroughs (require "Berlin" prefix or context to match)
  # e.g. "Lichtenberg" exists elsewhere. "Mitte" exists everywhere.
  x <- gsub("^berlin[ -](lichtenberg|mitte).*$", "berlin", x, ignore.case=TRUE, perl=TRUE)
  
  # Also catch "Berlin-X" for Group 1 explicitly if trailing chars exist
  x <- gsub("^berlin[ -](charlottenburg|wilmersdorf|pankow|friedrichshain|kreuzberg|tempelhof|sch[oö]neberg|spandau|steglitz|zehlendorf|treptow|k[oö]penick|marzahn|hellersdorf|reinickendorf|neukoelln|neukölln).*$", "berlin", x, ignore.case=TRUE, perl=TRUE)
  
  
  # Hamburg boroughs -> hamburg
  x <- gsub(".*\\b(altona|eimsb[uü]ttel|wandsbek|harburg|bergedorf|hamburg[- ]mitte|hamburg[- ]nord|st[ .-]?pauli|st[ .-]?georg|blankenese|winterhude|barmbek)\\b.*", "hamburg", x, perl=TRUE)
  
  # München boroughs -> muenchen
  x <- gsub(".*\\b(schwabing|sendling|maxvorstadt|moosach|trudering|bogenhausen|neuhausen|ramersdorf|giesing|untergiesing|obergiesing|ha[ie]dhausen|pasing|perlach|feldmoching|hasenbergl)\\b.*", "muenchen", x, perl=TRUE)
  
  # Frankfurt a. M. boroughs -> frankfurt am main
  x <- gsub(".*\\b(sachsenhausen|bornheim|bockenheim|niederrad|h[öo]chst|gallus|riedberg|fechenheim|griesheim|unterliederbach|oberrad|eschersheim|westend|nordend|seckbach|preungesheim|r[öo]delheim|sindlingen|sossenheim|bergen[- ]enkheim)\\b.*", "frankfurt am main", x, perl=TRUE)
  
  # Major Frankfurt fix: Do NOT match if "oder" follows
  x <- gsub("(?i).*(frankfurt am main|frankfurtammain|frankfurt\\s*a\\s*m|ffm|frnakfurt)\\b.*|(?i)\\bfrankfurt\\b(?!\\s*\\(?oder\\)?).*", "frankfurt am main", x, perl=TRUE)
  
  # Köln boroughs (safe subset) -> koeln
  x <- gsub(".*\\b(nippes|ehrenfeld|lindenthal|porz|kalk|chorweiler|rodenkirchen)\\b.*", "koeln", x, perl=TRUE)
  
  # Düsseldorf quarters -> duesseldorf
  x <- gsub(".*\\b(pempelfort|derendorf|golzheim|oberkassel|unterbilk|bilk|flingern|lohausen|heerdt|f[öo]rth?)\\b.*", "duesseldorf", x, perl=TRUE)
  
  # Stuttgart districts -> stuttgart
  x <- gsub(".*\\b(vaihingen|zuffenhausen|feuerbach|degerloch|m[öo]hringen|moehringen|untert[üu]rkheim|untertuerkheim|sillenbuch|hedelfingen|plieningen|bad cannstatt)\\b.*", "stuttgart", x, perl=TRUE)
  
  # Districts merged into larger cities
  x <- gsub(".*\\brheydt\\b.*", "moenchengladbach", x, perl=TRUE)
  x <- gsub(".*\\btravemuende\\b.*", "luebeck", x, perl=TRUE)
  
  # Rhein-Main ring (helps map suburbs frequently appearing in datasets)
  x <- gsub(".*\\b(eschborn)\\b.*", "eschborn", x, perl=TRUE)
  x <- gsub(".*\\b(neu[- ]isenburg)\\b.*", "neu-isenburg", x, perl=TRUE)
  x <- gsub(".*\\b(dreieich)\\b.*", "dreieich", x, perl=TRUE)
  x <- gsub(".*\\b(langen \\(hessen\\)|langen hessen|langen)\\b.*", "langen", x, perl=TRUE)
  x <- gsub(".*\\b(m[öo]rfelden[- ]walldorf|moerfelden[- ]walldorf)\\b.*", "moerfelden-walldorf", x, perl=TRUE)
  x <- gsub(".*\\b(raunheim)\\b.*", "raunheim", x, perl=TRUE)
  x <- gsub(".*\\b(kelkheim)\\b.*", "kelkheim", x, perl=TRUE)
  x <- gsub(".*\\b(maintal)\\b.*", "maintal", x, perl=TRUE)
  x <- gsub(".*\\b(hattersheim am main|hattersheim)\\b.*", "hattersheim am main", x, perl=TRUE)
  x <- gsub(".*\\b(k[öo]nigstein im taunus|koenigstein im taunus|koenigstein)\\b.*", "koenigstein im taunus", x, perl=TRUE)
  x <- gsub(".*\\b(kronberg im taunus|kronberg)\\b.*", "kronberg im taunus", x, perl=TRUE)
  x <- gsub(".*\\b(oberursel \\(taunus\\)|oberursel)\\b.*", "oberursel", x, perl=TRUE)
  x <- gsub(".*\\b(bad vilbel)\\b.*", "bad vilbel", x, perl=TRUE)
  x <- gsub(".*\\b(bad nauheim)\\b.*", "bad nauheim", x, perl=TRUE)
  x <- gsub(".*\\b(bad soden am taunus|bad soden)\\b.*", "bad soden am taunus", x, perl=TRUE)
  x <- gsub(".*\\b(r[üu]sselsheim am main|ruesselsheim am main)\\b.*", "ruesselsheim", x, perl=TRUE)
  x <- gsub(".*\\b(garching( bei| b\\.?| b )? ?m[üu]nchen)\\b.*", "garching bei muenchen", x, perl=TRUE)
  
  # Hamburg ring (common suburbs)
  x <- gsub(".*\\b(norderstedt)\\b.*", "norderstedt", x, perl=TRUE)
  x <- gsub(".*\\b(pinneberg)\\b.*", "pinneberg", x, perl=TRUE)
  x <- gsub(".*\\b(wedel)\\b.*", "wedel", x, perl=TRUE)
  x <- gsub(".*\\b(ahrensburg)\\b.*", "ahrensburg", x, perl=TRUE)
  x <- gsub(".*\\b(geesthacht)\\b.*", "geesthacht", x, perl=TRUE)
  x <- gsub(".*\\b(reinbek)\\b.*", "reinbek", x, perl=TRUE)
  
  # Stuttgart ring (airport/IT belt)
  x <- gsub(".*\\b(leinfelden[- ]echterdingen)\\b.*", "leinfelden-echterdingen", x, perl=TRUE)
  x <- gsub(".*\\b(filderstadt)\\b.*", "filderstadt", x, perl=TRUE)
  x <- gsub(".*\\b(boeblingen|b[öo]blingen)\\b.*", "boeblingen", x, perl=TRUE)
  x <- gsub(".*\\b(sindelfingen)\\b.*", "sindelfingen", x, perl=TRUE)
  
  # “Bad …” spa towns (frequent mismatches due to suffixes)
  x <- gsub(".*\\b(bad hersfeld)\\b.*", "bad hersfeld", x, perl=TRUE)
  x <- gsub(".*\\b(bad salzuflen)\\b.*", "bad salzuflen", x, perl=TRUE)
  x <- gsub(".*\\b(bad segeberg)\\b.*", "bad segeberg", x, perl=TRUE)
  x <- gsub(".*\\b(bad schwartau)\\b.*", "bad schwartau", x, perl=TRUE)
  x <- gsub(".*\\b(bad mergentheim)\\b.*", "bad mergentheim", x, perl=TRUE)
  x <- gsub(".*\\b(bad rappenau)\\b.*", "bad rappenau", x, perl=TRUE)
  x <- gsub(".*\\b(bad zwischenahn)\\b.*", "bad zwischenahn", x, perl=TRUE)
  x <- gsub(".*\\b(bad toelz|bad t[öo]lz)\\b.*", "bad toelz", x, perl=TRUE)
  x <- gsub(".*\\b(bad reichenhall)\\b.*", "bad reichenhall", x, perl=TRUE)
  x <- gsub(".*\\b(bad kissingen)\\b.*", "bad kissingen", x, perl=TRUE)
  x <- gsub(".*\\b(bad urach)\\b.*", "bad urach", x, perl=TRUE)
  x <- gsub(".*\\b(bad duerkheim|bad d[üu]rkheim)\\b.*", "bad duerkheim", x, perl=TRUE)
  
  # Sankt/St. towns beyond NRW block
  x <- gsub(".*\\b(sankt wendel|st[ .]?wendel)\\b.*", "sankt wendel", x, perl=TRUE)
  x <- gsub(".*\\b(sankt ingbert|st[ .]?ingbert)\\b.*", "sankt ingbert", x, perl=TRUE)
  x <- gsub(".*\\b(sankt goarshausen|st[ .]?goarshausen)\\b.*", "sankt goarshausen", x, perl=TRUE)
  x <- gsub(".*\\b(sankt goar|st[ .]?goar)\\b.*", "sankt goar", x, perl=TRUE)
  
  # Mönchengladbach abbreviations
  x <- gsub(".*(bad kreuznach).*", "bad kreuznach", x, perl=TRUE)
  x <- gsub(".*(limburg an der lahn).*", "limburg an der lahn", x, perl=TRUE)
  x <- gsub(".*(neuburg an der donau).*", "neuburg an der donau", x, perl=TRUE)
  x <- gsub(".*(bad cannstatt).*", "stuttgart", x, perl=TRUE)

  # Mönchengladbach abbreviations
  x <- gsub(".*\\bm[ .-]?gladbach\\b.*", "moenchengladbach", x, perl=TRUE)
  
  # Cologne/Bonn airport area aliases -> koeln
  x <- gsub(".*\\b(koeln[- ]bonn|k[öo]ln[- ]bonn|cologne[- ]bonn) flughafen\\b.*", "koeln", x, perl=TRUE)

  # Major German cities standardization (COMPREHENSIVE LIST) -- Robust Version
  x <- gsub("^berlin$", "berlin", x, perl = TRUE)
  x <- gsub(".*\\b(hamburg)\\b.*", "hamburg", x, perl = TRUE)
  x <- gsub(".*\\b(muenchen|münchen)\\b.*", "muenchen", x, perl = TRUE)
  x <- gsub(".*\\b(koeln|köln)\\b.*", "koeln", x, perl = TRUE)
  x <- gsub("(?i).*(frankfurt am main|frankfurtammain|frankfurt\\s*a\\s*m|ffm|frnakfurt)\\b.*|(?i)\\bfrankfurt\\b(?!\\s*\\(?oder\\)?).*", "frankfurt am main", x, perl=TRUE)
  x <- gsub(".*\\b(stuttgart)\\b.*", "stuttgart", x, perl = TRUE)
  x <- gsub(".*\\b(duesseldorf|düsseldorf)\\b.*", "duesseldorf", x, perl = TRUE)
  x <- gsub(".*\\b(leipzig)\\b.*", "leipzig", x, perl = TRUE)
  x <- gsub(".*\\b(dortmund)\\b.*", "dortmund", x, perl = TRUE)
  x <- gsub(".*\\b(essen)\\b.*", "essen", x, perl = TRUE)
  x <- gsub(".*\\b(hannover)\\b.*", "hannover", x, perl = TRUE)
  x <- gsub(".*\\b(nuernberg|nürnberg)\\b.*", "nuernberg", x, perl = TRUE)
  x <- gsub(".*\\b(bremen)\\b.*", "bremen", x, perl = TRUE)
  x <- gsub(".*\\b(duisburg)\\b.*", "duisburg", x, perl = TRUE)
  x <- gsub(".*\\b(bochum)\\b.*", "bochum", x, perl = TRUE)
  x <- gsub(".*\\b(wuppertal)\\b.*", "wuppertal", x, perl = TRUE)
  x <- gsub(".*\\b(bielefeld)\\b.*", "bielefeld", x, perl = TRUE)
  x <- gsub(".*\\b(bonn)\\b.*", "bonn", x, perl = TRUE)
  x <- gsub(".*\\b(muenster|münster)\\b.*", "muenster", x, perl = TRUE)
  x <- gsub(".*\\b(karlsruhe)\\b.*", "karlsruhe", x, perl = TRUE)
  x <- gsub(".*\\b(mannheim)\\b.*", "mannheim", x, perl = TRUE)
  x <- gsub(".*\\b(augsburg)\\b.*", "augsburg", x, perl = TRUE)
  x <- gsub(".*\\b(wiesbaden)\\b.*", "wiesbaden", x, perl = TRUE)
  x <- gsub(".*\\b(gelsenkirchen)\\b.*", "gelsenkirchen", x, perl = TRUE)
  x <- gsub(".*\\b(moenchengladbach|mönchengladbach)\\b.*", "moenchengladbach", x, perl = TRUE)
  x <- gsub(".*\\b(braunschweig)\\b.*", "braunschweig", x, perl = TRUE)
  x <- gsub(".*\\b(chemnitz)\\b.*", "chemnitz", x, perl = TRUE)
  x <- gsub(".*\\b(kiel)\\b.*", "kiel", x, perl = TRUE)
  x <- gsub(".*\\b(aachen)\\b.*", "aachen", x, perl = TRUE)
  x <- gsub(".*\\b(halle|halle saale|halle saale)\\b.*", "halle", x, perl = TRUE)
  x <- gsub(".*\\b(magdeburg)\\b.*", "magdeburg", x, perl = TRUE)
  x <- gsub(".*\\b(freiburg|freiburg im breisgau)\\b.*", "freiburg", x, perl = TRUE)
  x <- gsub(".*\\b(luebeck|lübeck)\\b.*", "luebeck", x, perl = TRUE)
  x <- gsub(".*\\b(erfurt)\\b.*", "erfurt", x, perl = TRUE)
  x <- gsub(".*\\b(mainz)\\b.*", "mainz", x, perl = TRUE)
  x <- gsub(".*\\b(rostock)\\b.*", "rostock", x, perl = TRUE)
  x <- gsub(".*\\b(saarbruecken|saarbrücken)\\b.*", "saarbruecken", x, perl = TRUE)
  x <- gsub(".*\\b(cottbus)\\b.*", "cottbus", x, perl = TRUE)
  x <- gsub(".*\\b(potsdam)\\b.*", "potsdam", x, perl = TRUE)
  x <- gsub(".*\\b(bad neuenahr|bad neuenahr-ahrweiler)\\b.*", "bad neuenahr", x, perl = TRUE)
  
  # More large/medium cities
  x <- gsub(".*(oberhausen).*", "oberhausen", x, perl = TRUE)
  x <- gsub(".*(hagen).*", "hagen", x, perl = TRUE)
  x <- gsub(".*(hammm|hamm).*", "hamm", x, perl = TRUE)
  x <- gsub(".*(krefeld).*", "krefeld", x, perl = TRUE)
  x <- gsub(".*(solingen).*", "solingen", x, perl = TRUE)
  x <- gsub(".*(herne).*", "herne", x, perl = TRUE)
  x <- gsub(".*(osnabrueck|osnabrück).*", "osnabrueck", x, perl = TRUE)
  x <- gsub(".*(oldenburg).*", "oldenburg", x, perl = TRUE)
  x <- gsub(".*(heilbronn).*", "heilbronn", x, perl = TRUE)
  x <- gsub(".*(pforzheim).*", "pforzheim", x, perl = TRUE)
  x <- gsub(".*(goettingen|göttingen).*", "goettingen", x, perl = TRUE)
  x <- gsub(".*(wolfsburg).*", "wolfsburg", x, perl = TRUE)
  x <- gsub(".*(ulm).*", "ulm", x, perl = TRUE)
  x <- gsub(".*(ingolstadt).*", "ingolstadt", x, perl = TRUE)
  x <- gsub(".*(regensburg).*", "regensburg", x, perl = TRUE)
  x <- gsub(".*(wuerzburg|würzburg).*", "wuerzburg", x, perl = TRUE)
  x <- gsub(".*(heidelberg).*", "heidelberg", x, perl = TRUE)
  x <- gsub(".*(darmstadt).*", "darmstadt", x, perl = TRUE)
  x <- gsub(".*(offenbach).*", "offenbach", x, perl = TRUE)
  x <- gsub(".*(neuss).*", "neuss", x, perl = TRUE)
  x <- gsub(".*(paderborn).*", "paderborn", x, perl = TRUE)
  x <- gsub(".*(reutlingen).*", "reutlingen", x, perl = TRUE)
  x <- gsub(".*(fuerth|fürth).*", "fuerth", x, perl = TRUE)
  x <- gsub(".*(boeblingen|böblingen).*", "boeblingen", x, perl = TRUE)
  x <- gsub(".*(ludwigsburg).*", "ludwigsburg", x, perl = TRUE)
  x <- gsub(".*(esslingen).*", "esslingen", x, perl = TRUE)
  x <- gsub(".*(tuebingen|tübingen).*", "tuebingen", x, perl = TRUE)
  x <- gsub(".*(sindelfingen).*", "sindelfingen", x, perl = TRUE)
  x <- gsub(".*(konstanz|constance).*", "konstanz", x, perl = TRUE)
  x <- gsub(".*(villingen[- ]schwenningen|villingen schwenningen).*", "villingen-schwenningen", x, perl = TRUE)
  x <- gsub(".*(konstanz).*", "konstanz", x, perl = TRUE)
  x <- gsub(".*(koblenz).*", "koblenz", x, perl = TRUE)
  x <- gsub(".*(trier).*", "trier", x, perl = TRUE)
  x <- gsub(".*(kassel).*", "kassel", x, perl = TRUE)
  x <- gsub(".*(marburg).*", "marburg", x, perl = TRUE)
  x <- gsub(".*(giessen|gießen).*", "giessen", x, perl = TRUE)
  x <- gsub(".*(wetzlar).*", "wetzlar", x, perl = TRUE)
  x <- gsub(".*(hanau).*", "hanau", x, perl = TRUE)
  x <- gsub(".*(ruesselsheim|rüsselsheim).*", "ruesselsheim", x, perl = TRUE)
  x <- gsub(".*(fulda).*", "fulda", x, perl = TRUE)
  x <- gsub(".*(siegen).*", "siegen", x, perl = TRUE)
  x <- gsub(".*(salzgitter).*", "salzgitter", x, perl = TRUE)
  x <- gsub(".*(hildesheim).*", "hildesheim", x, perl = TRUE)
  x <- gsub(".*(goerlitz|görlitz).*", "goerlitz", x, perl = TRUE)
  x <- gsub(".*(jena).*", "jena", x, perl = TRUE)
  x <- gsub(".*(weimar).*", "weimar", x, perl = TRUE)
  x <- gsub("(gera)", "gera", x, perl = TRUE)
  x <- gsub(".*(zwickau).*", "zwickau", x, perl = TRUE)
  x <- gsub(".*(dessau[- ]rosslau|dessau|rosslau|dessau rosslau|dessau-rosslau).*", "dessau-rosslau", x, perl = TRUE)
  x <- gsub(".*(schwerin).*", "schwerin", x, perl = TRUE)
  x <- gsub(".*(stralsund).*", "stralsund", x, perl = TRUE)
  x <- gsub(".*(greifswald).*", "greifswald", x, perl = TRUE)
  x <- gsub(".*(neubrandenburg).*", "neubrandenburg", x, perl = TRUE)
  x <- gsub(".*(wilhelmshaven).*", "wilhelmshaven", x, perl = TRUE)
  x <- gsub(".*(bremerhaven).*", "bremerhaven", x, perl = TRUE)
  x <- gsub(".*(emden).*", "emden", x, perl = TRUE)
  x <- gsub(".*(delmenhorst).*", "delmenhorst", x, perl = TRUE)
  x <- gsub(".*(aurich).*", "aurich", x, perl = TRUE)
  x <- gsub(".*(cuxhaven).*", "cuxhaven", x, perl = TRUE)
  x <- gsub(".*(flensburg).*", "flensburg", x, perl = TRUE)
  x <- gsub(".*(kiel).*", "kiel", x, perl = TRUE)
  x <- gsub(".*(luebeck|lübeck).*", "luebeck", x, perl = TRUE)
  x <- gsub(".*(neumuenster|neumünster).*", "neumuenster", x, perl = TRUE)
  x <- gsub(".*(rostock).*", "rostock", x, perl = TRUE)
  
  # Bavaria / Baden-Württemberg additions
  x <- gsub(".*(bamberg).*", "bamberg", x, perl = TRUE)
  x <- gsub(".*(bayreuth).*", "bayreuth", x, perl = TRUE)
  x <- gsub(".*(coburg).*", "coburg", x, perl = TRUE)
  x <- gsub(".*(aschaffenburg).*", "aschaffenburg", x, perl = TRUE)
  x <- gsub(".*(landshut).*", "landshut", x, perl = TRUE)
  x <- gsub(".*(passau).*", "passau", x, perl = TRUE)
  x <- gsub(".*(rosenheim).*", "rosenheim", x, perl = TRUE)
  x <- gsub(".*(kempten).*", "kempten", x, perl = TRUE)
  x <- gsub(".*(ansbach).*", "ansbach", x, perl = TRUE)
  x <- gsub(".*(schweinfurt).*", "schweinfurt", x, perl = TRUE)
  x <- gsub(".*(neu[- ]ulm|neu ulm).*", "neu-ulm", x, perl = TRUE)
  x <- gsub(".*(freiburg).*", "freiburg", x, perl = TRUE)
  x <- gsub(".*(konstanz).*", "konstanz", x, perl = TRUE)
  x <- gsub(".*(friedrichshafen).*", "friedrichshafen", x, perl = TRUE)
  x <- gsub(".*(ravensburg).*", "ravensburg", x, perl = TRUE)
  x <- gsub(".*(reutlingen).*", "reutlingen", x, perl = TRUE)
  x <- gsub(".*(heidenheim).*", "heidenheim", x, perl = TRUE)
  x <- gsub(".*(goppingen|göppingen).*", "goppingen", x, perl = TRUE)
  x <- gsub(".*(waiblingen).*", "waiblingen", x, perl = TRUE)
  x <- gsub(".*(neustadt an der weinstrasse|neustadt a d weinstrasse|neustadt ad weinstrasse).*", "neustadt an der weinstrasse", x, perl = TRUE)
  x <- gsub(".*(ludwigshafen|ludwigshafen am rhein).*", "ludwigshafen", x, perl = TRUE)
  x <- gsub(".*(speyer).*", "speyer", x, perl = TRUE)
  x <- gsub(".*(worms).*", "worms", x, perl = TRUE)
  x <- gsub(".*(kaiserslautern).*", "kaiserslautern", x, perl = TRUE)
  
  # NRW dense belt
  x <- gsub(".*(leverkusen).*", "leverkusen", x, perl = TRUE)
  x <- gsub(".*(bergisch gladbach).*", "bergisch gladbach", x, perl = TRUE)
  x <- gsub(".*(remscheid).*", "remscheid", x, perl = TRUE)
  x <- gsub(".*(moers).*", "moers", x, perl = TRUE)
  x <- gsub(".*(muelheim an der ruhr|mülheim an der ruhr|muelheim ruhr).*", "muelheim an der ruhr", x, perl = TRUE)
  x <- gsub(".*(ratingen).*", "ratingen", x, perl = TRUE)
  x <- gsub(".*(velbert).*", "velbert", x, perl = TRUE)
  x <- gsub(".*(dueren|düren).*", "dueren", x, perl = TRUE)
  x <- gsub(".*(luedenscheid|lüdenscheid).*", "luedenscheid", x, perl = TRUE)
  x <- gsub(".*(iserlohn).*", "iserlohn", x, perl = TRUE)
  x <- gsub(".*(herten).*", "herten", x, perl = TRUE)
  x <- gsub(".*(recke?linghausen).*", "recklinghausen", x, perl = TRUE)
  x <- gsub(".*(bottrop).*", "bottrop", x, perl = TRUE)
  x <- gsub(".*(minden).*", "minden", x, perl = TRUE)
  x <- gsub(".*(herford).*", "herford", x, perl = TRUE)
  x <- gsub(".*(detmold).*", "detmold", x, perl = TRUE)
  x <- gsub(".*(lipstadt|lippstadt).*", "lippstadt", x, perl = TRUE)
  x <- gsub(".*(gutersloh|gütersloh).*", "guetersloh", x, perl = TRUE)
  x <- gsub(".*(siegen).*", "siegen", x, perl = TRUE)
  x <- gsub(".*(soest).*", "soest", x, perl = TRUE)
  x <- gsub(".*(troisdorf).*", "troisdorf", x, perl = TRUE)
  x <- gsub(".*(dormagen).*", "dormagen", x, perl = TRUE)
  x <- gsub(".*(grevenbroich).*", "grevenbroich", x, perl = TRUE)
  x <- gsub(".*(rhein[- ]berg|rhein berg).*", "rhein-berg", x, perl = TRUE)
  x <- gsub(".*(huerth|hürth).*", "huerth", x, perl = TRUE)
  x <- gsub(".*(sankt augustin|st augustin).*", "sankt augustin", x, perl = TRUE)
  
  # North / East additions
  x <- gsub(".*(lueneburg|lüneburg).*", "lueneburg", x, perl = TRUE)
  x <- gsub(".*(celle).*", "celle", x, perl = TRUE)
  x <- gsub(".*(hameln).*", "hameln", x, perl = TRUE)
  x <- gsub(".*(hildesheim).*", "hildesheim", x, perl = TRUE)
  x <- gsub(".*(osnabrueck|osnabrück).*", "osnabrueck", x, perl = TRUE)
  x <- gsub(".*(whv|wilhelmshaven).*", "wilhelmshaven", x, perl = TRUE)
  x <- gsub(".*(stralsund).*", "stralsund", x, perl = TRUE)
  x <- gsub(".*(brandenburg an der havel|brandenburg a d havel|brandenburg a\\. d\\. havel).*", "brandenburg an der havel", x, perl = TRUE)
  x <- gsub(".*(frankfurt \\(oder\\)|frankfurt oder).*", "frankfurt (oder)", x, perl = TRUE)
  x <- gsub(".*(cottbus).*", "cottbus", x, perl = TRUE)
  
  # Smaller university / notable cities
  x <- gsub(".*(erlangen).*", "erlangen", x, perl = TRUE)
  x <- gsub(".*(passau).*", "passau", x, perl = TRUE)
  x <- gsub(".*(bayreuth).*", "bayreuth", x, perl = TRUE)
  x <- gsub(".*(luebeck|lübeck).*", "luebeck", x, perl = TRUE)
  x <- gsub(".*(konstanz).*", "konstanz", x, perl = TRUE)
  x <- gsub(".*(hildesheim).*", "hildesheim", x, perl = TRUE)
  x <- gsub(".*(coburg).*", "coburg", x, perl = TRUE)
  x <- gsub(".*(greifswald).*", "greifswald", x, perl = TRUE)
  x <- gsub(".*(weimar).*", "weimar", x, perl = TRUE)
  x <- gsub(".*(wismar).*", "wismar", x, perl = TRUE)
  x <- gsub(".*(stralsund).*", "stralsund", x, perl = TRUE)
  x <- gsub(".*(lippstadt).*", "lippstadt", x, perl = TRUE)
  x <- gsub(".*(hildesheim).*", "hildesheim", x, perl = TRUE)
  
  # Rhineland-Palatinate / Saarland extras
  x <- gsub(".*(neuwied).*", "neuwied", x, perl = TRUE)
  x <- gsub(".*(trier).*", "trier", x, perl = TRUE)
  x <- gsub(".*(speyer).*", "speyer", x, perl = TRUE)
  x <- gsub(".*(kaiserslautern).*", "kaiserslautern", x, perl = TRUE)
  x <- gsub(".*(saarlouis).*", "saarlouis", x, perl = TRUE)
  x <- gsub(".*(neunkirchen).*", "neunkirchen", x, perl = TRUE)
  
  # Hesse extras
  x <- gsub(".*(wetzlar).*", "wetzlar", x, perl = TRUE)
  x <- gsub(".*(giessen|gießen).*", "giessen", x, perl = TRUE)
  x <- gsub(".*(marburg).*", "marburg", x, perl = TRUE)
  x <- gsub(".*(darmstadt).*", "darmstadt", x, perl = TRUE)
  x <- gsub(".*(ruesselsheim|rüsselsheim).*", "ruesselsheim", x, perl = TRUE)
  x <- gsub(".*(bad homburg).*", "bad homburg", x, perl = TRUE)
  x <- gsub(".*(fulda).*", "fulda", x, perl = TRUE)
  x <- gsub(".*(hanau).*", "hanau", x, perl = TRUE)
  
  
  # Generic suffix cleanup
  x <- gsub("(gemeinde|bezirk)$", "", x)
  x <- gsub("[^[:alnum:] ]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
  
  # --- Set non-German cities to NA (one per line) ---
  
  # Switzerland
  x <- ifelse(grepl(".*(zuerich|zürich|zurich).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(genf|geneva|genève).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(basel).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("(bern|berne)", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(lausanne).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*st[ .-]?gallen.*|.*sankt[ .-]?gallen.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(luzern|lucerne).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(lugano).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("(?i)\\bbiel\\b|\\bbienne\\b", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("(thun)", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(köniz|koeniz).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("la chaux[- ]de[- ]fonds", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(fribourg|freiburg im uchtland).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*schaffhausen.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("(chur)", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(neuchâtel|neuchatel|neuenburg).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("vernier", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("(sion|sitten)", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("(?i)\\buster\\b", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("\\bzug\\b", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("yverdon", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("nyon", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("montreux", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("vevey", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("martigny", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*aarau.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("solothurn", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("(?i)\\bolten\\b", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("bellinzona", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("locarno", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("rapperswil[- ]jona", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("frauenfeld", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("wetzikon", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("dietikon", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("kloten", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("(?i)\\bbaden\\b(?!-baden)(?![- ]w[uü]rttemberg)(?:\\s*\\(ag\\)|\\s+ag)?$", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("weinfelden", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("(?i)^\\bwil\\b(?:\\s*(\\(sg\\)|sg))?$", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("bulle", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*sierre|siders.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("mendrisio", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("chiasso", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(zermatt).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(pfaeffikon).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  
  # Netherlands
  x <- ifelse(grepl(".*(amsterdam).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("rotterdam", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("den haag|the hague", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("utrecht", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("eindhoven", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("tilburg", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("groningen", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("almere", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("breda", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("nijmegen", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("enschede", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("apeldoorn", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("haarlem", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("arnhem", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("zwolle", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("amersfoort", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("maastricht", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("leeuwarden", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("dordrecht", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("leiden", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("zoetermeer", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("heerlen", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("hengelo", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("hilversum", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("venlo", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("vlaardingen", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("deventer", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("sittard", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("almelo", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("purmerend", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("middelburg", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("doetinchem", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # France
  x <- ifelse(grepl(".*(paris).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("marseille", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("lyon", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("toulouse", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("nice|nizza", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("nantes", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("strasbourg|straßburg", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("montpellier", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("bordeaux", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("reims", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("saint[- ]etienne", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("toulon", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("grenoble", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("dijon", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("angers", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("n[iî]mes|nimes", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("villeurbanne", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("clermont[- ]ferrand", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("le havre", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("caen", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("(?i)\\bmetz\\b(?!ingen)", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("besançon|besancon", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("orléans|orleans", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*mulhouse.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("rouen", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("tours", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("amiens", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("limoges", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("perpignan", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("boulogne[- ]billancourt", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("nancy", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("argenteuil", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("montreuil", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("avignon", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("poitiers", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("versailles", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("antibes", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("annecy", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("bayonne", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("biarritz", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("chambéry|chambery", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*colmar.*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # Luxembourg
  x <- ifelse(grepl(".*(luxembourg|luxemburg).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("esch[- ]sur[- ]alzette", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("differdange", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("dudelange", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("ettelbruck|ettelbrück|ettelbrueck", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("diekirch", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # Big non-German capitals / hubs
  x <- ifelse(grepl(".*(london).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*madrid.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("(roma|rome)", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(wien|vienna).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("praha|prague", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("warszawa|warsaw", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("bruxelles|brussels", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("københavn|kobenhavn|copenhagen", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("oslo", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*stockholm.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*helsinki.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("reykjavik", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("lisboa|lisbon", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*dublin.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("athens|athína|athina", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*budapest.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("bucurești|bucharest", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*sofia.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("beograd|belgrade", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*zagreb.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*ljubljana.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*sarajevo.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*skopje.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("moskva|moscow", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("kiev|kyiv", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*minsk.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("istanbul|ankara", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("tel aviv|jerusalem", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*cairo.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("dubai|abu dhabi", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("doha", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*riyadh.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*tehran.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("new delhi|delhi|mumbai|bombay|bengaluru|bangalore|chennai|madras|kolkata|calcutta", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*karachi.*|.*islamabad.*|.*dhaka.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("jakarta|manila|bangkok|kuala lumpur|hanoi|ho chi minh|saigon", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*taipei.*|.*tokyo.*|.*seoul.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("beijing|peking|shanghai|hong kong|singapore", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("sydney|melbourne|auckland", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("new york|los angeles|san francisco|chicago|washington", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("toronto|montreal|vancouver|ottawa", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("mexico city|são paulo|sao paulo|rio de janeiro|buenos aires|santiago|bogotá|bogota|lima", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("cape town|johannesburg", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*vaduz.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*cambridge.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*durham.*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  #Random ones
  x <- ifelse(grepl("(?i)\\blinz\\b(?!\\s+am\\s+rhein)|\\btampere\\b|\\blissabon\\b|\\bkeine niederlassung in dach\\b", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # --- Set Non-German Country Names to NA ---
  # This block flags text that strongly suggests the input refers to a country,
  # preventing misclassification of regional offices.
  
  # Core European Countries (German/English)
  x <- ifelse(grepl(".*(oesterreich|österreich|austria).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(?<!saechsische )\\b(schweiz|switzerland|suisse|svizzera)\\b.*", x, perl=TRUE), "Non-German City - Do not match", x) 
  x <- ifelse(grepl(".*(frankreich|france).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(italien|italy).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(spanien|spain).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(grossbritannien|großbritannien|great britain|uk|england).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(niederlande|holland|netherlands).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(belgien|belgium).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(daenemark|dänemark|denmark).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(schweden|sweden).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(norwegen|norway).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(finnland|finland).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(polen|poland).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(tschechien|czechia|tschechische republik).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(ungarn|hungary).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # Specific Requests and Neighbors
  x <- ifelse(grepl(".*(irland|eire|ireland).*", x, perl=TRUE), "Non-German City - Do not match", x) # IRELAND
  x <- ifelse(grepl(".*(indien|india).*", x, perl=TRUE), "Non-German City - Do not match", x)         # INDIA
  x <- ifelse(grepl("(?i)\\b(fürstentum\\s+)?liechtenstein\\b", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(luxemburg|luxembourg).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(russland|russia).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(portugal).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(griechenland|greece).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # Global and North American Hubs
  x <- ifelse(grepl("\\b(united states|vereinigte staaten|america|amerika|usa)\\b|\\bu\\s*s\\s*a\\b", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(kanada|canada).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(china|volksrepublik).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(japan).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(brasilien|brazil).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(mexiko|mexico).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(australien|australia).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(tuerkei|türkei|turkey).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(suedkorea|südkorea|south korea).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(singapur|singapore).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(arabische emirate|uae).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # Europe (more)
  x <- ifelse(grepl(".*(rumaenien|rumänien|romania).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(bulgarien|bulgaria).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(slowakei|slovakia|slovensko).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(slowenien|slovenia|slovenija).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(kroatien|croatia|hrvatska).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(serbien|serbia|srbija).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(bosnien|bosnia|herzegowina|herzegovina|bih|bosnia and herzegovina).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(montenegro|crna gora).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(nordmazedonien|north macedonia|mazedonien|macedonia|makedonija).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(albanien|albania|shqip[eë]ria).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(kosovo|kosova).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(ukraine|ukraina|ukrajina|ukrajine).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(belarus|weissrussland|weißrussland|byelorussia|bela[r|ß]ussland).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(moldau|moldova|moldavia|republik moldau|moldawien|moldova republic).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(estland|estonia|eesti).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(lettland|latvia|latvija).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(litauen|lithuania|lietuva).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(island|iceland|ísland).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(andorra).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(monaco|monaco-ville).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(san marino|sanmarino).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(vatikan|vatican|holy see|heiliger stuhl).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(zypern|cyprus|kipros|kibris).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(malta).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(schottland|scotland|wales|northern ireland|nordirland|isle of man|jersey|guernsey|gibraltar|channel islands).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # Caucasus
  x <- ifelse(grepl(".*(georgien|georgia|sakartvelo).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(armenien|armenia|hayastan).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(aserbaidschan|aserbaidschan|azerbaijan|azerbaidzhan|azerbajdzhan|azerbaycan).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # Middle East & North Africa
  x <- ifelse(grepl(".*(israel|palästina|palastina|palestine|state of palestine|westbank|gaza).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(libanon|lebanon|lubnan|lebanese republic).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(syrien|syria|syrian arab republic).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(irak|iraq|al iraq|iraqi republic).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(iran|islamic republic of iran|persia|persien).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(saudi[- ]arabien|saudi arabia|ksa|kingdom of saudi arabia).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(katar|qatar|state of qatar|doha state).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(bahrain|kingdom of bahrain).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(kuwait|state of kuwait).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl("(oman|sultanate of oman)", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(jemen|yemen|republic of yemen).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(aegypten|ägypten|egypt|arab republic of egypt).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(marokko|morocco|kingdom of morocco|maghreb).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(algerien|algeria|people's democratic republic of algeria|dzayer).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(tunesien|tunisia|tunisian republic).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(libyen|libya|state of libya|libyan arab jamahiriya).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # Sub-Saharan Africa
  x <- ifelse(grepl(".*(suedafrika|südafrika|south africa).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(namibia|republic of namibia).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(botswana|republic of botswana).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(simbabwe|zimbabwe|republic of zimbabwe).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(sambia|zambia|republic of zambia).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(malawi|republic of malawi).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(mosambik|mosambique|mozambique|republic of mozambique).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(angola|republic of angola).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(kenia|kenya|republic of kenya).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(tansania|tanzania|united republic of tanzania).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(uganda|republic of uganda).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(ruanda|rwanda|republic of rwanda).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(burundi|republic of burundi).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(aethiopien|äthiopien|ethiopia|federal democratic republic of ethiopia).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(eritrea|state of eritrea|eritrea state).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(dschibuti|djibouti|republic of djibouti).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(somalia|federal republic of somalia|somali).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(kongo|demokratische republik kongo|drc|dr kongo|congo[- ]kinshasa|congo democratic|democratic republic of the congo).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(republik kongo|republic of the congo|congo[- ]brazzaville).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(ghana|republic of ghana).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(nigeria|federal republic of nigeria).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(elfenbeinkueste|côte d'ivoire|cote d'ivoire|ivory coast).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(senegal|republic of senegal).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(mali|republic of mali|maali).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(niger|republic of niger).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(burkina faso|obervolta|republic of burkina faso).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(togo|togolese republic).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(benin|republic of benin|dahomey).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(liberia|republic of liberia).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(sierra leone|republic of sierra leone).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(guinea[- ]bissau|guinea bissau|republic of guinea[- ]bissau).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)guinea(\\b|$)|.*(republic of guinea).*", x, perl=TRUE), "Non-German City - Do not match", x)  # Guinea (not G.-Bissau)
  x <- ifelse(grepl(".*(gambia|the gambia|republic of the gambia).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(kap verde|cape verde|cabo verde|republic of cabo verde).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(kamerun|cameroon|republic of cameroon).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(zentralafrikanische republik|central african republic).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(tschad|chad|republic of chad).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(äquatorialguinea|equatorial guinea|republic of equatorial guinea).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(gabun|gabon|gabonese republic).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(são tom[eé]|sao tome|principe|príncipe|sao tome and principe).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(mauretanien|mauritania|islamic republic of mauritania).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # South & Central Asia
  x <- ifelse(grepl(".*(pakistan|islamic republic of pakistan).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(bangladesch|bangladesh|people's republic of bangladesh).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(nepal|federal democratic republic of nepal).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(bhutan|kingdom of bhutan).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(sri lanka|sri[- ]lanka|democratic socialist republic of sri lanka|ceylon|ceilan).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(malediven|maldives|republic of maldives|dhivehi raajje).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # Southeast Asia
  x <- ifelse(grepl(".*(myanmar|burma|union of myanmar|republic of the union of myanmar).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(thailand|kingdom of thailand).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(laos|lao pdr|lao people's democratic republic|lao volksrepublik).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(kambodscha|cambodia|kingdom of cambodia|kampuchea).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(vietnam|viet nam|socialist republic of viet nam).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(malaysia|malaysien|federation of malaysia).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(brunei|brunei darussalam|negara brunei darussalam).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(indonesien|indonesia|republic of indonesia|republik indonesia).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(philippinen|philippines|republic of the philippines).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(osta[ -]?timor|osttimor|timor[- ]leste|east timor|democratic republic of timor[- ]leste).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # East Asia & Central Asia
  x <- ifelse(grepl(".*(taiwan|roc taiwan|republic of china \\(taiwan\\)).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(mongolei|mongolia|mongol uls).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(kasachstan|kazakhstan|qazaqstan|republic of kazakhstan).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(kirgisistan|kyrgyzstan|kirgistan|kyrgyz republic|kyrgyzskaia).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(usbekistan|uzbekistan|o'zbekiston|uzbekiston).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(turkmenistan|tuerkmenistan|türkmenistan|turkmenistan).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(tadschikistan|tajikistan|tojikiston|republic of tajikistan).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(afghanistan|islamic republic of afghanistan).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # Oceania
  x <- ifelse(grepl(".*(neuseeland|new zealand|aotearoa).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(papua[- ]neu[- ]guinea|papua new guinea|png).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(fidschi|fiji|republic of fiji).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(samoa|independent state of samoa|wes[ts]amoa).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(tonga|kingdom of tonga).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(vanuatu|republic of vanuatu|new hebrides).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(solomon islands|salomonen|solomonen|solomons).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(föderierte staaten von mikronesien|micronesia|federated states of micronesia|fsm).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(palau|republic of palau|belau).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(marshallinseln|marshall islands|republic of the marshall islands|rmi).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(kiribati|republic of kiribati).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)nauru(\\b|$)|.*(republic of nauru).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # Americas (Latin America & Caribbean)
  x <- ifelse(grepl(".*(guatemala|republic of guatemala).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)belize(\\b|$)|.*(belize state).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(honduras|republic of honduras).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(el salvador|republic of el salvador|elsalvador).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(nicaragua|republic of nicaragua).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(costa rica|costa[- ]rica|republic of costa rica).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(panama|panamá|republic of panama).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(kolumbien|colombia|republic of colombia).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(venezuela|bolivarian republic of venezuela).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)guyana(\\b|$)|.*(co-operative republic of guyana).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)suriname(\\b|$)|.*(republic of suriname).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(ecuador|república del ecuador|republic of ecuador).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)peru(\\b|$)|.*(perú|republic of peru|rep[úu]blica del per[uú]).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)bolivia(\\b|$)|.*(plurinational state of bolivia|estado plurinacional de bolivia).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)paraguay(\\b|$)|.*(republic of paraguay).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)uruguay(\\b|$)|.*(eastern republic of uruguay|república oriental del uruguay).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)argentin[a|ien](\\b|$)|.*(argentina|argentinien|argentine republic).*", x, perl=TRUE), "Non-German City - Do not match", x)
  # (Brazil, Mexico already covered)
  
  # Caribbean
  x <- ifelse(grepl(".*(kuba|cuba|republic of cuba).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)haiti(\\b|$)|.*(republic of haiti|république d'haïti|republique d haiti).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(dominikanische republik|dominican republic|república dominicana).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(jamaika|jamaica).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(trinidad und tobago|trinidad and tobago|t&t|trinidad).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(^|\\b)barbados(\\b|$).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)bahamas(\\b|$)|.*(commonwealth of the bahamas).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(grenada|grenada state).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)st\\.? lucia(\\b|$)|.*(saint lucia).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(st\\.? vincent|saint vincent|grenadines|saint vincent and the grenadines).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(antigua|barbuda|antigua and barbuda).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)dominica(\\b|$)|.*(commonwealth of dominica).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*(st\\.? kitts|saint kitts|nevis|saint kitts and nevis).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # Additional territories often seen
  x <- ifelse(grepl(".*(hong kong|hongkong|hksar|macau|macao|macau sar|macao sar).*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*(puerto rico|us[- ]virgin islands|british virgin islands|cayman islands|bermuda|greenland|färöer|faeroe|faroe islands).*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # Exclude common company/legal endings (treat as non-city)
  x <- ifelse(grepl(".*\\b(gmbh[[:punct:] ]*(?:&|und)[[:punct:] ]*co[[:punct:] ]*kga?a?|gmbh[[:punct:] ]*co[[:punct:] ]*kg)\\b.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*\\b(gmbh i\\.?g\\.|gmbh i\\.? l\\.|gmbh i\\.?l\\.|gmbh in liqu?\\.|gmbh in gruendung|gmbh in gr[[:alpha:]]ndung)\\b.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*\\b(gmbh)\\b.*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  # Generic business buzzwords that usually signal a company name, not a city
  x <- ifelse(grepl(".*\\b(holding|gruppe|solutions|consulting|services|logistics|logistik|systems|systeme|technik|technologies)\\b.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*\\blidl\\b.*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- ifelse(grepl(".*\\bkeine in dach\\b.*", x, perl=TRUE), "Non-German City - Do not match", x)
  x <- ifelse(grepl(".*\\bueberall\\b.*", x, perl=TRUE), "Non-German City - Do not match", x)
  
  x <- trimws(x)
  
}

normalize_city_ch <- function(x) {
  stopifnot(is.character(x))
  x <- iconv(x, to = "UTF-8")
  x <- tolower(x)
  x <- trimws(x)
  
  # Umlaut & accents (DE/FR/IT/RM coverage)
  x <- gsub("ä", "ae", x); x <- gsub("ö", "oe", x); x <- gsub("ü", "ue", x); x <- gsub("ß", "ss", x)
  x <- gsub("à|á|â|ã|å|ā|ä", "a", x)
  x <- gsub("è|é|ê|ë|ē", "e", x)
  x <- gsub("ì|í|î|ï|ī", "i", x)
  x <- gsub("ò|ó|ô|õ|ö|ø|ō", "o", x)
  x <- gsub("ù|ú|û|ü|ū", "u", x)
  x <- gsub("ç", "c", x); x <- gsub("ñ", "n", x)
  
  # Remove street tokens, bracketed content, digits, stray punctuation
  x <- gsub("\\b\\S*(strasse|straße|str\\.)\\S*\\b", "", x, perl=TRUE)
  x <- gsub("\\(.*?\\)|\\[.*?\\]|\\{.*?\\}", "", x, perl=TRUE)
  x <- gsub("[0-9]+", "", x, perl=TRUE)
  x <- gsub("(?:(?![/&])[[:punct:]])+", " ", x, perl=TRUE)
  x <- gsub("/", " / ", x, perl=TRUE)
  
  # Truncate after common joiners
  x <- gsub("(auch|und| /|&)\\s+.*", "", x, ignore.case=TRUE, perl=TRUE)
  x <- gsub("\\b(auch|und)\\b| /|&", "", x, ignore.case=TRUE, perl=TRUE)
  
  # Generic office/region noise (DE/FR/IT)
  x <- gsub("(?i)\\b(raum|region|gebiet|ueberregional|weltweit|international|r[eé]gion|cantone|canton|quartier|kreis)\\b", "", x, perl=TRUE)
  x <- gsub("(?i)\\b(keine angabe|keine angaben|spielt keine rolle)\\b", "", x, perl=TRUE)
  x <- gsub("(?i)\\b(home office|head office|hauptsitz|hauptverwaltung|niederlassung|filiale|filialen|zentrale)\\b", "", x, perl=TRUE)
  
  # Remove standalone "bei" / "b."
  x <- gsub("(?i)\\bbei\\b|\\bb\\.?\\b", "", x, perl=TRUE)
  
  # Remove leading single-letter codes like "X-"
  x <- gsub("^[A-Za-z]-\\s*", "", x)
  
  # Remove canton abbreviations if standalone or in parentheses
  x <- gsub("\\b\\((zh|be|vd|ge|lu|ur|sz|ow|nw|gl|zg|fr|so|bs|bl|sh|ar|ai|sg|gr|ag|tg|ti|vs|ne|ju)\\)\\b", "", x, perl=TRUE)
  x <- gsub("\\b(zh|be|vd|ge|lu|ur|sz|ow|nw|gl|zg|fr|so|bs|bl|sh|ar|ai|sg|gr|ag|tg|ti|vs|ne|ju)\\b", "", x, perl=TRUE)
  
  x <- trimws(x)
  
  # Saint normalization
  x <- gsub("\\bst\\.?\\s+", "sankt ", x, perl=TRUE)
  
  # Accept ASCII digraphs as valid variants for umlauts (helps cross-source)
  x <- gsub("\\bbuer?glen\\b", "buerglen", x, perl=TRUE)
  x <- gsub("\\bmuenchwilen\\b", "muenchwilen", x, perl=TRUE)
  
  x <- gsub("\\bstadt\\b", "", x, perl = TRUE)
  
  # --- Swiss city canonicalization (safe, bilingual) ---
  # Use conservative word-boundaries to avoid over-capture.
  x <- gsub("(?i).*(zuerich|zürich|zurich|glattpark|zuerich flughafen|kloten flughafen).*", "zuerich", x, perl=TRUE)
  x <- gsub("(?i).*(geneve|genf|geneva|gen[eè]ve).*", "geneve", x, perl=TRUE)
  x <- gsub("(?i).*(bern | berne).*", "bern", x, perl=TRUE)
  x <- gsub("(?i)\\b(bern|berne)\\b", "bern", x, perl=TRUE)
  x <- gsub("(?i).*(basel|basle|bale|bâle).*", "basel", x, perl=TRUE)
  x <- gsub("(?i).*(lausanne).*", "lausanne", x, perl=TRUE)
  x <- gsub("(?i).*(luzern|lucerne).*", "luzern", x, perl=TRUE)
  x <- gsub("(?i)\\b(sankt[ -]?gallen|st[ .-]?gallen)\\b", "sankt gallen", x, perl=TRUE)
  x <- gsub("(?i)\\b(biel|bienne)\\b", "biel", x, perl=TRUE)
  x <- gsub("(?i)\\b(fribourg|freiburg im uech[t]?land|freiburg im uechtland|freiburg)\\b", "fribourg", x, perl=TRUE)
  x <- gsub("(?i)\\b(neuchatel|neuchâtel|neuenburg)\\b", "neuchatel", x, perl=TRUE)
  x <- gsub("(?i)\\b(sion|sitten)\\b", "sion", x, perl=TRUE)
  x <- gsub("(?i)\\b(chur|coira|cuira)\\b", "chur", x, perl=TRUE)
  x <- gsub("(?i)\\b(winterthur)\\b", "winterthur", x, perl=TRUE)
  x <- gsub("(?i)\\b(aarau)\\b", "aarau", x, perl=TRUE)
  x <- gsub("(?i)\\b(solothurn|soleure)\\b", "solothurn", x, perl=TRUE)
  x <- gsub("(?i)\\b(olten)\\b", "olten", x, perl=TRUE)
  x <- gsub("(?i)\\b(uster)\\b", "uster", x, perl=TRUE)
  x <- gsub("(?i)\\b(zug)\\b", "zug", x, perl=TRUE)           # beware of 'umzug' -> word-boundary safe
  x <- gsub("(?i)\\b(schaffhausen)\\b", "schaffhausen", x, perl=TRUE)
  x <- gsub("(?i)\\b(lugano)\\b", "lugano", x, perl=TRUE)
  x <- gsub("(?i)\\b(bellinzona)\\b", "bellinzona", x, perl=TRUE)
  x <- gsub("(?i)\\b(locarno)\\b", "locarno", x, perl=TRUE)
  x <- gsub("(?i)\\b(la chaux[ -]de[ -]fonds)\\b", "la chaux de fonds", x, perl=TRUE)
  x <- gsub("(?i)\\b(thun)\\b", "thun", x, perl=TRUE)
  x <- gsub("(?i)\\b(k[oö]eniz|köniz|koeniz)\\b", "koeniz", x, perl=TRUE)
  x <- gsub("(?i)\\b(vernier)\\b", "vernier", x, perl=TRUE)
  x <- gsub("(?i)\\b(meyrin)\\b", "meyrin", x, perl=TRUE)
  x <- gsub("(?i)\\b(lancy)\\b", "lancy", x, perl=TRUE)
  x <- gsub("(?i)\\b(onex)\\b", "onex", x, perl=TRUE)
  x <- gsub("(?i)\\b(vevey)\\b", "vevey", x, perl=TRUE)
  x <- gsub("(?i)\\b(montreux)\\b", "montreux", x, perl=TRUE)
  x <- gsub("(?i)\\b(yverdon[ -]les[ -]bains|yverdon)\\b", "yverdon les bains", x, perl=TRUE)
  x <- gsub("(?i)\\b(nyon)\\b", "nyon", x, perl=TRUE)
  x <- gsub("(?i)\\b(martigny)\\b", "martigny", x, perl=TRUE)
  x <- gsub("(?i)\\b(sierre|siders)\\b", "sierre", x, perl=TRUE)
  x <- gsub("(?i)\\b(davos)\\b", "davos", x, perl=TRUE)
  x <- gsub("(?i)\\b(zermatt)\\b", "zermatt", x, perl=TRUE)
  x <- gsub("(?i)\\b(interlaken)\\b", "interlaken", x, perl=TRUE)
  x <- gsub("(?i)\\b(rapperswil[ -]jona)\\b", "rapperswil jona", x, perl=TRUE)
  x <- gsub("(?i)\\b(jona)\\b", "rapperswil jona", x, perl=TRUE)
  x <- gsub("(?i)\\b(davos dorf)\\b", "davos", x, perl=TRUE)
  x <- gsub("(?i)\\b(wetzikon)\\b", "wetzikon", x, perl=TRUE)
  x <- gsub("(?i)\\b(dietikon)\\b", "dietikon", x, perl=TRUE)
  x <- gsub("(?i)\\b(kloten)\\b", "kloten", x, perl=TRUE)
  x <- gsub("(?i)\\b(frauenfeld)\\b", "frauenfeld", x, perl=TRUE)
  x <- gsub("(?i)\\b(ch[aä]x|chaux)\\b", "chaux", x, perl=TRUE) # gentle tidy
  x <- gsub(".*flims.*", "laax", x, ignore.case = TRUE, perl = TRUE)
  
  
  # Strip administrative wrappers
  x <- gsub("\\b(landkreis|kreis|stadtbezirk|bezirk|amt|gemeinde|kreisfreie stadt|gemeindefreies gebiet|kanton|station)\\b", "", x, perl=TRUE)
  
  # Some Zurich quarters -> 'zuerich' (safe subset)
  x <- gsub("(?i)\\b(oerlikon|oeerlikon)\\b", "zuerich", x, perl=TRUE)
  x <- gsub("(?i)\\b(altstetten|wiedikon|seefeld|enge|wipkingen|schwamendingen)\\b", "zuerich", x, perl=TRUE)
  
  # Generic suffix cleanup
  x <- gsub("(gemeinde|bezirk)$", "", x)
  x <- gsub("[^[:alnum:] ]", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  
  #Eliminate duplicate words in the same string
  x <- gsub("\\b(\\w+)\\b(?:\\s+\\1\\b)+", "\\1", x, perl = TRUE, ignore.case = TRUE)
  
  # --- Set non-Swiss cities/countries to NA marker ---
  # GERMANY (cities) — cautious: use word boundaries, avoid 'essen'
  de_city_pat <- paste0(
    "(?i)\\b(",
    paste(c(
      "berlin","hamburg","muenchen","münchen","koeln","köln",
      "frankfurt am main","frankfurt a m","ffm","stuttgart",
      "duesseldorf","düsseldorf","leipzig","dortmund",
      "hannover","bremen","dresden","nuernberg","nürnberg",
      "bochum","wuppertal","bielefeld","bonn","muenster","münster",
      "karlsruhe","mannheim","augsburg","wiesbaden","gelsenkirchen",
      "braunschweig","chemnitz","kiel","aachen","magdeburg",
      "freiburg","luebeck","lübeck","erfurt","mainz","rostock",
      "saarbruecken","saarbrücken","potsdam","osnabrueck","osnabrück",
      "heilbronn","pforzheim","goettingen","göttingen","wolfsburg",
      "ingolstadt","regensburg","wuerzburg","würzburg","heidelberg",
      "offenbach","neuss","paderborn","reutlingen","fuerth","fürth",
      "ludwigsburg","esslingen","tuebingen","tübingen","sindelfingen",
      "konstanz","villingen schwenningen","koblenz","trier","kassel",
      "marburg","giessen","gießen","wetzlar","hanau","fulda",
      "siegen","goerlitz","görlitz","jena","weimar","zwickau",
      "dessau rosslau","schwerin","stralsund","greifswald",
      "neubrandenburg","wilhelmshaven","bremerhaven","emden",
      "delmenhorst","aurich","cuxhaven","flensburg","neumuenster","neumünster"
    ), collapse="|"),
    ")\\b"
  )
  
  x <- ifelse(grepl(de_city_pat, x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Germany (country/state names)
  x <- ifelse(grepl("(?i)\\b(deutschland|germany|bundesrepublik|brd|bayern|baden[- ]wu[eü]rttemberg|sachsen|thueringen|thüringen|niedersachsen|hessen|nrw|rheinland[- ]pfalz|saarland|mecklenburg[- ]vorpommern|schleswig[- ]holstein|brandenburg|berlin|bremen|hamburg)\\b", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Netherlands
  x <- ifelse(grepl(".*(amsterdam).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("rotterdam", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("den haag|the hague", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("utrecht", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("eindhoven", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("tilburg", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("groningen", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("almere", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("breda", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("nijmegen", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("enschede", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("apeldoorn", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("haarlem", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("arnhem", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("zwolle", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("amersfoort", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("maastricht", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("leeuwarden", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("dordrecht", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("leiden", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("zoetermeer", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("heerlen", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("hengelo", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("hilversum", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("venlo", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("vlaardingen", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("deventer", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("sittard", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("almelo", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("purmerend", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("middelburg", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("doetinchem", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # France
  x <- ifelse(grepl(".*(paris).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("marseille", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("lyon", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("toulouse", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("nice|nizza", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("nantes", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("strasbourg|straßburg", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("montpellier", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("bordeaux", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("reims", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("saint[- ]etienne", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("toulon", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("grenoble", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("dijon", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("angers", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("n[iî]mes|nimes", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("villeurbanne", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("clermont[- ]ferrand", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("le havre", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("caen", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("(?i)\\bmetz\\b(?!ingen)", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("besançon|besancon", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("orléans|orleans", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*mulhouse.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("rouen", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("tours", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("amiens", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("limoges", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("perpignan", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("boulogne[- ]billancourt", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("nancy", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("argenteuil", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("montreuil", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("avignon", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("poitiers", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("versailles", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("antibes", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("annecy", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("bayonne", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("biarritz", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("chambéry|chambery", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*colmar.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Luxembourg
  x <- ifelse(grepl(".*(luxembourg|luxemburg).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("esch[- ]sur[- ]alzette", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("differdange", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("dudelange", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("ettelbruck|ettelbrück|ettelbrueck", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("diekirch", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Big non-Swiss capitals / hubs
  x <- ifelse(grepl(".*(london).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*madrid.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("(roma|rome)", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(wien|vienna).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("praha|prague", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("warszawa|warsaw", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("bruxelles|brussels", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("københavn|kobenhavn|copenhagen", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("oslo", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*stockholm.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*helsinki.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("reykjavik", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("lisboa|lisbon", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*dublin.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("athens|athína|athina", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*budapest.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("bucurești|bucharest", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*sofia.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("beograd|belgrade", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*zagreb.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*ljubljana.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*sarajevo.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*skopje.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("moskva|moscow", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("kiev|kyiv", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*minsk.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("istanbul|ankara", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("tel aviv|jerusalem", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*cairo.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("dubai|abu dhabi", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("doha", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*riyadh.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*tehran.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("new delhi|delhi|mumbai|bombay|bengaluru|bangalore|chennai|madras|kolkata|calcutta", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*karachi.*|.*islamabad.*|.*dhaka.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("jakarta|manila|bangkok|kuala lumpur|hanoi|ho chi minh|saigon", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*taipei.*|.*tokyo.*|.*seoul.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("beijing|peking|shanghai|hong kong|singapore", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("sydney|melbourne|auckland", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("new york|los angeles|san francisco|chicago|washington", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("toronto|montreal|vancouver|ottawa", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("mexico city|são paulo|sao paulo|rio de janeiro|buenos aires|santiago|bogotá|bogota|lima", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("cape town|johannesburg", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*vaduz.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*cambridge.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*durham.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  #Random ones
  x <- ifelse(grepl("(?i)\\blinz\\b(?!\\s+am\\s+rhein)|\\btampere\\b|\\blissabon\\b|\\bkeine niederlassung in dach\\b", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Core European Countries (German/English)
  x <- ifelse(grepl(".*(oesterreich|österreich|austria).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(frankreich|france).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(italien|italy).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(spanien|spain).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(grossbritannien|großbritannien|great britain|uk|england).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(niederlande|holland|netherlands).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(belgien|belgium).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(daenemark|dänemark|denmark).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(schweden|sweden).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(norwegen|norway).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(finnland|finland).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(polen|poland).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(tschechien|czechia|tschechische republik).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(ungarn|hungary).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Specific Requests and Neighbors
  x <- ifelse(grepl(".*(irland|eire|ireland).*", x, perl=TRUE), "Non-Swiss City - Do not match", x) # IRELAND
  x <- ifelse(grepl(".*(indien|india).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)         # INDIA
  x <- ifelse(grepl("(?i)\\b(fürstentum\\s+)?liechtenstein\\b", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(luxemburg|luxembourg).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(russland|russia).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(portugal).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(griechenland|greece).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Global and North American Hubs
  x <- ifelse(grepl("\\b(united states|vereinigte staaten|america|amerika|usa)\\b|\\bu\\s*s\\s*a\\b", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(kanada|canada).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(china|volksrepublik).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(japan).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(brasilien|brazil).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(mexiko|mexico).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(australien|australia).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(tuerkei|türkei|turkey).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(suedkorea|südkorea|south korea).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(singapur|singapore).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(arabische emirate|uae).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Europe (more)
  x <- ifelse(grepl(".*(rumaenien|rumänien|romania).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(bulgarien|bulgaria).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(slowakei|slovakia|slovensko).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(slowenien|slovenia|slovenija).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(kroatien|croatia|hrvatska).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(serbien|serbia|srbija).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(bosnien|bosnia|herzegowina|herzegovina|bih|bosnia and herzegovina).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(montenegro|crna gora).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(nordmazedonien|north macedonia|mazedonien|macedonia|makedonija).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(albanien|albania|shqip[eë]ria).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(kosovo|kosova).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(ukraine|ukraina|ukrajina|ukrajine).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(belarus|weissrussland|weißrussland|byelorussia|bela[r|ß]ussland).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(moldau|moldova|moldavia|republik moldau|moldawien|moldova republic).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(estland|estonia|eesti).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(lettland|latvia|latvija).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(litauen|lithuania|lietuva).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(island|iceland|ísland).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(andorra).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(monaco|monaco-ville).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(san marino|sanmarino).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(vatikan|vatican|holy see|heiliger stuhl).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(zypern|cyprus|kipros|kibris).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(malta).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(schottland|scotland|wales|northern ireland|nordirland|isle of man|jersey|guernsey|gibraltar|channel islands).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Caucasus
  x <- ifelse(grepl(".*(georgien|georgia|sakartvelo).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(armenien|armenia|hayastan).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(aserbaidschan|aserbaidschan|azerbaijan|azerbaidzhan|azerbajdzhan|azerbaycan).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Middle East & North Africa
  x <- ifelse(grepl(".*(israel|palästina|palastina|palestine|state of palestine|westbank|gaza).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(libanon|lebanon|lubnan|lebanese republic).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(syrien|syria|syrian arab republic).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(irak|iraq|al iraq|iraqi republic).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(iran|islamic republic of iran|persia|persien).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(saudi[- ]arabien|saudi arabia|ksa|kingdom of saudi arabia).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(katar|qatar|state of qatar|doha state).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(bahrain|kingdom of bahrain).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(kuwait|state of kuwait).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("\\b(oman|sultanate of oman)\\b", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(jemen|yemen|republic of yemen).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(aegypten|ägypten|egypt|arab republic of egypt).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(marokko|morocco|kingdom of morocco|maghreb).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(algerien|algeria|people's democratic republic of algeria|dzayer).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(tunesien|tunisia|tunisian republic).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(libyen|libya|state of libya|libyan arab jamahiriya).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Sub-Saharan Africa
  x <- ifelse(grepl(".*(suedafrika|südafrika|south africa).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(namibia|republic of namibia).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(botswana|republic of botswana).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(simbabwe|zimbabwe|republic of zimbabwe).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(sambia|zambia|republic of zambia).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(malawi|republic of malawi).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(mosambik|mosambique|mozambique|republic of mozambique).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(angola|republic of angola).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(kenia|kenya|republic of kenya).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(tansania|tanzania|united republic of tanzania).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(uganda|republic of uganda).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(ruanda|rwanda|republic of rwanda).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(burundi|republic of burundi).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(aethiopien|äthiopien|ethiopia|federal democratic republic of ethiopia).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(eritrea|state of eritrea|eritrea state).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(dschibuti|djibouti|republic of djibouti).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(somalia|federal republic of somalia|somali).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(kongo|demokratische republik kongo|drc|dr kongo|congo[- ]kinshasa|congo democratic|democratic republic of the congo).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(republik kongo|republic of the congo|congo[- ]brazzaville).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(ghana|republic of ghana).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(nigeria|federal republic of nigeria).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(elfenbeinkueste|côte d'ivoire|cote d'ivoire|ivory coast).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(senegal|republic of senegal).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(mali|republic of mali|maali).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(niger|republic of niger).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(burkina faso|obervolta|republic of burkina faso).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(togo|togolese republic).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(benin|republic of benin|dahomey).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(liberia|republic of liberia).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(sierra leone|republic of sierra leone).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(guinea[- ]bissau|guinea bissau|republic of guinea[- ]bissau).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)guinea(\\b|$)|.*(republic of guinea).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)  # Guinea (not G.-Bissau)
  x <- ifelse(grepl(".*(gambia|the gambia|republic of the gambia).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(kap verde|cape verde|cabo verde|republic of cabo verde).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(kamerun|cameroon|republic of cameroon).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(zentralafrikanische republik|central african republic).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl("\\b(tschad|chad|republic of chad)\\b", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(äquatorialguinea|equatorial guinea|republic of equatorial guinea).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(gabun|gabon|gabonese republic).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(são tom[eé]|sao tome|principe|príncipe|sao tome and principe).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(mauretanien|mauritania|islamic republic of mauritania).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # South & Central Asia
  x <- ifelse(grepl(".*(pakistan|islamic republic of pakistan).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(bangladesch|bangladesh|people's republic of bangladesh).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(nepal|federal democratic republic of nepal).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(bhutan|kingdom of bhutan).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(sri lanka|sri[- ]lanka|democratic socialist republic of sri lanka|ceylon|ceilan).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(malediven|maldives|republic of maldives|dhivehi raajje).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Southeast Asia
  x <- ifelse(grepl(".*(myanmar|burma|union of myanmar|republic of the union of myanmar).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(thailand|kingdom of thailand).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(laos|lao pdr|lao people's democratic republic|lao volksrepublik).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(kambodscha|cambodia|kingdom of cambodia|kampuchea).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(vietnam|viet nam|socialist republic of viet nam).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(malaysia|malaysien|federation of malaysia).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(brunei|brunei darussalam|negara brunei darussalam).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(indonesien|indonesia|republic of indonesia|republik indonesia).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(philippinen|philippines|republic of the philippines).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(osta[ -]?timor|osttimor|timor[- ]leste|east timor|democratic republic of timor[- ]leste).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # East Asia & Central Asia
  x <- ifelse(grepl(".*(taiwan|roc taiwan|republic of china \\(taiwan\\)).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(mongolei|mongolia|mongol uls).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(kasachstan|kazakhstan|qazaqstan|republic of kazakhstan).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(kirgisistan|kyrgyzstan|kirgistan|kyrgyz republic|kyrgyzskaia).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(usbekistan|uzbekistan|o'zbekiston|uzbekiston).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(turkmenistan|tuerkmenistan|türkmenistan|turkmenistan).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(tadschikistan|tajikistan|tojikiston|republic of tajikistan).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(afghanistan|islamic republic of afghanistan).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Oceania
  x <- ifelse(grepl(".*(neuseeland|new zealand|aotearoa).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(papua[- ]neu[- ]guinea|papua new guinea|png).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(fidschi|fiji|republic of fiji).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(samoa|independent state of samoa|wes[ts]amoa).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(tonga|kingdom of tonga).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(vanuatu|republic of vanuatu|new hebrides).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(solomon islands|salomonen|solomonen|solomons).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(föderierte staaten von mikronesien|micronesia|federated states of micronesia|fsm).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(palau|republic of palau|belau).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(marshallinseln|marshall islands|republic of the marshall islands|rmi).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(kiribati|republic of kiribati).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)nauru(\\b|$)|.*(republic of nauru).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Americas (Latin America & Caribbean)
  x <- ifelse(grepl(".*(guatemala|republic of guatemala).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)belize(\\b|$)|.*(belize state).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(honduras|republic of honduras).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(el salvador|republic of el salvador|elsalvador).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(nicaragua|republic of nicaragua).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(costa rica|costa[- ]rica|republic of costa rica).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(panama|panamá|republic of panama).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(kolumbien|colombia|republic of colombia).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(venezuela|bolivarian republic of venezuela).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)guyana(\\b|$)|.*(co-operative republic of guyana).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)suriname(\\b|$)|.*(republic of suriname).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(ecuador|república del ecuador|republic of ecuador).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)peru(\\b|$)|.*(perú|republic of peru|rep[úu]blica del per[uú]).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)bolivia(\\b|$)|.*(plurinational state of bolivia|estado plurinacional de bolivia).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)paraguay(\\b|$)|.*(republic of paraguay).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)uruguay(\\b|$)|.*(eastern republic of uruguay|república oriental del uruguay).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)argentin[a|ien](\\b|$)|.*(argentina|argentinien|argentine republic).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  # (Brazil, Mexico already covered)
  
  # Caribbean
  x <- ifelse(grepl("\\b(kuba|cuba|republic of cuba)\\b", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)haiti(\\b|$)|.*(republic of haiti|république d'haïti|republique d haiti).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(dominikanische republik|dominican republic|república dominicana).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(jamaika|jamaica).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(trinidad und tobago|trinidad and tobago|t&t|trinidad).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(^|\\b)barbados(\\b|$).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)bahamas(\\b|$)|.*(commonwealth of the bahamas).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(grenada|grenada state).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)st\\.? lucia(\\b|$)|.*(saint lucia).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(st\\.? vincent|saint vincent|grenadines|saint vincent and the grenadines).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(antigua|barbuda|antigua and barbuda).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(^|\\b)dominica(\\b|$)|.*(commonwealth of dominica).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*(st\\.? kitts|saint kitts|nevis|saint kitts and nevis).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Additional territories often seen
  x <- ifelse(grepl(".*(hong kong|hongkong|hksar|macau|macao|macau sar|macao sar).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*(puerto rico|us[- ]virgin islands|british virgin islands|cayman islands|bermuda|greenland|färöer|faeroe|faroe islands).*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Exclude common company/legal endings (treat as non-city)
  x <- ifelse(grepl(".*\\b(gmbh[[:punct:] ]*(?:&|und)[[:punct:] ]*co[[:punct:] ]*kga?a?|gmbh[[:punct:] ]*co[[:punct:] ]*kg)\\b.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*\\b(gmbh i\\.?g\\.|gmbh i\\.? l\\.|gmbh i\\.?l\\.|gmbh in liqu?\\.|gmbh in gruendung|gmbh in gr[[:alpha:]]ndung)\\b.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*\\b(gmbh)\\b.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  # Exclude company/legal endings & business buzzwords (not cities)
  x <- ifelse(grepl(".*\\b(gmbh|sarl|holding|gruppe|solutions|consulting|services|logistics|logistik|systems|systeme|technik|technologies)\\b.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  x <- ifelse(grepl(".*\\blidl\\b.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- ifelse(grepl(".*\\bkeine in dach\\b.*|.*\\bueberall\\b.*", x, perl=TRUE), "Non-Swiss City - Do not match", x)
  
  x <- trimws(x)

}
