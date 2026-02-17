# Title: Convert Neotropical Carnivores dataset to Darwin Core format
# Author: Rogério Nunes Oliveira
# Date: 2026-02-17
# Version: 2.0

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, uuid, stringr, lubridate)

# File paths
arquivo_autores      <- "nagy-reis_et_al_2020/autores.txt"
arquivo_instituicoes <- "nagy-reis_et_al_2020/instituições.txt"
arquivo_csv          <- "nagy-reis_et_al_2020/ecy3128-sup-0001-datas1/NEOTROPICAL_CARNIVORES_DATASET_repat_test.csv"

# Month name → number lookup
MONTH_MAP <- c(
  "jan" = "01", "january" = "01", "janeiro" = "01",
  "fev" = "02", "feb" = "02", "february" = "02", "fevereiro" = "02",
  "mar" = "03", "march" = "03", "março" = "03",
  "abr" = "04", "apr" = "04", "april" = "04", "abril" = "04",
  "mai" = "05", "may" = "05", "maio" = "05",
  "jun" = "06", "june" = "06", "junho" = "06",
  "jul" = "07", "july" = "07", "julho" = "07",
  "ago" = "08", "aug" = "08", "august" = "08", "agosto" = "08",
  "set" = "09", "sep" = "09", "september" = "09", "setembro" = "09",
  "out" = "10", "oct" = "10", "october" = "10", "outubro" = "10",
  "nov" = "11", "november" = "11", "novembro" = "11",
  "dez" = "12", "dec" = "12", "december" = "12", "dezembro" = "12"
)

convert_month_to_num <- function(month_vec) {
  month_vec <- tolower(trimws(month_vec))
  return(unname(MONTH_MAP[month_vec]))
}

# Extract institution acronym from free-text affiliation string.
# Priority: (1) explicit acronym in parentheses, (2) Brazilian university
# pattern heuristic, (3) initials of significant words.
extract_institution_acronym <- function(inst_text) {
  if (is.na(inst_text) || nchar(trimws(inst_text)) < 3) return(NA_character_)

  inst_text <- str_replace_all(inst_text, "\\s+", " ")
  inst_text <- trimws(inst_text)

  # Priority 1: explicit acronym inside parentheses (ex: USP, UNEMAT, INPA)
  paren_pattern <- "\\(([A-Z][A-Z0-9/-]{1,})\\)"
  paren_matches <- str_extract_all(inst_text, paren_pattern)[[1]]

  if (length(paren_matches) > 0) {
    acronyms <- str_replace_all(paren_matches, "\\(|\\)", "")
    acronyms <- acronyms[!str_detect(acronyms, "^\\d+$")]

    estados_br <- c(
      "SP", "RJ", "MG", "RS", "PR", "SC", "BA", "CE", "PE", "PA",
      "AM", "RO", "AC", "AP", "RR", "TO", "MA", "PI", "AL", "SE",
      "PB", "RN", "GO", "MT", "MS", "DF", "ES"
    )
    acronyms <- acronyms[!(nchar(acronyms) == 2 & toupper(acronyms) %in% estados_br)]

    if (length(acronyms) > 0) {
      return(paste(unique(acronyms), collapse = " | "))
    }
  }

  # Priority 2: Brazilian university name pattern
  if (str_detect(inst_text, "(?i)^Universidade")) {
    inst_core <- str_split(inst_text, ",")[[1]][1]
    inst_core <- trimws(inst_core)

    univ_pattern <- "^Universidade\\s+(Federal|Estadual|Católica|de)\\s+(de|do|da|Dom)\\s+(.+)$"
    match        <- str_match(inst_core, univ_pattern)

    if (!is.na(match[1, 1])) {
      tipo           <- match[1, 2]
      local          <- match[1, 4]
      tipo_letter    <- substr(toupper(tipo), 1, 1)
      local_clean    <- str_replace_all(local, "\\s+(de|do|da|dos|das|e)\\s+", " ")
      local_words    <- str_split(local_clean, "\\s+")[[1]]
      local_words    <- local_words[nchar(local_words) > 0]
      local_initials <- paste0(toupper(substr(local_words, 1, 1)), collapse = "")
      acronym        <- paste0("U", tipo_letter, local_initials)

      if (str_detect(inst_core, "São Paulo") && str_detect(inst_core, "^Universidade de")) return("USP")
      if (str_detect(inst_core, "Campinas"))                                                return("UNICAMP")
      if (str_detect(inst_core, "Rio de Janeiro") && str_detect(inst_core, "Federal"))     return("UFRJ")
      if (str_detect(inst_core, "Minas Gerais") && str_detect(inst_core, "Federal"))       return("UFMG")

      return(acronym)
    }

    univ_pattern2 <- "^Universidade\\s+(de|do|da)\\s+(.+)$"
    match2        <- str_match(inst_core, univ_pattern2)

    if (!is.na(match2[1, 1])) {
      local          <- match2[1, 3]
      local_clean    <- str_replace_all(local, "\\s+(de|do|da|dos|das|e)\\s+", " ")
      local_words    <- str_split(local_clean, "\\s+")[[1]]
      local_words    <- local_words[nchar(local_words) > 0]
      local_initials <- paste0(toupper(substr(local_words, 1, 1)), collapse = "")
      return(paste0("U", local_initials))
    }
  }

  # Priority 3: initials of significant words
  inst_core  <- str_split(inst_text, ",")[[1]][1]
  inst_core  <- trimws(inst_core)
  text_clean <- str_replace_all(inst_core, "[^A-Za-zÀ-ÿ\\s]", " ")

  stopwords <- c(
    "universidade", "university", "instituto", "institute", "centro", "center",
    "centre", "departamento", "department", "faculdade", "faculty", "escola",
    "school", "programa", "program", "laboratorio", "laboratory", "lab",
    "nacional", "national", "federal", "state", "estadual", "municipal",
    "de", "da", "do", "dos", "das", "of", "the", "and", "e", "for", "em",
    "secretaria", "ministerio", "conselho", "fundacao", "fundación",
    "associacao", "asociación", "grupo"
  )

  words       <- str_split(text_clean, "\\s+")[[1]]
  words       <- words[nchar(words) > 0]
  words_clean <- words[!tolower(words) %in% stopwords]

  if (length(words_clean) == 0) return(NA_character_)

  acronym <- paste0(toupper(substr(words_clean, 1, 1)), collapse = "")

  if (nchar(acronym) < 2 || nchar(acronym) > 15) return(NA_character_)

  return(acronym)
}

# Normalize a name string to ASCII lowercase with no punctuation.
normalize_name <- function(name) {
  if (is.na(name) || nchar(trimws(name)) < 2) return(NA_character_)
  name <- trimws(name)
  name <- iconv(name, to = "ASCII//TRANSLIT")
  name <- str_replace_all(name, "[^A-Za-z0-9\\s-]", " ")
  name <- str_replace_all(name, "-", " ")
  name <- str_replace_all(name, "\\s+", " ")
  name <- trimws(name)
  return(tolower(name))
}

extract_name_parts <- function(name) {
  if (is.na(name) || nchar(trimws(name)) < 2) return(list(surname = NA, given = NA, initials = NA))

  name <- trimws(name)

  if (str_detect(name, ",")) {
    parts     <- str_split(name, ",")[[1]]
    surname   <- normalize_name(trimws(parts[1]))
    given_raw <- trimws(parts[2])
    initials  <- str_extract_all(given_raw, "[A-Z]")[[1]]
    return(list(surname = surname, given = normalize_name(given_raw), initials = initials))
  }

  name_norm <- normalize_name(name)
  words     <- str_split(name_norm, "\\s+")[[1]]
  words     <- words[nchar(words) > 0]

  if (length(words) == 0) return(list(surname = NA, given = NA, initials = NA))

  surname <- words[length(words)]

  if (length(words) > 1) {
    given_words <- words[1:(length(words) - 1)]
    initials    <- toupper(substr(given_words, 1, 1))
    return(list(surname = surname, given = paste(given_words, collapse = " "), initials = initials))
  }

  return(list(surname = surname, given = NA, initials = NA))
}

# Match a CSV author string against a reference data frame of TXT authors.
# Scoring: 10 pts per full-word match, 5 pts per initial match.
# Returns the key of the best match (minimum score = 10).
match_author_by_words <- function(csv_name, txt_names_df) {
  if (is.na(csv_name) || nchar(trimws(csv_name)) < 2) return(NA_character_)

  csv_norm  <- normalize_name(csv_name)
  csv_words <- str_split(csv_norm, "\\s+")[[1]]
  csv_words <- csv_words[nchar(csv_words) > 0]

  if (length(csv_words) == 0) return(NA_character_)

  best_match <- NA_character_
  max_score  <- 0

  for (i in 1:nrow(txt_names_df)) {
    txt_name  <- txt_names_df$full_name[i]
    txt_norm  <- normalize_name(txt_name)
    txt_words <- str_split(txt_norm, "\\s+")[[1]]
    txt_words <- txt_words[nchar(txt_words) > 0]

    if (length(txt_words) == 0) next

    score <- 0

    for (word in csv_words) {
      if (word %in% txt_words) {
        score <- score + 10
      }
    }

    for (word in csv_words) {
      if (nchar(word) == 1) {
        for (txt_word in txt_words) {
          if (substr(txt_word, 1, 1) == word) {
            score <- score + 5
          }
        }
      }
    }

    if (score > max_score) {
      max_score  <- score
      best_match <- txt_names_df$key[i]
    }
  }

  if (max_score >= 10) return(best_match)
  return(NA_character_)
}

# Parse author and institution TXT files and return a named list:
# $authors — data frame with author name and affiliation IDs
# $lookup  — data frame linking author key to institution codes/names
process_txt_files <- function(autores_path, instituicoes_path) {
  if (!file.exists(autores_path))      stop(paste("Arquivo de autores não encontrado:", autores_path))
  if (!file.exists(instituicoes_path)) stop(paste("Arquivo de instituições não encontrado:", instituicoes_path))

  autores_lines <- readLines(autores_path, encoding = "UTF-8", warn = FALSE)
  autores_lines <- autores_lines[nchar(trimws(autores_lines)) > 0]

  author_pattern <- "^(.+?)\\s+(\\d+(?:,\\d+)*)\\s*$"
  authors_df     <- tibble()

  for (line in autores_lines) {
    match <- str_match(line, author_pattern)
    if (!is.na(match[1, 2])) {
      authors_df <- bind_rows(
        authors_df,
        tibble(full_name = str_trim(match[1, 2]), id_str = match[1, 3])
      )
    }
  }

  if (nrow(authors_df) == 0) stop("Nenhum autor extraído")

  authors_df <- authors_df %>%
    mutate(
      key = paste0("author_", row_number()),
      ids = str_split(id_str, ",")
    )

  instituicoes_lines <- readLines(instituicoes_path, encoding = "UTF-8", warn = FALSE)
  instituicoes_lines <- instituicoes_lines[nchar(trimws(instituicoes_lines)) > 0]

  affiliations_df <- tibble()

  for (line in instituicoes_lines) {
    parts <- str_split(line, "\\t+")[[1]]
    if (length(parts) >= 2) {
      id_part   <- trimws(parts[1])
      text_part <- trimws(paste(parts[-1], collapse = " "))

      if (str_detect(id_part, "^\\d+$")) {
        affiliations_df <- bind_rows(
          affiliations_df,
          tibble(id = as.integer(id_part), text = text_part)
        )
      }
    }
  }

  if (nrow(affiliations_df) == 0) stop("Nenhuma instituição extraída")

  lookup <- authors_df %>%
    select(key, ids) %>%
    unnest(ids) %>%
    mutate(id = as.integer(ids)) %>%
    left_join(affiliations_df, by = "id") %>%
    filter(!is.na(text)) %>%
    mutate(institution_code = sapply(text, extract_institution_acronym)) %>%
    filter(!is.na(institution_code)) %>%
    group_by(key) %>%
    summarise(
      institutions      = paste(unique(institution_code), collapse = " | "),
      institution_names = paste(unique(text), collapse = " | "),
      .groups = "drop"
    )

  return(list(authors = authors_df, lookup = lookup))
}

# Main processing

txt_data           <- process_txt_files(arquivo_autores, arquivo_instituicoes)
authors_reference  <- txt_data$authors
institution_lookup <- txt_data$lookup

if (nrow(institution_lookup) == 0) warning("ATENÇÃO: Lookup de instituições está vazio!")

input_df <- read_csv(arquivo_csv, col_types = cols(.default = "c"), show_col_types = FALSE)

unique_first_authors <- input_df %>%
  filter(!is.na(DATA_TEAM), DATA_TEAM != "") %>%
  mutate(
    first_author_raw = sapply(strsplit(DATA_TEAM, ";"), function(x) trimws(x[1]))
  ) %>%
  filter(nchar(first_author_raw) >= 3) %>%
  distinct(first_author_raw) %>%
  pull(first_author_raw)

pb <- txtProgressBar(min = 0, max = length(unique_first_authors), style = 3)

author_to_inst <- tibble(
  first_author = unique_first_authors,
  institutionCode = sapply(seq_along(unique_first_authors), function(i) {
    setTxtProgressBar(pb, i)
    fa         <- unique_first_authors[i]
    author_key <- match_author_by_words(fa, authors_reference)
    if (is.na(author_key)) return(NA_character_)
    matched <- institution_lookup %>% filter(key == author_key) %>% pull(institutions)
    if (length(matched) > 0) return(matched[1])
    return(NA_character_)
  }),
  institutionName = sapply(seq_along(unique_first_authors), function(i) {
    fa         <- unique_first_authors[i]
    author_key <- match_author_by_words(fa, authors_reference)
    if (is.na(author_key)) return(NA_character_)
    matched <- institution_lookup %>% filter(key == author_key) %>% pull(institution_names)
    if (length(matched) > 0) return(matched[1])
    return(NA_character_)
  })
)

close(pb)

# Darwin Core conversion

output_df <- input_df %>%
  mutate(across(everything(), ~na_if(., "NA"))) %>%
  mutate(across(everything(), ~na_if(., ""))) %>%
  mutate(
    datasetName         = "NEOTROPICAL CARNIVORES: a data set on carnivore distribution in the Neotropics",
    license             = "CC0",
    modified            = "2026-01-26",
    language            = "en",
    collectionCode      = NA_character_,
    day                 = NA_character_,
    month               = NA_character_,
    year                = NA_character_,
    dataGeneralizations = NA_character_,
    occurrenceID        = sapply(1:n(), function(x) UUIDgenerate(use.time = FALSE))
  ) %>%
  mutate(
    informationWithheld           = REF_TYPE,
    occurrenceRemarks             = DATA_TYPE,
    county                        = MUNICIPALITY,
    stateProvince                 = STATE,
    country                       = COUNTRY,
    decimalLongitude              = as.numeric(LONG_X),
    decimalLatitude               = as.numeric(LAT_Y),
    coordinateUncertaintyInMeters = as.numeric(PRECISION_m),
    scientificName                = SPECIES,
    verbatimIdentification        = SPECIES,
    genus                         = GENUS,
    family                        = FAMILY,
    type                          = TYPE_REC,
    individualCount               = as.integer(N_RECORDS),
    recordedBy                    = str_replace_all(DATA_TEAM, "\\s*;\\s*", " | "),
    bibliographicCitation         = ifelse(
      is.na(REFERENCE) | REFERENCE == "",
      "Nagy-Reis, M., et al. 2020. NEOTROPICAL CARNIVORES... Ecology 101(11):e03128",
      REFERENCE
    ),
    fieldNotes       = OBS,
    occurrenceStatus = ifelse(OCCUR == "1", "Present", "Absent"),
    taxonRank        = ifelse(str_detect(SPECIES, "sp"), "genus", "species"),
    specificEpithet  = ifelse(str_detect(SPECIES, "sp"), NA_character_, word(SPECIES, 2))
  ) %>%
  mutate(
    locationRemarks = case_when(
      !is.na(SITE) & !is.na(AREA_HA) ~ paste(SITE, paste0(AREA_HA, " HA"), sep = " | "),
      !is.na(SITE)                    ~ SITE,
      !is.na(AREA_HA)                 ~ paste0(AREA_HA, " HA"),
      TRUE                            ~ NA_character_
    ),
    is_protected = str_detect(SITE, "Parque|Nacional|Estadual|Reserva|Station|Rebio|APA"),
    locality     = ifelse((PROT_AREA == "Yes" | is_protected), SITE, NA_character_),
    habitat      = case_when(
      !is.na(VEG_LAND) & !is.na(VEG_LAND_BUFFER) ~ paste(VEG_LAND, VEG_LAND_BUFFER, sep = " | "),
      !is.na(VEG_LAND)                            ~ VEG_LAND,
      !is.na(VEG_LAND_BUFFER)                     ~ VEG_LAND_BUFFER,
      TRUE                                        ~ NA_character_
    ),
    s_m = convert_month_to_num(COL_START_MO),
    e_m = convert_month_to_num(COL_END_MO),
    eventDate = paste(
      ifelse(!is.na(s_m), paste0(COL_START_YR, "-", s_m), COL_START_YR),
      ifelse(!is.na(e_m), paste0(COL_END_YR,   "-", e_m), COL_END_YR),
      sep = "/"
    ),
    p_e = ifelse(!is.na(EFFORT) & !is.na(EFFORT_UNIT), paste(EFFORT, EFFORT_UNIT), NA),
    p_c = ifelse(!is.na(CAM_IND), paste0(CAM_IND, " min interv"), NA),
    p_l = SAMPLING_LEVEL,
    samplingEffort = case_when(
      !is.na(p_e) & !is.na(p_c) & !is.na(p_l) ~ paste(p_e, p_c, p_l, sep = " | "),
      !is.na(p_e) & !is.na(p_c)                ~ paste(p_e, p_c, sep = " | "),
      !is.na(p_e) & !is.na(p_l)                ~ paste(p_e, p_l, sep = " | "),
      !is.na(p_c) & !is.na(p_l)                ~ paste(p_c, p_l, sep = " | "),
      !is.na(p_e)                               ~ p_e,
      !is.na(p_c)                               ~ p_c,
      !is.na(p_l)                               ~ p_l,
      TRUE                                      ~ NA_character_
    ),
    basisOfRecord = case_when(
      str_starts(METHOD, "Camera trap|GPS|Radio tracking|Arboreal|Olfactory|Padded")                                                                  ~ "MachineObservation",
      str_starts(METHOD, "Active searching|Footprint|Informal|Line|Live trap|Opportunistic|Railway|Roadkill|Sand|Scat|Sign|Track|Tracking|Vehicular") ~ "HumanObservation",
      str_starts(METHOD, "Literature")                                                                                                                 ~ "MaterialCitation",
      str_starts(METHOD, "Museum")                                                                                                                     ~ "PreservedSpecimen",
      TRUE                                                                                                                                             ~ NA_character_
    )
  ) %>%
  mutate(
    first_author_raw = sapply(strsplit(DATA_TEAM, ";"), function(x) {
      if (length(x) > 0) trimws(x[1]) else NA_character_
    }),
    institutionCode = author_to_inst$institutionCode[match(first_author_raw, author_to_inst$first_author)],
    institutionName = author_to_inst$institutionName[match(first_author_raw, author_to_inst$first_author)]
  ) %>%
  select(-is_protected, -s_m, -e_m, -p_e, -p_c, -p_l, -first_author_raw)

final_cols <- c(
  "occurrenceID", "datasetName", "license", "modified", "language", "collectionCode",
  "institutionCode", "institutionName", "basisOfRecord", "occurrenceStatus",
  "eventDate", "day", "month", "year", "recordedBy",
  "scientificName", "verbatimIdentification", "genus", "specificEpithet",
  "taxonRank", "family", "country", "stateProvince", "county", "locality",
  "locationRemarks", "decimalLongitude", "decimalLatitude",
  "coordinateUncertaintyInMeters", "habitat", "samplingEffort", "fieldNotes",
  "individualCount", "type", "bibliographicCitation", "informationWithheld",
  "occurrenceRemarks", "dataGeneralizations",
  "STUDY_RECORD_ID", "DATASET", "FILENAME"
)

output_final <- output_df %>% select(any_of(final_cols))

write_csv(output_final, "saida_dwc.csv", na = "")
