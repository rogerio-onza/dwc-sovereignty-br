# Title: Conversão do dataset Jaguar Movement (Morato et al. 2018) para Darwin Core
# Author: Rogerio Nunes Oliveira
# Date: 2026-02-25
# Version: 1.0

# Carregar bibliotecas
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, uuid, lubridate)

# Configuração de arquivos

arquivo_csv <- "morato_et_al_2018/DataS1/DataS1/jaguar_movement_data.csv"
arquivo_saida <- "morato_et_al_2018/jaguar_movement_darwin_core.csv"

# Leitura dos dados

input_df <- read_csv(arquivo_csv, col_types = cols(.default = "c"), show_col_types = FALSE)

# Renomear coluna com espaço/parêntese para facilitar o uso
input_df <- input_df %>%
    rename(individual_id = `individual.local.identifier (ID)`)



# Conversão para Darwin Core

output_df <- input_df %>%
    mutate(across(everything(), ~ na_if(., "NA"))) %>%
    mutate(across(everything(), ~ na_if(., ""))) %>%
    mutate(
        # Campos fixos para o dataset
        datasetName = "Jaguar movement ecology: a data set on jaguar movement in South America",
        license = "CC0",
        modified = "2026-02-25",
        language = "en",

        # Identificadores
        occurrenceID = sapply(1:n(), function(x) UUIDgenerate(use.time = FALSE)),
        eventID = Event_ID,
        recordNumber = individual_id,
        organismID = tag.local.identifier,

        # Taxonomia — espécie única no dataset
        scientificName = individual.taxon.canonical.name,
        genus = word(individual.taxon.canonical.name, 1),
        specificEpithet = word(individual.taxon.canonical.name, 2),
        taxonRank = "species",
        family = "Felidae",

        # Tipo de registro: dados de GPS/telemetria
        basisOfRecord = "MachineObservation",
        occurrenceStatus = "Present",

        # Data e hora: separar timestamp "M/D/YY H:MM" em eventDate e eventTime
        timestamp_parsed = mdy_hm(timestamp),
        eventDate = format(timestamp_parsed, "%Y-%m-%d"),
        eventTime = format(timestamp_parsed, "%H:%M:00"),
        year = format(timestamp_parsed, "%Y"),
        month = format(timestamp_parsed, "%m"),
        day = format(timestamp_parsed, "%d"),

        # Coordenadas
        decimalLongitude = as.numeric(location.long),
        decimalLatitude = as.numeric(location.lat),

        # Localização
        higherGeography = study.name,
        country = country,

        # Citação
        bibliographicCitation = "Morato, R.G., et al. 2018. Jaguar movement database: a GPS telemetry dataset. Ecology."
    ) %>%
    select(-timestamp_parsed)

# Selecionar colunas finais (Darwin Core)

final_cols <- c(
    "occurrenceID", "eventID", "datasetName", "license", "modified", "language",
    "basisOfRecord", "occurrenceStatus",
    "eventDate", "eventTime", "year", "month", "day",
    "scientificName", "genus", "specificEpithet", "taxonRank", "family",
    "organismID", "recordNumber",
    "higherGeography", "country",
    "decimalLongitude", "decimalLatitude",
    "bibliographicCitation"
)

# Manter apenas colunas que existam no output
final_cols <- final_cols[final_cols %in% names(output_df)]

output_final <- output_df %>% select(all_of(final_cols))

# Salvar

write_csv(output_final, arquivo_saida, na = "")
