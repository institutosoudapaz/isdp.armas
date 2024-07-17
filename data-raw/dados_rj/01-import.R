# Gerar bases de armas e ocorrências para o Rio de Janeiro

# - Juntar arquivos dadosRJ_armas.csv e "Protocolo_042_2024_armas 2018.csv"
# - Juntar arquivos dadosRJ_ocorrencias.csv e "Protocolo_042_2024_ocorrencias 2018.csv"

# Dados armas

dados_armas_2018 <- readr::read_csv2(
  "data-raw/dados_rj/raw/Protocolo_042_2024_armas 2018.csv"
)
dplyr::glimpse(dados_armas_2018)

dados_armas_2019_2023 <- readr::read_csv2(
  "data-raw/dados_rj/raw/dadosRJ_armas.csv",
  locale = readr::locale(encoding = "latin1")
)
dplyr::glimpse(dados_armas_2019_2023)

dados_armas <- dplyr::bind_rows(
  dados_armas_2018,
  dados_armas_2019_2023
) # incluir cod ibge

dados_armas |>
  readr::write_rds("inst/dados_rj/dados_armas_rj.rds")

# Dados ocorrências

dados_ocorrencias_2018 <- readr::read_csv2(
  "data-raw/dados_rj/raw/Protocolo_042_2024_ocorrencias 2018.csv",
  locale = readr::locale(encoding = "latin1")
) |>
  dplyr::mutate(
    hora_fato = as.character(hora_fato),
    hora_com = as.character(hora_com)
  )
dplyr::glimpse(dados_ocorrencias_2018)

dados_ocorrencias_2019_2023 <- readr::read_csv2(
  "data-raw/dados_rj/raw/dadosRJ_ocorrencias.csv",
  locale = readr::locale(encoding = "latin1")
) |>
  dplyr::mutate(
    hora_fato = dplyr::case_when(
      is.na(hora_fato) | hora_fato == 99 ~ NA_character_,
      TRUE ~ paste0(hora_fato, ":00")
    ),
    hora_com = as.character(hora_com)
  )
dplyr::glimpse(dados_ocorrencias_2019_2023)

dados_ocorrencias <- dplyr::bind_rows(
  dados_ocorrencias_2018,
  dados_ocorrencias_2019_2023
) # incluir cod ibge

dados_ocorrencias |>
  readr::write_rds("inst/dados_rj/dados_ocorrencias_rj.rds")
