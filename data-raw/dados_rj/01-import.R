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
  readr::write_rds("data-raw/dados_rj/dados_armas_rj.rds")

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
  readr::write_rds("data-raw/dados_rj/dados_ocorrencias_rj.rds")


# Dados armas complementar

dados_armas_complementar_2017 <- readxl::read_excel(
  "data-raw/dados_rj/raw/24648_APREENS_ARMA_FOGO_2017_BASE_APREENSAO.xlsx",
  guess_max = 5000
) |>
  janitor::clean_names()


dados_armas_complementar_2018 <- readxl::read_excel(
  "data-raw/dados_rj/raw/24648_APREENS_ARMA_FOGO_2018_BASE_APREENSAO.xlsx",
  guess_max = 5000
) |>
  janitor::clean_names()

dados_armas_complementar_2019 <- readxl::read_excel(
  "data-raw/dados_rj/raw/RJ 24648_APREENS_ARMA_FOGO_2019_BASE_APREENSAO.xlsx",
  guess_max = 5000
) |>
  janitor::clean_names()

dados_armas_complementar_2020 <- readxl::read_excel(
  "data-raw/dados_rj/raw/24648_APREENS_ARMA_FOGO_2020_BASE_APREENSAO.xlsx",
  guess_max = 5000
) |>
  janitor::clean_names()

dados_armas_complementar_2021 <- readxl::read_excel(
  "data-raw/dados_rj/raw/24648_APREENS_ARMA_FOGO_2021_BASE_APREENSAO.xlsx",
  guess_max = 5000,
  skip = 2,
  col_types = c(rep("guess", 16), "date", rep("guess", 4))
) |>
  janitor::clean_names()

dados_armas_complementar_2022 <- readxl::read_excel(
  "data-raw/dados_rj/raw/24648_APREENS_ARMA_FOGO_2022_02_BASE_APREENSAO.xlsx",
  guess_max = 5000
) |>
  janitor::clean_names()

dados_armas_complementar <- dplyr::bind_rows(
  dados_armas_complementar_2017,
  dados_armas_complementar_2018,
  dados_armas_complementar_2019,
  dados_armas_complementar_2020,
  dados_armas_complementar_2021,
  dados_armas_complementar_2022
)

dados_armas_complementar |>
  readr::write_rds("data-raw/dados_rj/dados_armas_complementar.rds")
