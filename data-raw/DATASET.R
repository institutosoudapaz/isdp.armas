## code to prepare `DATASET` dataset goes here

library(tidyverse)

usethis::use_data(DATASET, overwrite = TRUE)

# dados_sp2 <- readxl::read_excel(
#   "data-raw/SP pos recurso 23042024SIC 55366246800 - Recurso_xlsx.xlsx",
#   sheet = "Base de Dados (1)")

dados_sp2 <- readRDS("data-raw/dados_sp2.rds")

ocorrencias <- dados_sp2 |>
  distinct(
    ID_DELEGACIA, CIDADE, ANO_BO, NUM_BO
  )

armas <- dados_sp2 |>
  distinct(
    ID_DELEGACIA, CIDADE, ANO_BO, NUM_BO,
    CONT_ARMA,
    DESCR_MODO_OBJETO,
    CALIBRE_ARMA,
    NUMERO_ARMA,
    MARCA_ARMA,
    DESCR_ARMA_FOGO
  )

estatistica_oficial <- tibble(
  ANO_BO = 2018:2023,
  estatistica_oficial = c(13138, 12810, 11551, 11787, 8408, 11754)
)

comparacao_contagens <- ocorrencias |>
  count(ANO_BO) |>
  rename(numero_ocorrencias = n) |>
  left_join(
    apreensoes |>
      count(ANO_BO) |>
      rename(
        armas_distintas_apreendidas = n
      )
  ) |>
  left_join(
    estatistica_oficial
  )

armas |>
  mutate(
    MARCA_ARMA = tolower(MARCA_ARMA)
  ) |>
  left_join(
    distinct(remake |>
               mutate(
                 MARCA_ARMA = tolower(MARCA_ARMA)
               ), MARCA_ARMA, .keep_all = TRUE)
  ) |>
  count(
    MARCA_ARMA, MARCA_ARMA_V2
  ) |>
  View()

regex_marcas<- remake |>
  mutate(
    MARCA_ARMA = tolower(MARCA_ARMA)
  ) |>
  group_by(MARCA_ARMA_V2) |>
  summarise(
    regex = regex(paste0(unique(MARCA_ARMA), collapse = "|"), ignore_case = TRUE)
  ) |>
  arrange(
    MARCA_ARMA_V2
  )

armas_arrumado <- armas |>
  mutate(
    MARCA_ARMA = tolower(MARCA_ARMA) |> stringr::str_remove_all("[:space:]"),
    MARCA_ARMA_V2 = incluir_marcas(MARCA_ARMA),
    CALIBRE_ARMA_V2 = incluir_calibres(CALIBRE_ARMA)
  )

armas_arrumado |>
  count(CALIBRE_ARMA, CALIBRE_ARMA_V2) |> View()

usethis::use_data(regex_marcas)


