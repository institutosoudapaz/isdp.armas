dados <- readxl::read_excel('data-raw/tabela_bruta.xlsx')

marca_legado <- readxl::read_excel("inst/tabelas_depara/depara_marca_legado.xlsx")

library(tidyverse)

final <- dados |>
  mutate(
    MARCA_ARMA = map(MARCA_ARMA, function(x){
      tibble(MARCA_ARMA = as.vector(str_split(x, pattern = ", ?", simplify = TRUE)))
    })
  ) |>
  unnest(MARCA_ARMA) |>
  rename(arma_marca = MARCA_ARMA, marca_arma_v2 = MARCA_ARMA_V2,
         pais_fabricacao = PAIS_FABRICACAO) |>
  bind_rows(marca_legado) |>
  distinct(arma_marca, marca_arma_v2, pais_fabricacao) |>
  arrange(marca_arma_v2) |>
  janitor::clean_names() |>
  filter(arma_marca != "") |>
  mutate(
    marca_arma_v2 = case_when(
      marca_arma_v2 == "SMITH & WESSON" ~ "S&W",
      marca_arma_v2 == "KEL-TEC" ~ "KEL TEC",
      TRUE ~ toupper(marca_arma_v2))
  ) |>
  distinct(arma_marca, marca_arma_v2, .keep_all = TRUE)

final |>
  writexl::write_xlsx("inst/tabelas_depara/depara_marca.xlsx")
