depara_1981_2006 <- expand.grid(primeiro_digito = LETTERS, segundo_digito = LETTERS[1:12]) |>
  left_join(tibble(primeiro_digito = LETTERS, ano_fabricacao_padrao2 = 1981:2006)) |>
  left_join(tibble(segundo_digito = LETTERS[1:12], mes_fabricacao_padrao2 = 1:12))

vetor_meses_2007_2009 <- c(
  "M",
  "N",
  "O",
  "P",
  "R",
  "S",
  "T",
  "U",
  "V",
  "W",
  "X",
  "Y"
)

depara_2007_2009 <- expand.grid(primeiro_digito = LETTERS[1:3], segundo_digito = vetor_meses_2007_2009) |>
  left_join(tibble(primeiro_digito = LETTERS[1:3], ano_fabricacao_padrao2 = 2007:2009)) |>
  left_join(tibble(segundo_digito = vetor_meses_2007_2009, mes_fabricacao_padrao2 = 1:12))

vetor_meses_2010_2019 <- c(
  "M",
  "N",
  "O",
  "P",
  "R",
  "S",
  "T",
  "U",
  "W",
  "X",
  "Y",
  "Z"
)

depara_2010_2019 <- expand.grid(primeiro_digito = LETTERS[4:13], segundo_digito = vetor_meses_2010_2019) |>
  left_join(tibble(primeiro_digito = LETTERS[4:13], ano_fabricacao_padrao2 = 2010:2019)) |>
  left_join(tibble(segundo_digito = vetor_meses_2010_2019, mes_fabricacao_padrao2 = 1:12))

padroes_taurus_1981_2019 <- bind_rows(
  depara_1981_2006,
  depara_2007_2009,
  depara_2010_2019
) |>
  dplyr::mutate(
    marca_padrao2 = "taurus"
  )

usethis::use_data(padroes_taurus_1981_2019, overwrite = TRUE)
