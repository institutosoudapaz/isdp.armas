serial_code <- c(
  "A",
  "B",
  "C",
  "D",
  "F",
  "G",
  "J",
  "K",
  "L",
  "N",
  "S",
  "T",
  "X"
)

calibre <- c(
  ".22",
  ".400",
  ".41",
  "6.35",
  "7.65",
  ".45",
  ".357",
  ".380",
  ".38",
  ".45",
  ".40",
  "9 x 19 mm",
  "9 x 21 mm"
)

marca <- "taurus"

modelo <- c(
  "LR",
  "COR-BON",
  "AE",
  NA_character_,
  NA_character_,
  "GAP",
  "SIG",
  "ACP",
  "Super Automatic",
  "ACP",
  "S&W",
  NA_character_,
  NA_character_
)

padroes_modelo_taurus_1987 <- tibble::tibble(
  digito_modelo = serial_code,
  calibre_padrao2 = calibre,
  tipo_arma_padrao2 = modelo,
  marca_padrao2 = marca
)

usethis::use_data(padroes_modelo_taurus_1987, overwrite = TRUE)

