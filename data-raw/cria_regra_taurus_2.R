# Este script é utilizado para criar a tabela `inst/tabelas_depara/taurus_regra_2.xlsx`
# e não precisará mais ser rodado a não ser em caso de ajustes.

anos <- 1981:2006
n_anos <- length(anos)
padrao_1981_2006 <- tibble::tibble(
  arma_ns_primeira_letra = rep(LETTERS, rep(12, n_anos)),
  arma_ns_segunda_letra = rep(LETTERS[1:12], n_anos),
  arma_ano_fabricacao = rep(anos, rep(12, n_anos)),
  arma_mes_fabricacao = rep(1:12, n_anos),
)

anos <- 2007:2009
n_anos <- length(anos)
letras <- c("M", "N", "O", "P", "R", "S", "T", "U", "V", "W", "X", "Y")
padrao_2007_2009 <- tibble::tibble(
  arma_ns_primeira_letra = rep(LETTERS[1:n_anos], rep(12, n_anos)),
  arma_ns_segunda_letra = rep(letras, n_anos),
  arma_ano_fabricacao = rep(anos, rep(12, n_anos)),
  arma_mes_fabricacao = rep(1:12, n_anos),
)

anos <- 2010:2019
n_anos <- length(anos)
letras <- c("M", "N", "O", "P", "R", "S", "T", "U", "W", "X", "Y", "Z")
padrao_2010_2019 <- tibble::tibble(
  arma_ns_primeira_letra = rep(LETTERS[4:(4+n_anos-1)], rep(12, n_anos)),
  arma_ns_segunda_letra = rep(letras, n_anos),
  arma_ano_fabricacao = rep(anos, rep(12, n_anos)),
  arma_mes_fabricacao = rep(1:12, n_anos),
)

dplyr::bind_rows(
  padrao_1981_2006,
  padrao_2007_2009,
  padrao_2010_2019
) |>
  writexl::write_xlsx("inst/tabelas_depara/taurus_regra_2.xlsx")
