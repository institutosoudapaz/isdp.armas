# Este script é utilizado para criar a tabela `inst/tabelas_depara/taurus_regra_3.xlsx`
# e não precisará mais ser rodado a não ser em caso de ajustes.

letras <- c("A", "B", "C", "D", "E", "G", "H", "J", "K", "L", "M", "N", "P", "R", "S", "T", "U", "X", "Y", "Z")

expand.grid(
  arma_ns_primeira_letra = letras,
  arma_ns_segunda_letra = letras,
  arma_ns_terceira_letra = letras[1:12]
) |>
  dplyr::arrange(
    arma_ns_primeira_letra,
    arma_ns_segunda_letra,
    arma_ns_terceira_letra
  ) |>
  dplyr::group_by(
    arma_ns_primeira_letra,
    arma_ns_segunda_letra,
  ) |>
  dplyr::mutate(
    arma_ano_fabricacao = dplyr::cur_group_id() + 2018,
    arma_mes_fabricacao = dplyr::row_number()
  ) |>
  dplyr::filter(arma_ano_fabricacao <= 2045) |>
  writexl::write_xlsx("inst/tabelas_depara/taurus_regra_3.xlsx")
