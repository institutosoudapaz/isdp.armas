tabela <- tibble::tibble(
  ano_fabricacao_padrao3 = c(2019:2045),
  primeiros_digitos = c("AA", "AB", "AC", "AD", "AE", "AG", "AH", "AJ", "AK", "AL", "AM", "NA", "AP", "AR", "AS", "AT", "AU", "AX", "AY", "AZ", "BA", "BB", "BC", "BD", "BE", "BG", "BH")
)

tabela_2019 <- tabela |>
  dplyr::filter(ano_fabricacao_padrao3 <= 2019) |>
  dplyr::mutate(
    terceiro_digito = list(
      tibble(
        terceiro_digito = LETTERS[9:12],
        mes_fabricacao_padrao3 = 9:12)
    )) |>
  tidyr::unnest(terceiro_digito)

tabela_2020_vigente <- tabela |>
  dplyr::filter(ano_fabricacao_padrao3 >= 2020) |>
  dplyr::mutate(
    terceiro_digito = list(
      tibble(
        terceiro_digito = LETTERS[1:12],
        mes_fabricacao_padrao3 = 1:12)
  )) |>
  tidyr::unnest(terceiro_digito)

padroes_taurus_vigente <-tabela_2019 |>
  bind_rows(tabela_2020_vigente)

usethis::use_data(padroes_taurus_vigente, overwrite = TRUE)


