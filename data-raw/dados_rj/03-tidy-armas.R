devtools::load_all()

dados_armas <- readr::read_rds("inst/dados_rj/dados_armas_rj.rds")
dados_ocorrencias <- readr::read_rds("inst/dados_rj/dados_ocorrencias_rj.rds")

dados_armas_formatado <- dados_armas |>
  dplyr::mutate(
    ano_bo = stringr::str_sub(controle, -4, -1)
  ) |>
  dplyr::select(
    id_bo = controle,
    ano_bo,
    arma_calibre = calibre,
    arma_marca = marca,
    arma_tipo = tipo,
    arma_origem = origem
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(arma_calibre, arma_marca, arma_tipo),
      \(x) x |>
        tolower() |>
        stringr::str_squish()
    )
  ) |>
  depara_calibre("arma_calibre") |>
  depara_marca("arma_marca") |>
  depara_tipo("arma_tipo")

dados_armas_consolidado <- dados_armas_formatado |>
  gerar_flag_tipo_arma_incompativel() |>
  gerar_tipo_arma_final() |>
  gerar_arma_calibre_final() |>
  gerar_arma_marca_final() |>
  gerar_flag_arma(base = "rj") |>
  gerar_flag_arma_artesanal() |>
  gerar_id_arma(base = "rj") |>
  gerar_flag_arma_policial(base = "rj")

armas_final <- dados_armas_consolidado |>
  dplyr::select(
    id_bo,
    id_arma,
    flag_arma,
    arma_tipo_original = arma_tipo,
    arma_tipo_formatado = tipo_formatado,
    arma_tipo_final,
    flag_arma_artesanal,
    arma_calibre_original = arma_calibre,
    arma_calibre_formatado = calibre_formatado_final,
    arma_calibre_final,
    flag_tipo_arma_incompativel_calibre,
    arma_marca_original = arma_marca,
    arma_marca_formatado = marca_arma_v2,
    arma_marca_final,
    pais_fabricacao,
    arma_origem,
    flag_arma_policial
  )

armas_final |> dplyr::select(arma_tipo_final, compatibilidade_tipo)


# Gerando arquivo

data <- stringr::str_remove_all(Sys.Date(), "-")
writexl::write_xlsx(
  armas_final,
  glue::glue("inst/dados_rj/{data}_dados_armas.xlsx")
)


# Validação

armas_final |>
  dplyr::filter(is.na(arma_marca_final), !is.na(arma_marca_original)) |>
  dplyr::count(
    arma_marca_original,
    arma_marca_final,
    sort = TRUE
  ) |>
  writexl::write_xlsx("data-raw/dados_rj/validacao/escape_marcas.xlsx")


armas_final |>
  dplyr::filter(is.na(arma_calibre_final), !is.na(arma_calibre_original)) |>
  dplyr::count(
    arma_calibre_original,
    compatibilidade_tipo,
    arma_calibre_final,
    sort = TRUE
  ) |>
  writexl::write_xlsx("data-raw/dados_rj/validacao/escape_calibre.xlsx")
