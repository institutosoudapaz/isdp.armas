devtools::load_all()

dados_armas <- readr::read_rds("inst/dados_rj/dados_armas_rj.rds")
dados_ocorrencias <- readr::read_rds("inst/dados_rj/dados_ocorrencias_rj.rds")

tab_delitos <- dados_ocorrencias |>
  depara_crime(nome_coluna = "titulo_do") |>
  dplyr::rename(
    crime_formatado_do = crime_formatado
  ) |>
  depara_crime(nome_coluna = "titulo") |>
  dplyr::select(controle, titulo, crime_formatado, titulo_do, crime_formatado_do) |>
  dplyr::distinct() |>
  dplyr::group_by(controle) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::everything(),
      ~ stringr::str_c(unique(.x), collapse = " | ")
    )
  )

tab_outras_info <- dados_ocorrencias |>
  dplyr::mutate(uf = "RJ") |>
  munifacil::limpar_colunas(
    municipio_fato,
    uf
  ) |>
  munifacil::incluir_codigo_ibge() |>
  dplyr::distinct(
    controle,
    ano_bo_original = ano,
    mes_bo = mes,
    data_bo = data_fato,
    hora_bo = hora_com,
    municipio = municipio_fato,
    cod_ibge = id_municipio,
    dp,
    cisp,
    aisp,
    risp
  ) |> 
  dplyr::mutate(
    periodo_bo = categorizar_periodo(hora_bo)
  )


# Fazendo de-paras

dados_armas_formatado <- dados_armas |>
  dplyr::select(
    controle,
    arma_calibre = calibre,
    arma_marca = marca,
    arma_tipo = tipo,
    arma_origem = origem
  ) |>
  dplyr::mutate(
    ano_bo = stringr::str_sub(controle, -4, -1),
    dplyr::across(
      c(arma_calibre, arma_marca, arma_tipo),
      \(x) x |>
        tolower() |>
        stringr::str_squish()
    )
  ) |>
  dplyr::left_join(
    tab_delitos,
    by = "controle"
  ) |>
  dplyr::left_join(
    tab_outras_info,
    by = "controle"
  ) |>
  depara_calibre("arma_calibre") |>
  depara_marca("arma_marca") |>
  depara_tipo("arma_tipo")

# Aplicando regras de negócio

dados_armas_consolidado <- dados_armas_formatado |>
  gerar_id_bo(base = "rj") |>
  gerar_rubrica_formatada(base = "rj") |>
  gerar_flag_tipo_arma_incompativel() |>
  gerar_tipo_arma_final() |>
  gerar_arma_calibre_final() |>
  gerar_arma_marca_final() |>
  gerar_flag_arma() |>
  gerar_flag_arma_artesanal() |>
  gerar_id_arma(base = "rj") |>
  gerar_flag_arma_policial(base = "rj")

armas_final <- dados_armas_consolidado |>
  dplyr::select(
    id_bo,
    controle,
    rubrica_original = titulo,
    rubrica_formatada,
    rubrica_original_do = titulo_do,
    rubrica_formatada_do,
    ano_bo_original,
    ano_bo,
    mes_bo,
    data_bo,
    hora_bo,
    periodo_bo,
    dp,
    cisp,
    aisp,
    risp,
    id_arma,
    arma_tipo_original = arma_tipo,
    arma_tipo_formatado = tipo_formatado,
    compatibilidade_tipo,
    flag_tipo_arma_incompativel_calibre,
    arma_marca_original = arma_marca,
    arma_marca_formatado = marca_arma_v2,
    arma_marca_final,
    arma_calibre_original = arma_calibre,
    arma_calibre_formatado = calibre_formatado_final,
    arma_calibre_final,
    flag_arma_de_fogo = flag_arma,
    flag_arma_artesanal,
    arma_origem,
    flag_mdoip,
    flag_calibre_policial,
    flag_arma_policial
  )

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
    arma_calibre_final,
    sort = TRUE
  ) |>
  writexl::write_xlsx("data-raw/dados_rj/validacao/escape_calibre.xlsx")
