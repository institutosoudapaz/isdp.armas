devtools::load_all()

dados_armas_complementar <- readr::read_rds("inst/dados_rj/dados_armas_complementar.rds")

# Fazendo de-paras

dados_armas_formatado <- dados_armas_complementar |>
  dplyr::select(
    controle_interno_sco,
    circunscricao,
    data_registro,
    bairro,
    arma_numero_serie = numero_de_serie,
    numero_procedimento,
    arma_calibre = calibre,
    arma_marca = marca,
    arma_tipo = tipo,
    arma_origem = origem_arma,
    arma_modelo = modelo,
    arma_pais_fabricacao_original = pais,
    arma_classe = classe,
    flag_restrita = restrita,
    patrimoniada,
    tipo_delito
  ) |>
  dplyr::mutate(
    flag_arma_original = arma_classe == "ARMA DE FOGO",
    dplyr::across(
      c(arma_calibre, arma_marca, arma_tipo),
      \(x) x |>
        tolower() |>
        stringr::str_squish()
    ),
    arma_pais_fabricacao_original = toupper(arma_pais_fabricacao_original),
    ano_bo = lubridate::year(data_registro),
    hora_ocorrencia_bo = hms::as_hms(data_registro),
    periodo_ocorrencia_bo = categorizar_periodo(as.character(hora_ocorrencia_bo))
  ) |>
  depara_calibre("arma_calibre") |>
  depara_marca("arma_marca") |>
  depara_tipo("arma_tipo")

# Aplicando regras de negócio

dados_armas_consolidado <- dados_armas_formatado |>
  gerar_id_bo(base = "rj_complementar") |>
  gerar_rubrica_formatada(base = "rj_complementar") |>
  gerar_flag_tipo_arma_incompativel() |>
  gerar_arma_tipo_final() |>
  gerar_arma_calibre_final() |>
  gerar_arma_marca_final() |>
  gerar_sn_disponivel() |>
  gerar_numero_serie_formatado() |>
  gerar_flag_arma() |>
  gerar_flag_arma_artesanal() |>
  gerar_id_arma(base = "rj_complementar") |>
  gerar_flag_arma_policial(base = "rj_complementar")

# Aplicação das regras Taurus

tab_regra_1 <- aplicar_regra_1(dados_armas_consolidado)
tab_regra_2_1 <- aplicar_regra_2_1(dados_armas_consolidado)
tab_regra_2_2 <- aplicar_regra_2_2(dados_armas_consolidado)
tab_regra_3 <- aplicar_regra_3(dados_armas_consolidado)

tab_taurus <- dplyr::bind_rows(
  tab_regra_1,
  tab_regra_2_1,
  tab_regra_2_2,
  tab_regra_3,
)

# Base final

armas_final <- dados_armas_consolidado |>
  dplyr::left_join(
    tab_taurus,
    by = c("id_bo", "id_arma")
  ) |>
  dplyr::select(
    id_bo,
    controle_interno_sco,
    numero_procedimento,
    rubrica_original = tipo_delito,
    rubrica_formatada,
    nome_delegacia = circunscricao,
    data_ocorrencia_bo = data_registro,
    ano_bo,
    hora_ocorrencia_bo,
    periodo_ocorrencia_bo,
    bairro,
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
    arma_modelo,
    arma_numero_serie_original = arma_numero_serie,
    arma_numero_serie_formatado,
    sn_disponivel,
    arma_classe,
    flag_arma_de_fogo = flag_arma,
    flag_arma_artesanal,
    arma_origem,
    nome_prop = patrimoniada,
    flag_mdoip,
    flag_arma_policia_prop,
    flag_calibre_policial,
    flag_arma_policial,
    pais_fabricacao_original = arma_pais_fabricacao_original,
    pais_fabricacao_formatado = pais_fabricacao,
    arma_ano_fabricacao,
    padrao_taurus,
    flag_restrita
  )

# Gerando arquivo

data <- stringr::str_remove_all(Sys.Date(), "-")
writexl::write_xlsx(
  armas_final,
  glue::glue("inst/dados_rj/{data}_dados_armas_complementar.xlsx")
)

# Validação

armas_final |>
  dplyr::filter(is.na(arma_marca_final), !is.na(arma_marca_original)) |>
  dplyr::count(
    arma_marca_original,
    arma_marca_final,
    sort = TRUE
  ) |>
  writexl::write_xlsx("data-raw/dados_rj/validacao/escape_marcas_complementar.xlsx")


armas_final |>
  dplyr::filter(is.na(arma_calibre_final), !is.na(arma_calibre_original)) |>
  dplyr::count(
    arma_calibre_original,
    compatibilidade_tipo,
    arma_calibre_final,
    sort = TRUE
  ) |>
  writexl::write_xlsx("data-raw/dados_rj/validacao/escape_calibre_complementar.xlsx")


armas_final |>
  dplyr::filter(!is.na(padrao_taurus)) |>
  dplyr::select(
    padrao_taurus,
    sn_disponivel,
    arma_numero_serie_original,
    arma_ano_fabricacao
  ) |>
  writexl::write_xlsx("data-raw/dados_rj/validacao/regra_taurus.xlsx")
