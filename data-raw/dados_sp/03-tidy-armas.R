devtools::load_all()

dados_armas <- readr::read_rds("data-raw/dados_sp/dados_sp.rds")

dados_armas_formatado <- dados_armas |>
  dplyr::select(
    id_bo,
    id_delegacia,
    nome_departamento,
    nome_seccional,
    nome_delegacia,
    cidade,
    cod_ibge,
    num_bo,
    nome_departamento_circ,
    nome_seccional_circ,
    nome_delegacia_circ,
    nome_municipio_circ,
    cod_ibge_circ,
    data_ocorrencia_bo,
    ano_bo,
    ano_bo_novo,
    mes_bo,
    hora_ocorrencia_bo,
    periodo_ocorrencia_bo,
    descricao_apresentacao,
    autoria_bo,
    flag_flagrante,
    flag_status,
    rubrica,
    descr_conduta,
    desdobramento,
    bairro,
    descr_tipolocal,
    descr_subtipolocal,
    logradouro,
    numero_logradouro,
    latitude,
    longitude,
    cont_pessoa,
    descr_tipo_pessoa,
    flag_vitima_fatal,
    flag_vitima_violencia_domestica,
    cont_arma,
    arma_calibre = calibre_arma,
    arma_numero_serie = numero_arma,
    arma_marca = marca_arma,
    arma_tipo = descr_arma_fogo,
    arma_estado = estado_arma,
    arma_proprietario_nome = nome_proprietario
  ) |>
  gerar_id_arma(base = "sp") |>
  dplyr::group_by(id_arma) |>
  dplyr::summarise(
    dplyr::across(
      c(descr_conduta, rubrica, desdobramento, arma_estado),
      \(x) paste0(unique(x), collapse = " -- ")
    ),
    dplyr::across(
      -c(descr_conduta, rubrica, desdobramento, arma_estado),
      dplyr::first
    )
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

# Aplicando regras de negócio

dados_armas_consolidado <- dados_armas_formatado |>
  gerar_flag_tipo_arma_incompativel() |>
  gerar_arma_marca_final() |>
  gerar_arma_tipo_final() |>
  gerar_arma_calibre_final() |>
  gerar_sn_disponivel() |>
  gerar_numero_serie_formatado() |>
  gerar_flag_arma_artesanal() |>
  gerar_flag_arma() |>
  gerar_flag_arma_policial(base = "sp")

# Aplicação das regras Taurus

tab_regra_1 <- aplicar_regra_1(dados_armas_consolidado)
tab_regra_2_1 <- aplicar_regra_2_1(dados_armas_consolidado)
tab_regra_2_2 <- aplicar_regra_2_2(dados_armas_consolidado)
tab_regra_3 <- aplicar_regra_3(dados_armas_consolidado)

tab_taurus <- dplyr::bind_rows(
  tab_regra_1,
  tab_regra_2_1,
  tab_regra_2_2,
  tab_regra_3
)

# Base final

armas_final <- dados_armas_consolidado |>
  dplyr::left_join(
    tab_taurus,
    by = c("id_bo", "id_arma")
  ) |>
  dplyr::select(
    id_bo,
    id_arma,
    flag_flagrante,
    cont_arma,
    arma_calibre_original = arma_calibre,
    arma_calibre_formatado = calibre_formatado_final,
    arma_calibre_final = arma_calibre_final,
    arma_numero_serie_original = arma_numero_serie,
    arma_numero_serie_formatado,
    arma_estado,
    sn_disponivel,
    arma_marca_original = arma_marca,
    arma_marca_final = arma_marca_final,
    pais_fabricacao,
    arma_tipo_original = arma_tipo,
    tipo_formatado,
    compatibilidade_tipo,
    flag_tipo_arma_incompativel_calibre,
    arma_ano_fabricacao,
    flag_padrao = padrao_taurus,
    flag_arma,
    flag_arma_artesanal,
    arma_proprietario_nome,
    flag_arma_policia_desd,
    flag_arma_policia_prop,
    flag_arma_policia_prop_indisp,
    flag_arma_policia
  )

# Gerando arquivo

data <- stringr::str_remove_all(Sys.Date(), "-")
writexl::write_xlsx(
  armas_final,
  glue::glue("data-raw/dados_sp/validacao/{data}_dados_armas.xlsx")
)

# Validação

armas_final |>
  dplyr::filter(is.na(arma_marca_final), !is.na(arma_marca_original)) |>
  dplyr::count(
    arma_marca_original,
    arma_marca_final,
    sort = TRUE
  ) |>
  writexl::write_xlsx("data-raw/dados_sp/validacao/escape_marcas_complementar.xlsx")


armas_final |>
  dplyr::filter(is.na(arma_calibre_final), !is.na(arma_calibre_original)) |>
  dplyr::count(
    arma_calibre_original,
    compatibilidade_tipo,
    arma_calibre_final,
    sort = TRUE
  ) |>
  writexl::write_xlsx("data-raw/dados_sp/validacao/escape_calibre_complementar.xlsx")


armas_final |>
  dplyr::filter(!is.na(padrao_taurus)) |>
  dplyr::select(
    padrao_taurus,
    sn_disponivel,
    arma_numero_serie_original,
    arma_ano_fabricacao
  ) |>
  writexl::write_xlsx("data-raw/dados_sp/validacao/regra_taurus.xlsx")

