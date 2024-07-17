dados_sp <- readr::read_rds("inst/dados_sp/dados_sp.rds")

dados_armas_sp <- dados_sp |>
  dplyr::mutate(
    id_arma = vctrs::vec_group_id(paste0(id_bo, "_", cont_arma)),
    flag_propriedade_policial = "",
    flag_arma_artesanal = "",
    arma_pais_origem = "",
  ) |>
  dplyr::select(
    id_bo,
    id_arma,
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
    arma_pais_origem,
    arma_tipo = descr_arma_fogo,
    arma_estado = estado_arma,
    arma_proprietario_nome = nome_proprietario,
    flag_propriedade_policial,
    flag_arma_artesanal
  ) |>
  dplyr::mutate(
    arma_numero_serie = toupper(arma_numero_serie)
  ) |>
  dplyr::arrange(id_bo, id_arma) 

# Pegando ano de produção segundo o número de série de armas Taurus

tab_regra_1 <- dados_armas_sp |>
  dplyr::filter(
    !is.na(arma_numero_serie),
    !stringr::str_detect(arma_numero_serie, "[A-Z]"),
    arma_marca == "taurus",
    arma_tipo == "Revolver"
  ) |>
  aplicar_regra_1() |>
  dplyr::select(
    id_bo,
    id_arma,
    arma_ano_fabricacao
  )

tab_regra_2_1 <- dados_armas_sp |>
  dplyr::filter(
    !is.na(arma_numero_serie),
    nchar(arma_numero_serie) == 8,
    stringr::str_detect(arma_numero_serie, "^[A-Z]{3}"),
    arma_marca == "taurus",
    arma_tipo == "Pistola" # confirmar
  ) |>
  dplyr::mutate(
    arma_ns_primeira_letra = stringr::str_sub(arma_numero_serie, 2, 2),
    arma_ns_segunda_letra = stringr::str_sub(arma_numero_serie, 3, 3)
  ) |>
  aplicar_regra_2() |>
  dplyr::select(
    id_bo,
    id_arma,
    arma_ano_fabricacao
  )

tab_regra_2_2 <- dados_armas_sp |>
  dplyr::filter(
    !is.na(arma_numero_serie),
    nchar(arma_numero_serie) %in% c(7, 8),
    stringr::str_detect(arma_numero_serie, "^[A-Z]{2}"),
    arma_marca == "taurus",
    arma_tipo == "Revolver"
  ) |>
  dplyr::mutate(
    arma_ns_primeira_letra = stringr::str_sub(arma_numero_serie, 1, 1),
    arma_ns_segunda_letra = stringr::str_sub(arma_numero_serie, 2, 2)
  ) |>
  aplicar_regra_2() |>
  dplyr::select(
    id_bo,
    id_arma,
    arma_ano_fabricacao
  )

tab_regra_3 <- dados_armas_sp |>
  dplyr::filter(
    !is.na(arma_numero_serie),
    nchar(arma_numero_serie) == 9,
    stringr::str_detect(arma_numero_serie, "^[A-Z]{3}"),
    arma_marca == "taurus"
  ) |>
  dplyr::mutate(
    arma_ns_primeira_letra = stringr::str_sub(arma_numero_serie, 1, 1),
    arma_ns_segunda_letra = stringr::str_sub(arma_numero_serie, 2, 2),
    arma_ns_terceira_letra = stringr::str_sub(arma_numero_serie, 2, 2)
  ) |>
  aplicar_regra_3() |>
  dplyr::select(
    id_bo,
    id_arma,
    arma_ano_fabricacao
  )

tab_taurus <- dplyr::bind_rows(
  tab_regra_1,
  tab_regra_2_1,
  tab_regra_2_2,
  tab_regra_3
)

# Gerando arquivo

dados_armas_sp |>
  # dplyr::left_join(
  #   tab_taurus,
  #   by = c("id_bo", "id_arma")
  # ) |>
  readr::write_rds("inst/dados_sp/dados_armas_sp.rds", compress = "xz")
