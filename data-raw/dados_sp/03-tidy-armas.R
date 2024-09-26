dados_sp <- readr::read_rds("inst/dados_sp/dados_sp.rds")

library(tidyverse)

devtools::load_all()


dados_armas_sp <- dados_sp |>
  dplyr::mutate(
    id_arma = vctrs::vec_group_id(paste0(id_bo, "_", cont_arma)),
    flag_propriedade_policial = "",
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
    arma_tipo = descr_arma_fogo,
    arma_estado = estado_arma,
    arma_proprietario_nome = nome_proprietario,
    flag_propriedade_policial
  ) |>
  dplyr::mutate(
    arma_numero_serie = toupper(arma_numero_serie) |>
      stringr::str_remove_all(" +")
  ) |>
  dplyr::arrange(id_bo, id_arma) |>
  dplyr::mutate(
    arma_marca = tolower(arma_marca)
  ) |>
  depara_calibre("arma_calibre") |>
  depara_marca("arma_marca") |>
  depara_tipo("arma_tipo")

dados_armas_sp_consolidado <- dados_armas_sp |>
  gerar_flag_tipo_arma_incompativel() |>
  gerar_arma_marca_final() |>
  gerar_tipo_arma_final() |>
  gerar_arma_calibre_final() |>
  gerar_sn_disponivel() |>
  gerar_flag_arma_artesanal() |>
  gerar_flag_arma(base = "sp") |>
  gerar_flag_arma_policial(base = "sp") |>
  distinct(id_bo, id_arma, .keep_all = TRUE)

tab_regra_1 <- dados_armas_sp_consolidado |> aplicar_regra_1()
tab_regra_2_1 <- dados_armas_sp_consolidado |> aplicar_regra_2_1()
tab_regra_2_2 <- dados_armas_sp_consolidado |> aplicar_regra_2_2()
tab_regra_3 <- dados_armas_sp_consolidado |> aplicar_regra_3()

tab_taurus <- dplyr::bind_rows(
  tab_regra_1,
  tab_regra_2_1,
  tab_regra_2_2,
  tab_regra_3
)

armas_final <- dados_armas_sp_consolidado |>
  dplyr::left_join(
    tab_taurus |> dplyr::distinct(id_bo, id_arma, .keep_all = TRUE),
    by = c("id_bo", "id_arma")
  ) |>
  dplyr::select(
    id_bo, id_arma, cont_arma,
    arma_calibre_original = arma_calibre,
    arma_calibre_formatado = calibre_formatado_final,
    arma_calibre_final = arma_calibre_final,
    arma_numero_serie_original = arma_numero_serie,
    arma_marca_original = arma_marca,
    arma_tipo_original = arma_tipo,
    arma_marca_final = arma_marca_final,
    pais_fabricacao,
    numero_serie_formatado = arma_numero_serie,
    compatibilidade_tipo = tipo_arma_calibre,
    tipo_formatado,
    dplyr::contains("final"),
    sn_disponivel,
    arma_ano_fabricacao,
    flag_arma,
    flag_tipo_arma_incompativel_calibre,
    flag_arma_artesanal
  )

writexl::write_xlsx(armas_final, "inst/dados_sp/20240902_dados_armas.xlsx")
