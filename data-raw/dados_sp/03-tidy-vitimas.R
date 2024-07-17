dados_sp <- readr::read_rds("inst/dados_sp/dados_sp.rds")

dados_vitimas_sp <- dados_sp |>
  dplyr::mutate(
    id_pessoa = vctrs::vec_group_id(paste0(id_bo, "_", cont_pessoa)),
  ) |>
  dplyr::select(
    id_bo,
    id_pessoa,
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
    rubrica,
    descr_conduta,
    desdobramento,
    descr_tipolocal,
    cont_pessoa,
    descr_tipo_pessoa,
    descr_relacionamento,
    flag_vitima_fatal,
    sexo_pessoa,
    descr_orientacao_sexual,
    descr_identidade_genero,
    idade_pessoa,
    cor_cutis,
    descr_profissao,
    descr_grau_instrucao,
    nacionalidade_pessoa,
    naturalidade_pessoa,
    descr_estado_civil,
    flag_vitima_violencia_domestica,
    cont_arma
  )

dados_vitimas_sp |>
  readr::write_rds("inst/dados_sp/dados_vitimas_sp.rds", compress = "xz")
