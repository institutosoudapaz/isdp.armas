colunas <- c(
  "id_delegacia",
  "nome_departamento",
  "nome_seccional",
  "nome_delegacia",
  "cidade",
  "ano_bo",
  "num_bo",
  "nome_departamento_circ",
  "nome_seccional_circ",
  "nome_delegacia_circ",
  "nome_municipio_circ",
  "descr_tipo_bo",
  "data_ocorrencia_bo",
  "hora_ocorrencia_bo",
  "descricao_apresentacao",
  "datahora_registro_bo",
  "data_comunicacao_bo",
  "datahora_impressao_bo",
  "descr_periodo",
  "autoria_bo",
  "flag_intolerancia",
  "tipo_intolerancia",
  "flag_flagrante",
  "flag_status",
  "rubrica",
  "descr_conduta",
  "desdobramento",
  "bairro",
  "descr_tipolocal",
  "descr_subtipolocal",
  "cep",
  "logradouro",
  "numero_logradouro",
  "latitude",
  "longitude",
  "descr_exame"
)

dados_sp <- readr::read_rds("inst/dados_sp/dados_sp.rds") 

dados_ocorrencias_sp <- dados_sp |> 
  dplyr::select(dplyr::any_of(colunas))

dados_ocorrencias_sp |> 
  readr::write_rds("inst/dados_sp/dados_ocorrencias_sp.rds")