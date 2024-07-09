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
  "cont_pessoa",
  "descr_tipo_pessoa",
  "flag_deficiencia",
  "descr_patologia",
  "descricao_deficiencia",
  "descr_relacionamento",
  "flag_vitima_fatal",
  "sexo_pessoa",
  "descr_orientacao_sexual",
  "descr_identidade_genero",
  "idade_pessoa",
  "cor_cutis",
  "descr_profissao",
  "descr_grau_instrucao",
  "nacionalidade_pessoa",
  "naturalidade_pessoa",
  "descr_estado_civil"
)

dados_sp <- readr::read_rds("inst/dados_sp/dados_sp.rds") 

dados_vitimas_sp <- dados_sp |> 
  dplyr::select(dplyr::any_of(colunas))

dados_vitimas_sp |> 
  readr::write_rds("inst/dados_sp/dados_vitimas_sp.rds")