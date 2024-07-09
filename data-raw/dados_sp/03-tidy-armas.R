colunas <- c(
  "id_bo",
  "id_delegacia",
  "nome_departamento",
  "nome_seccional",
  "nome_delegacia",
  "cidade",
  "cod_ibge",
  "num_bo",
  "nome_departamento_circ",
  "nome_seccional_circ",
  "nome_delegacia_circ",
  "nome_municipio_circ",
  "cod_ibge_circ",
  "data_ocorrencia_bo",
  "ano_bo",
  "ano_bo_novo",
  "mes_bo",
  "hora_ocorrencia_bo",
  "periodo_ocorrencia_bo",
  "descricao_apresentacao",
  "autoria_bo",
  "flag_flagrante",
  "flag_status",
  "rubrica",
  "descr_conduta",
  "desdobramento",
  "bairro",
  "descr_tipolocal",
  "descr_subtipolocal",
  "logradouro",
  "numero_logradouro",
  "latitude",
  "longitude",
  "cont_pessoa",
  "descr_tipo_pessoa",
  "flag_vitima_fatal",
  "flag_vitima_violencia_domestica",
  "id_arma",
  "cont_arma",
  "calibre_arma",
  "numero_arma",
  "marca_arma",
  "pais_origem_arma",
  "descr_arma_fogo",
  "estado_arma",
  "nome_proprietario",
  "flag_propriedade_policial",
  "flag_arma_artesanal",
  "ano_fabricacao_arma"
)

dados_sp <- readr::read_rds("inst/dados_sp/dados_sp.rds") 

dados_armas_sp <- dados_sp |> 
  dplyr::mutate(
    id_arma = "",
    flag_propriedade_policial = "",
    flag_arma_artesanal = "",
    ano_fabricacao_arma = "",
    pais_origem_arma = ""
  ) |> 
  dplyr::select(dplyr::all_of(colunas))

dados_armas_sp |> 
  readr::write_rds("inst/dados_sp/dados_armas_sp.rds")
