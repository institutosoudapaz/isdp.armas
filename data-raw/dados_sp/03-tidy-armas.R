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
  "cont_arma",
  "descr_modo_objeto",
  "calibre_arma",
  "numero_arma",
  "marca_arma",
  "descr_arma_fogo"
)

dados_sp <- readr::read_rds("inst/dados_sp/dados_sp.rds") 


dados_armas_sp <- dados_sp |> 
  dplyr::select(dplyr::any_of(colunas)) |> 
  dplyr::mutate(
    flag_propriedade_policial = "",
    flag_arma_artesanal = "",
    ano_fabricacao_arma = ""
  )

dados_armas_sp |> 
  readr::write_rds("inst/dados_sp/dados_armas_sp.rds")