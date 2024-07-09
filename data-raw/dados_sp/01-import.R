# Colunas selecionadas -------------------------------------------------------

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
  "descr_exame",
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
  "descr_estado_civil",
  "cont_arma",
  "descr_modo_objeto",
  "calibre_arma",
  "numero_arma",
  "marca_arma",
  "descr_arma_fogo"
)

# Lendo bases ----------------------------------------------------------------

dados_sp_2018_1 <- readxl::read_excel(
  "data-raw/dados_sp/raw/SIC 40606205187 1º Semestre 2018 Metodologia.xlsx",
  guess_max = 40000
) |>
  janitor::clean_names() |>
  janitor::remove_empty(c("rows", "cols")) |>
  dplyr::rename(
    autoria_bo = flag_autoria_bo
  ) |>
  dplyr::select(dplyr::any_of(colunas)) |> 
  dplyr::mutate(
    numero_logradouro = as.character(numero_logradouro),
    num_bo = as.character(num_bo)
  )

dados_sp_2018_2 <- readxl::read_excel(
  "data-raw/dados_sp/raw/SIC 40606205187 2º semestre 2018.xlsx",
  guess_max = 40000
) |>
  janitor::clean_names() |>
  janitor::remove_empty(c("rows", "cols")) |>
  dplyr::rename(
    autoria_bo = flag_autoria_bo
  ) |>
  dplyr::select(dplyr::any_of(colunas)) |> 
  dplyr::mutate(
    num_bo = as.character(num_bo)
  )

dados_sp_2019_2022 <- readxl::read_excel(
  "data-raw/dados_sp/raw/SP pre recurso armas apreendidas estadual SIC 55366246800_12042024_armas SP.xlsx",
  sheet = "Base de Dados (1)",
  guess_max = 100000
) |>
  janitor::clean_names() |>
  janitor::remove_empty(c("rows", "cols")) |>
  dplyr::select(dplyr::any_of(colunas)) |> 
  dplyr::mutate(
    dplyr::across(
      c(latitude, longitude),
      \(x) ifelse(x %in% c("NULL", "0"), NA_real_, x)
    ),
    dplyr::across(
      c(latitude, longitude),
      readr::parse_number
    ),
    num_bo = as.character(num_bo)
  )

# será utilizada apenas para pegar a coluna NUMERO_ARMA
dados_sp_2022_2023_pos_recurso <- readxl::read_excel(
  "data-raw/dados_sp/raw/SP pos recurso 23042024SIC 55366246800 - Recurso.xlsx",
  sheet = "Base de Dados (2)",
  guess_max = 20000
) |>
  janitor::clean_names() |>
  janitor::remove_empty(c("rows", "cols")) |>
  dplyr::select(
    id_delegacia,
    ano_bo,
    num_bo,
    numero_arma
  )
  
# dados_sp_2022_2023_pos_recurso |>
#   dplyr::mutate(
#     id = paste(id_delegacia, ano_bo, num_bo, cont_arma, cont_pessoa, sep = "_")
#   ) |> 
#   dplyr::distinct(id)

dados_sp_2022_2023 <- readxl::read_excel(
  "data-raw/dados_sp/raw/SP pre recurso armas apreendidas estadual SIC 55366246800_12042024_armas SP.xlsx",
  sheet = "Base de Dados (2)",
  guess_max = 40000
) |>
  janitor::clean_names() |>
  janitor::remove_empty(c("rows", "cols")) |>
  dplyr::rename(
    descr_patologia = nome_patologia,
    descr_modo_objeto = desc_objeto_modo,
    descr_estado_civil = desc_estado_civil
  ) |>
  dplyr::select(dplyr::any_of(colunas)) |> 
  dplyr::mutate(
    cep = as.character(cep),
    numero_logradouro = as.character(numero_logradouro),
    dplyr::across(
      c(latitude, longitude),
      \(x) ifelse(x %in% c("NULL", "0"), NA_real_, x)
    ),
    dplyr::across(
      c(latitude, longitude),
      readr::parse_number
    )
  )

# Juntando bases -------------------------------------------------------------

dplyr::bind_rows(
  dados_sp_2018_1,
  dados_sp_2018_2,
  dados_sp_2019_2022,
  dados_sp_2022_2023
) |> 
  dplyr::filter(
    !descr_arma_fogo %in% c("Outros", "Arma Branca", "Colete", "Granada", "Bomba")
  ) |> 
  dplyr::distinct() |> 
  # incluir cod_ibge aqui
  #
  # dplyr::count(descr_arma_fogo, sort = TRUE) |> print(n = 100)
  readr::write_rds("inst/dados_sp/dados_sp.rds", compress = "xz")


# DOCUMENTAÇÃO

# - As colunas descr_orientacao_sexual, descr_identidade_genero e descr_estado_civil
#   só existem a partir de 2022.
# - A coluna descr_exame só existe até meados de 2022.
# - A coluna descr_patologia só existe a partir de 2019.
