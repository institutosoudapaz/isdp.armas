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
  "autoria_bo",
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
  "flag_vitima_violencia_domestica",
  "cont_arma",
  "descr_modo_objeto",
  "calibre_arma",
  "numero_arma",
  "flag_ato_infracional",
  "marca_arma",
  "descr_arma_fogo",
  "estado_arma",
  "nome_proprietario"
)

# Lendo bases ----------------------------------------------------------------

dados_sp_2018_1 <- readxl::read_excel(
  "data-raw/dados_sp/raw/SIC 40606205187 1º Semestre 2018 Metodologia.xlsx",
  guess_max = 40000,
  na = c("", "NULL"),
  col_types = c(rep("guess", 13), "date", rep("guess", 42)) # pegar hora como date
) |>
  janitor::clean_names() |>
  janitor::remove_empty(c("rows", "cols")) |>
  dplyr::rename(
    autoria_bo = flag_autoria_bo
  ) |>
  dplyr::select(dplyr::any_of(colunas)) |>
  dplyr::mutate(
    numero_logradouro = as.character(numero_logradouro),
    num_bo = as.character(num_bo),
    hora_ocorrencia_bo = as.character(hora_ocorrencia_bo)
  )

dados_sp_2018_2 <- readxl::read_excel(
  "data-raw/dados_sp/raw/SIC 40606205187 2º semestre 2018.xlsx",
  guess_max = 40000,
  na = c("", "NULL"),
  col_types = c(rep("guess", 13), "date", rep("guess", 42)) # pegar hora como date
) |>
  janitor::clean_names() |>
  janitor::remove_empty(c("rows", "cols")) |>
  dplyr::rename(
    autoria_bo = flag_autoria_bo
  ) |>
  dplyr::select(dplyr::any_of(colunas)) |>
  dplyr::mutate(
    numero_logradouro = as.character(numero_logradouro),
    num_bo = as.character(num_bo),
    hora_ocorrencia_bo = as.character(hora_ocorrencia_bo)
  )

dados_sp_2019_2022 <- readxl::read_excel(
  "data-raw/dados_sp/raw/SP pos recurso 23042024SIC 55366246800 - Recurso.xlsx",
  sheet = "Base de Dados (1)",
  guess_max = 400000,
  na = c("", "NULL"),
  col_types = c(rep("guess", 13), "guess", rep("guess", 46)) # pegar hora como date
) |>
  janitor::clean_names() |>
  janitor::remove_empty(c("rows", "cols")) |>
  dplyr::select(dplyr::any_of(colunas)) |>
  dplyr::mutate(
    dplyr::across(
      c(latitude, longitude),
      \(x) ifelse(x == 0, NA_real_, x)
    ),
    num_bo = as.character(num_bo),
    hora_ocorrencia_bo = as.character(hora_ocorrencia_bo),
    data_ocorrencia_bo = as.POSIXct(data_ocorrencia_bo, tz = "UTC"),
    data_comunicacao_bo = as.POSIXct(data_comunicacao_bo, tz = "UTC"),
    datahora_registro_bo = as.POSIXct(datahora_registro_bo, tz = "UTC"),
    numero_logradouro = as.character(numero_logradouro),
  )

# Será utilizada apenas para pegar a coluna NUMERO_ARMA
dados_sp_2022_2023_pos_recurso <- readxl::read_excel(
  "data-raw/dados_sp/raw/SP pos recurso 23042024SIC 55366246800 - Recurso.xlsx",
  sheet = "Base de Dados (2)",
  na = c("", "NULL"),
  guess_max = 20000
) |>
  janitor::clean_names() |>
  janitor::remove_empty(c("rows", "cols")) |>
  dplyr::select(
    id_delegacia,
    num_bo,
    ano_bo,
    cont_arma,
    numero_arma
  ) |>
  dplyr::distinct(id_delegacia, num_bo, ano_bo, cont_arma, numero_arma)

dados_sp_2022_2023_pre_recurso <- readxl::read_excel(
  "data-raw/dados_sp/raw/SP armas apreendidas estadual SIC 55366246800_12042024_armas SP.xlsx",
  sheet = "Base de Dados (2)",
  na = c("", "NULL"),
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
      \(x) ifelse(x == 0, NA_real_, x)
    ),
    naturalidade_pessoa = as.character(naturalidade_pessoa)
  )

dados_sp_2022_2023 <- dados_sp_2022_2023_pre_recurso |>
  dplyr::left_join(
    dados_sp_2022_2023_pos_recurso,
    by = c("id_delegacia", "num_bo", "ano_bo", "cont_arma")
  )

# Juntando bases -------------------------------------------------------------

dados_sp <- dplyr::bind_rows(
  dados_sp_2018_1,
  dados_sp_2018_2,
  dados_sp_2019_2022,
  dados_sp_2022_2023
) |>
  dplyr::distinct() |>
  dplyr::arrange(ano_bo, id_delegacia, num_bo) |>
  dplyr::mutate(
    id_bo = vctrs::vec_group_id(paste(id_delegacia, num_bo, ano_bo)),
    .before = 1
  ) |>
  dplyr::mutate(
    cod_ibge = "",
    cod_ibge_circ = "",
    ano_bo_novo = lubridate::year(data_ocorrencia_bo),
    mes_bo = lubridate::month(data_ocorrencia_bo),
    hora_ocorrencia_bo = stringr::str_extract(
      hora_ocorrencia_bo,
      "[0-9]{2}:[0-9]{2}"
    ),
    flag_arma_fogo = ifelse(
      descr_arma_fogo %in% c("Outros", "Arma Branca", "Colete", "Granada", "Bomba"),
      FALSE,
      TRUE
    ),
    periodo_ocorrencia_bo = dplyr::case_when(
      is.na(hora_ocorrencia_bo) ~ NA_character_,
      hora_ocorrencia_bo <= "05:59" ~ "Madrugada",
      hora_ocorrencia_bo <= "11:59" ~ "Manhã",
      hora_ocorrencia_bo <= "17:59" ~ "Tarde",
      TRUE ~ "Noite"
    )
  ) |>
  dplyr::arrange(id_bo)


readr::write_rds(dados_sp, "inst/dados_sp/dados_sp.rds", compress = "xz")


# DOCUMENTAÇÃO

# - As colunas descr_orientacao_sexual, descr_identidade_genero e descr_estado_civil
#   só existem a partir de 2022.
# - A coluna descr_patologia só existe a partir de 2019.
# - O ano_bo_novo foi criado pois não confiamos na informação da coluna ano_bo.
