dados_sp <- readr::read_rds("inst/dados_sp/dados_sp.rds")

devtools::load_all()

calibre_bruno <- readxl::read_excel(sheet = 2, "data-raw/dados_sp/raw/Planilhas correções armas de fogo .xlsx")

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
    arma_numero_serie = toupper(arma_numero_serie)
  ) |>
  dplyr::arrange(id_bo, id_arma) |>
  dplyr::distinct(
    id_bo, id_arma, cont_arma, arma_calibre,
    arma_numero_serie, arma_marca,
    arma_tipo, arma_estado, arma_proprietario_nome,
    flag_propriedade_policial
  ) |>
  dplyr::mutate(
    arma_marca = tolower(arma_marca)
  ) |>
  depara_calibre("arma_calibre") |>
  depara_marca("arma_marca") |>
  depara_tipo("arma_tipo")

dados_armas_sp_consolidado <- dados_armas_sp |>
  dplyr::mutate(
    arma_tipo_final = tolower(dplyr::case_when(
        tipo_formatado == "artesanal"~ "artesanal",
        !is.na(tipo_formatado) & (stringr::str_detect(tipo_arma_calibre, tipo_formatado) | is.na(tipo_arma_calibre)) ~ tipo_formatado,
        !is.na(tipo_arma_calibre) ~ tipo_arma_calibre,
        TRUE ~ NA_character_
    )) |>
      stringr::str_squish(),
    arma_calibre_final = dplyr::case_when(
      calibre_formatado_final == ".32" & tipo_formatado == "revolver" ~ ".32 S&W long",
      calibre_formatado_final == ".32" & tipo_formatado == "pistola" ~ ".32 acp",
      TRUE ~ calibre_formatado_final
    ),
    arma_marca_final = tolower(marca_arma_v2),
    tipo_sem_numero = dplyr::case_when(
      stringr::str_detect(tolower(arma_numero_serie), "expu|supri|obl|subt|apag") ~ "removido",
      stringr::str_detect(tolower(arma_numero_serie), "raspa|esm|lixa") ~ "raspado",
      stringr::str_detect(tolower(arma_numero_serie), "picot|pinad") ~ "puncionamento",
      stringr::str_detect(tolower(arma_numero_serie), "riscad|adul|sobrepost|remarcad|danific") ~ "regravado/adulterado",
      stringr::str_detect(tolower(arma_numero_serie), "desgast|oxidad|corroi|ferru") ~ "desgastado",
      TRUE ~ "outros"
    ),
    flag_arma_artesanal = stringr::str_detect(arma_tipo_final, "artesanal"),
    flag_arma = dplyr::case_when(
      flag_arma == "TRUE" ~ flag_arma,
      stringr::str_detect(tolower(arma_numero_serie), "simulacro|muni[çc][ãa]o|chumbo") ~ "FALSE",
      stringr::str_detect(tolower(arma_calibre), "cbc") ~ "FALSE",
      arma_marca_final == "cbc" ~ "FALSE",
      TRUE ~ flag_arma
    ),
    arma_numero_serie = dplyr::if_else(
      tipo_sem_numero == "outros" | flag_arma != "TRUE", arma_numero_serie, NA_character_
    )
  )

# Pegando ano de produção segundo o número de série de armas Taurus

tab_regra_1 <- dados_armas_sp_consolidado |>
  dplyr::filter(
    !is.na(arma_numero_serie),
    !stringr::str_detect(arma_numero_serie, "[A-Z]"),
    arma_marca_final == "taurus",
    arma_tipo_final == "revolver"
  ) |>
  aplicar_regra_1() |>
  dplyr::select(
    id_bo,
    id_arma,
    arma_ano_fabricacao
  )

tab_regra_2_1 <- dados_armas_sp_consolidado |>
  dplyr::filter(
    !is.na(arma_numero_serie),
    nchar(arma_numero_serie) == 8,
    stringr::str_detect(arma_numero_serie, "^[A-Z]{3}"),
    arma_marca_final == "taurus",
    arma_tipo_final == "pistola" # confirmar
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

tab_regra_2_2 <- dados_armas_sp_consolidado |>
  dplyr::filter(
    !is.na(arma_numero_serie),
    nchar(arma_numero_serie) %in% c(7, 8),
    stringr::str_detect(arma_numero_serie, "^[A-Z]{2}"),
    arma_marca_final == "taurus",
    arma_tipo_final == "revolver"
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

tab_regra_3 <- dados_armas_sp_consolidado |>
  dplyr::filter(
    !is.na(arma_numero_serie),
    nchar(arma_numero_serie) == 9,
    stringr::str_detect(arma_numero_serie, "^[A-Z]{3}"),
    arma_marca_final == "taurus"
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
  tab_regra_2_1 |> dplyr::mutate(arma_ano_fabricacao = as.character(arma_ano_fabricacao)),
  tab_regra_2_2 |> dplyr::mutate(arma_ano_fabricacao = as.character(arma_ano_fabricacao)),
  tab_regra_3 |> dplyr::mutate(arma_ano_fabricacao = as.character(arma_ano_fabricacao))
)

# Gerando arquivo

armas_final <- dados_armas_sp_consolidado |>
  dplyr::left_join(
    tab_taurus |> dplyr::distinct(id_bo, id_arma, .keep_all = TRUE),
    by = c("id_bo", "id_arma")
  )

readr::write_rds(armas_final, "inst/dados_sp/dados_armas_sp.rds", compress = "xz")

writexl::write_xlsx(armas_final, "inst/dados_sp/dados_armas_bruto.xlsx")
