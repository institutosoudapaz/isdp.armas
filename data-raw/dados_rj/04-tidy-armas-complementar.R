devtools::load_all()

dados_armas_complementar <- readr::read_rds("inst/dados_rj/dados_armas_complementar.rds")

dados_armas_formatado <- dados_armas_complementar |>
  dplyr::mutate(
    flag_arma_original = classe == "ARMA DE FOGO",
  ) |>
  dplyr::select(
    id_bo = controle_interno_sco,
    arma_numero_serie = numero_de_serie,
    arma_calibre = calibre,
    arma_marca = marca,
    arma_tipo = tipo,
    arma_origem = origem_arma,
    arma_modelo = modelo,
    arma_pais_fabricacao_original = pais,
    flag_restrita = restrita,
    flag_patrimoniada = patrimoniada,
    flag_arma_original
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(arma_calibre, arma_marca, arma_tipo),
      \(x) x |>
        tolower() |>
        stringr::str_squish()
    ),
    arma_pais_fabricacao_original = toupper(arma_pais_fabricacao_original)
  ) |>
  depara_calibre("arma_calibre") |>
  depara_marca("arma_marca") |>
  depara_tipo("arma_tipo")

dados_armas_consolidado <- dados_armas_formatado |>
  dplyr::mutate(
    flag_tipo_arma_incompativel_calibre = dplyr::case_when(
      is.na(tipo_formatado) ~ NA,
      is.na(tipo_arma_calibre) ~ TRUE,
      stringr::str_detect(tipo_arma_calibre, tipo_formatado) ~ FALSE,
      TRUE ~ TRUE
    ),
    arma_tipo_final = dplyr::case_when(
      tipo_formatado == "artesanal" ~ "artesanal",
      !flag_tipo_arma_incompativel_calibre ~ tipo_formatado,
      !is.na(tipo_arma_calibre) ~ tipo_arma_calibre,
      TRUE ~ tipo_formatado
    ),
    arma_tipo_final = tolower(arma_tipo_final),
    arma_tipo_final = stringr::str_squish(arma_tipo_final),
    arma_calibre_final = dplyr::case_when(
      calibre_formatado_final == ".32 ou 34 G" &
        arma_tipo_final == "espingarda" ~ "32 gauge",
      calibre_formatado_final == ".32 ou 34 G" &
        arma_tipo_final %in% c("revolver", "garrucha") ~ ".32 S&W long",
      calibre_formatado_final == ".32 ou 34 G" &
        arma_tipo_final %in% c("pistola", "carabina", "submetralhadora") ~ ".32 acp",
      stringr::str_detect(calibre_formatado_final, "32$") ~ ".32 S&W long",
      calibre_formatado_final == ".32 ou 34 G" ~ ".32",
      TRUE ~ calibre_formatado_final
    ),
    arma_calibre_final = toupper(arma_calibre_final),
    arma_calibre_join = ifelse(
      stringr::str_detect(arma_calibre_final, "[.](32|22|38) "),
      stringr::str_sub(arma_calibre_final, 1, 3),
      arma_calibre_final
    ),
    arma_marca_final = dplyr::case_when(
      stringr::str_detect(tolower(arma_marca), "taur") ~ "taurus",
      stringr::str_detect(tolower(arma_marca), "ross") ~ "rossi",
      stringr::str_detect(tolower(arma_marca), "glo[ck]") ~ "glock",
      stringr::str_detect(tolower(arma_marca), "smith") ~ "s&w",
      TRUE ~ tolower(marca_arma_v2)
    ),
    arma_numero_serie_original = arma_numero_serie,
    arma_sn_formatado = tolower(arma_numero_serie),
    sn_disponivel = dplyr::case_when(
      is.na(arma_sn_formatado) | arma_sn_formatado == "" ~ NA_character_,
      nchar(arma_sn_formatado) <= 4 ~ "não aparente",
      stringr::str_detect(arma_sn_formatado, "expu|supri|obl|subt|apag") ~ "removido",
      stringr::str_detect(arma_sn_formatado, "raspa|esm|lixa") ~ "raspado",
      stringr::str_detect(arma_sn_formatado, "picot|pinad") ~ "puncionamento",
      stringr::str_detect(arma_sn_formatado, "riscad|adul|sobrepost|remarcad|danific") ~ "regravado/adulterado",
      stringr::str_detect(arma_sn_formatado, "desgast|oxidad|corroi|ferru") ~ "desgastado",
      stringr::str_detect(arma_sn_formatado, "^[:punct:]+$") ~ "só pontuações",
      stringr::str_detect(arma_sn_formatado, "^[a-z/\\-]+$") ~ "só texto",
      stringr::str_detect(arma_sn_formatado, "^[0-9a-z/\\-]+$") ~ "Sim",
      TRUE ~ "não aparente"
    ),
    arma_numero_serie = ifelse(
      sn_disponivel == "Sim",
      toupper(arma_numero_serie),
      ""
    ),
    flag_arma_artesanal = stringr::str_detect(arma_tipo_final, "artesanal"),
    flag_arma = as.logical(flag_arma),
    flag_arma = dplyr::case_when(
      flag_arma ~ TRUE,
      stringr::str_detect(arma_sn_formatado, "simulacro|muni[çc][ãa]o|chumbo") ~ FALSE,
      stringr::str_detect(tolower(arma_calibre), "cbc") ~ FALSE,
      arma_marca_final == "cbc" ~ FALSE,
      TRUE ~ FALSE
    ),
    id_arma = vctrs::vec_group_id(
      paste(
        id_bo,
        arma_tipo_final,
        arma_calibre_final,
        arma_marca_final,
        arma_origem,
        sep = "_"
      )
    )
  )

# Taurus

tab_regra_1 <- dados_armas_consolidado |>
  dplyr::filter(
    sn_disponivel == "Sim",
    !is.na(arma_numero_serie),
    !stringr::str_detect(arma_numero_serie, "[A-Z\\-]"),
    arma_marca_final == "taurus",
    arma_tipo_final == "revolver"
  ) |>
  aplicar_regra_1() |>
  dplyr::select(
    id_bo,
    id_arma,
    arma_ano_fabricacao
  ) |>
  dplyr::distinct(id_bo, id_arma, .keep_all = TRUE) |>
  dplyr::mutate(
    padrao_taurus = ifelse(is.na(arma_ano_fabricacao), NA_character_, "Regra 1")
  )

tab_regra_2_1 <- dados_armas_consolidado |>
  dplyr::filter(
    sn_disponivel == "Sim",
    !is.na(arma_numero_serie),
    nchar(arma_numero_serie) == 8,
    stringr::str_detect(arma_numero_serie, "^[A-Z]{3}"),
    arma_marca_final == "taurus",
    arma_tipo_final == "pistola"
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
  ) |>
  dplyr::mutate(
    padrao_taurus = ifelse(is.na(arma_ano_fabricacao), NA_character_, "Regra 2.1")
  )

tab_regra_2_2 <- dados_armas_consolidado |>
  dplyr::filter(
    sn_disponivel == "Sim",
    !is.na(arma_numero_serie),
    nchar(arma_numero_serie) %in% c(7, 8),
    stringr::str_detect(arma_numero_serie, "^[A-Z]{2}[0-9]"),
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
  ) |>
  dplyr::mutate(
    padrao_taurus = ifelse(is.na(arma_ano_fabricacao), NA_character_, "Regra 2.2")
  )

tab_regra_3 <- dados_armas_consolidado |>
  dplyr::filter(
    !is.na(arma_numero_serie),
    nchar(arma_numero_serie) == 9,
    stringr::str_detect(arma_numero_serie, "^[A-Z]{3}"),
    arma_marca_final == "taurus"
  ) |>
  dplyr::mutate(
    arma_ns_primeira_letra = stringr::str_sub(arma_numero_serie, 1, 1),
    arma_ns_segunda_letra = stringr::str_sub(arma_numero_serie, 2, 2),
    arma_ns_terceira_letra = stringr::str_sub(arma_numero_serie, 3, 3)
  ) |>
  aplicar_regra_3() |>
  dplyr::select(
    id_bo,
    id_arma,
    arma_ano_fabricacao
  ) |>
  dplyr::mutate(
    padrao_taurus = ifelse(is.na(arma_ano_fabricacao), NA_character_, "Regra 3")
  )


tab_taurus <- dplyr::bind_rows(
  tab_regra_1,
  tab_regra_2_1 |> dplyr::mutate(arma_ano_fabricacao = as.character(arma_ano_fabricacao)),
  tab_regra_2_2 |> dplyr::mutate(arma_ano_fabricacao = as.character(arma_ano_fabricacao)),
  tab_regra_3 |> dplyr::mutate(arma_ano_fabricacao = as.character(arma_ano_fabricacao))
)

armas_final <- dados_armas_consolidado |>
 dplyr::left_join(
    tab_taurus,
    by = c("id_bo", "id_arma")
  ) |>
  dplyr::select(
    id_bo,
    id_arma,
    flag_arma,
    arma_numero_serie_original,
    sn_disponivel,
    padrao_taurus,
    arma_ano_fabricacao,
    arma_tipo_original = arma_tipo,
    arma_tipo_formatado = tipo_formatado,
    arma_tipo_final,
    flag_arma_artesanal,
    arma_calibre_original = arma_calibre,
    arma_calibre_formatado = calibre_formatado_final,
    arma_calibre_final,
    compatibilidade_tipo = tipo_arma_calibre,
    flag_tipo_arma_incompativel_calibre,
    arma_marca_original = arma_marca,
    arma_marca_formatado = marca_arma_v2,
    arma_marca_final,
    arma_pais_fabricacao_original,
    arma_pais_fabricacao_final = pais_fabricacao,
    arma_origem,
    arma_modelo,
    flag_restrita,
    flag_patrimoniada,
    flag_arma_original
  )

    
# Gerando arquivo

data <- stringr::str_remove_all(Sys.Date(), "-")
writexl::write_xlsx(
  armas_final,
  glue::glue("inst/dados_rj/{data}_dados_armas_complementar.xlsx")
)


# Validação

armas_final |>
  dplyr::filter(is.na(arma_marca_final), !is.na(arma_marca_original)) |>
  dplyr::count(
    arma_marca_original,
    arma_marca_final,
    sort = TRUE
  ) |>
  writexl::write_xlsx("data-raw/dados_rj/validacao/escape_marcas_complementar.xlsx")


armas_final |>
  dplyr::filter(is.na(arma_calibre_final), !is.na(arma_calibre_original)) |>
  dplyr::count(
    arma_calibre_original,
    compatibilidade_tipo,
    arma_calibre_final,
    sort = TRUE
  ) |>
  writexl::write_xlsx("data-raw/dados_rj/validacao/escape_calibre_complementar.xlsx")


armas_final |> 
  dplyr::filter(!is.na(padrao_taurus)) |> 
  dplyr::select(
    padrao_taurus,
    sn_disponivel,
    arma_numero_serie_original,
    arma_ano_fabricacao
  ) |>
  writexl::write_xlsx("data-raw/dados_rj/validacao/regra_taurus.xlsx") 


