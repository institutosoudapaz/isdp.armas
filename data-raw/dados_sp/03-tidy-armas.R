dados_sp <- readr::read_rds("inst/dados_sp/dados_sp.rds")

library(tidyverse)

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
    arma_numero_serie = toupper(arma_numero_serie) |>
      stringr::str_remove_all(" +")
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
    flag_tipo_arma_incompativel_calibre = !(!is.na(tipo_formatado) & (stringr::str_detect(tipo_arma_calibre, tipo_formatado) | is.na(tipo_arma_calibre))),
    arma_tipo_join = tolower(dplyr::case_when(
        tipo_formatado == "artesanal" | stringr::str_detect(tipo_formatado, "artesanal|casei") ~ "artesanal",
        !is.na(tipo_formatado) & (stringr::str_detect(tipo_arma_calibre, tipo_formatado) | is.na(tipo_arma_calibre)) ~ tipo_formatado,
        !is.na(tipo_arma_calibre) & !is.na(tipo_formatado)  ~ tipo_arma_calibre,
        TRUE ~ tipo_formatado
    )) |>
      stringr::str_squish(),
    arma_calibre_final = dplyr::case_when(
      calibre_formatado_final %in% c(".32 ou 34 G", ".32") & tipo_formatado == "espingarda" ~ "32 gauge",
      calibre_formatado_final %in% c(".32 ou 34 G", ".32", ".32 S&W") & tipo_formatado %in% c("revolver", "garrucha") ~ ".32 S&W long",
      calibre_formatado_final %in% c(".32 ou 34 G", ".32") & tipo_formatado %in% c("pistola", "carabina", "submetralhadora") ~ ".32 acp",
      stringr::str_detect(calibre_formatado_final, "32^") ~ ".32 S&W long",
      TRUE ~ calibre_formatado_final
    ),
    arma_marca_final = dplyr::case_when(
      stringr::str_detect(tolower(arma_marca), "taur") ~ "taurus",
      stringr::str_detect(tolower(arma_marca), "ross") ~ "rossi",
      stringr::str_detect(tolower(arma_marca), "glo[ck]") ~ "glock",
      stringr::str_detect(tolower(arma_marca), "smith") ~ "s&w",
      TRUE ~ tolower(marca_arma_v2)),
    sn_disponivel = dplyr::case_when(
      is.na(arma_numero_serie) | arma_numero_serie == "" ~ NA_character_,
      nchar(arma_numero_serie) <= 4 ~ "não aparente",
      stringr::str_detect(tolower(arma_numero_serie), "expu|supri|obl|subt|apag") ~ "removido",
      stringr::str_detect(tolower(arma_numero_serie), "raspa|esm|lixa") ~ "raspado",
      stringr::str_detect(tolower(arma_numero_serie), "picot|pinad") ~ "puncionamento",
      stringr::str_detect(tolower(arma_numero_serie), "riscad|adul|sobrepost|remarcad|danific") ~ "regravado/adulterado",
      stringr::str_detect(tolower(arma_numero_serie), "desgast|oxidad|corroi|ferru") ~ "desgastado",
      stringr::str_detect(arma_numero_serie, "^[:punct:]+$") ~ "só pontuações",
      stringr::str_detect(arma_numero_serie, "^[A-Z/\\-]+$") ~ "só texto",
      stringr::str_detect(arma_numero_serie, "^[0-9A-Z/\\-]+$") ~ "Sim",
      TRUE ~ "não aparente"
    ),
    arma_numero_serie_original = arma_numero_serie,
    arma_numero_serie = dplyr::if_else(
      sn_disponivel == "Sim", arma_numero_serie, ""
    ),
    flag_arma_artesanal = stringr::str_detect(arma_tipo_join, "artesanal"),
    flag_arma = dplyr::case_when(
      flag_arma == "TRUE" ~ flag_arma,
      stringr::str_detect(tolower(arma_numero_serie), "simulacro|muni[çc][ãa]o|chumbo") ~ "FALSE",
      stringr::str_detect(tolower(arma_calibre), "cbc") ~ "FALSE",
      arma_marca_final == "cbc" ~ "FALSE",
      TRUE ~ flag_arma
    ),
    arma_calibre_final = if_else(arma_calibre_final == ".32 ou 34 G", ".32", arma_calibre_final),
    arma_calibre_final = toupper(arma_calibre_final),
    arma_calibre_join = ifelse(stringr::str_detect(arma_calibre_final, "[.](32|22|38) "), stringr::str_sub(arma_calibre_final, 1, 3), arma_calibre_final)
  )

#arma Pegando ano de produção segundo o número de série de armas Taurus

tab_regra_1 <- dados_armas_sp_consolidado |>
  dplyr::filter(
    sn_disponivel == "Sim",
    !is.na(arma_numero_serie),
    !stringr::str_detect(arma_numero_serie, "[A-Z]"),
    arma_marca_final == "taurus",
    arma_tipo_join == "revolver"
  ) |>
  aplicar_regra_1() |>
  dplyr::select(
    id_bo,
    id_arma,
    arma_ano_fabricacao
  ) |>
  dplyr::distinct(id_bo, id_arma, .keep_all = TRUE) |>
  dplyr::mutate(flag_padrao = ifelse(is.na(arma_ano_fabricacao), NA_character_, "Padrão 1"))

tab_regra_2_1 <- dados_armas_sp_consolidado |>
  dplyr::filter(
    sn_disponivel == "Sim",
    !is.na(arma_numero_serie),
    nchar(arma_numero_serie) == 8,
    stringr::str_detect(arma_numero_serie, "^[A-Z]{3}"),
    arma_marca_final == "taurus",
    arma_tipo_join == "pistola" # confirmar
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
  dplyr::mutate(flag_padrao = ifelse(is.na(arma_ano_fabricacao), NA_character_, "Padrão 2.1"))

tab_regra_2_2 <- dados_armas_sp_consolidado |>
  dplyr::filter(
    sn_disponivel == "Sim",
    !is.na(arma_numero_serie),
    nchar(arma_numero_serie) %in% c(7, 8),
    stringr::str_detect(arma_numero_serie, "^[A-Z]{2}"),
    arma_marca_final == "taurus",
    arma_tipo_join == "revolver"
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
  dplyr::mutate(flag_padrao = ifelse(is.na(arma_ano_fabricacao), NA_character_, "Padrão 2.2"))

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
  ) |>
  dplyr::mutate(flag_padrao = ifelse(is.na(arma_ano_fabricacao), NA_character_, "Padrão 3"))


tab_taurus <- dplyr::bind_rows(
  tab_regra_1,
  tab_regra_2_1 |> dplyr::mutate(arma_ano_fabricacao = as.character(arma_ano_fabricacao)),
  tab_regra_2_2 |> dplyr::mutate(arma_ano_fabricacao = as.character(arma_ano_fabricacao)),
  tab_regra_3 |> dplyr::mutate(arma_ano_fabricacao = as.character(arma_ano_fabricacao))
)

flag_arma_policia <- dados_sp |>
  select(id_bo, rubrica, descr_conduta, desdobramento, nome_proprietario) |>
  group_by(id_bo) |>
  summarise(
    condutas = paste0(unique(descr_conduta), collapse = " -- "),
    rubricas = paste0(unique(rubrica), collapse = " -- "),
    desdobramentos = paste0(unique(desdobramento), collapse = " -- "),
    flag_arma_policia_desd = any(str_detect(desdobramento, "decorrente de intervenção policial"), na.rm = TRUE),
    flag_arma_policia_prop = any(str_detect(nome_proprietario, regex("estado|PM|PC|Pol[ií]cia|guarda|gcm|PF|prefeitura|SMCASP|secretaria|ssp|SECRETARFIA DA SEGURANÇA PUBLICA - PLOICIA CIVIL SP", ignore_case = TRUE)), na.rm = TRUE),
    flag_arma_policia_prop_indisp = all(is.na(nome_proprietario))
  )

# Gerando arquivo

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
    arma_numero_serie_original,
    arma_marca_original = arma_marca,
    arma_tipo_original = arma_tipo,
    arma_marca_final = arma_marca_final,
    pais_fabricacao,
    arma_numero_serie_original,
    numero_serie_formatado = arma_numero_serie,
    compatibilidade_tipo = tipo_arma_calibre,
    arma_tipo_join,
    tipo_formatado,
    dplyr::contains("final"),
    sn_disponivel,
    arma_ano_fabricacao,
    flag_arma,
    flag_padrao,
    flag_tipo_arma_incompativel_calibre,
    flag_arma_artesanal
  ) |>
  dplyr::left_join(flag_arma_policia) |>
  dplyr::mutate(
    flag_arma_policia = case_when(
      sn_disponivel != "Sim" ~ FALSE,
      flag_arma_policia_prop ~ TRUE,
      flag_arma_policia_desd & flag_arma_policia_prop_indisp & (arma_calibre_final %in% c(".30 CARBINE", ".40 S&W", "5.56X45MM(223REM)", "7,62X51MM(.308WIN)") & tipo_formatado == "carabina") ~ TRUE,
      flag_arma_policia_desd & flag_arma_policia_prop_indisp & (arma_calibre_final %in% c("12 GAUGE") & tipo_formatado == "espingarda") ~ TRUE,
      flag_arma_policia_desd & flag_arma_policia_prop_indisp & (arma_calibre_final %in% c("5.56X45MM(223REM)", "7,62X51MM(.308WIN)") & tipo_formatado == "fuzil") ~ TRUE,
      flag_arma_policia_desd & flag_arma_policia_prop_indisp & (arma_calibre_final %in% c(".40 S&W", "9X19MM") & tipo_formatado == "metralhadora") ~ TRUE,
      flag_arma_policia_desd & flag_arma_policia_prop_indisp & (arma_calibre_final %in% c(".40 S&W", "9X19MM", ".45 ACP") & tipo_formatado == "pistola") ~ TRUE,
      flag_arma_policia_desd & flag_arma_policia_prop_indisp & (arma_calibre_final %in% c(".40 S&W", ".30 CARBINE", "9X19MM") & tipo_formatado == "submetralhadora") ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  distinct(id_bo, id_arma, .keep_all = TRUE)

# readr::write_rds(armas_final, "inst/dados_sp/dados_armas_sp.rds", compress = "xz")

writexl::write_xlsx(armas_final, "inst/dados_sp/20240902_dados_armas.xlsx")


armas_final |>
  View()

armas_final |>
  summarise(
    soma = sum(is.na(arma_calibre_final))
  )

readxl::read_excel("inst/dados_sp/dados_armas_bruto.xlsx") |>
  summarise(
    soma = sum(is.na(arma_calibre_final))
  )
