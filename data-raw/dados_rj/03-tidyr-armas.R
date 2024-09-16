devtools::load_all()

dados_armas <- readr::read_rds("inst/dados_rj/dados_armas_rj.rds")
dados_ocorrencias <- readr::read_rds("inst/dados_rj/dados_ocorrencias_rj.rds")

dados_armas_formatado <- dados_armas |>
  dplyr::mutate(
    ano_bo = stringr::str_sub(controle, -4, -1)
  ) |>
  dplyr::select(
    controle,
    ano_bo,
    arma_calibre = calibre,
    arma_marca = marca,
    arma_tipo = tipo,
    arma_origem = origem
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(arma_calibre, arma_marca, arma_tipo),
      \(x) x |>
        tolower() |>
        stringr::str_squish()
    )
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
    arma_marca_final = dplyr::case_when(
      stringr::str_detect(tolower(arma_marca), "taur") ~ "taurus",
      stringr::str_detect(tolower(arma_marca), "ross") ~ "rossi",
      stringr::str_detect(tolower(arma_marca), "glo[ck]") ~ "glock",
      stringr::str_detect(tolower(arma_marca), "smith") ~ "s&w",
      TRUE ~ tolower(marca_arma_v2)
    ),
    flag_arma_artesanal = stringr::str_detect(arma_tipo_final, "artesanal"),
    flag_arma = as.logical(flag_arma),
    flag_arma = dplyr::case_when(
      flag_arma ~ TRUE,
      stringr::str_detect(tolower(arma_calibre), "cbc") ~ FALSE,
      arma_marca_final == "cbc" ~ FALSE,
      TRUE ~ FALSE
    ),
    id_arma = vctrs::vec_group_id(
      paste(
        controle,
        arma_tipo_final,
        arma_calibre_final,
        arma_marca_final,
        arma_origem,
        sep = "_"
      )
    )
  )

tab_arma_policial <- dados_ocorrencias |>
  dplyr::mutate(
    flag_arma_policial = stringr::str_detect(
      tolower(titulo),
      "interven[cç][aã]o|resist[êe]ncia"
    )
  ) |>
  dplyr::distinct(controle, flag_arma_policial) |> 
  dplyr::group_by(controle) |> 
  dplyr::summarise(
    flag_arma_policial = any(flag_arma_policial)
  )

armas_final <- dados_armas_consolidado |>
  dplyr::left_join(
    tab_arma_policial,
    by = "controle"
  ) |>
  dplyr::select(
    id_bo = controle,
    id_arma,
    flag_arma,
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
    pais_fabricacao,
    arma_origem,
    flag_arma_policial
  )

# Gerando arquivo

data <- stringr::str_remove_all(Sys.Date(), "-")
writexl::write_xlsx(
  armas_final,
  glue::glue("inst/dados_rj/{data}_dados_armas.xlsx")
)


# Validação

armas_final |> 
  dplyr::filter(is.na(arma_marca_final), !is.na(arma_marca_original)) |> 
  dplyr::count(
    arma_marca_original,
    arma_marca_final,
    sort = TRUE
  ) |> 
  writexl::write_xlsx("data-raw/dados_rj/validacao/escape_marcas.xlsx")


armas_final |> 
  dplyr::filter(is.na(arma_calibre_final), !is.na(arma_calibre_original)) |> 
  dplyr::count(
    arma_calibre_original,
    compatibilidade_tipo,
    arma_calibre_final,
    sort = TRUE
  ) |>
  writexl::write_xlsx("data-raw/dados_rj/validacao/escape_calibre.xlsx")
