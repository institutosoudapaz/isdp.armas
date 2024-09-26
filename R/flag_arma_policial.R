gerar_flag_arma_policia_prop <- function(tab, base) {
  if (base == "rj_complementar") {
    tab |>
      dplyr::mutate(
        flag_arma_policia_prop = dplyr::case_when(
          tolower(patrimoniada) == "polícia militar" ~ TRUE,
          tolower(patrimoniada) == "polícia civil" ~ TRUE,
          tolower(patrimoniada) == "corpo de bombeiro" ~ TRUE,
          tolower(patrimoniada) == "força nacional" ~ TRUE,
          tolower(patrimoniada) == "forças armadas" ~ TRUE,
          tolower(patrimoniada) == "patrimoniada / outras" ~ TRUE,
          TRUE ~ FALSE
        )
      )
  }
}

gerar_flag_mdoip <- function(tab) {
  tab_mdoip <- ler_depara("crimes") |>
    dplyr::filter(crime_formatado == "MDOIP")

  tab |>
    dplyr::mutate(
      flag_mdoip = purrr::map_lgl(
        tipo_delito,
        \(x) any(stringr::str_detect(x, tab_mdoip$crime_original))
      )
    )
}

gerar_flag_arma_policial <- function(tab, base) {
  if (base == "rj") {
    tab_mdoip <- ler_depara("crimes") |>
      dplyr::filter(crime_formatado == "MDOIP")

    tab_ocorrencias_mdoip <- dados_ocorrencias |>
      dplyr::mutate(
        flag_mdoip = dplyr::case_when(
          titulo %in% tab_mdoip$crime_original |
            titulo_do %in% tab_mdoip$crime_original ~ TRUE,
          TRUE ~ FALSE
        )
      ) |>
      dplyr::distinct(id_bo = controle, flag_mdoip) |>
      dplyr::group_by(id_bo) |>
      dplyr::summarise(
        flag_mdoip = any(flag_mdoip)
      )

    tab |>
      dplyr::left_join(
        tab_ocorrencias_mdoip,
        by = "id_bo"
      ) |>
      depara_calibre_policial() |>
      dplyr::mutate(
        flag_arma_policial = dplyr::case_when(
          flag_calibre_policial & flag_mdoip ~ TRUE,
          TRUE ~ FALSE
        )
      )
  } else if (base == "rj_complementar") {
    tab |>
      gerar_flag_arma_policia_prop(base = "rj_complementar") |>
      gerar_flag_mdoip() |>
      depara_calibre_policial() |>
      dplyr::mutate(
        flag_arma_policial = dplyr::case_when(
          sn_disponivel != "Sim" ~ FALSE,
          flag_arma_policia_prop & flag_mdoip ~ TRUE,
          flag_calibre_policial & flag_mdoip ~ TRUE,
          TRUE ~ FALSE
        )
      )
  } else if (base == "sp") {

    flag_arma_policia <- tab |>
        dplyr::select(id_bo, rubrica, descr_conduta, desdobramento, arma_proprietario_nome) |>
        dplyr::group_by(id_bo) |>
        dplyr::summarise(
          condutas = paste0(unique(descr_conduta), collapse = " -- "),
          rubricas = paste0(unique(rubrica), collapse = " -- "),
          desdobramentos = paste0(unique(desdobramento), collapse = " -- "),
          flag_arma_policia_desd = any(str_detect(desdobramento, "decorrente de intervenção policial"), na.rm = TRUE),
          flag_arma_policia_prop = any(str_detect(arma_proprietario_nome, regex("estado|PM|PC|Pol[ií]cia|guarda|gcm|PF|prefeitura|SMCASP|secretaria|ssp|SECRETARFIA DA SEGURANÇA PUBLICA - PLOICIA CIVIL SP", ignore_case = TRUE)), na.rm = TRUE),
          flag_arma_policia_prop_indisp = all(is.na(arma_proprietario_nome))
        )

      tab |>
        dplyr::left_join(flag_arma_policia) |>
        depara_calibre_policial() |>
        dplyr::mutate(
          flag_arma_policia = case_when(
            sn_disponivel != "Sim" | is.na(sn_disponivel) ~ FALSE,
            flag_arma_policia_prop ~ TRUE,
            flag_arma_policia_desd & flag_arma_policia_prop_indisp & flag_calibre_policial ~ TRUE,
            # flag_arma_policia_desd & flag_arma_policia_prop_indisp & (arma_calibre_final %in% c(".30 CARBINE", ".40 S&W", "5.56X45MM(223REM)", "7,62X51MM(.308WIN)") & tipo_formatado == "carabina") ~ TRUE,
            # flag_arma_policia_desd & flag_arma_policia_prop_indisp & (arma_calibre_final %in% c("12 GAUGE") & tipo_formatado == "espingarda") ~ TRUE,
            # flag_arma_policia_desd & flag_arma_policia_prop_indisp & (arma_calibre_final %in% c("5.56X45MM(223REM)", "7,62X51MM(.308WIN)") & tipo_formatado == "fuzil") ~ TRUE,
            # flag_arma_policia_desd & flag_arma_policia_prop_indisp & (arma_calibre_final %in% c(".40 S&W", "9X19MM") & tipo_formatado == "metralhadora") ~ TRUE,
            # flag_arma_policia_desd & flag_arma_policia_prop_indisp & (arma_calibre_final %in% c(".40 S&W", "9X19MM", ".45 ACP") & tipo_formatado == "pistola") ~ TRUE,
            # flag_arma_policia_desd & flag_arma_policia_prop_indisp & (arma_calibre_final %in% c(".40 S&W", ".30 CARBINE", "9X19MM") & tipo_formatado == "submetralhadora") ~ TRUE,
            TRUE ~ FALSE
          )
        )
  }
}
