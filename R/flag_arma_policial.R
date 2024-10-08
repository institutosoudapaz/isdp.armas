#' Gerar Flag de Arma de Propriedade Policial
#'
#' Esta função gera uma flag indicando se a arma pertence à polícia ou não, 
#' com base em diferentes critérios dependendo da base de dados fornecida.
#'
#' @param tab Data frame contendo os dados a serem processados.
#' @param base String indicando a base de dados a ser utilizada. Pode ser "rj_complementar" ou "sp".
#'
#' @return Data frame com as colunas adicionais `flag_arma_policia_prop`, 
#' `flag_arma_policia_desd` (apenas para a base "sp") e `flag_arma_policia_prop_indisp` (apenas para a base "sp").
#'
#' @details
#' Para a base "rj_complementar", a função verifica se a coluna `patrimoniada` contém 
#' valores que indicam que a arma pertence a alguma força policial ou militar.
#'
#' Para a base "sp", a função utiliza expressões regulares para verificar se a coluna 
#' `arma_proprietario_nome` contém termos que indicam propriedade policial, além de 
#' verificar se a coluna `desdobramento` indica intervenção policial.
#'
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
  } else if (base == "sp") {
    regex_arma_prop_policial <- stringr::regex(
      "estado|PM|PC|Pol[ií]cia|guarda|gcm|PF|prefeitura|SMCASP|secretaria|ssp|SECRETARFIA DA SEGURANÇA PUBLICA - PLOICIA CIVIL SP",
      ignore_case = TRUE
    )

    tab |>
      dplyr::mutate(
        flag_arma_policia_desd = stringr::str_detect(
          desdobramento,
          "decorrente de intervenção policial"
        ),
        flag_arma_policia_prop = stringr::str_detect(
          arma_proprietario_nome,
          regex_arma_prop_policial
        ),
        flag_arma_policia_prop_indisp = is.na(arma_proprietario_nome)
      )
  }
}

#' Gerar Flag MDOIP
#'
#' Esta função gera uma flag para identificar registros de MDOIP
#' (Morte Decorrente de Intervenção Policial) em uma tabela de dados.
#'
#' @param tab Um data frame contendo os dados a serem processados.
#' 
#' @return Um data frame com uma nova coluna `flag_mdoip` indicando se o registro é um MDOIP.
#' 
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

#' Gerar Flag de Arma Policial
#'
#' Esta função gera uma flag indicando se uma arma é policial com base em diferentes critérios dependendo da base de dados fornecida.
#'
#' @param tab Data frame contendo os dados a serem processados.
#' @param base String indicando a base de dados a ser utilizada. Pode ser "rj", "rj_complementar" ou "sp".
#'
#' @return Data frame com a flag de arma policial adicionada.
#'
#' @details
#' - Para a base "rj", a flag é gerada com base na rubrica formatada e no calibre policial.
#' - Para a base "rj_complementar", a flag é gerada com base na disponibilidade
#' de número de série, na propriedade da arma pela polícia e no calibre policial.
#' - Para a base "sp", a flag é gerada com base na disponibilidade de número de 
#' série, na propriedade da arma pela polícia e no calibre policial.
#'
#' @export
gerar_flag_arma_policial <- function(tab, base) {
  if (base == "rj") {
    tab |>
      dplyr::mutate(
        flag_mdoip = rubrica_formatada == "MDOIP" | rubrica_formatada_do == "MDOIP"
      ) |>
      depara_calibre_policial(base = "rj") |>
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
      depara_calibre_policial(base = "rj_complementar") |>
      dplyr::mutate(
        flag_arma_policial = dplyr::case_when(
          sn_disponivel != "Sim" | is.na(sn_disponivel) ~ FALSE,
          flag_arma_policia_prop & flag_mdoip ~ TRUE,
          flag_calibre_policial & flag_mdoip ~ TRUE,
          TRUE ~ FALSE
        )
      )
  } else if (base == "sp") {
    tab |>
      gerar_flag_arma_policia_prop(base = "sp") |>
      depara_calibre_policial(base = "sp") |>
      dplyr::mutate(
        flag_arma_policia = dplyr::case_when(
          sn_disponivel != "Sim" | is.na(sn_disponivel) ~ FALSE,
          flag_arma_policia_prop ~ TRUE,
          flag_arma_policia_prop_indisp & flag_arma_policia_desd & flag_calibre_policial ~ TRUE,
          TRUE ~ FALSE
        )
      )
  }
}
