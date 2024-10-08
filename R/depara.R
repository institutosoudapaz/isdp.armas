#' Ler De/Para
#'
#' Esta função lê uma aba específica de um arquivo Excel que contém 
#' as tabelas de de/para.
#'
#' @param aba Nome da aba a ser lida no arquivo Excel.
#' 
#' @return Um data frame contendo os dados da aba especificada.
#' 
#' @examples
#' \dontrun{
#'   dados <- ler_depara("calibre")
#' }
ler_depara <- function(aba) {
  readxl::read_excel(
    system.file(
      "tabelas_depara/gabarito_correcoes_pacote_armas.xlsx",
      package = "isdp.armas"
    ),
    sheet = aba
  )
}

#' Funções de-para
#'
#' Estas funções realizam a junção de uma tabela com uma tabela de "de-para".
#'
#' @param tab Data frame. A tabela que será unida com a tabela de "de-para".
#' @param nome_coluna Character. O nome da coluna na tabela `tab` que contém a chave para o de-para.
#' @param base Character. A base de dados que será utilizada para o de-para. Pode ser "rj_complementar" ou "rj_principal".
#'
#' @return Data frame. A tabela original `tab` com as novas colunas provinientes do de-para.
#'
#'
#' @export
depara_tipo <- function(tab, nome_coluna = "arma_tipo") {
  tab_depara_tipo <- ler_depara("tipo") |>
    dplyr::distinct(tipo, .keep_all = TRUE)

  if (nome_coluna != "tipo") {
    tab_depara_tipo <- tab_depara_tipo |>
      dplyr::rename_with(
        \(x) ifelse(x == "tipo", nome_coluna, x)
      )
  }

  tab |>
    dplyr::left_join(
      tab_depara_tipo,
      by = nome_coluna
    )
}

#' @rdname depara_tipo
depara_calibre <- function(tab, nome_coluna = "calibre") {
  tab_depara_calibre <- ler_depara("calibre") |>
    dplyr::distinct(calibre, .keep_all = TRUE)

  if (nome_coluna != "calibre") {
    tab_depara_calibre <- tab_depara_calibre |>
      dplyr::rename_with(
        \(x) ifelse(x == "calibre", nome_coluna, x)
      )
  }

  tab |>
    dplyr::left_join(
      tab_depara_calibre,
      by = nome_coluna
    )
}

#' @rdname depara_tipo
depara_calibre_policial <- function(tab, base) {
  if (base == "rj_complementar") {
    tab_depara_calibre <- ler_depara("calibre_arma_policial") |>
      dplyr::distinct()

    tab |>
      dplyr::left_join(
        tab_depara_calibre,
        by = c("arma_calibre_final", "tipo_formatado", "arma_marca_final")
      )
  } else {
    tab_depara_calibre <- ler_depara("calibre_arma_policial") |>
      dplyr::distinct(arma_calibre_final, tipo_formatado, flag_calibre_policial)

    tab |>
      dplyr::left_join(
        tab_depara_calibre,
        by = c("arma_calibre_final", "tipo_formatado")
      )
  }
}

#' @rdname depara_tipo
depara_marca <- function(tab, nome_coluna = "marca") {
  tab_depara_marca <- ler_depara("marca")

  if (nome_coluna != "marca") {
    tab_depara_marca <- tab_depara_marca |>
      dplyr::rename_with(
        \(x) ifelse(x == "marca", nome_coluna, x)
      )
  }

  tab |>
    dplyr::left_join(
      tab_depara_marca,
      by = nome_coluna
    )
}

#' @rdname depara_tipo
depara_crime <- function(tab, nome_coluna) {
  tab_depara_crime <- ler_depara("crimes") |>
    dplyr::rename_with(
      \(x) ifelse(x == "crime_original", nome_coluna, x)
    ) |> 
    dplyr::distinct()

  tab |>
    dplyr::left_join(
      tab_depara_crime,
      by = nome_coluna
    )
}
