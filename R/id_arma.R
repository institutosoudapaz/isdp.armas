#' Gerar Identificador de Arma
#'
#' Esta função gera um identificador único para cada arma em uma tabela, 
#' baseado em diferentes colunas dependendo da base de dados fornecida.
#'
#' @param tab Um data frame contendo os dados das armas.
#' @param base Uma string indicando a base de dados. Pode ser "rj", "rj_complementar" ou "sp".
#'
#' @return Um data frame com uma nova coluna `id_arma` contendo os identificadores únicos.
#'
#'@export
gerar_id_arma <- function(tab, base) {
  if (base %in% c("rj", "rj_complementar")) {
    tab |>
      dplyr::mutate(
        id_arma = vctrs::vec_group_id(
          paste(
            id_bo,
            tipo_formatado,
            arma_calibre_final,
            arma_marca_final,
            arma_origem,
            sep = "_"
          )
        )
      )
  } else if (base == "sp") {
    tab |> 
      dplyr::mutate(
        id_arma = vctrs::vec_group_id(paste0(id_bo, "_", cont_arma))
      )
  }
}
