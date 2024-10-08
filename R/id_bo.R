#' Gerar ID de Boletim de Ocorrência (BO)
#'
#' Esta função gera um identificador único para boletins de ocorrência (BO) 
#' com base em diferentes colunas, dependendo da base de dados especificada.
#'
#' @param tab Um data frame contendo os dados dos boletins de ocorrência.
#' @param base Uma string que especifica a base de dados. Pode ser "rj_complementar" ou "rj".
#'
#' @return Um data frame com uma nova coluna `id_bo` contendo os identificadores 
#' únicos para os boletins de ocorrência, se a base for "rj_complementar" ou "rj". 
#' Caso contrário, retorna o data frame original sem modificações.
#'
#' @export
gerar_id_bo <- function(tab, base) {
  if (base == "rj_complementar") {
    tab |>
      dplyr::mutate(
        id_bo = vctrs::vec_group_id(controle_interno_sco)
      )
  } else if (base == "rj") {
    tab |>
      dplyr::mutate(
        id_bo = vctrs::vec_group_id(controle)
      )
  } else {
    tab
  }
}
