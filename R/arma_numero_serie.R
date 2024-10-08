#' Gerar Disponibilidade do Número de Série da Arma
#'
#' Esta função recebe uma tabela e adiciona uma coluna `sn_disponivel` que indica a disponibilidade do número de série da arma.
#'
#' @param tab Um data frame contendo uma coluna `arma_numero_serie` com os números de série das armas.
#'
#' @return Um data frame com uma nova coluna `sn_disponivel` que classifica a disponibilidade do número de série da arma em várias categorias, como "não aparente", "removido", "raspado", "puncionamento", "regravado/adulterado", "desgastado", "só pontuações", "só texto" e "Sim".
#'
#' @examples
#' \dontrun{
#' tab <- data.frame(arma_numero_serie = c("1234", "expu123", "raspa567", "abcd", "1234abcd"))
#' gerar_sn_disponivel(tab)
#' }
gerar_sn_disponivel <- function(tab) {
  tab |>
    dplyr::mutate(
      arma_sn_formatado = arma_numero_serie |>
        tolower() |>
        stringr::str_remove_all(" +"),
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
      )
    ) |>
    dplyr::select(-arma_sn_formatado)
}

#' @title Gerar Número de Série Formatado
#' 
#' @description Esta função formata o número de série das armas em uma tabela.
#' 
#' @param tab Um data frame contendo a coluna `sn_disponivel` e `arma_numero_serie`.
#' 
#' @return Um data frame com uma nova coluna `arma_numero_serie_formatado` onde:
#' \itemize{
#'   \item Se `sn_disponivel` for "Sim", o número de série será convertido para letras maiúsculas e espaços serão removidos.
#'   \item Caso contrário, a coluna será preenchida com uma string vazia.
#' }
#' 
#' @export
gerar_numero_serie_formatado <- function(tab) {
  tab |>
    dplyr::mutate(
      arma_numero_serie_formatado = ifelse(
        sn_disponivel == "Sim",
        toupper(arma_numero_serie),
        ""
      ),
      arma_numero_serie_formatado = stringr::str_remove_all(
        arma_numero_serie_formatado,
        " +"
      )
    )
}
