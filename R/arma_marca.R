#' Gerar Marca Final da Arma
#'
#' Esta função processa uma tabela fornecida para gerar uma coluna final de marca de arma (`arma_marca_final`) com base nos valores da coluna `arma_marca`.
#'
#' @param tab Um data frame contendo a coluna `arma_marca` que possui as informações da marca da arma (coluna criada pela função [depara_marca()]).
#'
#' @return Um data frame com uma coluna adicional `arma_marca_final` que contém os nomes padronizados das marcas de armas.
#'
#' @details A função aplica regras para a criação da coluna final de calibre,
#' que possivelmente sobrescrevem os padrões definidos pela função [depara_marca()]. 
#' Ela faz isso definindo uma marca específica à arma quando um certo padrão é detectado na marca original. Os seguintes mapeamentos são aplicados:
#' \itemize{
#'   \item "taur" -> "taurus"
#'   \item "ross" -> "rossi"
#'   \item "glo\[ck\]" -> "glock"
#'   \item "smith" -> "s&w"
#' }
#' Se nenhum desses padrões for detectado, a função atribui o valor da coluna `marca_arma_v2` (convertido para minúsculas) à `arma_marca_final`.
#'
#' @export
gerar_arma_marca_final <- function(tab) {
  tab |>
    dplyr::mutate(
      arma_marca_final = dplyr::case_when(
        stringr::str_detect(tolower(arma_marca), "taur") ~ "taurus",
        stringr::str_detect(tolower(arma_marca), "ross") ~ "rossi",
        stringr::str_detect(tolower(arma_marca), "glo[ck]") ~ "glock",
        stringr::str_detect(tolower(arma_marca), "smith") ~ "s&w",
        TRUE ~ tolower(marca_arma_v2)
      )
    )
}
