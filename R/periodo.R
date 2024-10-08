#' Categoriza o período do dia com base em um horário fornecido
#'
#' @param x Um vetor de caracteres representando horários no formato "HH:MM"
#' @return Um vetor de caracteres com a categoria do período do dia correspondente:
#' \describe{
#'   \item{Madrugada}{Para horários entre "00:00" e "04:00"}
#'   \item{Início Manhã}{Para horários até "08:00"}
#'   \item{Manhã}{Para horários até "12:00"}
#'   \item{Início Tarde}{Para horários até "16:00"}
#'   \item{Tarde}{Para horários até "20:00"}
#'   \item{Noite}{Para horários entre "20:00" e "23:59"}
#' }
#' @examples
#' categorizar_periodo("03:00") # Retorna "Madrugada"
#' categorizar_periodo("09:00") # Retorna "Início Manhã"
#' categorizar_periodo("15:00") # Retorna "Início Tarde"
#' 
#' @export 
categorizar_periodo <- function(x) {
  dplyr::case_when(
    x <= "04:00" ~ "Madrugada",
    x <= "08:00" ~ "Início Manhã",
    x <= "12:00" ~ "Manhã",
    x <= "16:00" ~ "Início Tarde",
    x <= "20:00" ~ "Tarde",
    x > "20:00" ~ "Noite",
    TRUE ~ NA_character_
  )
}