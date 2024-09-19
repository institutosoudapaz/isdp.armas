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