dtct <- stringr::str_detect

re <- list(
  mdip = stringr::regex("art ?329[^0-9]|decorrente de intervenção policial|resistencia|execucao de ato legal"),
  homicidio = stringr::regex("art 121[^0-9]"),
  posse_ilegal_uso_permitido = stringr::regex("art ?14[^0-9]|art ?12[^0-9]"),
  posse_ilegal_uso_restrito = stringr::regex("art ?16[^0-9]"),
  disparo_arma_de_foto = stringr::regex("art ?15[^0-9]"),
  roubo = stringr::regex("art ?157[^0-9]"),
  furto = stringr::regex("art ?155[^0-9]"),
  receptacao = stringr::regex("art ?180[^0-9]"),
  lesao_corporal = stringr::regex("art ?129[^0-9]")
)

gera_crime_unificado <- function(rubrica, desdobramento, descr_conduta){

  unificado <- paste0(rubrica, " - ", desdobramento, " - ", descr_conduta) |>
    stringr::str_to_lower() |>
    abjutils::rm_accent() |>
    stringr::str_remove_all("[:punct:]")

  artigo_mencionado <- unificado |>
    stringr::str_extract("art ?[0-9]+") |>
    stringr::str_remove_all("[:space:]")

  dplyr::case_when(
    dtct(unificado, re$mdip) ~ "Intervenção policial",
    dtct(unificado, re$homicidio) ~ "Homicídio Doloso",
    dtct(unificado, re$posse_ilegal_uso_permitido) ~ "Posse ilegal de armas de uso permitido",
    dtct(unificado, re$posse_ilegal_uso_restrito) ~ "Posse ilegal de armas de uso restrito",
    dtct(unificado, re$disparo_arma_de_foto) ~ "Disparo de arma de fogo",
    dtct(unificado, re$roubo) ~ "Roubo",
    dtct(unificado, re$furto) ~ "Furto",
    dtct(unificado, re$receptacao) ~ "Receptação",
    dtct(unificado, re$lesao_corporal) ~ "Lesão corporal",
    TRUE ~ artigo_mencionado
  )
}
