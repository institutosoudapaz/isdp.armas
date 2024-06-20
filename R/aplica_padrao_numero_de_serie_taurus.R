aplica_padrao_taurus <- function(numero_de_serie){
  dplyr::bind_cols(
    tibble::tibble(numero_original = numero_de_serie),
    aplica_padrao_tipo1(numero_de_serie),
    aplica_padrao_tipo2(numero_de_serie),
    aplica_padrao_tipo3(numero_de_serie)
  ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        as.character
      )
    ) |>
    dplyr::mutate(
      ano_fabricacao_final = dplyr::coalesce(ano_fabricacao_padrao3, ano_fabricacao_padrao2, ano_fabricacao_padrao1),
      mes_fabricacao_final = dplyr::coalesce(mes_fabricacao_padrao3, mes_fabricacao_padrao2, mes_fabricacao_padrao1),
      marca_final = dplyr::coalesce(marca_padrao2, marca_padrao1),
      calibre_final = dplyr::coalesce(calibre_padrao2, calibre_padrao1),
    ) |>
    dplyr::select(
      numero_original, dplyr::contains("_final"), dplyr::contains("_padrao")
    )
}

aplica_padrao_tipo1 <- function(numero_de_serie){

  padrao_tipo1 <- stringr::regex("^[0-9]{4}[0-9]+$")

  padrao_detectado <- stringr::str_detect(numero_de_serie, padrao_tipo1)

  calibre <- dplyr::case_when(
    padrao_detectado ~ ".32",
    TRUE ~ NA_character_
  )

  tipo_arma <- dplyr::case_when(
    padrao_detectado ~ "revolver",
    TRUE ~ NA_character_
  )

  marca <- dplyr::case_when(
    padrao_detectado ~ "taurus",
    TRUE ~ NA_character_
  )

  numerico <- as.numeric(numero_de_serie)

  ano_padrao1 <- dplyr::case_when(
    padrao_detectado & numerico >= 2120 & numerico <= 14945 ~ 1954,
    padrao_detectado & numerico >= 14946 & numerico <= 32799 ~ 1955,
  )

  tibble::tibble(
    calibre_padrao1 = calibre,
    tipo_arma_padrao1 = tipo_arma,
    marca_padrao1 = marca,
    ano_fabricacao_padrao1 = ano_padrao1,
    mes_fabricacao_padrao1 = NA_character_
  )

}

aplica_padrao_tipo2 <- function(numero_de_serie){

  padrao_tipo2_2letras <- stringr::regex("^[A-Z]{2}[0-9]{5,6}$")
  padrao_tipo2_3letras <- stringr::regex("^[A-Z]{3}[0-9]{5}$")

  padrao1_detectado <- stringr::str_detect(numero_de_serie, padrao_tipo2_2letras)
  padrao2_detectado <- stringr::str_detect(numero_de_serie, padrao_tipo2_3letras)

  primeiro_digito <- dplyr::case_when(
    padrao1_detectado ~ stringr::str_sub(numero_de_serie, 1, 1),
    padrao2_detectado ~ stringr::str_sub(numero_de_serie, 2, 2),
    TRUE ~ NA_character_
  )

  segundo_digito <- dplyr::case_when(
    padrao1_detectado ~ stringr::str_sub(numero_de_serie, 2, 2),
    padrao2_detectado ~ stringr::str_sub(numero_de_serie, 3, 3),
  )

  digito_modelo <- dplyr::case_when(
    padrao2_detectado ~ stringr::str_sub(numero_de_serie, 1, 1),
    TRUE ~ NA_character_
  )

  return(tibble::tibble(
    digito_modelo = digito_modelo,
    primeiro_digito = primeiro_digito,
    segundo_digito = segundo_digito
  ) |>
  dplyr::left_join(
    padroes_taurus_1981_2019
  ) |>
  dplyr::left_join(
    padroes_modelo_taurus_1987
  ))
}

aplica_padrao_tipo3 <- function(numero_de_serie){

  padrao_tipo3 <- stringr::regex("^[A-Z]{3}[0-9]{6}$")

  padrao_detectado <- stringr::str_detect(numero_de_serie, padrao_tipo3)

  primeiros_digitos <- dplyr::case_when(
    padrao_detectado ~ stringr::str_sub(numero_de_serie, 1, 2),
    TRUE ~ NA_character_
  )

  terceiro_digito <- dplyr::case_when(
    padrao_detectado ~ stringr::str_sub(numero_de_serie, 3, 3),
    TRUE ~ NA_character_
  )

  return(tibble::tibble(
    primeiros_digitos = primeiros_digitos,
    terceiro_digito = terceiro_digito
  ) |>
    dplyr::left_join(
      padroes_taurus_vigente
    ))

}
