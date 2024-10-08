#' Aplica a Regra 1 para armas da marca Taurus
#'
#' Esta função aplica a Regra 1 para um conjunto de dados de armas da marca Taurus.
#' A função filtra as armas que possuem número de série disponível, converte o número
#' de série para formato numérico, ajusta o calibre da arma e realiza um join com uma
#' tabela de referência para determinar o ano de fabricação da arma.
#'
#' @param tab Data frame contendo os dados das armas.
#' @param calibre Calibre da arma a ser considerado na regra.
#'
#' @return Data frame com as colunas `id_bo`, `id_arma`, `arma_ano_fabricacao` e `padrao_taurus`. A coluna `padrao_taurus` indica se a arma segue a Regra 1.
#'
#' @export 
aplicar_regra_1 <- function(tab, calibre) {
  depara_regra_1 <- ler_depara("regra_taurus_1")

  tab |>
    dplyr::filter(
      sn_disponivel == "Sim",
      !is.na(arma_numero_serie_formatado),
      !stringr::str_detect(arma_numero_serie_formatado, "[A-Z\\-/]"),
      arma_marca_final == "taurus",
      tipo_formatado == "revolver"
    ) |>
    dplyr::mutate(
      arma_numero_serie_formatado = as.numeric(arma_numero_serie_formatado),
      arma_calibre = ifelse(
        stringr::str_detect(arma_calibre_final, "[.](32|22|38) "),
        stringr::str_sub(arma_calibre_final, 1, 3),
        arma_calibre_final
      )
    ) |>
    dplyr::left_join(
      depara_regra_1,
      by = dplyr::join_by(
        "arma_calibre",
        dplyr::between("arma_numero_serie_formatado", "num_serie_min", "num_serie_max")
      )
    ) |>
    dplyr::distinct(
      id_bo,
      id_arma,
      arma_ano_fabricacao
    ) |>
    dplyr::mutate(
      arma_ano_fabricacao = as.character(arma_ano_fabricacao),
      padrao_taurus = ifelse(is.na(arma_ano_fabricacao), NA_character_, "Regra 1")
    )
}


#' Aplica a Regra 2.1 para Filtragem e Transformação de Dados de Armas
#'
#' Esta função aplica a Regra 2.1 para filtrar e transformar dados de armas da 
#' marca Taurus.
#' 
#' @param tab Data frame contendo os dados das armas.
#' 
#' @details A função realiza as seguintes operações:
#' 1. Filtra as armas que possuem número de série disponível, número de série 
#' formatado com 8 caracteres, começando com três letras maiúsculas, marca 
#' final "taurus" e tipo "pistola".
#' 2. Extrai a segunda e terceira letras do número de série formatado.
#' 3. Realiza um join com a tabela depara da Regra 2.
#' 4. Remove duplicatas com base nos campos `id_bo`, `id_arma` e 
#' `arma_ano_fabricacao`.
#' 5. Converte o ano de fabricação da arma para caractere e adiciona a coluna
#' `padrao_taurus` com o valor "Regra 2.1" se o ano de fabricação não for NA.
#' 
#' @return Data frame transformado após a aplicação da Regra 2.1.
#' 
#' @export
aplicar_regra_2_1 <- function(tab) {
  depara_regra_2 <- ler_depara("regra_taurus_2")

  tab |>
    dplyr::filter(
      sn_disponivel == "Sim",
      !is.na(arma_numero_serie_formatado),
      nchar(arma_numero_serie_formatado) == 8,
      stringr::str_detect(arma_numero_serie_formatado, "^[A-Z]{3}"),
      arma_marca_final == "taurus",
      tipo_formatado == "pistola"
    ) |>
    dplyr::mutate(
      arma_ns_primeira_letra = stringr::str_sub(arma_numero_serie_formatado, 2, 2),
      arma_ns_segunda_letra = stringr::str_sub(arma_numero_serie_formatado, 3, 3)
    ) |>
    dplyr::left_join(
      depara_regra_2,
      by = dplyr::join_by(
        "arma_ns_primeira_letra",
        "arma_ns_segunda_letra"
      )
    ) |>
    dplyr::distinct(
      id_bo,
      id_arma,
      arma_ano_fabricacao
    ) |>
    dplyr::mutate(
      arma_ano_fabricacao = as.character(arma_ano_fabricacao),
      padrao_taurus = ifelse(is.na(arma_ano_fabricacao), NA_character_, "Regra 2.1")
    )
}

#' Aplica a Regra 2.2 para Filtragem e Mutação de Dados de Armas
#'
#' Esta função aplica a Regra 2.2 a uma tabela de dados de armas, filtrando e 
#' modificando os dados conforme especificado.
#' 
#' @param tab Data frame contendo os dados das armas.
#' 
#' @details A função realiza as seguintes
#' operações:
#' 
#' 1. Filtra as armas que possuem número de série disponível, número de série 
#' formatado com 7 ou 8 caracteres, que começam com duas letras seguidas de um 
#' número,  marca "taurus" e tipo "revolver".
#' 2. Adiciona colunas com a primeira e segunda letra do número de série.
#' 3. Realiza um join com a tabela depara da regra 2.
#' 4. Remove duplicatas com base em `id_bo`, `id_arma` e `arma_ano_fabricacao`.
#' 5. Converte o ano de fabricação para caractere e adiciona a coluna `padrao_taurus` 
#' indicando "Regra 2.2" quando o ano de fabricação não é NA. 
#' 
#' @return Data frame modificado após a aplicação da Regra 2.2.
#' 
#' @export 
aplicar_regra_2_2 <- function(tab) {
  depara_regra_2 <- ler_depara("regra_taurus_2")

  tab |>
    dplyr::filter(
      sn_disponivel == "Sim",
      !is.na(arma_numero_serie_formatado),
      nchar(arma_numero_serie_formatado) %in% c(7, 8),
      stringr::str_detect(arma_numero_serie_formatado, "^[A-Z]{2}[0-9]"),
      arma_marca_final == "taurus",
      tipo_formatado == "revolver"
    ) |>
    dplyr::mutate(
      arma_ns_primeira_letra = stringr::str_sub(arma_numero_serie_formatado, 1, 1),
      arma_ns_segunda_letra = stringr::str_sub(arma_numero_serie_formatado, 2, 2)
    ) |>
    dplyr::left_join(
      depara_regra_2,
      by = dplyr::join_by(
        "arma_ns_primeira_letra",
        "arma_ns_segunda_letra"
      )
    ) |>
    dplyr::distinct(
      id_bo,
      id_arma,
      arma_ano_fabricacao
    ) |>
    dplyr::mutate(
      arma_ano_fabricacao = as.character(arma_ano_fabricacao),
      padrao_taurus = ifelse(is.na(arma_ano_fabricacao), NA_character_, "Regra 2.2")
    )
}
#' Aplica a Regra 3 para a Tabela de Armas
#'
#' Esta função aplica a Regra 3 para uma tabela de armas, filtrando e transformando os dados conforme necessário.
#'
#' @param tab Um data frame contendo os dados das armas.
#' 
#' @return Um data frame com as colunas `id_bo`, `id_arma`, `arma_ano_fabricacao` e `padrao_taurus` após a aplicação da Regra 3.
#' 
#' @details
#' A função `aplicar_regra_3` realiza as seguintes operações:
#' 1. Filtra as armas que possuem número de série formatado, com exatamente 9 caracteres, começando com três letras maiúsculas, e cuja marca final é "taurus".
#' 2. Extrai as três primeiras letras do número de série formatado.
#' 3. Realiza um join com a tabela depara da Regra 3, utilizando as três primeiras letras do número de série.
#' 4. Remove duplicatas, mantendo apenas as colunas `id_bo`, `id_arma` e `arma_ano_fabricacao`.
#' 5. Converte o ano de fabricação para caractere e adiciona a coluna `padrao_taurus` com o valor "Regra 3" para registros com ano de fabricação não nulo.
#' 
#' @export
#'
aplicar_regra_3 <- function(tab) {
  depara_regra_3 <- ler_depara("regra_taurus_3")

  tab |>
    dplyr::filter(
      !is.na(arma_numero_serie_formatado),
      nchar(arma_numero_serie_formatado) == 9,
      stringr::str_detect(arma_numero_serie_formatado, "^[A-Z]{3}"),
      arma_marca_final == "taurus"
    ) |>
    dplyr::mutate(
      arma_ns_primeira_letra = stringr::str_sub(arma_numero_serie_formatado, 1, 1),
      arma_ns_segunda_letra = stringr::str_sub(arma_numero_serie_formatado, 2, 2),
      arma_ns_terceira_letra = stringr::str_sub(arma_numero_serie_formatado, 3, 3)
    ) |>
    dplyr::left_join(
      depara_regra_3,
      by = dplyr::join_by(
        "arma_ns_primeira_letra",
        "arma_ns_segunda_letra",
        "arma_ns_terceira_letra"
      )
    ) |>
    dplyr::distinct(
      id_bo,
      id_arma,
      arma_ano_fabricacao
    ) |>
    dplyr::mutate(
      arma_ano_fabricacao = as.character(arma_ano_fabricacao),
      padrao_taurus = ifelse(is.na(arma_ano_fabricacao), NA_character_, "Regra 3")
    )

}
