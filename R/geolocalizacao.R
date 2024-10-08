#' Geolocalizar endereços
#'
#' Esta função realiza a geolocalização de endereços fornecidos, retornando as coordenadas de latitude e longitude.
#'
#' @param rua Vetor de caracteres com os nomes das ruas.
#' @param cidade Vetor de caracteres com os nomes das cidades.
#' @param estado Vetor de caracteres com os nomes dos estados.
#' @param cep Vetor de caracteres com os códigos postais (CEP).
#' @param pais Vetor de caracteres com os nomes dos países.
#'
#' @return Um data frame com as colunas `lat` e `long` contendo as coordenadas geográficas dos endereços fornecidos.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ruas <- c("Av. Paulista", "Praça da Sé")
#' cidades <- c("São Paulo", "São Paulo")
#' estados <- c("SP", "SP")
#' ceps <- c("01311-000", "01001-000")
#' paises <- c("Brasil", "Brasil")
#'
#' geolocalizar(rua = ruas, cidade = cidades, estado = estados, cep = ceps, pais = paises)
#' }
geolocalizar <- function(rua = "", cidade = "",
                         estado = "", cep = "", pais = "") {
  purrr::pmap(
    list(
      rua = rua,
      cidade = cidade,
      estado = estado,
      cep = cep,
      pais = pais
    ),
    function(rua, cidade, estado, cep, pais) {
      tidygeocoder::geo(
        street = rua,
        city = cidade,
        state = estado,
        postalcode = cep,
        country = pais
      )
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::select(lat, long)
}

#' Geolocalizar Endereço
#'
#' Esta função recebe um vetor de endereços e realiza a geolocalização de cada um deles,
#' salvando os resultados em um arquivo CSV e retornando as coordenadas (latitude e longitude).
#'
#' @param endereco Vetor de caracteres contendo os endereços a serem geolocalizados.
#'
#' @return Data frame com as colunas de latitude e longitude dos endereços geolocalizados.
#'
#' @examples
#' \dontrun{
#' enderecos <- c("Rua Exemplo 123, Cidade, Estado", "Avenida Teste 456, Cidade, Estado")
#' coordenadas <- geolocalizar_endereco(enderecos)
#' print(coordenadas)
#' }
#'
#' @export
geolocalizar_endereco <- function(endereco) {
  for (i in 1:length(endereco)) {
    tab <- tidygeocoder::geo(endereco[i])
    readr::write_csv(
      tab,
      file = "data-raw/dados_rj/validacao/tab_geo.csv",
      append = TRUE
    )
    cli::cli_alert_success("Endereço {i} geolocalizado.")
  }
  readr::read_csv(
    "data-raw/dados_rj/validacao/tab_geo.csv",
    col_names = c("endereco", "lat", "long")
  ) |>
    dplyr::select(lat, long)
}

#' Montar Endereço RJ
#'
#' Esta função recebe uma tabela e monta uma string de endereço.
#'
#' @param tab Um data frame contendo as colunas `bairro_fato` e `municipio_fato`.
#' @param base String indicando a base de dados a ser utilizada. Pode ser "rj", "rj_complementar" ou "sp".
#'
#' @return Um data frame com colunas adicionais: `estado`, `pais` e `endereco`.
#'
#' @export
montar_endereco <- function(tab, base) {
  if (base == "rj") {
    tab |>
      dplyr::mutate(
        bairro_fato = ifelse(
          stringr::str_detect(bairro_fato, "nao cadastrado| sem informa[cç][aã]o"),
          "",
          bairro_fato
        ),
        municipio_fato = stringr::str_remove(municipio_fato, " \\(Capital\\)"),
        estado = "RJ",
        pais = "Brasil",
        endereco = paste(
          bairro_fato,
          municipio_fato,
          estado,
          pais,
          sep = ", "
        )
      )
  } else {
    tab
  }
}
