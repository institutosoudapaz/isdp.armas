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

montar_endereco_rj <- function(tab) {
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
}