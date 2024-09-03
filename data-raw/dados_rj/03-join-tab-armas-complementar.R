# Verificar se é possível juntar as informações da tabela
# "RJ 24648_APREENS_ARMA_FOGO_2019_BASE_APREENSAO.xlsx" com a tabela de armas
# gerada

dados_armas <- readr::read_rds("inst/dados_rj/dados_armas_rj.rds")
dados_ocorrencias <- readr::read_rds("inst/dados_rj/dados_ocorrencias_rj.rds")
dplyr::glimpse(dados_armas)
dplyr::glimpse(dados_ocorrencias)

dados_armas_complementar <- readxl::read_excel(
  "data-raw/dados_rj/raw/RJ 24648_APREENS_ARMA_FOGO_2019_BASE_APREENSAO.xlsx",
  guess_max = 5000
) |>
  janitor::clean_names() |>
  dplyr::filter(classe == "ARMA DE FOGO")

dplyr::glimpse(dados_armas_complementar)

# Por controle ❌
dados_armas_complementar |>
  dplyr::count(numero_de_serie, sort = TRUE)

controle <- dados_armas |>
  dplyr::mutate(controle = stringr::str_sub(controle, 1, 7)) |>
  dplyr::pull(controle)

sum(controle %in% dados_armas_complementar$controle_interno_sco)

# Por dados da ocorrência ❌
dados_armas |>
  dplyr::left_join(
    dados_ocorrencias |>
      dplyr::mutate(
        data_com = as.character(data_com),
        data_com = paste(data_com, hora_com)
      ) |>
      dplyr::distinct(controle, data_com),
    by = "controle"
  ) |>
  dplyr::left_join(
    dados_armas_complementar |>
      dplyr::select(
        numero_de_serie,
        tipo,
        calibre,
        marca,
        data_com = data_registro
      ) |>
      dplyr::mutate(
        dplyr::across(
          c(tipo, calibre, marca),
          tolower
        ),
        data_com = lubridate::with_tz(data_com, "America/Sao_Paulo"),
        data_com = as.character(data_com)
      ),
    by = c("tipo", "calibre", "marca", "data_com")
  ) |>
  dplyr::filter(!is.na(numero_de_serie))

# Por controle e número de procedimento ❌

controle <- dados_armas |>
  dplyr::mutate(controle = stringr::str_remove(controle, "-")) |>
  dplyr::pull(controle)

numero_procedimento <- dados_armas_complementar |>
  dplyr::mutate(
    numero_procedimento = stringr::str_remove_all(numero_procedimento, "-|/")
  ) |>
  dplyr::pull(numero_procedimento)

sum(numero_procedimento %in% controle)


# Rascunho

sum(dados_armas$controle %in% dados_ocorrencias$controle)
dados_ocorrencias$data_com |> min()
dados_ocorrencias$data_com |> max()

length(unique(dados_armas$controle)) -
  sum(unique(dados_armas$controle) %in% dados_ocorrencias$controle)


dados_armas |> dplyr::count(controle, sort = TRUE)
