# Dados armas ----------------------------------------------------------------

dados_armas <- readr::read_rds("inst/dados_rj/dados_armas_rj.rds")

dplyr::glimpse(dados_armas)

# Número de armas apreendidas
dados_armas |>
  dplyr::mutate(ano = stringr::str_sub(controle, -4, -1)) |>
  dplyr::count(ano)

# controle
dados_armas$controle |>
  nchar() |>
  unique()

# todos os códigos de controle possuem 13 caracteres

# categoria
dados_armas |>
  dplyr::count(categoria)

# só existe categoria = 1

# tipo
dados_armas |>
  dplyr::count(tipo)

# 11 tipos de armas
# existe garrucha e garruchão

# calibre
dados_armas |>
  dplyr::count(calibre, sort = TRUE)

dados_armas |>
  dplyr::count(calibre, sort = TRUE) |>
  dplyr::filter(n == 1)


# - 167 tipos diferentes
# - 44 tipos com apenas 1 ocorrência

# marca
dados_armas |>
  dplyr::count(marca, sort = TRUE)

dados_armas |>
  dplyr::count(marca, sort = TRUE) |>
  dplyr::filter(n == 1)

# - 157 tipos diferentes
# - 26 tipos com apenas 1 ocorrência
# - Existem vários nomes para NA: inderterminado, não identificada,
#   não identificado, s/marca...

# quantidade
dados_armas |>
  dplyr::count(quantidade, sort = TRUE)

# Com exceção de 2, com quantidade = 7, todos são quantidade = 1

# origem
dados_armas |>
  dplyr::count(origem, sort = TRUE)


# Dados ocorrências ----------------------------------------------------------

dados_ocorrencias <- readr::read_rds("inst/dados_rj/dados_ocorrencias_rj.rds")

dplyr::glimpse(dados_ocorrencias)

# controle
dados_ocorrencias$controle |>
  nchar() |>
  unique()

# todos os códigos de controle possuem 13 caracteres

# data_com e data_fato
min(dados_ocorrencias$data_com) # 2018
max(dados_ocorrencias$data_com) # 2023

min(dados_ocorrencias$data_fato, na.rm = TRUE) # 1916
max(dados_ocorrencias$data_fato, na.rm = TRUE) # 2023

# sexo
dados_ocorrencias |>
  dplyr::count(sexo)

# cor
dados_ocorrencias |>
  dplyr::count(cor)

# escolaridade
dados_ocorrencias |>
  dplyr::count(escolaridade)

# profissao
dados_ocorrencias |>
  dplyr::count(profissao)

# relacao
dados_ocorrencias |>
  dplyr::count(relacao)

# conteudo
dados_ocorrencias |>
  dplyr::count(conteudo)



# Lista de crimes -----------------------------------------------------------

dados_ocorrencias <- readr::read_rds("inst/dados_rj/dados_ocorrencias_rj.rds")
dados_armas_complementar <- readr::read_rds("inst/dados_rj/dados_armas_complementar.rds")

dplyr::bind_rows(
  dados_ocorrencias |>
    dplyr::select(titulo, titulo_do) |>
    tidyr::pivot_longer(
      cols = c(titulo, titulo_do),
      names_to = "coluna_origem",
      values_to = "crime"
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(base_origem = "dados_ocorrencias"),
  dados_armas_complementar |>
    dplyr::select(crime = tipo_delito) |>
    tidyr::separate_longer_delim(
      col = crime,
      delim = "///"
    ) |>
    dplyr::mutate(
      crime = stringr::str_squish(crime),
      coluna_origem = "tipo_delito",
      base_origem = "dados_armas_complementar"
    ) |>
    dplyr::distinct()
) |> 
dplyr::select(
  base_origem,
  coluna_origem,
  crime
) |> 
  writexl::write_xlsx("data-raw/dados_rj/validacao/crimes_rj.xlsx")


# Lista de valores da coluna patrimoniada ------------------------------------

dados_armas_complementar <- readr::read_rds("inst/dados_rj/dados_armas_complementar.rds")

dados_armas_complementar |>
  dplyr::count(patrimoniada, sort = TRUE) |> 
  writexl::write_xlsx("data-raw/dados_rj/validacao/patrimoniada_rj.xlsx")
  

# De para calibre

tab_depara_calibre <- ler_depara("calibre") |> 
  dplyr::distinct()

chaves_repitidas <- tab_depara_calibre |>
  dplyr::count(calibre, sort = TRUE) |> 
  dplyr::filter(n > 1) |> 
  dplyr::pull(calibre)

tab_depara_calibre |> 
  dplyr::filter(calibre %in% chaves_repitidas)  |> 
  dplyr::arrange(calibre) |> 
  writexl::write_xlsx("data-raw/dados_rj/validacao/depara_calibre_duplicado.xlsx")


# De para marca


tab_depara_marca <- ler_depara("marca") |> 
  dplyr::distinct()

chaves_repitidas <- tab_depara_marca |>
  dplyr::count(arma_marca, sort = TRUE) |> 
  dplyr::filter(n > 1) |> 
  dplyr::pull(arma_marca)

tab_depara_marca |> 
  dplyr::filter(arma_marca %in% chaves_repitidas)  |> 
  dplyr::arrange(arma_marca) |> 
  writexl::write_xlsx("data-raw/dados_rj/validacao/depara_marca_duplicado.xlsx")


# tipo

ler_depara("tipo") |> 
  dplyr::mutate(tipo = tolower(tipo)) |> 
  dplyr::distinct(tipo, tipo_formatado, flag_arma) |> 
  writexl::write_xlsx("~/Desktop/tipo.xlsx")
