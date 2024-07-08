# Dados armas ----------------------------------------------------------------

dados_armas <- readr::read_rds("data-raw/dados_armas_rj.rds")

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

dados_ocorrencias <- readr::read_rds("data-raw/dados_ocorrencias_rj.rds")

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
