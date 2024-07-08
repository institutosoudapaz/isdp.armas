library(tidyverse)

devtools::load_all()

# dados_sp2 <- readxl::read_excel(
#   "data-raw/SP pos recurso 23042024SIC 55366246800 - Recurso_xlsx.xlsx",
#   sheet = "Base de Dados (1)")

dados_sp2 <- readRDS("data-raw/dados_sp2.rds")

ocorrencias <- dados_sp2 |>
  mutate(
    crime_unificado = gera_crime_unificado(RUBRICA, DESDOBRAMENTO, DESCR_CONDUTA)
  ) |>
  group_by(
    ID_DELEGACIA, CIDADE, ANO_BO, NUM_BO,
    NOME_DEPARTAMENTO_CIRC,
    NOME_SECCIONAL_CIRC,
    NOME_DELEGACIA_CIRC,
    NOME_MUNICIPIO_CIRC,
    DESCR_TIPO_BO,
    DATA_OCORRENCIA_BO,
    HORA_OCORRENCIA_BO,
    DESCRICAO_APRESENTACAO,
    DATAHORA_REGISTRO_BO,
    DATA_COMUNICACAO_BO,
    DATAHORA_IMPRESSAO_BO,
    DESCR_PERIODO,
    AUTORIA_BO,
    FLAG_INTOLERANCIA,
    TIPO_INTOLERANCIA,
    FLAG_FLAGRANTE
    #RUBRICA,
    #DESCR_CONDUTA,
    #DESDOBRAMENTO,
    #FLAG_STATUS
    #BAIRRO,
    #DESCR_TIPOLOCAL,
    #DESCR_SUBTIPOLOCAL,
    #CEP,
    #LOGRADOURO,
    #NUMERO_LOGRADOURO,
    #DESCR_EXAME
  ) |>
  summarise(
    crimes_tentados = paste0(unique(crime_unificado[FLAG_STATUS != "CONSUMADO"]), collapse = ", "),
    crimes_consumados = paste0(unique(crime_unificado[FLAG_STATUS == "CONSUMADO"]), collapse = ", "),
    endereco1 = stringr::str_glue("{first(unique(LOGRADOURO))}, {first(unique(NUMERO_LOGRADOURO))}, {first(unique(BAIRRO))}, {first(unique(CEP))}"),
    endereco2 =stringr::str_glue("{first(unique(LOGRADOURO)[-1])}, {first(unique(NUMERO_LOGRADOURO)[-1])}, {first(unique(BAIRRO)[-1])}, {first(unique(CEP)[-1])}"),
  ) |>
  ungroup()

armas <- dados_sp2 |>
  distinct(
    ID_DELEGACIA, CIDADE, ANO_BO, NUM_BO,
    CONT_ARMA,
    DESCR_MODO_OBJETO,
    CALIBRE_ARMA,
    NUMERO_ARMA,
    MARCA_ARMA,
    DESCR_ARMA_FOGO
  ) |>
  mutate(
    FLAG_ARMA = !str_detect(DESCR_ARMA_FOGO, "Outros|Colete")
  )

estatistica_oficial <- tibble(
  ANO_BO = 2018:2023,
  estatistica_oficial = c(13138, 12810, 11551, 11787, 8408, 11754)
)

comparacao_contagens <- ocorrencias |>
  count(ANO_BO) |>
  rename(numero_ocorrencias = n) |>
  left_join(
    armas |>
      group_by(ANO_BO) |>
      summarise(
        n = n(),
        armas_distintas_apreendidas = sum(FLAG_ARMA, na.rm = TRUE)
      ) |>
      rename(
        objetos_distintas_apreendidas = n
      )
  ) |>
  left_join(
    estatistica_oficial
  )

writexl::write_xlsx(comparacao_contagens, "data-raw/comparacao_contagens.xlsx")

mascara_padroes_taurus <- armas$NUMERO_ARMA |> stringr::str_to_upper() |> aplica_padrao_taurus()

armas_arrumado <- armas |>
  mutate(
    MARCA_ARMA = tolower(MARCA_ARMA) |> stringr::str_remove_all("[:space:]"),
    MARCA_ARMA_V2 = incluir_marcas(MARCA_ARMA),
    CALIBRE_ARMA_V2 = incluir_calibres(CALIBRE_ARMA)
  ) |>
  bind_cols(mascara_padroes_taurus)

