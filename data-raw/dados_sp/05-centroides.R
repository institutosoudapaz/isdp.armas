shape <- readRDS("shp_capital.rds")

library(sf)

centroides <- st_centroid(shape$geometry)

processa_centroide <- function(centro){
  tibble::tibble(
    lat = centro[[1]],
    lon = centro[[2]]
  )
}

tabela_dados_dps <- tibble::tibble(
  DpGeoDes = shape$DpGeoDes,
  SecGeoDes = shape$SecGeoDes,
  DepGeoDes = shape$DepGeoDes
)

centros_latlon <- tabela_dados_dps |>
  dplyr::bind_cols(
    purrr::map_dfr(centroides, processa_centroide)
  )

saveRDS(centros_latlon, "data-raw/dados_sp/centroides_dps.rds")
