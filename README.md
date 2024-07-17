
<!-- README.md is generated from README.Rmd. Please edit that file -->

# isdp.armas

<!-- badges: start -->
<!-- badges: end -->

## Dados São Paulo

### Variáveis criadas

- `id_bo`
  - onde: `data-raw/dados_sp/01-import.R`
  - descrição: identificador de boletim de ocorrência
  - metodologia: representa a combinação única de `id_delegacia`,
    `ano_bo` e `num_bo`
  - código:
    `id_bo = vctrs::vec_group_id(paste(id_delegacia, num_bo, ano_bo))`
- `id_arma`
  - onde: `data-raw/dados_sp/03-tidy-armas.R`
  - descrição: identificador de arma
  - metodologia: representa a combinação única de `id_bo` e `cont_arma`
  - código:
    `id_arma = vctrs::vec_group_id(paste0(id_bo, "_", cont_arma))`

### Observações
