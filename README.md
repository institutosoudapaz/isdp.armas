
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
    `id_bo = vctrs::vec_group_id(paste0(id_delegacia, num_bo, ano_bo))`

### Observações

- Existem algumas armas com mesmo `id_arma` e `arma_numero_serie`
  diferentes. São apenas 6 casos e são sempre apenas 2 números de série
  distintos.

| id_arma | arma_numero_serie |
|--------:|:------------------|
|    1070 | AF26030           |
|    1070 | SBX30288          |
|    1074 | PN08804           |
|    1074 | 855059            |
|    1107 | 6432              |
|    1107 | 481384            |
|    5376 | 4327              |
|    5376 | BRA07556          |
|    5388 | 164552            |
|    5388 | SUPRIMIDO         |
|    6403 | 1958GO            |
|    6403 | ACK434879         |
