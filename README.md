
# isdp.armas

## Dados São Paulo

### Variáveis criadas

- `id_bo`
    - onde: `data-raw/dados_sp/01-import.R`
    - descrição: identificador de boletim de ocorrência
    - metodologia: representa a combinação única de `id_delegacia`, `ano_bo` e `num_bo`
    - código: `id_bo = vctrs::vec_group_id(paste0(id_delegacia, num_bo, ano_bo))`
