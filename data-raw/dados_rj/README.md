## Arquivos nesta pasta

- `01-import.R`: scripts que monta as bases de armas e ocorrências do RJ, a partir dos dados enviados pelo ISDP.

- `02-validacao.R`: faz uma validação geral dos dados, para detectar possíveis problemas.

- `03-join-tab-armas-complementar.R`: tenta juntar a base de armas vigente com uma base complementar, com o objetivo de agregar ao menos o número de série.

- `/raw`: arquivos originais enviados pelo ISPD.

- `dic`: dicionário de dados enviados pelo ISDP.



## Dúvidas

- A base de armas possuei 2423 linhas duplicadas. O que fazer com elas?

- Existem 6752 números de controle (de um total de 33978) na base de armas que não possuem correspondência na base de ocorrências. A gente vai precisar juntar essas bases?

- O que seria o id da arma na base do Rio?

- Usar as colunas quantidade e origem da base de armas?