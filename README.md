# Violência Contra a Mulher na Pandemia
**Dados de origem**: Portal de Dados Abertos do Estado de Minas Gerais - Polícia Civil de MG - ocorrências policiais de [Feminicídio e Violência Contra Mulher](https://dados.mg.gov.br/dataset/violencia-contra-mulher) dos anos 2018, 2019, 2020, 2021 e 2022.

**Cruzamento de dados e hipóteses**: O objetivo é confirmar se a percepção da maioria das brasileiras de que a [violência contra a mulher cresceu na pandemia](https://www12.senado.leg.br/noticias/audios/2021/12/violencia-contra-a-mulher-cresceu-na-pandemia-indica-pesquisa-do-datasenado) se aplica ao estado de Minas Gerais.

**Métodos**: Análise Exploratória dos Dados (AED): análise descritiva, visualização de dados (gráficos de linha). Análise Inferencial: teste de proporções.
>[!Note]
>A pandemia foi oficialmente declarada no Brasil em 20 de março de 2020 e a [pesquisa do DataSenado](https://www12.senado.leg.br/noticias/arquivos/2021/12/09/pesquisa-violencia-domestica-e-familiar-contra-a-mulher_relatorio-final.pdf) foi realizada entre 14 de outubro a 5 de novembro de 2021, trazendo percepção subjetiva das consultadas em relação aos 12 meses anteriores.


## Passo 1) IMPORTAÇÃO E PREPARAÇÃO DOS DADOS
### instalar e habilitar pacotes
```
install.packages(c("readr", "dplyr", "ggplot2"))
   library(readr)
   library(dplyr)
   library(ggplot2)
```
### importar os arquivos
```
# Feminicídio
feminicidio_2018 <- read_csv2("feminicidio_2018.csv")
feminicidio_2019 <- read_csv2("feminicidio_2019.csv")
feminicidio_2020 <- read_csv2("feminicidio_2020.csv")
feminicidio_2021 <- read_csv2("feminicidio_2021.csv")
feminicidio_2022 <- read_csv2("feminicidio_2022.csv")

# Violência Doméstica
violencia_domestica_2018 <- read_csv2("violencia_domestica_2018.csv")
violencia_domestica_2019 <- read_csv2("violencia_domestica_2019.csv")
violencia_domestica_2020 <- read_csv2("violencia_domestica_2020.csv")
violencia_domestica_2021 <- read_csv2("violencia_domestica_2021.csv")
violencia_domestica_2022 <- read_csv2("violencia_domestica_2022.csv")
```
### checar as estruturas
```
str(feminicidio_2018)
str(feminicidio_2019)
str(feminicidio_2020)
str(feminicidio_2021)
str(feminicidio_2022)
str(violencia_domestica_2018)
str(violencia_domestica_2019)
str(violencia_domestica_2020)
str(violencia_domestica_2021)
str(violencia_domestica_2022)
```
### combinar dados
```
# feminicídio
dados_feminicidio <- bind_rows(feminicidio_2018, feminicidio_2019, feminicidio_2020, feminicidio_2021, feminicidio_2022)
# violência doméstica
dados_violencia_domestica <- bind_rows(violencia_domestica_2018, violencia_domestica_2019, violencia_domestica_2020, violencia_domestica_2021, violencia_domestica_2022)
```
### formatar data
```
# Converter data_fato para o formato Date
dados_feminicidio <- dados_feminicidio %>%
  mutate(data_fato = as.Date(data_fato, format = "%Y-%m-%d"))
dados_violencia_domestica <- dados_violencia_domestica %>%
  mutate(data_fato = as.Date(data_fato, format = "%Y-%m-%d"))
```
### verificar o resultado
```
View(dados_feminicidio)
View(dados_violencia_domestica)
```

## Passo 2) ANÁLISE EXPLORATÓRIA DOS DADOS

### resumo descritivo

**Feminicídio**
```
summary(dados_feminicidio)
```
![image](https://github.com/user-attachments/assets/4619386d-b2fb-4a66-b9f0-ceaacefe6e0f)

**Violência Doméstica**
```
summary(dados_violencia_domestica)
```
![image](https://github.com/user-attachments/assets/e826e5a6-54a1-4bd2-9c26-fb3cd122d633)

### visualização dos dados
**Feminicídios por Ano**
```
feminicidios_por_ano <- dados_feminicidio %>%
  group_by(ano) %>%
  summarise(total_feminicidios = sum(qtde_vitimas))

ggplot(feminicidios_por_ano, aes(x = ano, y = total_feminicidios)) +
  geom_line(color = "darkred") +  # Adiciona a cor darkred à linha
  geom_point(color = "black") +  # Adiciona pontos para melhor visualização
  labs(title = "Feminicídios por Ano", x = "Ano", y = "Total de Feminicídios") +
  theme_minimal()
```
![](https://github.com/alcangio/femStatistics/blob/main/feminicidio_ano.png)

**Violência Doméstica por Ano**
```
violencia_domestica_por_ano <- dados_violencia_domestica %>%
  group_by(ano) %>%
  summarise(total_violencia_domestica = sum(qtde_vitimas))
  
ggplot(violencia_domestica_por_ano, aes(x = ano, y = total_violencia_domestica)) +
  geom_line(color = "navyblue") +  # Adiciona a cor purple à linha
  geom_point(color = "black") +  # Adiciona pontos para melhor visualização
  labs(title = "Violência Doméstica por Ano", x = "Ano", y = "Total de Casos de Violência Doméstica") +
  theme_minimal()
```
![](https://github.com/alcangio/femStatistics/blob/main/violencia_ano.png)

## Passo 3) ANÁLISE INFERENCIAL
### Teste de proporções (comparando anos consecutivos)
**Calcular o total de casos por ano**
```
# Feminicídio
feminicidios_por_ano <- dados_feminicidio %>%
  group_by(ano) %>%
  summarise(total_feminicidios = sum(qtde_vitimas))

# Violência Doméstica
violencia_domestica_por_ano <- dados_violencia_domestica %>%
  group_by(ano) %>%
  summarise(total_violencia_domestica = sum(qtde_vitimas))
```

**Teste de proporção para feminicídio**
```
for (i in 1:(nrow(feminicidios_por_ano) - 1)) {
  # Total de casos do ano atual e do anterior
  casos_atual <- feminicidios_por_ano$total_feminicidios[i + 1]
  casos_anterior <- feminicidios_por_ano$total_feminicidios[i]
  # Total de registros (assumindo que o número total de casos é o mesmo)
  n_total <- sum(feminicidios_por_ano$total_feminicidios)
  # Realizando o teste de proporção
  teste_feminicidio <- prop.test(x = c(casos_atual, casos_anterior),
                                 n = c(n_total, n_total))
  # Resultados do teste
  cat("Comparação entre", feminicidios_por_ano$ano[i], "e", feminicidios_por_ano$ano[i + 1], ":\n")
  print(teste_feminicidio)
  cat("\n")
}
```
**Teste de proporção para violência doméstica**
```
for (i in 1:(nrow(violencia_domestica_por_ano) - 1)) {
  # Total de casos do ano atual e do anterior
  casos_atual <- violencia_domestica_por_ano$total_violencia_domestica[i + 1]
  casos_anterior <- violencia_domestica_por_ano$total_violencia_domestica[i]

  # Total de registros (assumindo que o número total de casos é o mesmo)
  n_total_violencia <- sum(violencia_domestica_por_ano$total_violencia_domestica)

  # Realizando o teste de proporção
  teste_violencia_domestica <- prop.test(x = c(casos_atual, casos_anterior),
                                         n = c(n_total_violencia, n_total_violencia))

  # Resultados do teste
  cat("Comparação entre", violencia_domestica_por_ano$ano[i], "e", violencia_domestica_por_ano$ano[i + 1], ":\n")
  print(teste_violencia_domestica)
  cat("\n")
}
```
![image](https://github.com/user-attachments/assets/44b1fd29-6630-4f0c-959a-44dc6a059b03)

Apenas entre 2018 e 2019 houve uma diferença estatisticamente significativa nas proporções de feminicídios, com uma leve redução. Nos demais anos (2019-2020, 2020-2021 e 2021-2022), as proporções de feminicídios não apresentaram mudanças significativas, sugerindo uma estabilidade.

Houve diferenças estatisticamente significativas nas proporções de violência doméstica entre os anos de 2018-2019, 2019-2020, e 2021-2022, porém não houve uma diferença significativa entre 2020 e 2021, indicando que as proporções foram semelhantes nesses dois anos.

## CONCLUSÃO
Esses resultados não confirmam a hipótese de que violência contra a mulher cresceu na pandemia, baseado nas ocorrências policiais registradas no estado de Minas Gerais.

>[!Warning]
>Vale ressaltar que [Estudo do Senado aponta subnotificação de 61% no registro de violência contra a mulher](https://www.camara.leg.br/noticias/1038979-estudo-do-senado-aponta-subnotificacao-de-61-no-registro-de-violencia-contra-mulher/).

***
### Referência Bibliográfica
GUERREIRO, Christian; ANDRADE, Ana Luiza Cerchiari de. _Fundamentos em Data Science e Estatísticas de Dados_. Ser Educacional, 2022.

**SENADO**. Violência contra a mulher cresceu na pandemia, indica pesquisa do DataSenado. Brasília, DF: Senado Federal, 2021. Disponível em: [https://www12.senado.leg.br/noticias/audios/2021/12/violencia-contra-a-mulher-cresceu-na-pandemia-indica-pesquisa-do-datasenado](https://www12.senado.leg.br/noticias/audios/2021/12/violencia-contra-a-mulher-cresceu-na-pandemia-indica-pesquisa-do-datasenado). Acesso em: 29 out. 2024.

**SENADO FEDERAL (Brasil)**. Violência doméstica e familiar contra a mulher: relatório final. 9. ed. Brasília, DF: Instituto DataSenado, nov. 2021. Disponível em: [https://www12.senado.leg.br/noticias/arquivos/2021/12/09/pesquisa-violencia-domestica-e-familiar-contra-a-mulher_relatorio-final.pdf](https://www12.senado.leg.br/noticias/arquivos/2021/12/09/pesquisa-violencia-domestica-e-familiar-contra-a-mulher_relatorio-final.pdf)). Acesso em: 29 out. 2024.

**BRASIL. Congresso Nacional**. Decreto Legislativo nº 6, de 2020. Disponível em: [https://legislacao.presidencia.gov.br/atos/?tipo=DLG&numero=6&ano=2020&ato=b1fAzZU5EMZpWT794](https://legislacao.presidencia.gov.br/atos/?tipo=DLG&numero=6&ano=2020&ato=b1fAzZU5EMZpWT794). Acesso em: 29 out. 2024.

**MINAS GERAIS**. Portal de Dados Abertos do Estado de Minas Gerais. Violência contra Mulher. Disponível em: [https://dados.mg.gov.br/dataset/violencia-contra-mulher](https://dados.mg.gov.br/dataset/violencia-contra-mulher). Acesso em: 30 out. 2024.

RSTUDIO TEAM. RStudio: Integrated Development Environment for R. Versão ‘2024.9.0.375’ "Cranberry Hibiscus". Boston, MA: RStudio, PBC, ano. Disponível em: [https://posit.co/download/rstudio/](https://posit.co/download/rstudio/). Acesso em: 29 out. 2024.

R CORE TEAM. R: A Language and Environment for Statistical Computing. Versão R version 4.4.1 (2024-06-14 ucrt). Viena, Áustria: R Foundation for Statistical Computing, ano. Disponível em: [https://www.r-project.org/](https://www.r-project.org/). Acesso em: 29 out. 2024.

**CÂMARA DOS DEPUTADOS**. Estudo do Senado aponta subnotificação de 61% no registro de violência contra mulher. Câmara dos Deputados, 2023. Disponível em: [https://www.camara.leg.br/noticias/1038979-estudo-do-senado-aponta-subnotificacao-de-61-no-registro-de-violencia-contra-mulher/](https://www.camara.leg.br/noticias/1038979-estudo-do-senado-aponta-subnotificacao-de-61-no-registro-de-violencia-contra-mulher/). Acesso em: 30 out. 2024.
