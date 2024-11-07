# 1. IMPORTAÇÃO E PREPARAÇÃO DOS DADOS
## Instalação e ativação dos pacotes
install.packages(c("readr", "dplyr", "ggplot2", "e1071"))
library(readr)
library(dplyr)
library(ggplot2)
library(e1071)

## Importando os arquivos
feminicidio_2018 <- read_csv2("feminicidio_2018.csv")
feminicidio_2019 <- read_csv2("feminicidio_2019.csv")
feminicidio_2020 <- read_csv2("feminicidio_2020.csv")
feminicidio_2021 <- read_csv2("feminicidio_2021.csv")
feminicidio_2022 <- read_csv2("feminicidio_2022.csv")
violencia_domestica_2018 <- read_csv2("violencia_domestica_2018.csv")
violencia_domestica_2019 <- read_csv2("violencia_domestica_2019.csv")
violencia_domestica_2020 <- read_csv2("violencia_domestica_2020.csv")
violencia_domestica_2021 <- read_csv2("violencia_domestica_2021.csv")
violencia_domestica_2022 <- read_csv2("violencia_domestica_2022.csv")

## Checar estruturas
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

## Combinação dos dados
### feminicídio
dados_feminicidio <- bind_rows(feminicidio_2018, feminicidio_2019, feminicidio_2020, feminicidio_2021, feminicidio_2022)
### violência doméstica
dados_violencia_domestica <- bind_rows(violencia_domestica_2018, violencia_domestica_2019, violencia_domestica_2020, violencia_domestica_2021, violencia_domestica_2022)

## Formatação de data
### converter data_fato para o formato Date
dados_feminicidio <- dados_feminicidio %>%
  mutate(data_fato = as.Date(data_fato, format = "%Y-%m-%d"))
dados_violencia_domestica <- dados_violencia_domestica %>%
  mutate(data_fato = as.Date(data_fato, format = "%Y-%m-%d"))

## Verificar o resultado
View(dados_feminicidio)
View(dados_violencia_domestica)

# 2. ANÁLISE EXPLORATÓRIA DOS DADOS
## Resumo Descritivo
summary(dados_feminicidio)
summary(dados_violencia_domestica)

## Medidas de tendência central, variância e desvio padrão
#Feminicídio
media_fem <- mean(dados_feminicidio$qtde_vitimas, na.rm = TRUE)
mediana_fem <- median(dados_feminicidio$qtde_vitimas, na.rm = TRUE)
variancia_fem <- var(dados_feminicidio$qtde_vitimas, na.rm = TRUE)
desvio_padrao_fem <- sd(dados_feminicidio$qtde_vitimas, na.rm = TRUE)

print(media_fem)
print(mediana_fem)
print(variancia_fem)
print(desvio_padrao_fem)

#Violência doméstica
media_vd <- mean(dados_violencia_domestica$qtde_vitimas, na.rm = TRUE)
mediana_vd <- median(dados_violencia_domestica$qtde_vitimas, na.rm = TRUE)
variancia_vd <- var(dados_violencia_domestica$qtde_vitimas, na.rm = TRUE)
desvio_padrao_vd <- sd(dados_violencia_domestica$qtde_vitimas, na.rm = TRUE)

print(media_vd)
print(mediana_vd)
print(variancia_vd)
print(desvio_padrao_vd)

## Assimetria e Curtose
# Feminicídio
skewness_fem <- skewness(dados_feminicidio$qtde_vitimas, na.rm = TRUE)
kurtosis_fem <- kurtosis(dados_feminicidio$qtde_vitimas, na.rm = TRUE)
print(skewness_fem)
print(kurtosis_fem)

# Violência doméstica
skewness_vd <- skewness(dados_violencia_domestica$qtde_vitimas, na.rm = TRUE)
kurtosis_vd <- kurtosis(dados_violencia_domestica$qtde_vitimas, na.rm = TRUE)
print(skewness_vd)
print(kurtosis_vd)

## Correlação entre vítimas de feminicídio e violência doméstica por ano
# Combinar as médias por ano
dados_combinados <- data.frame(
  ano = c(2018, 2019, 2020, 2021, 2022),
  media_fem = c(mean(feminicidio_2018$qtde_vitimas, na.rm = TRUE), mean(feminicidio_2019$qtde_vitimas, na.rm = TRUE), mean(feminicidio_2020$qtde_vitimas, na.rm = TRUE), mean(feminicidio_2021$qtde_vitimas, na.rm = TRUE), mean(feminicidio_2022$qtde_vitimas, na.rm = TRUE)),
  media_vd = c(mean(violencia_domestica_2018$qtde_vitimas, na.rm = TRUE), mean(violencia_domestica_2019$qtde_vitimas, na.rm = TRUE), mean(violencia_domestica_2020$qtde_vitimas, na.rm = TRUE), mean(violencia_domestica_2021$qtde_vitimas, na.rm = TRUE), mean(violencia_domestica_2022$qtde_vitimas, na.rm = TRUE))
)

# Calcular a correlação
correlacao <- cor(dados_combinados$media_fem, dados_combinados$media_vd, use = "complete.obs")
print(correlacao) 

# Gráfico de dispersão para visualização da correlação
library(ggplot2)
ggplot(dados_combinados, aes(x = media_fem, y = media_vd)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Correlação entre Feminicídio e Violência Doméstica",
       x = "Média de Feminicídio",
       y = "Média de Violência Doméstica") +
  theme_minimal()

## Visualização dos Dados
### feminicidios por ano
feminicidios_por_ano <- dados_feminicidio %>%
  group_by(ano) %>%
  summarise(total_feminicidios = sum(qtde_vitimas))

ggplot(feminicidios_por_ano, aes(x = ano, y = total_feminicidios)) +
  geom_line(color = "darkred") +  # Adiciona a cor darkred à linha
  geom_point(color = "black") +  # Adiciona pontos para melhor visualização
  labs(title = "Feminicídios por Ano", x = "Ano", y = "Total de Feminicídios") +
  theme_minimal()

### violência doméstica por ano
violencia_domestica_por_ano <- dados_violencia_domestica %>%
  group_by(ano) %>%
  summarise(total_violencia_domestica = sum(qtde_vitimas))
  
ggplot(violencia_domestica_por_ano, aes(x = ano, y = total_violencia_domestica)) +
  geom_line(color = "navyblue") +  # Adiciona a cor purple à linha
  geom_point(color = "black") +  # Adiciona pontos para melhor visualização
  labs(title = "Violência Doméstica por Ano", x = "Ano", y = "Total de Casos de Violência Doméstica") +
  theme_minimal()

# 3. ANÁLISE INFERENCIAL
## Teste de Proporções (comparando anos consecutivos)
#### calcular o total de casos por ano para feminicídio
feminicidios_por_ano <- dados_feminicidio %>%
  group_by(ano) %>%
  summarise(total_feminicidios = sum(qtde_vitimas))

#### calcular o total de casos por ano para violência doméstica
violencia_domestica_por_ano <- dados_violencia_domestica %>%
  group_by(ano) %>%
  summarise(total_violencia_domestica = sum(qtde_vitimas))

#### teste de proporção para feminicídio
for (i in 1:(nrow(feminicidios_por_ano) - 1)) {
  # Total de casos do ano atual e do anterior
  casos_atual <- feminicidios_por_ano$total_feminicidios[i + 1]
  casos_anterior <- feminicidios_por_ano$total_feminicidios[i]
  
  # Total de registros
  n_total <- sum(feminicidios_por_ano$total_feminicidios)
  
  # Realizando o teste de proporção
  teste_feminicidio <- prop.test(x = c(casos_atual, casos_anterior),
                                 n = c(n_total, n_total))
  
  # Resultados do teste
  cat("Comparação entre", feminicidios_por_ano$ano[i], "e", feminicidios_por_ano$ano[i + 1], ":\n")
  print(teste_feminicidio)
  cat("\n")
}

#### teste de proporção para violência doméstica
for (i in 1:(nrow(violencia_domestica_por_ano) - 1)) {
  # Total de casos do ano atual e do anterior
  casos_atual <- violencia_domestica_por_ano$total_violencia_domestica[i + 1]
  casos_anterior <- violencia_domestica_por_ano$total_violencia_domestica[i]
  
  # Total de registros
  n_total_violencia <- sum(violencia_domestica_por_ano$total_violencia_domestica)
  
  # Realizando o teste de proporção
  teste_violencia_domestica <- prop.test(x = c(casos_atual, casos_anterior),
                                         n = c(n_total_violencia, n_total_violencia))
  
  # Resultados do teste
  cat("Comparação entre", violencia_domestica_por_ano$ano[i], "e", violencia_domestica_por_ano$ano[i + 1], ":\n")
  print(teste_violencia_domestica)
  cat("\n")
}
