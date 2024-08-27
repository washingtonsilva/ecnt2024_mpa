
# Script: Analise e Estimacao do CAPM para Diferentes Ativos
# Autor: Washington S. Silva
# Data: 2024-08-13
# última modificação: 2024-08-20 19:12 horas
#
# Descrição: 
# Este script realiza a análise e estimação do CAPM (Capital Asset Pricing Model) 
# para uma série de preços de acões, incluindo Ford, General Electric, 
# Microsoft, Oracle e o índice S&P 500. O script busca atualizar o código 
# disponibilizado por Brooks (2019).
#
# O script inclui as seguintes etapas:
# 1. Importação dos dados financeiros de uma planilha Excel.
# 2. Cálculo dos retornos compostos continuamente.
# 3. Cálculo dos retornos excedentes ajustados pela taxa de rendimentos dos títulos do tesouro dos EUA.
# 4. Armazenamento dos dados limpos em diferentes formatos (.rds, .csv, .parquet).
# 5. Análise exploratória dos dados, incluindo estatísticas descritivas e gráficos.
# 6. Estimação do modelo CAPM para os retornos excedentes da Ford.
# 7. Diagnóstico do modelo via simulação dos retornos excedentes da Ford.


# Carrega os pacotes necessários ------------------------------------------

library(here)      # Permite utilizar caminhos relativos no projeto
library(tidyverse) # Metapacote que inclui dplyr, ggplot2, etc.
library(readxl)    # Para importar planilhas Excel
library(nanoparquet) # Para leitura e escrita de arquivos Parquet
library(skimr)       # alternativa para summary()


# Importação da planilha Excel --------------------------------------------

## define o caminho para o arquivo Excel contendo os dados
path_capm <- here::here("data/raw/capm.xls")

## importa os dados da planilha Excel
dados_capm <- readxl::read_xls(path_capm)

## verifica a estrutura dos dados importados
dplyr::glimpse(dados_capm)

## gera estatísticas descritivas básicas para os dados
summary(dados_capm)

## apresenta uma visão geral mais detalhada dos dados
skimr::skim(dados_capm)


# Cálculo dos Retornos Compostos Continuamente ----------------------------

## calcula e adiciona colunas para os retornos compostos continuamente 
## (logarítmicos) 
dados_capm_retornos <- dados_capm %>% 
  mutate(
    ret_sp500 = c(NA, 100 * diff(log(SANDP))),
    ret_ford = c(NA, 100 * diff(log(FORD))),
    ret_ge = c(NA, 100 * diff(log(GE))),
    ret_microsoft = c(NA, 100 * diff(log(MICROSOFT))),
    ret_oracle = c(NA, 100 * diff(log(ORACLE)))
  )

## verifica a estrutura da nova base de dados com os retornos
dplyr::glimpse(dados_capm_retornos)


# Cálculo dos Retornos Excedentes -----------------------------------------

## calcula os retornos excedentes para cada ativo, ajustando pela taxa 
## de rendimentos dos títulos do tesouro dos EUA (USTB3M).
dados_capm_retexc <- dados_capm_retornos %>% 
  mutate(
    # ajusta a série anual de rendimentos para base mensal
    ustb3m = USTB3M / 12,
    # calcula os retornos excedentes
    retexc_sp500 = ret_sp500 - ustb3m,
    retexc_ford = ret_ford - ustb3m,
    retexc_ge = ret_ge - ustb3m,
    retexc_microsoft = ret_microsoft - ustb3m,
    retexc_oracle = ret_oracle - ustb3m
  )

## verifica a estrutura da nova base de dados com os retornos excedentes
dplyr::glimpse(dados_capm_retexc)


# Armazenamento dos Dados Limpos ------------------------------------------

## define o caminho para salvar os dados limpos no formato .rds
path_clean <- here::here("data/clean/dados_capm_clean.rds")
write_rds(dados_capm_retexc, path_clean)

## define o caminho para salvar os dados limpos no formato .csv
path_csv <- here::here("data/clean/dados_capm_clean.csv")
write_csv(dados_capm_retexc, path_csv)

## define o caminho para salvar os dados limpos no formato .parquet
path_parquet <- here::here("data/clean/dados_capm_clean.parquet") 
write_parquet(dados_capm_retexc, path_parquet)


# Análise Exploratória dos Dados ------------------------------------------

## importa os dados limpos no formato .rds para análise
path_clean <- here::here("data/clean/dados_capm_clean.rds")
dados_capm_retexc <- readr::read_rds(path_clean)

## remove valores faltantes dos dados para a análise
dados_capm_retexc <- dados_capm_retexc %>% drop_na()


## calcula estatísticas descritivas dos retornos excedentes da Ford
dados_capm_retexc %>% 
  summarise(
    media = mean(retexc_ford),
    mediana = median(retexc_ford),
    desvio_padrao = sd(retexc_ford),
    minimo = min(retexc_ford),
    maximo = max(retexc_ford),
    curtose = fBasics::kurtosis(retexc_ford),
    assimetria = fBasics::skewness(retexc_ford)
  ) %>% 
  tidyr::pivot_longer(cols = everything(), 
                      names_to = "Estatísticas (Ford)", 
                      values_to = "valor")

## calcula estatísticas descritivas dos retornos excedentes do S&P 500
dados_capm_retexc %>% 
  summarise(
    media = mean(retexc_sp500),
    mediana = median(retexc_sp500),
    desvio_padrao = sd(retexc_sp500),
    minimo = min(retexc_sp500),
    maximo = max(retexc_sp500),
    curtose = fBasics::kurtosis(retexc_sp500),
    assimetria = fBasics::skewness(retexc_sp500)
  ) %>% 
  pivot_longer(cols = everything(), 
               names_to = "Estatísticas (S&P 500)", 
               values_to = "valor")

## gera um gráfico de dispersão dos retornos excedentes da Ford vs. S&P 500
ford_dispersao <-
  ggplot(
    dados_capm_retexc,
    aes(x = retexc_sp500, y = retexc_ford)
  ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Retornos Excedentes da Ford vs. Retornos Excedentes do S&P 500",
    caption = "Fonte: Brooks (2019)",
    x = "Retornos Excedentes do S&P 500 (%)", 
    y = "Retornos Excedentes da Ford (%)"
  ) +
  theme_minimal()

## exibe o gráfico gerado
ford_dispersao


# Estimação do CAPM -------------------------------------------------------

## estima o modelo CAPM para os retornos excedentes da Ford
capm_ford <- lm(retexc_ford ~ retexc_sp500, data = dados_capm_retexc)

## exibe o resumo dos resultados da estimação do CAPM para a Ford
summary(capm_ford)

## calcula os intervalos de confiança para os parâmetros estimados
confint(capm_ford)


# Diagnóstico do Modelo via Simulação -------------------------------------

## Estratégia 1: Simulação dos retornos excedentes da Ford

## define a semente para reprodutibilidade da simulação
set.seed(1234)

## simula os retornos excedentes da Ford com base no modelo estimado
dados_capm_retexc <- dados_capm_retexc %>%
  mutate(retexc_ford_sim = -0.9560 +  1.8898*retexc_sp500 
         + rnorm(193, 0, 10.97)
  )

## gera um gráfico comparando as densidades dos retornos simulados e observados
ggplot(dados_capm_retexc, aes(x = retexc_ford_sim)) +
  geom_density(col = "red") +
  geom_density(aes(x = retexc_ford), col = "blue") +
  annotate(
    "text",
    x = -40,
    y = 0.013,
    label = "densidade dos retornos simulados",
    col = "red"
  ) +
  annotate(
    "text",
    x = 20,
    y = 0.035,
    label = "densidade dos retornos observados",
    col = "blue"
  ) + 
  theme_minimal()
