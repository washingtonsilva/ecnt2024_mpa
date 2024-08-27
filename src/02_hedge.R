
# Script: Estimação da Razão Ótima de Hedge
# Autor: Washington S. Silva
# Data: 2024-08-27
#
# Descrição: 
# Este script ilustra a estimação da razão ótima de hedge usando um 
# modelo de regressao linear simples. O script busca atualizar o código 
# disponibilizado por Brooks (2019).
#
# O script inclui as seguintes etapas:
# 1. Importação dos dados de uma planilha Excel.
# 2. Cálculo dos retornos compostos continuamente.
# 3. Remove os dados faltantes.
# 4. Armazenamento dos dados limpos no formato .rds para analises posteriores


# Carrega os pacotes necessários ------------------------------------------

library(here)      # Permite utilizar caminhos relativos no projeto
library(tidyverse) # Metapacote que inclui readr, dplyr, ggplot2...
library(readxl)    # Para importar planilhas Excel
library(skimr)     # alternativa para summary()



# Importação da planilha Excel --------------------------------------------

## define o caminho para o arquivo Excel contendo os dados
path_hedge <- here::here("data/raw/SandPhedge.xls")

## importa os dados da planilha Excel
dados_hedge <- readxl::read_xls(path_hedge)

## verifica a estrutura dos dados importados
dplyr::glimpse(dados_hedge)

## gera estatísticas descritivas básicas para os dados
summary(dados_hedge)

## apresenta uma visão geral mais detalhada dos dados
skimr::skim(dados_hedge)



# Cálculo dos Retornos Compostos Continuamente ----------------------------

## calcula e adiciona colunas para os retornos compostos continuamente 
## (logarítmicos) 
dados_hedge_retornos <- dados_hedge %>% 
  mutate(
    ret_spot = c(NA, 100 * diff(log(Spot))),
    ret_futuro = c(NA, 100 * diff(log(Futures))),
  )

## verifica a estrutura da nova base de dados com os retornos
dplyr::glimpse(dados_hedge_retornos)

## estatísticas descritivas básicas
summary(dados_hedge_retornos)

## remove os dados faltantes 
dados_hedge_clean <- dados_hedge_retornos %>% drop_na()

## verifica se os dados faltantes foram removidos
summary(dados_hedge_clean)



# Armazenamento dos Dados Limpos ------------------------------------------

## define o caminho para salvar os dados limpos no formato .rds
path_clean <- here::here("data/clean/dados_hedge_clean.rds")
write_rds(dados_hedge_clean, path_clean)
