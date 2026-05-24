#=======================LIMPEZA DO AMBIENTE=====================================
rm(list = ls())
options(scipen = 999, digits = 4)
graphics.off()

#=======================PACOTES NECESSÁRIOS=====================================
if(!require("sidrar")) install.packages("sidrar")
if(!require("conflicted")) install.packages("conflicted")

library(readr)
library(dplyr)
library(sidrar)
library(tidyverse)
library(conflicted)
library(janitor)

#===================ESTABELECENDO AS PASTAS DE TRABALHO=========================
here::here()
wd_maykol <- "C:/Users/USER/OneDrive/Documentos/Pós Graduação/Estatística Aplicada/Trabalho"
wd1 <- file.path(wd_maykol, "arquivos_gerados/")
setwd(wd1)

# =========================================================================
# 2.2 Coleta de Dados
# a) Construção do Dataframe - Dados de Criminalidade
# =========================================================================

url_csv <- "https://drive.google.com/uc?export=download&id=12_MRBwS1QP26HpwuVAJr4cb-gylc0m5d"

api_dados_de_criminalidade_sp <- readr::read_delim(url_csv, 
                                                   delim = ",",
                                                   col_names = TRUE, 
                                                   locale = locale(encoding = "UTF-8", decimal_mark = "."),
                                                   col_types = cols(uf             = col_character(),
                                                                    municipio      = col_character(),
                                                                    cod_municipio  = col_integer(),
                                                                    total_vitima   = col_integer(),
                                                                    populacao_2022 = col_integer())
) %>%
  mutate(taxa_100mil = round(taxa_100mil, 2))

estado <- "SP" 

df_final_sp <- api_dados_de_criminalidade_sp %>%
  dplyr::filter(uf == estado) %>%
  mutate(cod_municipio = as.character(cod_municipio))

# CONFERÊNCIA PARTE 2.2 A)
print(paste("Total de Municípios filtrados para o estado: ", estado, "-> ", nrow(df_final_sp)))
head(df_final_sp)

write_csv2(df_final_sp, "p2_2_a_db_com_dados_de_criminalidade.csv") 

# =========================================================================
# 2.2 b) Utilizar o pacote SIDRAR - Dados do Produto Interno Bruto de 2023
# =========================================================================

api_pib_tot_2023 <- sidrar::get_sidra(api = "/t/5938/v/37/p/2023/n6/in%20n3%2035")

pib_sp <- api_pib_tot_2023 %>%
  select(Município, `Município (Código)`, Valor) %>%
  dplyr::filter(str_ends(Município, " - SP")) %>%
  mutate(Valor = as.numeric(Valor))

df_pib_sp <- pib_sp %>% clean_names()

df_final_sp <- df_final_sp %>%
  left_join(df_pib_sp, by = c("cod_municipio" = "municipio_codigo")) %>%
  mutate(
    valor = as.numeric(valor),
    populacao_2022 = as.numeric(populacao_2022)
  )

df_final_sp <- df_final_sp %>%
  mutate(pib_per_capita = (valor * 1000) / populacao_2022) %>%
  select(municipio = municipio.x, cod_municipio, populacao_2022, total_vitima, taxa_100mil, pib_per_capita)

# CONFERÊNCIA PARTE 2.2 B)
print("Primeiras linhas do modelo unificado (Criminalidade + PIB):")
head(df_final_sp)

write_csv2(df_final_sp, "p2_2_b_db_com_Pib_per_capita.csv")

# =========================================================================
# 2.2 c) Quantidade de aglomerados urbanos por município (Censo 2022)
# =========================================================================

api_aglomerados_br <- sidrar::get_sidra(api = "/t/9883/n6/all/v/all/p/all")

api_aglomerados_sp <- api_aglomerados_br %>%
  janitor::clean_names()

df_aglomerados_sp <- api_aglomerados_sp %>%
  dplyr::filter(variavel_codigo == "9910") %>%
  dplyr::filter(stringr::str_ends(municipio, " - SP")) %>%
  dplyr::select(municipio, codigo = "municipio_codigo", valor) %>%
  dplyr::mutate(
    codigo = as.character(codigo),
    aglomerados = as.numeric(valor)
  ) %>%
  # Removendo as colunas que já existem no DataFrame Central
  dplyr::select(-valor, -municipio) 

# Juntando com o DataFrame Central de forma limpa
df_final_sp <- df_final_sp %>%
  dplyr::left_join(df_aglomerados_sp, by = c("cod_municipio" = "codigo")) %>%
  dplyr::mutate(
    aglomerados = ifelse(is.na(aglomerados), 0, aglomerados)
  ) %>%
  select(municipio, cod_municipio, populacao_2022, total_vitima, taxa_100mil, pib_per_capita, aglomerados)

# CONFERÊNCIA PARTE 2.2 C)
print("Verificação do acúmulo de variáveis no modelo:")
names(df_final_sp)

print("Exemplo de municípios que tiveram o NA convertido para 0:")
df_final_sp %>% 
  dplyr::select(municipio, total_vitima, pib_per_capita, aglomerados) %>% 
  head(10)

write_csv2(df_final_sp, "p2_2_c_db_com_aglomerados.csv")

# =========================================================================
# 2.2 d) Taxa de pessoas não alfabetizadas (Censo 2022)
# =========================================================================

api_escolaridade_br <- sidrar::get_sidra(api = "/t/9543/n6/all/v/all/p/all")

api_escolaridade_sp <- api_escolaridade_br %>%
  janitor::clean_names()

api_escolaridade_sp <- api_escolaridade_sp %>%
  select(municipio, codigo = "municipio_codigo", valor) %>%
  dplyr::filter(str_ends(municipio, " - SP")) %>%
  mutate(valor = as.numeric(valor)) %>%
  # A tabela original trás o valor como alfabetização em percentual, então o analfabetismo é a porcentagem cheia menos a alfabetização
  mutate(analfabetismo = 100 - valor ) %>%
  # Removendo as colunas que já existem no DataFrame Central
  dplyr::select(-valor, -municipio) 

# Juntando com o DataFrame Central
df_final_sp <- df_final_sp %>%
  dplyr::left_join(api_escolaridade_sp, by = c("cod_municipio" = "codigo")) %>%
  select(municipio, cod_municipio, populacao_2022, total_vitima, taxa_100mil, pib_per_capita, aglomerados, analfabetismo)

# CONFERÊNCIA PARTE 2.2 D)

print("Verificação do acúmulo de variáveis no modelo centralizado (Inclusão do Analfabetismo):")
print(names(df_final_sp))

print("Resumo estatístico da Taxa de Analfabetismo gerada para os municípios de SP:")
summary(df_final_sp$analfabetismo)

print("Exemplo das 10 primeiras cidades para validação do relatório:")
df_final_sp %>% 
  dplyr::select(municipio, populacao_2022, taxa_100mil, analfabetismo) %>% 
  head(10)

write_csv2(df_final_sp, "p2_2_d_db_com_analfabetismo.csv")


# =========================================================================
# 2.2 e) Percentual de população jovem (Censo 2022)
# =========================================================================

# Variáveis utilizadas para cobrir o intervalo de 15 a 29 anos:
# 93086 (15 a 19 anos) | 93087 (20 a 24 anos) | 93088 (25 a 29 anos)
api_jovem_br <- sidrar::get_sidra(api = "/t/9514/n6/all/v/all/p/all/c2/6794/c287/93086,93087,93088/c286/113635/l/v")

api_jovem_sp <- api_jovem_br %>%
  janitor::clean_names()

api_jovem_sp <- api_jovem_sp %>%
  # Filtro por município
  dplyr::filter(str_ends(municipio, " - SP")) %>%
  # A tabela original possui uma linha com as pessoas e uma linha com percentual total, vou pegar o percentual
  dplyr::filter(unidade_de_medida_codigo == "2") %>%
  # Converter o valor para numérico
  dplyr::mutate(valor = as.numeric(valor)) %>%
  # Agrupar pelo código do município para somar as 3 faixas etárias
  dplyr::group_by(municipio_codigo, municipio) %>%
  dplyr::summarise(pop_jovem_total = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  # Renomeia o código e descarta o nome do município para o join
  dplyr::select(codigo = "municipio_codigo", pop_jovem_total) %>%
  dplyr::mutate(codigo = as.character(codigo))

# Juntando com o DataFrame Central
df_final_sp <- df_final_sp %>%
  dplyr::left_join(api_jovem_sp, by = c("cod_municipio" = "codigo")) %>%
  mutate()
  select(municipio, cod_municipio, populacao_2022, total_vitima, taxa_100mil, pib_per_capita, aglomerados, analfabetismo, perc_pop_jovem = pop_jovem_total)

# CONFERÊNCIA PARTE 2.2 E)

  print("Estrutura do Banco de Dados com Percentual de Jovens:")
  print(names(df_final_sp))
  
  print("Resumo Estatístico da Proporção de Jovens nos Municípios Paulistas:")
  summary(df_final_sp$perc_pop_jovem)
  
  print("Amostra das 10 primeiras cidades com TODAS as variáveis integradas:")
  df_final_sp %>% head(10)
  
    write_csv2(df_final_sp, "p2_2_e_db_com_percentual_jovens.csv")

