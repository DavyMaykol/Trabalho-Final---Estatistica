
#=======================LIMPEZA DO AMBIENTE=====================================
#limpa objetos do workspace e previne conflitos
rm(list = ls())

#Evita notação cientifica e melhora a legibilidade numérica no console
options(scipen = 999, digits = 4)

#Fecha janelas graficas abertas em execuções anteriores
graphics.off()

#=======================PACOTES NECESSÁRIOS=====================================
#instalação caso necessário
if(!require("sidrar")) install.packages("sidrar")
if(!require("conflicted")) install.packages("conflicted")

#chamando bibliotecas
library(readr)
library(dplyr)
library(sidrar)
library(tidyverse)
library(conflicted)
library(janitor)

#===================ESTABELECENDO AS PASTAS DE TRABALHO=========================
# Exibe o diretório raiz do projeto, caso esteja usando projeto RStudio
here::here()

#Estabelecendo as pastas de trabalho
wd <-"~/Documentos/IFSM/ModuloII/Introducao_a_Estatistica_e_Probabilidade/Trabalho-Final---Estatistica"
wd1 <- file.path(wd, "arquivos_gerados/")

# Define a pasta de trabalho ativa
setwd(wd1)

# Lista arquivos da pasta para conferência
list.files()

#=========================================================================
# 2.2 Coleta de Dados
#=========================================================================
# a) Cosntrução do Dataframe - Dados de Criminalidade
#VARIÁVEL: df_dados_de_criminalidade
#----------------------------

#recebe o link de download da tabela
url_csv <- "https://drive.google.com/uc?export=download&id=12_MRBwS1QP26HpwuVAJr4cb-gylc0m5d"

#recebe a leitura do arquivo
dados_de_criminalidade <- readr::read_delim(url_csv, 
                                            delim = ",",
                                            col_names = TRUE, 
                                            locale = locale(encoding = "UTF-8", decimal_mark = "."),
                                            col_types = cols(uf             = col_character(),
                                                             municipio      = col_character(),
                                                             cod_municipio  = col_integer(),
                                                             total_vitima   = col_integer(),
                                                             populacao_2022 = col_integer())
                                            ) %>%
  #arredondamento de casas decimais
  mutate(taxa_100mil = round(taxa_100mil, 2))


# Seleção do Estado
estado <- "SP" 

df_dados_de_criminalidade <- dados_de_criminalidade %>%
dplyr::filter(uf == estado) %>%
# Garantir que o código seja tratado como texto para evitar perda de zeros à esquerda
mutate(cod_municipio = as.character(cod_municipio))

# Verificar se o filtro funcionou
print(paste("Total de Municípios no estado selecionado:", nrow(df_dados_de_criminalidade)))
head(df_dados_de_criminalidade)

write_csv(df_dados_de_criminalidade, "dados_de_criminalidade.csv") 
list.files() #Lista arquivos na pasta para conferência
  
#b)Ulilizar o pacote SIDRAR - Dados do Produto Interno Bruto de 2023 para cada município
#VARIÁVEL: pib_2023
#---------------------------
pib_tot_2023 <- get_sidra(api = "/t/5938/n6/35/v/37/p/last%201/d/v37%200")
pib_sp <- pib_2023 %>%
  select(Município,`Município (Código)`, Valor) %>%
  dplyr::filter(str_ends(Município, " - SP")) %>%
  mutate(Valor = as.numeric(Valor))

head(pib_sp)

head(df_dados_de_criminalidade)

df_pib_sp_limpo <- pib_sp %>% clean_names()
df_dados_de_criminalidade_limpo = df_dados_de_criminalidade %>% clean_names()

names(df_pib_sp_limpo)
names(df_dados_de_criminalidade_limpo)


df_pib_per_capita <- df_dados_de_criminalidade_limpo %>%
  left_join(df_pib_sp_limpo, by = c("cod_municipio" = "municipio_codigo")) %>%
  mutate(
    valor = as.numeric(valor),
    populacao_2022 = as.numeric(populacao_2022)
  )

#head(df_pib_per_capita)

df_pib_per_capita <- df_pib_per_capita %>%
  mutate(
    pib_per_capita = (valor * 1000) / populacao_2022) %>%
  select(municipio.x, pib_total_mil = valor, populacao_2022,pib_per_capita)

head(df_pib_per_capita )

#*Obs: NECESSITA FORMATAR/CONFERIR DADOS FINAIS
write_csv(df_pib_per_capita, "Pib_per_capita.csv")

  
  
#c) Quantidade de aglomerados urbanos por municipio
#VARIÁVEL: aglomerados_por_municipio
#---------------------------

#d) Pessoas alfabetizadas por municipio, criar coluna com taxa de analfabetismo municipal
#VARIÁVEL: analfabetismo_por_municipio
#---------------------------

#e)Percentual da população jovem (entre 15 e 29 anos)nos municípios
#VARIÁVEL: populacao_jovem
#---------------------------

#f)Familiasa beneficiadas pelo Bolsa Familia em dezembro de 2024
#VARIÁVEL: bolsa_familia_2024
#---------------------------


#=========================================================================
# 2.3 Construção do Modelo
#=========================================================================

#=========================================================================
# 2.4 Análise do Modelo
#=========================================================================

#=========================================================================
# 2.5 Ajuste do Modelo
#=========================================================================

#=========================================================================
# 2.6 Simplificando o Modelo
#=========================================================================

#=========================================================================
# 2.7 Customizando o Modelo
#=========================================================================



