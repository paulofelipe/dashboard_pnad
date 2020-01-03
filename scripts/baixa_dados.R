library(PNADcIBGE)
library(tidyverse)
library(janitor)

baixa_dados <- FALSE

if(baixa_dados){
  variaveis_selecionadas <- c("Ano", "Trimestre", "UF", "V1028", "V2007", "V2009", "V2010", "V3007",
                              "VD4001", "VD4002", "VD4009", "VD4020", "VD4035")
  
  baixa_pnadc <- function(year, quarter){
    
    dadosPNADc <- get_pnadc(year = year, quarter = quarter, vars = variaveis_selecionadas, 
                            design = FALSE)  
    
    dadosPNADc
  }
  
  pnad_trimestres <- crossing(
    year = 2012:2019,
    quarter = 1:4
  ) %>% 
    filter(!(year == 2019 & quarter == 4))
  
  dados_pnad <- pmap_df(pnad_trimestres, baixa_pnadc)
  
  write_csv(dados_pnad, 'data/dados_pnadc.csv')
  rm(dados_pnad); gc()
}

dados_pnad <- read_csv('data/dados_pnadc.csv')

# Escreve dados do último semestre ----------------------------------------

dados_pnad %>% 
  filter(Ano == "2019", Trimestre == 3) %>% 
  write_csv('data/dados_pnadc_2019_3.csv')

# Série de desemprego -----------------------------------------------------

dados_pnad %>% 
  group_by(Ano, Trimestre, VD4002) %>%
  summarise(n = sum(V1028)) %>%
  na.omit() %>% 
  ungroup() %>% 
  spread(key = VD4002, value = n) %>% 
  clean_names() %>%
  replace_na(list(pessoas_desocupadas = 0)) %>% 
  group_by(ano, trimestre) %>% 
  summarise(
    pessoas_ocupadas = sum(pessoas_ocupadas),
    pessoas_desocupadas = sum(pessoas_desocupadas),
    tx_desemprego = sum(pessoas_desocupadas)/sum(pessoas_desocupadas + pessoas_ocupadas) * 100
  ) %>%
  write_csv('data/serie_desemprego.csv')

