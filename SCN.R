library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)

df <- readxl::read_xls("tab13_1.xls") 

dados <- df %>%
  slice(-c(1:3,5,17:18)) %>%
  rename("Num" = "Tabela 13.1 - Participação no valor adicionado bruto a preços básicos,",
         "0" = "...2", "1"  = "...3", "2"  = "...4", "3"  = "...5", "4"  = "...6",
         "5"  = "...7", "6"  = "...8", "7"  = "...9", "8"  = "...10", "9"  = "...11",
         "10"  = "...12", "11"  = "...13", "12"  = "...14", "13"  = "...15",
         "14"  = "...16", "15"  = "...17", "16"  = "...18", "17"  = "...19",
         "18"  = "...20", "19"  = "...21", "20"  = "...22") %>%
  select(-Num)

  
dados_scn <- as.data.frame(t(dados)) %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  rename("Ano" = "na") %>%
  mutate_all(as.numeric) %>%
  select(-c(14:15)) %>%
  mutate(outras_industrias = eletricidade_e_gas_agua_esgoto_atividades_de_gestao_de_residuos + construcao) %>%
  select(-c(6:7,9:13)) %>%
  mutate_at(2:7, funs(round(., digits = 2)))
  
col_order <- c("Ano", "agropecuaria", "industria", "industrias_extrativas", 
               "industrias_de_transformacao","outras_industrias","servicos")
dados_scn <- dados_scn[,col_order]

write.table(dados_scn, "dados_scn_1.txt", sep = ",", quote = FALSE, row.names = F)

ggplot(dados_scn, aes(x=Anos, group=cut, fill=cut)) +
  geom_density(adjust=1.5, position="fill") +
  theme_ipsum()

