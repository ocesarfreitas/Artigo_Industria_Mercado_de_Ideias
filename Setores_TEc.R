# Bibliotecas
library(readxl)
library(dplyr)
library(tidyr)
library(geomtextpath)
library(ggplot2)
library(janitor)
library(ggthemes)

# Importando arquivo
p_pib <- readxl::read_xls("Porcentagem_PIB.xls")

# Limpando nomes das colunas
names(p_pib)[2] <- "agropecuaria"
names(p_pib)[3] <- "industria"
names(p_pib)[4] <- "industria_ext"
names(p_pib)[5] <- "industria_trans"
names(p_pib)[6] <- "industria_outros1"
names(p_pib)[7] <- "industria_outros2"
names(p_pib)[8] <- "servicos"

# Limpando base
pib_setor <- p_pib %>%
  mutate(industria_outros = industria_outros1 + industria_outros2,
         Data = as.numeric(Data)) %>%
  select(-c(6:7))

col_order <- c("Data", "agropecuaria", "industria", "industria_ext", 
               "industria_trans","industria_outros","servicos")

pib_setor <- pib_setor[,col_order]

# Gráfico da particapação da industria de transformação no PIB
a <- ggplot(pib_setor, aes(x = Data, y = industria_trans)) +
  geom_point() +
  geom_line() +
  labs(x = "Ano",
       y = "Participação no PIB (%)") +
  theme_clean() +
  coord_cartesian(xlim = c(2000,2020),
                  ylim = c(10,20)) +
  scale_x_continuous(breaks = seq(2000, 2020, 1)) +
  labs(y = "Participação (%)", x = "Ano", caption = "Fonte: SCN Anual (IBGE)")
  

ggsave(plot = a, "part_pib3.png",
       width = 20, height = 8, units = "cm")

# Grafico com setores
pib_setor_long <- pib_setor %>%
  pivot_longer(cols = c(2:7), names_to = "Setor", values_to = "Porcentagem") %>%
  filter(Setor %in% c("agropecuaria", "industria", "industria_trans", "servicos") == T)

b<-ggplot(pib_setor_long, aes(x = Data, y = Porcentagem, shape = Setor)) +
  geom_line() +
  geom_point() +
  theme_clean() +
  scale_x_continuous(breaks = seq(1950, 2020, 5)) +
  scale_shape_discrete(labels = c("Agropecuária",
                                  "Indústria (Total)",
                                  "Indústria de Transfomação",
                                  "Serviços")) + 
  labs(y = "Participação (%)", x = "Ano", caption = "Fonte: SCN Anual (IBGE)")
ggsave(plot = b, "part_pib_2.png",
       width = 25, height = 10, units = "cm")

# Por Tecnolgia (Porcentagem)
df <- readxl::read_xlsx("tab10_2.xlsx") %>%
  rename("1" = "...1", "2" = "...2", "3" = "2010.0", "4" = "2011.0",
         "5" = "2012.0", "6" = "2013.0", "7" = "2014.0", "8" = "2015.0",
         "9" = "2016.0", "10" = "2017.0", "11" = "2018.0", "12" = "2019.0") %>%
  slice(-c(1:5,36:38)) %>%
  mutate(tecnologia = case_when(
   `1` %in% c("1091", "1092", "1092", "1100", "1200", "1300",
              "1400", "1500", "1600", "1700", "1800", "3180") ~ "Baixa",
   `1` %in% c("1991", "1992", "2200", "2300", "2491", "2492",
              "2500", "3300") ~ "Média-Baixa",
   `1` %in% c("2091", "2092", "2093", "2700", "2800", "2991",
              "2992", "3000") ~ "Média-Alta",
    TRUE ~ "Alta")) %>%
  select(-c(1:2))  

# Salvando dados totais
a <- df[1,] %>% select(-c(12))
a <- as.data.frame(t(a)) %>%
  row_to_names(row_number = 1) %>%
  mutate(Ano = 2010:2019)

# 
setores <- df %>%
  rename("2010" = "3", "2011" = "4", "2012" = "5", "2013" = "6", "2014" = "7",
         "2015" = "8", "2016" = "9", "2017" = "10", "2018" = "11", "2019" = "12") %>%
  pivot_longer(1:10, names_to = "Ano", values_to = "Valor") %>%
  mutate(Ano = as.numeric(Ano)) %>%
  group_by(tecnologia, Ano) %>%
  mutate(Valor = sum(Valor)) %>%
  distinct() %>%
  ungroup()

setores <- setores %>%
  group_by(Ano) %>%
  mutate(Total = sum(Valor)) %>% 
  ungroup() %>%
  group_by(Ano, tecnologia) %>%
  mutate(Porcentagem = Valor/Total) %>%
  select(-c(3:4))

c <- ggplot(setores, aes(x = Ano, y = Porcentagem, shape = tecnologia)) + 
  geom_point() +
  geom_line() + 
  scale_x_continuous(breaks = seq(2010, 2019, 1)) +
  labs(y = "Porcentagem (%)",
       caption = "Fonte: SCN Anual (IBGE)") +
  scale_shape_discrete(name = "Intensidade\nTecnológica",
                       labels = c("Baixa", "Média-Baixa", "Média-Alta", "Alta"))+
  theme_clean()

ggsave(plot = c, "part_pib_int_1.png",
       width = 17.5, height = 10, units = "cm")


# Por Tecnolgia (Trabalho)
ocup <- readxl::read_xls("tab15_2.xls") %>%
  slice(-c(1:3,5:12,43:75)) %>%
  row_to_names(row_number = 1)

names(ocup)[1] <- "Cod"
names(ocup)[2] <- "Setor"

ocup_tec <- ocup %>%
  mutate(tecnologia = case_when(
    Cod %in% c("1091", "1092", "1092", "1100", "1200", "1300",
               "1400", "1500", "1600", "1700", "1800", "3180") ~ "Baixa",
    Cod %in% c("1991", "1992", "2200", "2300", "2491", "2492",
               "2500", "3300") ~ "Média-Baixa",
    Cod %in% c("2091", "2092", "2093", "2700", "2800", "2991",
               "2992", "3000") ~ "Média-Alta",
    TRUE ~ "Alta")) %>%
  select(-c(1:2)) %>%
  mutate_at(1:10, as.numeric) %>%
  pivot_longer(1:10, names_to = "Ano", values_to = "Valor") %>%
  mutate(Ano = as.numeric(Ano)) %>%
  group_by(tecnologia, Ano) %>%
  mutate(Valor = sum(Valor)) %>%
  distinct() %>%
  ungroup() %>%
  group_by(Ano) %>%
  mutate(Total = sum(Valor)) %>% 
  ungroup() %>%
  group_by(Ano, tecnologia) %>%
  mutate(Porcentagem = Valor/Total) %>%
  select(-c(3:4))

d<-ggplot(ocup_tec, aes(x = Ano, y = Porcentagem, shape = tecnologia)) +
  geom_point() +
  geom_line() + 
  scale_x_continuous(breaks = seq(2010, 2019, 1)) +
  labs(y = "Porcentagem (%)",
       caption = "Fonte: SCN Anual (IBGE)") +
  scale_shape_discrete(name = "Intensidade\nTecnológica",
                       labels = c("Baixa", "Média-Baixa", "Média-Alta", "Alta"))+
  theme_clean()

ggsave(plot = d, "part_pib_int_2.png",
       width = 17.5, height = 10, units = "cm")

# População ocupada

