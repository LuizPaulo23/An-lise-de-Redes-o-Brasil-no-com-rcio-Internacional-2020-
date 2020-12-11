# An-lise-de-Redes-o-Brasil-no-com-rcio-Internacional-2020-
Busco aplicar análise de redes nos dados referente ao comércio internacional brasileiro; para ser mais preciso, busco mapear as redes estabelecidas entre os Estados brasileiros exportadores com os países de destino (importadores). Nada além, de observar a relação dos vértices com o conjunto de ligações existente entre os vértices (arestas).

rm(list=ls())

# Packages 

library(tidyverse)
library(data.table)
library(ggplot2)
library(readr)
library(knitr)
library(RColorBrewer)
library(igraph)
library(cowplot)

data_base<- fread("C:/Users/LP/Projetos/EXP_2020.csv")

paises <- read.csv("http://www.mdic.gov.br/balanca/bd/tabelas/PAIS.csv", sep = ";", encoding = "latin1")


# Limpando, organizando 

# Identificando os países de destino 

data_base<- na.omit(data_base)

data_base = merge(data_base, paises, by= c("CO_PAIS"))

data_base <- select(data_base, CO_MES, CO_PAIS, SG_UF_NCM, CO_PAIS_ISOA3, VL_FOB) 

data_base$CO_MES= factor(data_base$CO_MES,
  levels = c("1", "2","3", "4", "5", "6", "7", "8", "9", "10", "11"), 
   labels = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", 
     "Julho", "Agosto", "Setembro", "Outubro", "Novembro"))

sumarizando_export<-data_base %>%
  group_by(CO_PAIS_ISOA3, SG_UF_NCM, CO_MES)%>%
  summarise(Total_export := sum(VL_FOB)/10^9)%>%
  arrange(desc(Total_export))


# Visualizando 

ggplot(sumarizando_export) +
  aes(x = SG_UF_NCM, y = Total_export) +
  geom_bar(stat="identity",fill='pink')+
  ylab("Em bilhões (US$)")+
  xlab("")+
  ggtitle("Exportações brasileiras por Estado - Janeiro a Novembro de 2020 - FOB (US$)")+
  xlim("SP", "MG","RJ","PA", "MT", "PR", "RS","GO","SC", 
       "BA", "MS", "ES", "MA","ND","CE","PE", "TO", "RO",
       "AM","PI", "AL", "AP","RN","RR", "DF", "PB", 
       "SE", "AC")+
  ylim(0, 40)+
  theme_bw()

# Janeiro, Fevereiro, Março

sumarizando_export %>%
  filter(!(CO_MES %in% c("Setembro", "Outubro", "Julho", "Novembro", 
  "Agosto", "Julho", "Junho", "Abril", "Maio"))) %>%
  ggplot() +
  aes(x = SG_UF_NCM, weight = Total_export) +
  geom_bar(fill = "pink") +
  coord_flip() +
  theme_bw() +
  facet_wrap(vars(CO_MES))+
  ylab("Em bilhões (US$)")+
  xlab("")+
  xlim("SP", "MG","RJ","PA", "MT", "PR", "RS","GO","SC", 
       "BA", "MS", "ES", "MA","ND","CE","PE", "TO", "RO",
       "AM","PI", "AL", "AP","RN", "DF", "PB", 
       "RR", "SE", "AC")+
  ggtitle("Exportações brasileiras mensais por Estado - FOB (US$)")


# Abril, Maio, Junho

sumarizando_export %>%
  filter(!(CO_MES %in% c("Setembro", "Outubro", "Julho", "Novembro", 
     "Agosto", "Julho", "Janeiro", "Fevereiro", "Março"))) %>%
  ggplot() +
  aes(x = SG_UF_NCM, weight = Total_export) +
  geom_bar(fill = "pink") +
  coord_flip() +
  theme_bw() +
  facet_wrap(vars(CO_MES))+
  ylab("Em bilhões (US$)")+
  xlab("")+
  xlim("SP", "MG","RJ","PA", "MT", "PR", "RS","GO","SC", 
       "BA", "MS", "ES", "MA","ND","CE","PE", "TO", "RO",
       "AM","PI", "AL", "AP","RN", "DF", "PB", 
       "RR", "SE", "AC")+
  ylim(0, 4)
  ggtitle("Exportações brasileiras mensais por Estado - FOB (US$)")



# Julho, Agosto, Setembro

sumarizando_export %>%
  filter(!(CO_MES %in% c( "Outubro", "Novembro", "Abril",
   "Janeiro", "Fevereiro", "Março", "Maio", "Junho"))) %>%
  ggplot() +
  aes(x = SG_UF_NCM, weight = Total_export) +
  geom_bar(fill = "pink") +
  coord_flip() +
  theme_bw() +
  facet_wrap(vars(CO_MES))+
  ylab("Em bilhões (US$)")+
  xlab("")+
  xlim("SP", "MG","RJ","PA", "MT", "PR", "RS","GO","SC", 
       "BA", "MS", "ES", "MA","ND","CE","PE", "TO", "RO",
       "AM","PI", "AL", "AP","RN", "DF", "PB", 
       "RR", "SE", "AC")+
  ylim(0,4)+
  ggtitle("Exportações brasileiras mensais por Estado - FOB (US$)")


# Outubro, Novembro 

sumarizando_export %>%
  filter(!(CO_MES %in% c("Setembro", "Julho", "Junho", "Maio", "Abril",
     "Agosto", "Julho", "Janeiro", "Fevereiro", "Março"))) %>%
  ggplot() +
  aes(x = SG_UF_NCM, weight = Total_export) +
  geom_bar(fill = "pink") +
  coord_flip() +
  theme_bw() +
  facet_wrap(vars(CO_MES))+
  ylab("Em bilhões (US$)")+
  xlab("")+
  xlim("SP", "MG","RJ","PA", "MT", "PR", "RS","GO","SC", 
       "BA", "MS", "ES", "MA","ND","CE","PE", "TO", "RO",
       "AM","PI", "AL", "AP","RN", "DF", "PB", 
       "RR", "SE", "AC")+
  ggtitle("Exportações brasileiras mensais por Estado - FOB (US$)")


# Principais importadores do Brasil 

origem_importacao<-data_base %>% 
  group_by(CO_PAIS)%>%
  summarise(Total_export := sum(VL_FOB)/10^9)%>%
  arrange(desc(Total_export))

origem_importacao <- left_join(origem_importacao, paises %>%
select(CO_PAIS, NO_PAIS), by = c("CO_PAIS" = "CO_PAIS"))%>%
  mutate(US_billions = round(origem_importacao$Total_export,
                             digits = 2))

# Os 10 principais destinos das exportações
mais_10<- dplyr::slice(origem_importacao, 1:10)


# Visualizar os 10 mais 

ggplot(mais_10) +
  aes(x = NO_PAIS, weight = US_billions) +
  geom_bar(fill = "pink") +
  coord_flip() +
  theme_bw()+
  ylab("Em bilhões (US$)")+
  xlab("")+
  xlim("China", "Estados Unidos", "Argentina", 
       "Países Baixos (Holanda)", "Espanha", "Japão", "Canadá",
       "Alemanha", "Chile", "México")+
  ggtitle("Top 10 Destinos das exportações brasileiras (2020, até novembro)")


# Análise de Rede

data_base$CO_MES = NULL 
data_base$CO_PAIS = NULL

# Excluindo redundância, agregando
new_base<-  data_base%>% 
  ungroup() %>% 
  count(SG_UF_NCM, CO_PAIS_ISOA3, sort = TRUE)

rede<- graph_from_data_frame(new_base, directed = T)


Vértices<-gorder(rede) # Vértices 
Densidade<-edge_density(rede)*100 # Densidade 

Direcionamento<-igraph::is.directed(rede) # Direcionamento 

tabela<- data.frame(Vértices, Densidade, Direcionamento)

kable(tabela, caption = "Métricas da rede de comércio")

# Métricas Básicas 

metricas <- data.frame(degree(rede, mode = "out"),
                      degree(rede, mode = "in", normalized = TRUE))
                      

colnames(metricas) <- c("Degree out",
                        "Degree in")
                       

kable(metricas, caption = "Métricas de Centralidade")

# Reduzindo a rede 

lim_inf <- quantile(new_base$n, prob = 0.99)
rede_reduzida <- new_base %>% filter(n > lim_inf)

quantile(new_base$n, seq(0, 1, 0.01))

# Rede
rede_reduzida<- graph_from_data_frame(rede_reduzida, directed = T)

plot(rede_reduzida, vertex.size = 20,
     vertex.color = "pink",
     edge.arrow.size=0.5,
     edge.width = 0.9, 
     edge.color = "gray",
     edge.lty = "dotted", 
     main="Estados brasileiros e o comércio Internacional", 
     sub="Com 1% dos vértices que possuem maior interação")


# Subgráfico 

grafo_sub <- subgraph.edges(rede_reduzida, 
                            E(rede_reduzida)[inc(c("SP"))])

plot(grafo_sub, vertex.color="pink",
     vertex.size=20, 
     main="Redes: Estado de São Paulo e o Comércio Externo", 
     sub="1% de interações",  edge.label.size=0.1)

plot(grafo_sub, 
     vertex.color="pink",
     vertez.size = 20, 
     edge.label.cex = 0.6,
     edge.arrow.size = 0.5,
     edge.label = E(rede_reduzida)$n, 
     main="Redes: com o número de interações", 
     sub="Com 1% dos vértices que possuem maior interação")



### Interações de 1% a 4% 

lim_inf <- quantile(new_base$n, prob = 0.98)
lim_inf <- quantile(new_base$n, prob = 0.97)

rede98 <- new_base %>% filter(n > lim_inf)
rede97 <- new_base %>% filter(n > lim_inf)

rede98<- graph_from_data_frame(rede98, directed = T)
rede97<- graph_from_data_frame(rede97, directed = T)

par(mfrow=c(1,1))

plot(rede98, vertex.size = 20,
     vertex.color = "pink",
     edge.arrow.size=0.5,
     edge.width = 0.9, 
     edge.color = "gray",
     edge.lty = "dotted", 
     main="", 
     sub="Com 2% dos vértices")

plot(rede97, vertex.size = 20,
     vertex.color = "pink",
     edge.arrow.size=0.5,
     edge.width = 0.9, 
     edge.color = "gray",
     edge.lty = "dotted", 
     main="", 
     sub="Com 3% dos vértices")
