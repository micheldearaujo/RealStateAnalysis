#"""
#Created on Thu Apr 29 15:35:30 2021

#@author: micheldearaujo

#url = 'https://www.zapimoveis.com.br/aluguel/imoveis/pe+recife/?pagina=%s&onde=,Pernambuco,Recife,,,,,city,BR%3EPernambuco%3ENULL%3ERecife,-8.057838,-34.882897%3B,Pernambuco,Paulista,,,,,city,BR%3EPernambuco%3ENULL%3EPaulista,-7.918455,-34.820956%3B,Pernambuco,Jaboat%C3%A3o%20dos%20Guararapes,,Jaboatao,,,neighborhood,BR%3EPernambuco%3ENULL%3EJaboatao%20dos%20Guararapes%3EBarrios%3EJaboatao,-8.12113,-35.01541%3B,Pernambuco,Olinda,,,,,city,BR%3EPernambuco%3ENULL%3EOlinda,-7.990632,-34.84166%3B,Pernambuco,Paulista,,Janga,,,neighborhood,BR%3EPernambuco%3ENULL%3EPaulista%3EBarrios%3EJanga,-7.928936,-34.822733&transacao=Aluguel&tipo=Im%C3%B3vel%20usado'.%i
# https://forest-gis.com/download-de-shapefiles/
#"""


# Limpando o workspace
rm(list=ls())

# Importanto as biblitoecas necess?rias
library(stringr)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidygeocoder)
endereco='Jardim Flor da Montanha, Guarulhos - SP'
retorno <- geocode(.tbl=anuncios[1:10,], address='Endereco')
# - Ploting
library(ggplot2)
library(ggmap)
library(mapview)
library(sf)
library(geobr)
library(ggthemes)
library(ggalt)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(sp)
# Definindo o work directory
setwd('D:/michel/Universidade/Mestrado/disciplina/repo/2_VA/ProjetoFinal')

# Abrindo o arquivo e explorando os dados
file <- 'housing_prices.csv'
annun = read.csv(file)
str(annun)
View(annun)

rents_geo <- read.csv(file5, encoding='utf-8')
View(rents_geo)


# Perceba que este dataset possui apenas uma coluna, com 6120 observa??es. Perceba tamb?m que cada observa??o
# ? composta por uma string que cont?m um texto completo de um an?ncio de aluguel de apartamento do site
# Zap im?veis, ou seja, ? um conjunto de dados n?o estruturados, pois possui diferentes informa??es em apenas
# uma coluna.

# A primeiro padrao a se perceber eh que as diferentes informacoes do anuncio estao separadas por uma quebra de linha
# \n. Vamos usar isso para separar as informacoes em colunas.

dados <- unlist(strsplit(annun[,1], split='\n'))
# Verificando uma linha qualquer
annun[9,]
# Perceba que cada linha possui uma caracteristica unica que pode ser utilizada para 
# Separar os dados.
# Por exemplo, o endereco sempre possui pelo menos um h?fen '-' e nenhuma outra linha possui hifen.
# Logo:

# O dataframe original possui 981 linhas, e o vetor endereco tamb?m. Isto mostra que apenas e todos
# Os enderecos possuem h?fen.
# Dessa forma, podemos generalizar:

# Vamos definir se o imovel eh casa ou apartamento
annun$Tipo <- unlist(grep(pattern='apartamento|casa|sobrado', x=dados, value=T))
# Alguns dos anuncios n?o especifica, ent?o vamos remov?-los da seguinte maneira:
annun <- data.frame(Anuncio=annun[grepl(pattern='apartamento|casa|sobrado', x=annun$Anuncio),])
# Atualizamos o vetor 'dados
dados <- unlist(strsplit(annun[,1], split='\n'))
# Entao
annun$Tipo <- unlist(grep(pattern='apartamento|casa|sobrado', x=dados, value=T))
# Agora ? necess?rio limpar o 'tipo'
annun$Tipo <- unlist(str_extract_all(pattern='apartamento|casa|sobrado',string=annun$Anuncio))
annun$Tipo <- gsub(pattern='apartamento', replacement='AP', x=annun$Tipo)
annun$Tipo <- gsub(pattern='casa|sobrado', replacement='CS', x=annun$Tipo)
dados <- unlist(strsplit(annun[,1], split='\n'))
View(annun)
str(annun)


# --- Preco ----
annun$Preco <- grep(pattern='/m?s', x=dados, value=T)
# Este m?todo nos retorna um erro, pois nem todos os pre?os possuem a palavra 'm?s', observe o seguinte
annun[9,]
# Existem alguns apartamentos que est?o sendo alugados por dia! Vamos excluir estes dos nossos dados,
# Pois estamos interesados em alugel mensal. Ent?o vamos usar o pattern='\m?s', para pegar todos os precos
annun <- data.frame(Anuncio=annun[grepl(pattern='/m?s', x=annun$Anuncio),])
annun$Preco <- grep(pattern='/m?s', x=dados, value=T)
dados <- unlist(strsplit(annun[,1], split='\n'))

# Deixando apenas os numeros
annun$Preco <- parse_number(x=annun$Preco, locale=locale(decimal_mark=',', grouping_mark='.'))
names(annun) <- c("Anuncio","Tipo","Preco")

# SO FAR SO GOOD!
# --- Area

# A Area sempre est? escrita na forma: (numero ^2), ent?o podemos extrair da seguinte forma
annun$Area <- grep(pattern='[0-9]+\\sm', x=dados, value=T)
# Apenas um apartamento da lista n?o possui informa??o sobre a area. Vamos remover esta linha
# annun <- annun[grepl("[0-9]+\\sm", annun$Anuncio),]

# Removendo o 'm^2'
annun$Area <- parse_number(x=annun$Area, locale=locale(decimal_mark=',', grouping_mark='.'))

# Agora ? necessario atualizar a vari?vel dados, j? que removemos linhas
dados <- unlist(strsplit(annun[,1], split='\n'))
str(annun)

# --- Quartos
annun$Quartos <- grep(pattern='quartos?', x=dados, value=T)

# Aqui precisamos remover a palavra 'quartos?' e deixar apenas o valor numerico
annun$Quartos <- parse_number(annun$Quartos)


# ---- Banheiros
annun$Banheiros <- grep(pattern='banheiros?', x=dados, value=T)
# 5 apartamentos n?o possuem informa??o sobre o banheiro, vamos remov?-la.
annun<- annun[grepl('banheiros?', annun$Anuncio),]
annun$Banheiros <- grep(pattern='banheiros?', x=dados, value=T)
annun$Banheiros <- parse_number(annun$Banheiros)
dados <- unlist(strsplit(annun[,1], split='\n'))
str(annun)

# ---- Endereco
annun$Endereco <- grep(pattern='-', x=dados, value=T)
str(annun)

# Agora vamos separar em Estado.

annun$Estado <- unlist(str_extract_all(pattern='-\\s[A-Z]{2}$', string=annun$Endereco))
annun <- annun[grepl(pattern='-\\s[A-Z]{2}$', x=annun$Endereco),]
annun$Estado <- unlist(str_extract_all(pattern='-\\s[A-Z]{2}$', string=annun$Endereco))
# Limpando
annun$Estado <- unlist(str_extract_all(pattern='[A-Z]{2}', string=annun$Estado))

# Vamos adicionar a palavra 'Brasil' no Endere?o, para aumentar a precis?o (O porque disso
# aparecer? mais tarde.)
annun$Endereco <- paste(annun$Endereco,'Brazil', sep=', ')
str(annun)

# Cidade
annun$Cidade <- unlist(str_extract_all(pattern=',\\s\\D+\\s-$', string=annun$Endereco))
annun$Cidade <- unlist(str_extract_all(pattern='([[:alpha:]]*\\s)*', string=annun$Endereco))

annun <- annun[grepl(pattern=',\\s\\D+\\s-', x=annun$Endereco),]
unique(annun$Cidade)

# Removendo as linhas repetidas:
annun <- annun[!duplicated(annun),]

# Salvando os dados
rents <- select(annun, -Anuncio)
View(rents)
write.csv(rents, file4, row.names=FALSE)

# ------------- An?lise Explorat?ria de Dados --------------

options(scipen=999) # Desliga a nota??o cient?fica
# Agora que os dados est?o limpos podemos extrair valor deles!
max(rents$Preco)
# O maior valo de aluguel ? de 425000! ? mais dinheiro do que vou ganhar minha vida inteira
rents[rents$Preco>100000,]
rents[rents$Preco>50000,]

# Vamos fazer um histogama dos precos
histo1 <- ggplot(data=rents, aes(x=Preco)) +
  geom_histogram() +
  labs(title='Distribuição dos preços dos apartamentos de aluguel',
       xlab='Preco (R$)',
       y='Frequência') 
  
plot(histo1)

# Agora vamos refazer o histograma sem os outliers
histo2 <- ggplot(data=rents, aes(x=Preco)) +
  geom_histogram(color='black', fill='steelblue', bins =100) +
  labs(title='Distribui??o dos pre?os dos apartamentos de aluguel',
       xlab='Preco (R$)',
       y='Frequ?ncia') +
  xlim(c(0,20000)) +
  theme_bw()
  
plot(histo2)

# Ok, como era de se esperar, a grande maioria dos pre?oes ? menor do que 5 mil reais.
# Vamos dar uma olhada mais de perto

rm(histo3)
histo3 <- ggplot(data=rents, aes(x=Preco)) +
  geom_histogram(aes(col=Tipo),fill='steelblue', binwidth=500) +
  labs(title='Distribui??o dos pre?os de aluguel de apartamentos',
       xlab='Preco(R$)',
       ylab='Frequ?ncia') +
  xlim(c(0,5000)) +
  theme_bw() +
  scale_x_continuous(breaks=seq(0,5000,500), limits=c(0,5000))+
  scale_y_continuous(breaks=seq(0,2750,250), limits=c(0,2750))
plot(histo3)


# Vamos ver como os pre?os se comportam de acordo com cada estado.
by.state <- annun %>%
  group_by(Estado) %>%
  summarise(Media=mean(Preco)) %>%
  arrange(Media)
View(by.state)

count.by.state <- annun %>%
  group_by(Estado) %>%
  summarise(Quantidade=n()) %>%
  arrange(Quantidade)
View(count.by.state)



bar1 <- ggplot(data=by.state, aes(x=Estado, y=Media)) +
  geom_bar(stat='identity', fill='steelblue',position=position_dodge()) +
  labs(title='M?dia dos pre?os dos im?veis por estado',
       xlab='Estados',
       ylab='Preco\\(R$)')+
  scale_x_discrete(limits=by.state$Estado)+
  scale_y_continuous(breaks=seq(0,3500,500))

plot(bar1)

# Agora vamos ver como o pre?o do aluguel se comporta de acordo com as caracter?sticas do AP

scatter1 <- ggplot(data=rents, aes(x=Area, y=Preco))+
  geom_point(aes(col=Tipo))+
  geom_smooth(method='lm', color='black')+
  labs(title='Pre?o do aluguel em fun??o da ?rea do apartamento',
       ylab='Preco(R$)',
       xlab='Area(m^2)') +
  theme_bw()
plot(scatter1)

# Perceba que tamb?m h? outliers no valor da ?rea, vamos reduzir para 500m^2

scatter1 <- scatter1 + xlim(c(0,500)) + ylim(c(0, 10000))+
  scale_x_continuous(breaks=seq(0, 500, 50), limits=c(1, 500))+
  scale_y_continuous(breaks=seq(0, 10000, 1000), limits=c(0, 10000))
plot(scatter1)

# Parece que a rela??o entre pre?o e ?rea n?o ? t?o linear assim. Existe um fator que n?o est? nos nossos
# dados: a localiza??o!

# Agora vamos ver como se comparta a distribui??o do pre?o com a quantidade de quartos
scatter2 <- ggplot(data=rents, aes(x=Quartos, y=Preco))+
  geom_point(aes(col=Tipo))+
  geom_smooth(method='lm')+
  labs(title='Pre?o do aluguel em fun??o da quantidade de quartos',
       ylab='Preco(R$)',
       xlab='Quartos') +
  ylim(c(0,10000)) +
  theme_bw() +
  scale_x_continuous(breaks=seq(1, 6, 1), limits=c(1, 6))+
  scale_y_continuous(breaks=seq(0,10000,1000), limits=c(0,10000))

plot(scatter2)

# Ou seja, tamb?m parece que a rela??o quartos~pre?o n?o ? t?o simples assim. Existem apartamentos
# Pequenos e muito caros, assim como existem apartamentos grandes e mais baratos.

# Por fim, vamos analisar a quantidade de banheiros
scatter3 <- ggplot(data=rents, aes(x=Banheiros, y=Preco))+
  geom_point(aes(col=Tipo)) +
  geom_smooth(method='lm') +
  labs(title='Pre?o do aluguel em fun??o da quantidade de banheiros',
       ylab='Preco(R$)',
       xlab='Quartos') +
  ylim(c(0,10000)) +
  theme_bw() +
  scale_x_continuous(breaks=seq(1, 7, 1), limits=c(1, 7))+
  scale_y_continuous(breaks=seq(0,10000,1000), limits=c(0,10000))

plot(scatter3)

# Vamos ver no mapa como est? a distribui??o geogr?fica dos im?veis

points_sf <- st_as_sf(rents, coords=c('Longitude','Latitude'), crs=4326)
countries_sf <- ne_countries(scale='medium', returnclass='sf')
states_sf <- ne_states(country='brazil', returnclas='sf')

ggplot() +
  geom_sf(data=states_sf,
          fill=gray(0.8), color=gray(0.7)) +
  geom_sf(data=points_sf,
          aes(color=Tipo, size=Preco/100),
          alpha=0.7,
          show.legend='point') +
  coord_sf(xlim = c(-75,-30), ylim = c(-32, 5),
           datum = NA) + # removes graticules
  labs(title = "Distribui??o dos pre?os dos im?veis",
       size = "Preco",
       color = "Tipo") +
  guides(color = guide_legend(override.aes = list(size = 6))) +
  theme_void()




