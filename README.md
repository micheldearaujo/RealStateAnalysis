---
title: "Estudo do Mercado Imobiliário Brasileiro"
author: "Michel de Araújo"
date: "06/06/2021"
#output: html_document
output:
  rmdformats::downcute
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
setwd('D:/michel/Universidade/Mestrado/disciplina/repo/2_VA/ProjetoFinal')
```

## Introdução

Durante minha infância, por diversos motivos, minha família sempre precisou se mudar constantemente. Hoje, aos 23 anos, já tenho na bagagem mais de 12 mudanças. Era praticamente uma a cada dois anos. Esse estilo de vida pode ter seus pontos positivos, como estar sempre conhecendo novos lugares, novas pessoas, novas experências e sair da caixinha ao descobrir que o mundo é muito mais do que aparenta. No entanto, estar sempre mudando de lar trás consigo alguns perrengues indesejados, como estar sempre gastando dinheiro com frete dos móveis, lidar com móveis quebrados no transporte e encontrar boas escolas para as crianças (por experiência própria, esta parte é bem difícil!). Além disso tudo, existe a difícil tarefa de encontrar o imóvel perfeito, ou pelo menos próximo da perfeição, que ofereça tudo que se procura combinado com um preço justo - o famoso custo x benefício. Fatores como área, quantidade de quartos, banheiros, vagas de garagem e principalmente a localização intereferem bastante no preço de uma casas ou apartamento. Aos 22 anos decidi sair da casa da minha família e juntar os trapos com minha namorada, e foi aí que me deparei com todas as dificuldades e preocupações que uma criança não imagina. Foi baseado nisto que resolvi fazer este projeto, para conhecer um pouco mais do mercado imobiliário brasileiro, não só do ponto de vista econômico mas do ponto de vista prático. Vale a pena alugar uma casa com mais espaço ou um apartamento studio já é o suficiente? Devo gastar mais morando no centro perto do trabalho onde os preços são maiores e os apartamentos são pequenos ou gastar menos em uma casa mais distante? Esta é uma decisão muito importante a ser tomada, visto que exige de nós esforço físico e mental. Este projeto também serve para quem tem um imóvel e deseja alugar mas não faz a mínima ideia de como precificar sua casa ou apartamento. É possível criar um modelo de Machine Learning (ML) para, através dos dados coletados de imóveis do Brasil inteiro, determinar, através das características do seu imóvel, o seu preço de aluguel.

Este trabalho será construído da seguinte forma: Irei coletar informações sobre imóveis via *Webs craping* utilizando a linguagem de programação **Python**, juntamente com o biblioteca `Selenium`, que permite vasculhar páginas da web, interagir com elas e extrair informações.
De posse dos dados brutos, utilizarei para a linguagem **R** para realizar toda a análise dos dados: Limpeza, transformação, análise exploratória e construção do modelo de ML para prever o preço baseado em características da moradia.
Considerei que uma boa forma de realizar esta análise seria utilizando um site comum de aluguél e venda de imóveis, pois lá pessoas individuais e corretoras anunciam seus produtos. A linguagem Python será utilizada em dois momentos. Primeiro para extrair os dados da web e, posteriormente, para coletar informações geográficas dos endereços dos imóveis (latitude e longitude). Todo o resto da análise será feito utilizando a linguagem R. Provavelmente será necessário fazer um bom trabalho de extração de dados de *strings*, através de *expressões regulares* e *splits*.

Esta análise será muito útil para dois públicos diferentes. O primeiro são as pessoas em geral, que estão buscando um novo lugar para morar e querem tomar uma decisão assertiva baseada em dados reais. Através desta análise o usuário poderá decidir qual é o tipo de imóvel se adequa à suas necessidades, se é uma casa ou apartamento, se é perto ou longe do centro, grande ou pequeno, etc. O segundo publico é composto tanto por pessoas em geral, que possuem um imóvel e desejam alugá-lo mas não sabem o quanto ele vale, tanto para corretoras de imóveis, que tecnicamente fazem o mesmo, porém com imóveis de terceiros. Dessa forma, o cliente poderá saber com maior assertividade qual deve ser o valor do seu imóvel baseado em centenas de outros imóveis com características parecidas, do mesmo bairro e de bairros próximos.

## Pacotes necessários
Para o desenvolvimento deste projeto serão necessários os seguintes pacotes:
```{r libraries, message=FALSE, warning=FALSE}
# Conhecido como a gramática da manipulação de dados, o dplyr fornece diversas funções para manipulação de dataframes, como mutate(), select(), filter().
library(dplyr)

# O pacote readr fornece funções muito eficiente para leitura de dados retangulares, como dataframes, além de funções para extrair dados, como a parse_number.
library(readr)

# O pacote stringr, também pertencente ao tidyverse, possui funções para manipular strings, como str_replace, str_to_upper e expressões regulares.
library(stringr)

# O pacote tidyr tem como objetivo deixar os dados na forma tidy, ou seja, organizados. Possui funções como drop_na e fill, para lidar com dados nulos.
library(tidyr)

# O pacote DT faz uma ponte com o R e as bibliotecas de JavaScript para criação de tabelas interativas.
library(DT)

# O tidygeocoder fornece uma interface para realizar requisições de geoinformações, como obter CEPs, latitudes e longitudes.
library(tidygeocoder) 

# O ggplot2 é um sistema de criação de gráficos. É conhecido como a gramática dos gráficos pois a estrutura dos gráficos funciona de forma declarativa e bem estruturada.
library(ggplot2)

# O plotly é uma biblioteca grafica interativa com capacidade de gerar gráficos publicáveis online sem perder sua interatividade.
library(plotly)

# O pacote ggpubr é um pacote com o objetivo de criar visualizações elegantes de dados no R. Ele facilita a construção dos gráficos.
library(ggpubr)

# O pacote nortest fornece funções para testar normalidade de distribuições estatísticas.
require(nortest)

# O pacote RColorBrewer fornece várias paletas de cores extras para os gráficos.
library(RColorBrewer)

# O pacote rnaturalearth fornece conexão com dados de mapas da Natural Earth. Com ele, vamos baixar o mapa do Brasil dividido em estados
library(rnaturalearth)

# Este pacote serve para criar intervalos discretos de variáveis contínuas.
library(classInt) 

# O pacote sf fornece uma série de ferramentas para trabalhar com vetores geoespaciais, como pontos, linhas e poligonos. Vamos utilizá-lo para gerar geo-dataframes.
library(sf)

# O pacote geobr é parecido com o rnaturalearth. Contudo, ele possui mais informações específicas ao Brasil, como mapas municipais e sensos.
library(geobr)
```

## Capítulo I: Preparação dos dados
Todos os dados foram extraídos via *web scraping* de apenas um único website, o [Zap Imóveis](https://www.zapimoveis.com.br/). Nele, é possível fazer filtrar entre comprar, alugar, o tipo do imóvel e a localização. O site apresenta diversos tipos de imóveis, como apartamento, studio, kitnet casa, casa de condomínio, casa de vila, cobertura, flat, loft, terreno, e vários outros tipos de estabelecimentos comerciais. Neste projeto focaremos apenas em imóveis residenciais do tipo casa e apartamento.
O objetivo orignal dos dados é a comercialização de imóveis, pois é um site onde qualquer pessoa pode anunciar sua propriedade para venda ou aluguel.
Como o processo de Webscraping depende bastante da velocidade da internet, realizei a coleta de todos anúncios durante o período de Abril à Junho. Pode ter levado mais tempo do que o necessário, mas eu ainda estava aprendendo sobre o processo de automatização e com certeza cometi algumas redundâncias no algoritmo (por exemplo, tive que ir manualmente escolhendo as regiões geográficas porque só é possivel filtrar até 5 regiões e o site não disponibiliza todas as páginas sem filtragem.)
O processo de Webscraping retorna uma string contendo todas as informações do anúncio em questão, como pode ser visto na figura 1.

![Figura 1](D:/michel/Universidade/Mestrado/disciplina/repo/2_VA/ProjetoFinal/fig1.png)
Dessa forma, o dataset original possui apenas uma variável. No entanto, ela será expandida e novas variáveis serão criadas para que seja possível realizar a análise.

#### Importando e conhecendo o dataset
Vamos botar a mão na massa e começar o trabalho. O primeiro passo é importar os dados e conhecer a estrutura do mesmo.
```{r importando, message=FALSE, warning=FALSE}
# As strings possuem muitos acentos, então é recomendável escolher o encoding como 'latin' para que todos
# os caracteres sejam reconhecidos.
#anuncios <- read.csv('housing_prices.csv', encoding='latin')
anuncios <- read.csv('housing_prices.csv')
print('Estrutura do dataset: ')
str(anuncios)
print('Algumas linhas')
head(anuncios, 5)
tail(anuncios, 5)

```
Dessa forma, o que temos aqui é o texto bruto de `r nrow(anuncios)` anúncios de casas e apartamentos de todo o Brasil. O objetivo é separar cada linha em características do ímovel para poder realizar análises e tirar conclusões.

#### Limpeza e transformação

Primeiro é importante entender porque as etapas de limpeza e transformação estão juntas porque os dois processos acontecerão de forma simultânea, hora transformando, hora realizando excluindo algumas linhas.
O primeiro passo na limpeza e transformação é criar uma coluna que informa se o imóvel é do tipo casa (CS) ou apartamento (AP). Para isso utilizarei a função `str_extract_all`, do pacote *stringr*

#### Tipo
```{r casaouap, error=TRUE, message=FALSE, warning=FALSE}
anuncios$Tipo <- unlist(str_extract_all(pattern='apartamento|casa|sobrado',string=anuncios$Anuncio))
```
Foi retornado um erro, que diz que a quantidade de items a adicionar no dataframe é menor que a quantidade de linhas do dataframe. Isso ocorreu porque nem todos os anúncios especificaram se era casa ou apartamento ou sobrado(que é um tipo de casa). Como a quantidade de anúncios sem essa informação é relativamente baixa (`r nrow(anuncios) - (length(unlist(grep(pattern='casa|apartamento|sobrado', x=anuncios$Anuncio, value=TRUE))))` de `r nrow(anuncios)`)  anuncios, irei remover as linhas que não possuem essa informação e em seguida extrair a informação "Tipo".
```{r}
anuncios <- data.frame(Anuncio=anuncios[grepl(pattern='casa|apartamento|sobrado', x=anuncios$Anuncio), ])
anuncios$Tipo <- unlist(str_extract_all(pattern='apartamento|casa|sobrado',string=anuncios$Anuncio))
```
A função `grepl` retorna `TRUE` para uma determinada linha quando ela possui o padrão fornecido e `FALSE` caso contrário. Dessa forma, é possível fazer uma indexação lógica.

Agora, para simplificar, vamos trocar os nomes por siglas.
```{r}
anuncios$Tipo <- gsub(pattern='apartamento', replacement='AP', x=anuncios$Tipo)
anuncios$Tipo <- gsub(pattern='casa|sobrado', replacement='CS', x=anuncios$Tipo)
```

Já temos a primeira informação! Em seguida, vamos extrair o valor de aluguel do imóvel. Para facilitar o processo, vamos criar um vetor que contem todos os anúncios porém separados em suas quebras de linhas, da seguinte forma:
#### Preço

```{r}
dados <- unlist(strsplit(x=anuncios$Anuncio, split='\n'))
head(dados, 10)
```
Porque fazer isto? A função `grep` retorna apenas os elementos de um vetor que possuem o padrão estabelecido. Se eu utilizasse `grep` na própria coluna de anúncios, iria ser retornado a linha inteira, visto que é uma única string. Ao separar cada anúncio em diversos vetores, através da quebra de linha, obtenho diversas linhas, e apenas uma linha por anúncio contém o preço de aluguel. Dessa forma, ao utilizar o `grep` será retornado apenas as linhas com o preço. Como cada anúncio possui apenas uma informação de cada tipo, este processo não irá alterar nossos dados. Então, podemos extrair o valor do aluguel:

```{r error=TRUE}
anuncios$Preco <- grep(pattern='/mês', x=dados, value=TRUE)
```

Novamente um erro, informando que a nova coluna possui menos linhas do que o dataframe. Vamos ver o que está acontecendo.

```{r}
head(data.frame(anuncios[!grepl(pattern='/mês', x=anuncios$Anuncio), ]), 3)
```
Como pode ser visto, alguns anúncio informam o valor de aluguel utilizando a palavra "Aluguel" e outros anúncios estão anunciados "/dia". Como estamos interessados apenas em algueis mensais, vamos excluir os alugueis diários.

```{r}
anuncios <- data.frame(anuncios[grepl(pattern='Aluguel|/mês', x=anuncios$Anuncio), ])
```

Perceba que, após ter excluído algumas linhas do dataframe, é necessário redefinir o vetor `dados`, que contem todas as linhas dos anuncios.

```{r}
dados <- unlist(strsplit(x=anuncios$Anuncio, split='\n'))
```

Então podemos extrair os preços

```{r}
anuncios$Preco <- grep(pattern='/mês|Aluguel', x=dados, value=TRUE)
head(anuncios, 2)
```

Agora temos a coluna com os preços. Porém, ainda é necessário realizar a limpeza e manter apenas os valores. Para isso, utilizarei a função `parse_number`.

```{r message=FALSE, warning=FALSE}
anuncios$Preco <- parse_number(x=anuncios$Preco, locale=locale(decimal_mark=',', grouping_mark='.'))
head(anuncios, 2)
```

#### Área
Agora precisamos extrair a área de cada imóvel. Perceba que em todos os anúncios esta informação possui a seguinte estrutura: número + m^2. Então,

```{r}
anuncios$Area <- grep(pattern='[0-9]+\\sm', x=dados, value=TRUE)
head(anuncios,2)
```

Dessa vez não foi necessário remover nenhuma linha. Novamente, Limpo a coluna, deixando apenas os números:


```{r}
anuncios$Area <- parse_number(x=anuncios$Area, locale=locale(decimal_mark=',', grouping_mark='.'))
head(anuncios,2)
```
O dataframe está começando a ganhar forma. Mas ainda não tenho toda a informação necessária para realizar um estudo, então vamos em frente.

#### Quartos
A próxima informação que vou extrair é a quantidade de quartos. Está provavelmente não apresentará nenhuma dificuldade.

```{r error=TRUE}
anuncios$Quartos <- grep(pattern='quartos?', x=dados, value=TRUE)
```
Aparentemente estava errado. Irei remover os anúncios que não informaram a quantidade de quartos, extrair a informação e limpar a coluna, deixando apenas os valores.

```{r}
anuncios <- data.frame(anuncios[grepl(pattern='quartos?', x=anuncios$Anuncio), ]) # Filtrando dataframe
anuncios$Quartos <- grep(pattern='quartos?', x=dados, value=TRUE) # Extraindo a quantidade de quartos
anuncios$Quartos <- parse_number(x=anuncios$Quartos, locale=locale(decimal_mark=',', grouping_mark='.')) # Extraindo o valor numérico
head(anuncios)
```
Lembrando que, como excluí linhas do dataframe, é necessário atualizar o vetor dados.
```{r}
dados <- unlist(strsplit(x=anuncios$Anuncio, split='\n'))
```

#### Banheiros
Até aqui não nada de novo:
```{r}
anuncios <- data.frame(anuncios[grepl(pattern='banheiros?', x=anuncios$Anuncio), ])
anuncios$Banheiros <- grep(pattern='banheiros?', x=dados, value=T)
anuncios$Banheiros <- parse_number(x=anuncios$Banheiros, locale=locale(decimal_mark=',', grouping_mark='.'))
dados <- unlist(strsplit(x=anuncios$Anuncio, split='\n'))
```

#### Endereço
Por fim, chegou a hora de extrair o endereço do imóvel. Perceba que os endereços sempre possui um hífen no seu texto e nenhuma outra informação possui hífen, então usarei isto a meu favor. Como o endereço é uma característica muito importante do imóve, é pouco provável que esta informação esteja faltando em algum deles.

```{r}
anuncios$Endereco <- grep(pattern='-', x=dados, value=TRUE)
head(anuncios,2)
```

#### Estado
Mas o endereço completo de um anúncio pode ser específico de mais para analisar, então vamos extrair **estado** e **cidade** no qual o anúncio está localizado. Perceba que o final de cada string da coluna endereço termina com o estado, escrito através de sigla. Assim sendo, utilizarei o `pattern='[A-Z]{2}$`.

```{r}
anuncios$Estado <- unlist(str_extract_all(pattern='[A-Z]{2}$', string=anuncios$Endereco))
head(anuncios,3)
```
Perfeito!

#### Cidade
Esta parte é um pouco mais complicada. À princípio, eu poderia definir o padrão como `',\\s.*\\s-'`:
```{r error=TRUE}
anuncios$Cidade <- unlist(str_extract_all(pattern=',\\s.*\\s-', string=anuncios$Endereco))
```

Algumas linhas não possuem a informação sobre a cidade, irei excluí-las.
```{r}
anuncios <- data.frame(anuncios[grepl(pattern=',\\s.*\\s-', x=anuncios$Endereco), ])
dados <- unlist(strsplit(x=anuncios$Anuncio, split='\n'))
anuncios$Cidade <- unlist(str_extract_all(pattern=',\\s.*\\s-', string=anuncios$Endereco))
head(anuncios[3:8,])
```
Funciona para um parte dos anúncios, porém, em outros, este padrão se repete duas vezes, trazendo consigo o nome do bairro e o número do imóvel.
Para extrair apenas o nome da cidade, perceba que todas linhas que possuem o bairro estão acompanhadas com um número, então criarei uma função que se baseia nessa informação para extrair o nome da cidade. Ela verifica se determinada linha da coluna Anuncio possui caractere númerico, e, caso sim, divide a string em suas vírgulas e extrai o último elemento, caso contrário, ela retorna a própria string.

```{r}
cidadeExtract <- function(y){
  if (grepl(pattern='[[:digit:]]', x=y)==TRUE){
    cidade <- unlist(strsplit(x=y, split=','))[3]
    return (cidade)
  } else{
    return (y)
  }
}

anuncios$Cidade <- sapply(X=anuncios$Cidade, FUN=cidadeExtract)
head(anuncios[3:8,])
```

Agora é só limpar a coluna Cidade, substituindo a pontuação por um caractere vazio.

```{r}
anuncios$Cidade <- gsub(pattern='[[:punct:]]', replacement='', anuncios$Cidade)
head(anuncios[,2:7],3)
```
Perceba que existem espaços no início e no final do nome da cidade, para remover estes espaços, basta utilizar a função `str_trim`.

```{r}
anuncios$Cidade <- str_trim(string=anuncios$Cidade)
```


#### Geolocalização

O que farei agora é utilizar a função `geocode` da biblioteca *tidygeocoder* para extrair informações de latitude e longitude dos endereços dos imóveis. O objetivo disto é olhar para o dataset do ponto de vista geográfico e conhecer como os imóveis se comportam de acordo com as regiões do Brasil. Esta função recebe como entrada um dataframe e uma coluna especifica deste dataframe, que é onde estão os endereços. Antes disso, irei adicionar **Brazil** ao final de todos os endereços, para que não seja retornado nenhuma localização de outro país que também fale português.

```{r}
anuncios$Endereco <- paste(anuncios$Endereco, 'Brazil', sep=', ')
#anuncios <- geocode(.tbl=anuncios, address='Endereco')
```

Ufa! Chegamos ao final da extração das informações. Para finalizar esta etapa de limpeza, vamos excluir as linhas repetidas e selecionar o dataframe sem a coluna "Anuncio".

```{r}
anuncios <- anuncios[!duplicated(anuncios),] # Remove as duplicatas
anuncios <- select(anuncios, -Anuncio) # Exclui a coluna "Anuncio"
```
Por fim, vamos olhar novamente a estrutura do dataframe após o processo de limpeza e extração.

```{r}
head(anuncios, 10)
str(anuncios)
```

Como pode ser visto, o número de linhas foi reduzido bastante devido a quantidade de anúncios repetidos. Essa duplicação pode ter ocorrido tanto na hora de coletar os dados, em que o programa passou mais de uma vez pelo mesmo link, quanto por parte do website, que pode ter anúncios repetidos.
Exportarei o arquivo .csv para salvar o progresso.
```{r}
#write.csv(anuncios, 'imoveis.csv')

```
#### Resumo
Chegamos ao final da etapa de limpeza e transformação do dataset. No início era apenas uma coluna do tipo string, na qual cada linha era anúncio completo de um imóvel retirado do website Zap Imoveis. Ao final temos um dataframe com `r ncol(anuncios)` colunas, em que cada uma delas representa uma infomação diferente acerca do estabelecimento.
A tabela a seguir fornece uma descrição resumida do que cada coluna representa.

```{r}
variaveis <- c('Tipo', 'Preco', 'Area ', 'Quartos', 'Banheiros', 'Endereco', 'Estado', 'Cidade', 'long', 'lat')
descricao <- c('Tipo do imóvel (Casa ou Apartamento).',
               'Valor, em reais, do aluguel mensal do imóvel.',
               'Valor, em metros quadrados, da área do imóvel.',
               'Quantidade de quartos que o imóvel possui. Essa informação não inclui a sala.',
               'Quantidade de banheiros na residência.',
               'Endereço completo (ou quase) da residência.',
               'Estado brasileiro na qual o imóvel está localizado.',
               'Município brasileiro no qual a residência está localizada.',
               'Longitude (coordenada geográfica) do imóvel. Essa e a próxima informação serão úteis para plotar mapas.',
               'Latitude (coordenada geográfica) do imóvel.')
tipo <- c('Caractere', 'Numérico', 'Numérico', 'Numérico', 'Numérico', 'Caractere',
'Caractere', 'Caractere', 'Numérico', 'Numérico')
Resumo <- datatable(data.frame(Variáveis=variaveis, Tipo=tipo, Descrição=descricao))
Resumo
```
Na próxima seção darei início à análise exploratória dos dados.

## Capítulo II: Análise Exploratória de Dados

Bem vindo(a) à segunda parte deste projeto.
A EDA, do inglês "Exploratory Data Analysis", se refere à etapa de investigação inicial conduzida nos dados limpos na etapa de preparação. O objetivo aqui é, como já diz o nome, explorar. Conhecer os dados para saber não só o que aconteceu no passado mas também saber explicar o que e como determinado fenômeno aconteceu e o que fazer se ele acontecer de novo - ditas análises descritiva e diagnóstica, são duas as principais características de uma análise exploratória (AE). Neste momento não vamos nos preocupar em tentar utilizar os dados para prever o futuro. Queremos apenas *insights* sobre o que aconteceu e o que está acontecendo.
Contextualizado com o mundo dos negócios, a AE está diretamente relacionada com o Business Intelligence (Inteligência de Negócio). O BI está justamente preocupado em se basear em informações científicas (os dados) para poder tomar decisões de negócios. Empresas que seguem esta "filosofia" são conhecidas como Data Driven companies (empresas orientadas à dados).
Dessa forma, irei conduzir agora uma AE nos dados limpos e transformados na seção anterior.

#### Exploração inicial
Começo importando o arquivo `imoveis.csv` que salvei no final da seção passada.
```{r message=FALSE, warning=FALSE}
anuncios <- read.csv('imoveis.csv')
#anuncios <- read_csv('imoveis.csv', locale=locale(encoding='latin1'))
head(anuncios,5)
```

Como será a distribuição dos preços dos imóveis? Quanto você acha que é o preço máximo de um aluguel? Vamos descobrir isso utilizando um histograma simples.

```{r message=FALSE, warning=FALSE}
options(scipen=999) # Desativando a notação científica

# Criando um histograma com Plotly
hist1 <- plot_ly(data=anuncios,
                type='histogram',
                colors=c('#FF5653')) %>%  # Adicionando a cor vermelho para o preço
  add_histogram(x=~Preco, # Adicionando o traço
            name='Preço',
            color='#FF5653') %>%
            layout(title='Distribuição dos preços dos imóveis', # Configurando os eixos
                   xaxis=list(title='Preço'),
                   yaxis=list(title='Frequência')) %>%
  layout(width=800) # Configurando o tamanho da figura
hist1
```


Uau! Temos outliers chegando até R$ 1 milhão! Tudo bem se fosse o valor de venda, mas R\$ 1 milhão mensal? Parece um pouco absurdo!
Podemos investigar melhor a distribuição da variável e os outliers através de um gráfico `boxplot`.
O *boxplot* é um gráfico que traz informações importantes sobre a estatística da variável em questão, como mediana, primeiro e terceiro quartil, máximo, mínimo e outliers.


```{r message=FALSE, warning=FALSE}

# Criando a base para o boxplot
plot_ly(data=anuncios,
        type='box',
        colors=c('#FF5653')) %>%  
  add_boxplot(y=~Preco,   # Adicionando o traço do boxplot
            name='Preço',
            color='#FF5653') %>%
  layout(width=800,      
         yaxis=list(title='Valor'))

```

Certo, aparentemente existem muitos outliers neste conjunto de dados. Vamos verificar se o mesmo ocorre para a variável Área:


```{r message=FALSE, warning=FALSE}
plot_ly(data=anuncios,
        type='box',
        colors=c('#157B38')) %>% # Adicionando a cor verde para a área
  add_trace(y=~Area,
            name='Área (m²)',
            color='#157B38') %>%
  layout(width=800,               # Aumentando a largura do gráfico
         yaxis=list(title='Valor'))

```


Infelizmente o resultado é parecido. Temos um imóvel com mais de 450 mil metros quadrados (talvez seja uma fazenda ou algo do tipo).
Sendo assim, seguirei nossos boxplots e irei remover os outliers do nosso dataset. É possível extrair os outliers da função `boxplot` através da coluna `out` e então fazer uma indexação lógica para criar um novo dataframe sem eles.
```{r}
# Removendo outliers:
anunciosf <- anuncios[-which(anuncios$Preco %in% boxplot(anuncios$Preco, plot=F)$out),]
anunciosf <- anunciosf[-which(anunciosf$Area %in% boxplot(anunciosf$Area, plot=F)$out),]
```

Agora vamos refazer os gráficos e ver melhor como estão distribuidas as varíáveis Preco e Area.


```{r message=FALSE, warning=FALSE}
hist1 <- plot_ly(data=anunciosf,
                 type='histogram') %>%
  add_trace(name='Preço',
                 x=~Preco,
                 marker=list(color='#FF5653',
                             line=list(color="rgb(20,20,20)",
                             width=0.5)),
                xbins=10) %>%
  layout(title='Distribuição dos preços dos imóveis (sem outliers)',
  xaxis=list(title='Preço'),
  yaxis=list(title='Frequência'),
  width=700)

hist1
```

Temos um resultando interesante aqui. Perceba que os "picos" de preço estão sempre em volta de valores arrendondados: 1000, 1200, 1500, 2000, 2500, 3000, 3500... 5000. É muito pouco provável que a precificação do imóvel resulte em valores redondos como estes. O que temos aqui é a simples preferência das pessoas que utilizar números fáceis e "bonitos" para simplificar! E, quanto aos boxplots:

```{r}
# Criando o boxplot do preço com os dados fitlrados
bx1 <- plot_ly(data=anunciosf,
        type='box',
        colors=c('#FF5653')) %>%
  add_boxplot(y=~Preco,
            name='Preço',
            color='#FF5653') %>%
  layout(title='Distribuição dos preços e áreas dos imóveis (sem outliers)',
         width=700)

# Criando o boxplot da área com os dados fitlrados
bx2 <- plot_ly(data=anunciosf,
        type='box',
        colors=c('#157B38')) %>%
  add_boxplot(y=~Area,
            name='Área',
        color='#157B38') %>%
  layout(width=700)

fig1 <- subplot(bx1, bx2, nrows=1)
fig1
```

Muito melhor!
Agora os dados possuem uma distribuição mais amigável. Perceba que, ao filtrar o dataset, novos outliers surgem. Mas não vamos nos preocupar tanto com eles assim. Visto que a quantidade de valores maiores do que 4000 é alta, aparentemente existem vários imóveis listados com estes preços e, por isso, não devesem ser considerados outliers. Voltemos nossa atenção para a relação entre Preco e Area. É de se esperar que, quanto maior um imóvel, mais caro ele seja. Vamos também verificar como essa relação varia de acordo com o tipo do imóvel.
Para isso, farei um scatterplot com cores diferentes para casas e apartamentos.


```{r message=FALSE, warning=FALSE}
# Criando um gráfico de dispersão (scatter plot) para a variável Preço
# O gráfico divide Apartamentos e Casas em diferentes cores
plot_ly(data=anunciosf,
        type='scatter',
        x=~Area,
        y=~Preco,
        color=~Tipo,
        symbol = ~Tipo,
        symbols = c('circle', 'x'),
        colors=c('#FF5653', '#5A41F3'),
        marker = list(size=7),
        mode='markers') %>%
  layout(title='Relação entre Área e Preço do imóvel',
         xaxis=list(title='Área (m²)'),
         yaxis=list(title='Preço'),
         width=800)
```


A relação entre preço e área não é tão linear quanto pensávamos. Perceba também que existem conjuntos de pontos alinhados verticalmente no mesmo valor. Você consegue explicar porquê? Voltarei a isso mais tarde.
Visualmente também é possível perceber que os preços dos apartamentos atingem valores maiores que os de casas quando a área é menor que 100 metros quadrados. Além disso, após este valor começa-se a perceber uma diminuição na quantidade de apartamentos. Isso deve acontece pela limitação espacial que os apartamentos estão submetidos.
Não obstante, fixando a área em 200 metros quadrados, o preço varia desde `r min(anunciosf[anunciosf$Tipo=='CS',]$Preco)` e `r max(anunciosf[anunciosf$Tipo=='CS',]$Preco)`. O que será que pode causar essa variação de preço para imóveis do mesmo tamanho? Foi pensando nisto que coletei as informações geográficas das residências! A localização é um fator decisivo na precificação de um imóvel.

Agora falta verificar como o preço de aluguel varia de acordo com as outras variáveis numéricas quantidade de quartos e banheiros. No gráfico a seguir pode ser visto, à esquerda, a relação entre preço e quartos e, à direita, a relação entre preço e banheiros.


```{r message=FALSE, warning=FALSE}
# Criando o primeiro scatter plot, para Preço vs Quartos
sct1 <- plot_ly(data=anunciosf,
        type='scatter',
        x=~Quartos,
        y=~Preco,
        color=~Tipo,
        symbol=~Tipo,
        symbols=c('circle', 'x'),
        colors=c('#FF5653', '#5A41F3'),
        marker = list(size=7),
        mode='markers') %>%
  layout(xaxis=list(title='Quartos', autotick=FALSE, tick0=0, dtick=1),
         yaxis=list(title='Preço'),
         width = 700)

# Criando o segundo scatter plot, para Preço vs Banheiros
sct2 <- plot_ly(data=anunciosf,
        type='scatter',
        x=~Banheiros,
        y=~Preco,
        color=~Tipo,
        symbol=~Tipo,
        symbols=c('circle', 'x'),
        colors=c('#FF5653', '#5A41F3'),
        marker = list(size=7),
        mode='markers',
        showlegend=F) %>%
  layout(xaxis=list(title='Banheiros', autotick=FALSE, tick0=0, dtick=1),
         yaxis=list(title='Preço'),
         width = 700)

fig2 <- subplot(sct1, sct2, titleX = T, titleY=T, shareY=T, which_layout = c(1,2))
fig2
```


Voltando à minha indagação sobre as linhas verticais, elas representam justamente os imóveis com valores iguais de quartos e banheiros.
Parece que essas duas variáveis pouco nos dizem algo sobre o preços. Podemos verificar isso fazendo um teste de correlação.
```{r message=FALSE, warning=FALSE}
# Primeiro vamos remover os dados faltantes do dataset
anunciosf <- anunciosf[!is.na(anunciosf$Preco),]

# Teste de correlação Preco vs Quartos
pversusq <- cor(x=anunciosf$Preco, y=anunciosf$Quartos)
paste("O coeficiente de correlação Pearson entre Preco e Quartos é", pversusq)

# Teste de correlação Preco vs Banheiros
pversusb <- cor(x=anunciosf$Preco, y=anunciosf$Banheiros)
paste("O coeficiente de correlação Pearson entre Preco e Banheiros é", pversusb)
```

Ambos os coeficientes são relativamente baixos, como era de se esperar pela análise do gráfico.
Podemos concluir até agora que a precificação de residências vai além do tamanho total e da quantidade de cômodos. Algumas outras características, que não estão no nosso dataset, podem contribuir para a valorização (ou desvalorização) de um imóvel, como fundação (se a casa foi projetada para receber 1 ou mais pisos superiores), tipo de piso (cimento, porcelanato ou piso vinílico), cobertura (laje ou telhado), ano de construção, entre outras.

#### Existe diferença relevante entre os preços de casas e apartamentos?
Vamos voltar nossa atenção agora para a polêmica que diz que apartamentos são mais baratos do que casas, quando comparados imóveis as mesmas dimensões. Será que isso é verdade mesmo? Como já visto, a definição do preço pode ser bem complexa, então, para fins de estudo, vamos realizar um teste de hipótese para descobrir se a diferença entre as médias de preço de casas e apartamentos é estatisticamente relevante. É possível ter uma noção das diferenças das medianas dos preços visualizando os *boxplots* da variável em função da variável Tipo:


```{r message=FALSE, warning=FALSE}
# Criando um boxplot em que os dois tipos de imóveis são separados por cores.
bx3 <- plot_ly(data=anunciosf,
               color=~Tipo, # Separação
               colors=c('#FF5653', '#5A41F3')) %>%
  add_boxplot(y=~Preco) %>%
  layout(title='Distribuição dos preços dos imóveis',
         width=700)
bx3
```


As duas classes possuem valores muito parecidos! Poderíamos concluir aqui a verificação das hipóteses, mas no primeiro gráfico de dispersão observamos que, para um número de quartos pequenos, os valores dos apartamentos são maiores que os das casas. Dessa forma, vamos refazer estes *boxplots* considerando o número de quartos igual a 1.


```{r message=FALSE, warning=FALSE}
# Recriando os boxplots apenas para imóveis com 1 quarto, através da função Filter
bx3 <- plot_ly(data=filter(anunciosf, Quartos==1),
               color=~Tipo,
               colors=c('#FF5653', '#5A41F3')) %>%
  add_boxplot(y=~Preco) %>%
  layout(title='Distribuição dos preços dos imóveis',
         width=700)
bx3
```


Agora é possível observar uma diferença significativa! Os preços das casas possuem uma variância bem menor do que os dos apartamentos. Sendo assim, vale a pena realizar um teste de hipóteses para verificar se, para 1 quarto, existe diferênca significativa entre estas médias.
Antes de partir para o teste de hipótese, é necessário conhecer o tipo de distribuição das variáveis. Para isso, consideremos os histogramas:


```{r message=FALSE, warning=FALSE}
# Verificando a normalidade pelo histograma
hist2 <- plot_ly(data=filter(anunciosf, Quartos==1)) %>%
  add_histogram(x=~Preco,
                color=~Tipo,
                nbinsx=20, # Definindo a quantidade de barras, 5000/250 = 20
                colors=c('#FF5653', '#5A41F3')) %>%
  layout(xaxis=list(title='Preço', tick0=0, autotick=F, dtick=500), # Especificando a formatação do eixo x.
         yaxis=list(title='Frequência'),
         title='Verificando a Normalidade do Preço',
         width=700)
hist2
# Verificando a normalidade pelo QQ-plot
qqnorm(anunciosf$Preco, main='QQ plot da variável Preço', pch=18)
qqline(anunciosf$Preco)
```


Podemos concluir que o preço não segue uma distribuição Normal. Está até mais parecida com uma distribuição de Poisson. Para finalizar a verificação utilizamos o **teste de Anderson-Darling**, tendo em vista que o tamanho da amostra é maior do que 51.
Segue as hipóteses:

$$ H_{0}: \text{A variável Preco segue distribuição Normal}\\
H_{1}: \text{A variável Preco não segue distribuição Normal} $$

```{r message=FALSE, warning=FALSE}
# Realizando o teste de Anderson-Darling para Casas
ad.test(filter(anunciosf, Quartos==1, Tipo=='CS')$Preco)

# Realizando o teste de Anderson-Darling para Apartamentos
ad.test(filter(anunciosf, Quartos==1, Tipo=='AP')$Preco)
```
Com o valor de p igual a`r  ad.test(filter(anunciosf, Quartos==1, Tipo=='CS')$Preco)$p.value`, independentemente do nível de confiança, a distribuição definitivamente não é Normal!
Pensando nisto, precisamos ou realizar testes não parámetricos ou transformar a distribuição original em uma distribuição Normal utilizando o Teorema Central do Limite (TCL). Vamos utilizar o teste não paramétrico U de Mann-Whitey, já que as amostras não são relacionadas.

Dessa forma, as hipóteses são:
$$ H_{0}: \text{Não existe diferença entre os preços de casas e apartamentos}\\
H_{1}: \text{Existe diferença entre os preços de casas e apartamentos} $$



```{r}
# Realizando o tesde U de Mann-Whitney para variável Preco em função da variável categórica Tipo,
# Com nível de confiança de 95%.
wilcox.test(Preco ~ Tipo,
            data=filter(anunciosf, Quartos==1),
            conf.level=0.95)
```
O valor de p é menor do que 0.05, para 95% de confiança. Dessa forma, podemos rejeitar a hipótese nula, ou seja, a mediana dos preços de casas são estatisticamente diferentes da de apartamentos que possuem 1 quarto! Este resultado é bem interessante (e controverso). De acordo com os ultimos *boxplots*, para imóveis com 1 quarto, os apartamentos são estatisticamente mais caros do que as casas. Eu estava esperando encontrar um resultado contrário a este!


Para finalizar esta primeira parte da EDA, vamos resumir algumas as informações vistas até aqui

```{r}
# Agrupando os dados de acordo com a variável categórica Tipo e criando algumas estatísticas
resumo <- anunciosf %>%
  group_by(Tipo) %>%
  summarise(Quantidade = n(), # A função summarise é muito útil para criar estatísticas
          Media.Precos = round(mean(Preco, na.rm=T),2),
          DesvioPadrao.Precos = round(sd(Preco, na.rm=T),2),
          Media.Quartos = round(mean(Quartos),2),
          Media.Banheiros = round(mean(Banheiros),2))

datatable(data=resumo, caption = 'Resumo das variáveis numéricas')
```

Além das descobertas numéricas, temos que

1. Quantidade de quartos e banheiros, sozinhas, pouco dizem sobre a precificação de um imóvel. Ou pelo menos não possuem uma relação linear com o preço. Como pôde ser visto nos gráficos de dispersões, existem imóveis baratos com muitos quartos e banheiros, assim como existem imóveis caros com poucos cômodos.
2. Os valores dos aluguéis mensais de casas e apartamentos são estatisticamente diferentes, quando comparados imóveis com 1 quarto. Este resultado foi controverso, pois, pelo menos para mim, era esperado que casas fossem mais caras do que apartamentos.
3. A precificação de um imóvel vai além do seu tamanho e localização. Para obter um preço mais assertivo de uma residência, é necessário saber muitas outras informações relacionadas a sua estrutura e materiais de acabamento.
4. Brasileiros gostam de números arredondados. Vimos que a maioria dos preços são números inteiros e fáceis de trabalhar analiticamente. Afinal de contas, quem gosta de decimais?


## Capítulo III: Análise Geoespacial
#### Nesta seção vamos explorar como a localização afeta a precificação de um imóvel.
Na busca do lar perfeito a localização desempenha um papel crucial no processo de escolha da nova moradia. Questões como proximidade à padarias, escolas, universidade, centros de lazer, praia, integrações de transporte público, entre outros, surgem e devemos tomar a decisão baseados tanto em nossas necessidades quanto em nossos gostos. Existem aqueles que gostam da agitação do centro urbano e de morar próximo ao trabalho. Já outras pessoas preferem morar mais afastadas, isoladas do barulho e correria do dia a dia. Cada uma dessas alternativas leva à escolhas diferentes. Escolhas estas que trazem consigo seus pontos negativos, como despesas com transporte, para quem mora longe do trabalho, e alto custo de aluguel, para quem mora no centro.
Nesta seção vamos explorar como o preço do imóvel varia ao longo do território brasileiro. Descobrir as médias, máximos e mínimos de cada Estado e como esses valores se relacionam com a renda média estadual.


Para começar, vamos olhar novamente a aparêcia do nossso dataframe em relação às colunas com informações geográficas.

```{r}
head(select(anunciosf, -c(1:5)), 5)
```
Existem várias linhas, nas colunas *lat* e *long*, com valores `r NA` (sem valores). Isso aconteceu pois nem todos os endereços foram encontrados pela função `geocode`. Vamos remover estas linhas e trabalhar apenas com as que possuem as coordenadas completas.

```{r}
anunciosf <- data.frame(drop_na(anunciosf))
```

Infelizmente perdemos uma boa quantidade de observações, mas seguimos em frente.
Para começar, vamos descobrir a média de preços e quantas observações foram registradas por Estado. Podemos observar isso através de uma tabela:


```{r message=FALSE, warning=FALSE}
# Agrupando os dados em relação ao estado para contar as ocorrências e calcular a média de preço por estado
by_state1 <- anunciosf %>%
  group_by(Estado) %>%
  summarise(Quantidade=n(), 
            MediaPreco = round(mean(Preco),2)) %>%
  arrange(desc(Quantidade, MediaPreco))             # Ordendando a tabela por quantidade de observações

# Criando a tabela com a função datatable, do pacote DT
datatable(data=by_state1, caption='Número de observações e média por estado')
```


É notável que São Paulo ocupa bastante espaço nos sites de imóveis. Existe um grande desbalanço na proporção geográfica deste dataset. Isso pode (e deve) ser corrigido com uma nova coleta focada nos estados com menor quantidade de observações.
Além da tabela, com um gráfico de barras podemos visualizar melhor as médias por estado e tipo de imóvel:


```{r message=FALSE, warning=FALSE}
# Realizando o agrupamento por estado e por tipo de imóvel
by_state2 <- anunciosf %>%
  group_by(Estado, Tipo) %>%
  summarise(Quantidade=n(),               # Contando as ocorrências 
            MediaPreco = mean(Preco)) %>% # Calculando as médias
  arrange(MediaPreco)

bar1 <- plot_ly(data=by_state2,
                type='bar',
                color=~Tipo,
                colors= c('#FF5653', '#5A41F3')) %>%
  add_trace(x=~Estado, y=~MediaPreco) %>%
  layout(width=800)

bar1
```


Também é possível notar que alguns estados não possuem nenhuma observação relativa à casas, e que não há nenhuma observação referentes aos estados do Acre, Amapá, Rondônia, Roraima e Amazonas.

Para finalizar esta parte, podemos olhar a relação entre a média dos valores de aluguéis com as médias saláriais por Estado. Essa informação é relevante pois alguém pode se sentir empolgado para se mudar para São Paulo porque os salários são maiores, mas quando chegam lá se deparando com aluguéis e custo de vida mais altos. Não consegui encontrar essa informação no site do IBGE (Instituo Brasileiro de Geografia e Estatística), que é o orgão oficial resposável por coleta de dados, então irei usar uma pesquisa realizada pela empresa [Catho](https://www.mundorh.com.br/a-atual-media-salarial-do-brasileiro/), que atua no ramo de pesquisas estatísticas.

```{r message=FALSE, warning=TRUE}
# Lendo o arquivo que contém a média salarial por estado
salarios <- read.csv('salarios.csv', sep=';')
names(salarios)[2] <- 'MediaSalario'
head(salarios, 5)
```

Agora precisamos unir os dois dataframes. Podemos fazer isso utilizando a função `merge`.

```{r}
anunciosg <- merge(x=by_state1,
                   y=salarios,
                   by='Estado')
```

Perceba que a coluna Salario é do tipo caractere. Vamos transformá em numérica.
```{r}
anunciosg$MediaSalario <- parse_number(anunciosg$MediaSalario, locale=locale(decimal_mark=',', grouping_mark = '.'))
```

E, por fim, criamos um vetor que contém a proporção entre o salário médio e o custo médio de aluguel por estado.
```{r message=FALSE, warning=FALSE}
# Ordendando o dataframe de acordo com a média dos preços
anunciosg <- arrange(anunciosg, MediaPreco)

# Calculando a proporção entre salário e aluguel medios
anunciosg <- mutate(anunciosg, Proporcao = anunciosg$MediaPreco/anunciosg$MediaSalario)

# Criando um barplot na horizontal que mostra tanto o valor médio do aluguel
# quanto a porcentagem que um aluguel médio custa do salário médio, para cada estado

bar2 <- plot_ly(data=anunciosg, type='bar',
                orientation='h',
                color=~Proporcao, # As cores mostram o valor da proporção, que vai de 0 a 1.
                colors=) %>%
  add_trace(y=~Estado,
            x=~MediaPreco,
            name='Preco') %>%
  add_annotations(xref=anunciosg$MediaPreco, yref=anunciosg$Estado,    # Adicionando as porcentagens ao gráfico
                  y=anunciosg$Estado, x=anunciosg$MediaPreco*1 + 200,
                  text=paste(round(anunciosg$Proporcao, 2)*100, '%'),
                  showarrow=F) %>%
  layout(title='Proporção entre valor médio de aluguel e salário médio',
  xaxis=list(title='Preço Aluguel'),
  yaxis=list(title='Estado'),
  width=700)
bar2
```



Como pode ser visto, em algumas estados brasileiros o preço médio de um aluguel residencial é maior do que a própria média salárial:
```{r message=FALSE, warning=FALSE}
Acima_da_Media <- filter(anunciosg, Proporcao>1)[-2]
datatable(Acima_da_Media, caption='Estados com alto custo de vida')
```


#### Mapas

Uma terceira forma de analisar nossos dados, além dos gráficos e tabelas já explorados anteriormente, é através de mapas. Os mapas podem ser muito úteis neste momento pois trazem uma noção de espaço e distância que estão ausentes nos indicadores anteriores. Além disso, também é possível perceber melhor a distribuição das residências nas regiões de cada estado. Você acha que existem mais imóveis listados no site provindos das capitais ou do interior?
Para começar, precisamso utilizar um tipo especial de variável. Perceba que o nosso dataframe já possui duas colunas com as longitudes e latitudes das residências. No entando, para que possamos localizar esses pontos em um mapa é necessário transformá-los para o formado `sf` (single feature). Essa manipulação pode ser feita através da função `st_as_sf` do pacote `sf`, que transforma um dataframe que possui colunas com latitudes e longitudes em um dataframe do tipo `sf`, com uma coluna chamada `geometry`. Esta coluna possui dados no formato `point`, na forma `(long, lat)`.
Além disso, utilizaremos as função `ne_states`, da biblioteca `rnaturalearth` e `read_municipality`, da biblioteca `geobr`, para  baixar o mapa do Brasil, que também será armazenado em um objeto do tipo *sf*.

```{r message=FALSE, warning=FALSE}
# Vamos transformar o dataframe original em um dataframe do tipo sf, atraves da função st_as_sf.
pontos_sf <- st_as_sf(anunciosf, coords=c('long','lat'), crs=4326)

# Baixando o mapa do Brasil, separado por estados.
estados_sf <- ne_states(country='brazil', returnclas='sf')

# Plotando o mapa do Brasil
mapa <- ggplot() +
  geom_sf(data=estados_sf, # o ggplot possui uma funcao especial para plotar mapas, a geom_sf
          fill=gray(0.85), color=gray(0.7)) +
  labs(title='Mapa do Brasil')
mapa
```


Agora podemos adicionar pontos ao mapa através da nova coluna `geometry`, do dataframe `pontos_sf`, que foi adicionada ao dataframe.

```{r message=FALSE, warning=FALSE}
mapa <- ggplot() +
  geom_sf(data=estados_sf, # Mapa do Brasil
          fill=gray(0.85),
          color=gray(0.7)) +
  geom_sf(data=pontos_sf,  # um 'scatter plot' espacial das residências
          alpha=0.7,
          aes(color=Preco), # definindo a cor de acordo com o preço
          size=2.5,
          show.legend = 'point') +
  labs(title='Distribuição dos imóveis no Brasil')
mapa
```



E por fim, para complementar, podemos mostrar no mapa a média de preço de aluguel por estado:

```{r}
# Primeiro é necessário unir o dataframe estados_sf com o dataframe que possui as informações das médias dos preços
new.df <- merge(x=estados_sf, y=anunciosg,
                by.x ='postal', by.y='Estado', all=T)

# Criando intervalos discretos para a variável Preco
breaks_media <- classIntervals(c(min(anunciosg$MediaPreco) - 0.00001, anunciosg$MediaPreco), n=7, style='quantile')

# Transformando a variável preço em categorias
new.df <- mutate(new.df, MediaPrecoCat = cut(MediaPreco, breaks_media$brks))

# Renomeando os nomes das categorias
levels(new.df$MediaPrecoCat) <- c('(1000,1210]', '(1210,1390]', '(1390,1580]', '(1580,1780]',
                                  '(1780,2000]','(2000,2070]', '(2070,2850]')
mapa2 <- ggplot() +
  geom_sf(data=new.df,
          aes(fill=MediaPrecoCat)) + # Preenchendo os estados de acordo com a média do preço
          scale_fill_brewer(palette='OrRd') +
  labs(title='Média de preço por Estado',
       fill='Preço Médio')
mapa2
```

Podemos concluir com este mapa que os estados são bem variados em relação à sua média de preços. Não podemos generalizar as médias por regiões.
Perceba também que alguns estados estão transparentes. Isso aconteceu pois estes estados não possuem nenhum registro!
Outra informação que talvez seja até mais importante do que olhar apenas as médias é ver, para cada estado, a proporção que o aluguel ocupa no orçamento mensal das pessoas. Isto pode ser feito a partir de um mapa parecido com o anterior, so que mostrando a razão Aluguel/Salário:

```{r}
# Mapa da proporcao
# Criando intervalos para a proporção, que já foi calculada anteriormente.
breaks_prop <- classIntervals(c(min(anunciosg$Proporcao) - 0.00001, anunciosg$Proporcao), n=7, style='quantile')

# Transformando
new.df <- mutate(new.df, ProporcaoCat = cut(Proporcao, breaks_prop$brks))

# Criando o mapa
mapa3 <- ggplot() +
  geom_sf(data=new.df,
          aes(fill=ProporcaoCat)) +       # Preenchendo de acordo com o valor da proporção
          scale_fill_brewer(palette='OrRd') +
  labs(title='Proporção entre aluguel e salário',
       fill='Proporção')
mapa3
```

Temos um resultado interessante aqui. A maior parte da região Nordeste possui um valor muito alto (igual ou maior do que o próprio salário) de aluguel comprado com seu salário médio. O contrário ocorre para a região Sul/Sudeste, em que o aluguel ocupa entre `r breaks_prop[[1]][1]` e `r breaks_prop[[1]][13]` do orçamento mensal das pessoas. Dessa forma, temos indícios de que o custo de vida na região Sul e Sudeste é menor comparado com o da região Nordeste. 

#### Região Metropolitana do Recife
Vamos agora restringir nossa análise para a Região Metropolitana do Recife para descobrir como varia o preço de aluguel entre as cidades.

```{r message=FALSE, warning=FALSE}
# Lendo o mapa do Brasil separado em seus municípios
cities <- read_municipality(year=2018, showProgress=FALSE)

# Plotando
ggplot() +
  geom_sf(data=cities, fill=gray(0.85), color=gray(0.7))

# Vamos ver como é essa variavel sf
str(cities)

# Filtrando apenas as cidades que estão na RMR:
cidades_rmr <- c('Abreu E Lima', 'Araçoiaba', 'Cabo De Santo Agostinho', 'Camaragibe', 'Igarassu', 'Ilha De Itamaracá', 'Ipojuca', 'Itapissuma', 'Jaboatão Dos Guararapes', 'Moreno', 'Olinda', 'Paulista', 'Recife', 'São Lourenço Da Mata')

rmr_sf <- filter(cities, name_muni %in% cidades_rmr)
rmr_sf$geom

# Plotando o mapa da RMR com seus municípios
rmr_map <- ggplot() +
  geom_sf(data=rmr_sf, fill=gray(0.85), color=gray(0.7)) +
  labs(title='Região Metropolitana do Recife')
rmr_map

```

Temos 2 cidades chamadas Paulista. Precisamos remover a que possui Longitude menor do que 37° W. Ela é a primeira do dataframe, então
```{r}
rmr_sf <- rmr_sf[-1,]

# Plotando o mapa da RMR com seus municípios
rmr_map <- ggplot() +
  geom_sf(data=rmr_sf, fill=gray(0.85), color=gray(0.7)) +
  labs(title='Região Metropolitana do Recife')
rmr_map
```

Agora é necessário agrupar nosso dataframe em função das cidades, assim como já fizemos em função dos estados.
```{r}
# Redefinindo o vetor com as cidades. Perceba que existe uma diferença entre
# as letras minúsculas das preposições
cidades_rmr <- c('Abreu e Lima', 'Araçoiaba', 'Cabo de Santo Agostinho', 'Camaragibe', 'Igarassu', 'Ilha de Itamaracá', 'Ipojuca', 'Itapissuma', 'Jaboatão dos Guararapes', 'Moreno', 'Olinda', 'Paulista', 'Recife', 'São Lourenço da Mata')

rmr_df <- filter(anunciosf, Cidade %in% cidades_rmr)

# Agrupando os dados de acordo com a cidade
rmr_df_grouped <- rmr_df %>%
  group_by(Cidade) %>%
  summarise(Quantidade = n(),
            Media.Preco = round(mean(Preco),2)) %>%
  arrange(desc(Quantidade))

datatable(rmr_df_grouped)
```

Perceba que novamente existe um grande disparidade em relação à quantidade de observações dos dados quando se trata da RMR. Vamos dar uma olhada nos mapas de dispersão.

```{r}
sct_map2 <- ggplot() +
  geom_sf(data=rmr_sf, # Mapa da RMR
          fill=gray(0.85),
          color=gray(0.7)) +
  geom_sf(data=filter(pontos_sf, Cidade %in% cidades_rmr),  # filtrando os pontos que estão na RMR
          alpha=0.7,
          aes(color=Preco), # definindo a cor de acordo com o preço
          size=2.5,
          show.legend = 'point') +
  labs(title='Distribuição dos imóveis na RMR')
sct_map2
```

Como a distribuição dos imóveis está quase totalmente centralizada no município de Recife, não vale a pena estudar a distribuição das médias dos outros munípicios, pois não há  observações suficientes para obter uma média representativa.
Mas, por outro lado, podemos ver como como estão distribuídos os imóveis dentro do Recife.
Mas antes disso, vamos baixar o mapa do Brasil separado por bairro e depois filtrar para o município Recife.
```{r}
# Baixando o mapa do Brasil separando em bairros
nbhd <- read_neighborhood(showProgress=FALSE)

# Perceba que existe algumas diferenças em relação anterior, como os nomes dos bairros
# E seus códigos
str(nbhd)

# Plotando apenas o Recife
sct_map3 <- ggplot() +
  geom_sf(data=filter(nbhd, name_muni == 'Recife'), # Mapa do Recife
          fill=gray(0.85),
          color=gray(0.7)) +
  geom_sf(data=filter(pontos_sf, Cidade == 'Recife'), # Selecionando apenas o Recife
          alpha=0.5,
          aes(color=Preco), # definindo a cor de acordo com o preço
          size=2.5,
          show.legend = 'point') +
  labs(title='Distribuição dos imóveis no Recife')
sct_map3

```

Percebe-se que os imóveis estão distribuídos em dois grandes centros, o primeiro é na Zona Sul, caracterizada pelos bairros de Boa Viagem, Janga e Piedade. O segundo é na Zona Norte, porém com uma distribuição um pouco mais ampla.
Este tipo de análise pode ser feita em todas as regiões do Brasil, ou até do mundo, basta adequar os filtros de acordo com a região desejada. Mas, o que podemos perceber é que a grande maioria dos dados são referentes à capitais. Porque isto acontece?
A explicação para esse fenômeno pode está ligada à diversos fatores, como educação formal e acesso a internet. Como é de se esperar, quanto mais distante dos grandes centros econômicos menor é o investimento em infraestrutura e menor a acessibilidade à tecnologia.

## Capítulo IV: Conclusão

#### Nesta última seção farei uma revisão dos resultados obtidos neste projeto e alguns breves comentários

Chegamos ao final deste projeto de análise de dados. Espero que tenha gostado que acompanhar esta trajetória até aqui.
Durante este projeto exploramos a precificação de imóveis de aluguel residencial de acordo com algumas características sobre os mesmos, o seu tamanho, em metros quadrados, a quantidade de quartos, banheiros e sua localização. Foi utilizado um dataset coletado via *web scraping* do website [Zap Imóveis](https://www.zapimoveis.com.br/), no qual pessoas físicas e jurídicas podem anunciar imóveis para comércio. Este problema foi abordado de forma exploratória, ou seja, através de uma análise descritiva e diagnóstica dos dados, fomos capazes de responder as seguintes questões:

**1. Área, quantidade de quartos e banheiros são suficientes para precificar um imóvel?**

Quantidade de quartos e banheiros, sozinhas, pouco dizem sobre a precificação de um imóvel. Ou pelo menos não possuem uma relação linear com o preço. Como pôde ser visto nos gráficos de dispersão, existem imóveis baratos e caros com muitos quartos e banheiros, assim como existem imóveis baratos e caros com poucos cômodos.
A precificação de um imóvel vai além do seu tamanho e localização. Para obter um preço mais assertivo de uma residência é necessário saber muitas outras informações relacionadas a sua estrutura e materiais de acabamento.

**2. Existe diferença significativa nos preços de casas e apartamentos?**

Os valores dos aluguéis mensais de casas e apartamentos são estatisticamente diferentes, quando comparados imóveis com 1 quarto. Este resultado foi controverso, pois, pelo menos para mim, era esperado que casas fossem mais caras do que apartamentos.


**3. Como estão distribuidos os anúncios virtuais de aluguel no Brasil?**

Quanto fizemos um gráfico de dispersão no mapa do Brasil vimos que a maior parte das residências listadas no website Zap Imóveis estão concentradas nas capitais, com um peso maior para a região Sudeste. Dessa forma, é possível concluir que, nas demais regiões, os websites não são uma forma popular de se comercializar imóveis. Provavelmente os moradores dessa regiões estejam mais acostumados com os anúncios colados em pontos de ônibus.

**4. Como é a distribuição dos preços em por estados no Brasil?**

Como vimos nos mapas, não podemos generalizar as médias por regiões pois a relação não é exatamente clara. Em uma mesma região pode haver estados com altos preços médios quando baixos.

**5. Como é a distribuição do custo de vida no Brasil?**

Ao criarmos um mapa que mostra a razão entre o aluguel médio e o salário médio de cada estado descobrimos que existem estados com uma razão maior do que 1. Isso nos mostra que estes estados possuem um custo de vida alto, provavelmente por causa dos baixos valores de salário. Concomitantemente a isto, outros estados possuem uma razão mais amena, o que significa que o custo do aluguel ocupa pouco espaço no orçamento das famílias.
Dessa forma, podemos citar, por exemplo, estados com custo de vida "baixo": Rio Grande do Sul, Santa Catarina e Goias. E estados com alto custo de vida, como Pernambuco, Amapá e Tocantins.

Dessa forma, este modelo exploratório, juntamente com sua continuação, descrita na seção por vir, é de grande utilizadade para as pessoas que estão à procura de um novo lar para morar, para as pessoas que possuiem uma propriedade e desejam comercializá-la e para as corretoras de imóveis, que lidam todos os dias com comercialização de imóveis e desejam se tornar empresas guiadas por dados.
Utilizando o [*Microsoft Power BI*](https://powerbi.microsoft.com/en-us/) é possível resumir algumas as informações através de um Dashboard:
![Dasboard Estratégico](Dashboard_Final.png)

#### Este projeto não acaba por aqui
Apesar de ter obtido boas conclusões, este projeto ainda precisa de melhorias, tais como:

1. **Coletar mais dados.** Vimos que existem estados com pouquíssimos imóveis registrados e outros com nenhum imóvel listado no dataset. Este é um problema a ser resolvido, pois poucos imóveis irão definir uma média para o estado que provavelmente não representa a população. Um exemplo disso é o estado do Tocantins, que teve apenas 2 imóveis listados e ficou com uma média de R$ 2850, o que talvez não seja condizente com a realidade.

2. **Modelo preditivo.** Depois de melhorar a qualidade do dataset através de uma nova coleta, irei implementar um modelo de regressão para tentar prever o preço de um imóvel baseado nas características vistas nesse projeto. O objetivo disto é tentar descobrir se essas informações são suficientes para realizar uma precificação inicial de um imóvel, com objetivo de ajudar uma corretora de imóveis a definir seus preços.