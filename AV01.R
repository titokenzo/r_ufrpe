# UFRPE
# CAD - Computação para Análise de Dados 2020.3
# set/2020
# Autor: Tito Kenzo 
# AV 01

setwd("D:/home/GoogleDrive/UFRPE/CAD 2020.3/exercicios")
getwd()

install.packages(c('tidyr', 'dplyr', 'ggplot2', 'AER'))
library(tidyr)
library(dplyr)
library(ggplot2)
library(AER)


# Um alquimista tem um vetor de string com nomes de substâncias 
# (palavras mágicas) e um número após o nome que representa uma proporção em 
# percentagem (‘%’): strg <- c("Voda 30", "bylinky 25", "ZEM 23", "zlAto 22").
# Em apenas uma linha de comando, usando as funções tolower() e paste(), 
# escreva um código que deixe todas as palavras em letras minúsculas e com o 
# sinal ‘%’ após o número. 
# Note que não pode haver espaço entre o número e a unidade de medida. 
# Além disso, remova todos os espaços em branco do comando antes de submeter o 
# mesmo.
strg <- c("Voda 30", "bylinky 25", "ZEM 23", "zlAto 22")
paste(tolower(strg),"%",sep="")

# Qual(is) da(s) opçao(ões) abaixo correspondem a regexp "^([0-9][[:punct:]][a-z])"?
txt = c('5!eAZ','!6!rrrrDFGT','R99Tf34!RR','123?aSd')
grep(pattern = "^([0-9][[:punct:]][a-z])", txt,value = T)

# Carregue o arquivo de texto que contém um trecho do romance “Gambler” de 
# Fyodor Dostoyevsky. Após isso, submeta quantos caracteres tem nesse texto. 
# Mas note que é importante importar o arquivo de texto como uma string de 
# caractere única. Para isso use "read_file" do pacote "readr".
txt <- readr::read_file("entradas/gambler.txt")
nchar(txt)


# Submeta a regex que extrai todos os domínios que começam com http ou https 
# do vetor a seguir: c ("www.dogman.com", "http://rotterdam.com", "https://facebook.com", "httpx://sims.com") 
vetor <- c("www.dogman.com", "http://rotterdam.com", "https://facebook.com", "httpx://sims.com")
grep(pattern="^https?://",vetor,value=T)

# Escreva uma função chamada 'ranqueamento' que gera o ranqueamento do dataset 
# Forbes2000 de acordo com o valor de mercado (marketvalue). 
# Esse ranqueamento pode ser tanto por categoria quanto geral. 
# A função 'ranqueamento ' recebe três argumentos: 'ranque', 'categoria' e 'opcao'.
# Se a 'opcao' 1 for escolhida, o ranqueamento geral deverá ser feito. 
# Por outro lado, se a 'opcao' 2 for escolhida, o ranqueamento por categoria 
# deverá ser realizado. Dado esses argumentos, a função retorna o nome da 
# empresa, a categoria e o ranque. 
# O protótipo dessa função é: 
# ranqueamento  <- function( ranque,  categoria=NULL, opcao){}
# Adicionalmente, se o raking ou a categoria não existir uma mensagem de erro 
# deverá ser apresentada. De posse dessa função, responda as questões abaixo.
forbes <- read.csv("entradas/Forbes2000_V2.csv")
View(forbes)
str(forbes)
forbes$ranking <- rank(desc(forbes$marketvalue))
forbes = forbes[order(forbes$ranking,decreasing = F),]

ranqueamento <- function(ranque, categoria=NULL, opcao){
  if(opcao==1){
    #geral
    return(forbes[ranque,])
  }else if(opcao==2){
    saida <- (forbes %>% filter(category==categoria))
    return(saida[ranque,])
    #por categoria
  }else{
    return(print("Opção inválida!"))
  }
}

#TESTES
ranqueamento(ranque=1, opcao=1)

ranqueamento(ranque=1, categoria="Diversified financials", opcao=1)

ranqueamento(ranque=1, categoria="Software & services", opcao=2)

ranqueamento(ranque=1, categoria="Comida e Serviço", opcao=2)

# Qual o nome da empresa do seguinte código 
ranqueamento(ranque=1234, opcao=1)

# Qual a categoria da empresa do seguinte código ?
ranqueamento(ranque=198, opcao=1)

# Qual o nome da empresa do seguinte código ?
ranqueamento(ranque=45, categoria="Technology hardware & equipment", opcao=2)

# Qual o nome da empresa do seguinte código 
ranqueamento(ranque=7, categoria="Bancos", opcao=2)

# Verifique o dataset diamonds que pode ser encontrado no pacote ggplot2. 
# Você pode obter uma descrição dos dados digitando ?diamonds ou help(diamonds)
# no console. Note que para visualizar os dados você precisa instalar e carregar
# o pacote ggplot2. Também carregue o pacote dplyr. 
# Após isso, responda as perguntas abaixo.
View(diamonds)
help(diamonds)

# Qual o preço médio (price) dos diamantes cujo o corte (cut) é "Very Good" e 
# o quilate (carat) é maior que 0.7.
diamonds %>% 
  filter(cut=="Very Good" & carat>0.7) %>%
  summarise(média = mean(price))

# Qual a cor (color ) do diamante que possui o menor preço (price) com mais de 
# 0.5 quilates (carat) ?
diamonds %>% 
  filter(carat>0.5) %>%
  arrange(price)

# Qual a proporção de Diamantes cujo o corte (cut) é "Premium" ?
diamonds %>%
  group_by(cut) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n))

# Instale e carregue o pacote AER e execute o comando data("Fertility") que 
# carregará o dataset Fertility no seu RStudio. Esse dataset apresenta 
# informações do censo americano de 1980 de mulheres casadas entre 21 e 35 anos
# com dois ou mais filhos. Resolva as questões abaixo. No entanto, primeiro 
# verifique o dataset Fertility digitando "?Fertility" ou "help(Fertility)" no
# console.
data("Fertility")
help(Fertility)
str(Fertility)

# Digite o código que seleciona as linhas de 35 a 50 das variáveis idade (age) 
# e trabalho (work). Remova todos os espaços em branco, não altere o nome do 
# dataset e use o dplyr.
Fertility%>%select(age,work)%>%slice(35:50)

# Qual a proporção de mulheres passaram a ter um terceiro filho (morekids) com
# mais de 30 semanas trabalhadas (work)? 
Fertility %>% 
  filter(work>30) %>%
  group_by(morekids) %>%
  summarise(n = n()) %>%
  mutate(prop = n/sum(n))

F2 <- Fertility %>% filter(morekids=="yes" & work>30) %>% summarize(n = n())
F3 <- Fertility %>% summarize(n = n())

F2/F3

# Filtre o subconjunto de mulheres entre 22 e 24 anos e determine à proporção 
# que tiveram menino como primogênito (gender1). Note que 22 e 24 deve ser incluído. 
F1 <- Fertility %>% filter(age>=22 & age<=24)
F2 <- F1 %>% filter(gender1=="male") %>% summarize(n = n())
F3 <- F1 %>% summarize(n = n())

F2/F3

# Faça o download dos arquivos de dados Catfish.csv e Treatment.csv do Google
# classroom e importe-os para o R. Catfish.csv contém medições de peso 
# (variáveis de março e abril) de duas espécies de bagres que são comumente 
# usadas na aquicultura. Ambas as espécies foram criadas sob diferentes
# tratamentos de temperatura e alimentos, os quais são especificados em 
# Treatment.csv. Obtenha uma visão geral da estrutura dos dados usando as 
# funções str () e head (). Após isso, realize os seguintes tratamentos: 
# (a) converta o dataframe do arquivo Catfish.csv em um formato longo, 
# de modo que as medições de peso para março e abril sejam combinadas em uma 
# coluna denominada Weight. 
# (b) Una as colunas Genus e Species em uma variável chamada Species. 
# O resultado deve ficar assim: Silirus.glanis. 
# (c) faça a combinação dos dados de Treatment.csv e Catfish.csv e salve o 
# objeto resultante numa variável chamada de Catfish_Treatment. A mesclarem deve
# ser feita através da coluna Tank. Em posse do novo Catfish_Treatment, responda
# as perguntas abaixo.
catfish <- read.csv("entradas/Catfish.csv")
str(catfish)
treatment <- read.csv("entradas/Treatment.csv")
str(treatment)

catfish <- catfish %>% 
  gather(key="Month",value="Weight",March, April) %>%
  unite(col="Species", Genus, Species, sep=".")

Catfish_Treatment <- merge(x = catfish, 
                           y = treatment, 
                           by = "Tank",
                           all = T)
str(Catfish_Treatment)
View(Catfish_Treatment)

# Qual o peso médio dos bagres do sexo masculino considerando o mês de Abril e 
# o tratamento 2?
Catfish_Treatment %>%
  filter(Sex=="Male" & Month=="April" & Food=="Treatment2") %>%
  summarize(media = mean(Weight))

# Use a função "ifelse" para adicionar os elementos de uma nova coluna chamada 
# de "AcimaMedia" ao dataset Catfish_Treatment. Essa coluna, que deve ser 
# adicionada ao data frame, é uma coluna lógica, indicando "V" se o peso do 
# bagre é maior que a média dos pesos. Caos contrário, atribua "F". Note que 
# você deve tirar todos os espaços em branco e a coluna "AcimaMedia" deve ser 
# atribuída ao data frame através do operador "$". Além disso, as colunas do
# data frame devem ser acessadas pelo nome e usem aspas simples.
Catfish_Treatment$AcimaMedia<-ifelse(Catfish_Treatment[,'Weight']>mean(Catfish_Treatment[,'Weight']),"V","F")

# Com base na coluna que foi adicionada na questão anterior, responda: 
# Qual tank possui mais bagres com o peso acima da média?
Catfish_Treatment %>%
  filter(AcimaMedia=="V") %>%
  group_by(Tank) %>%
  summarize(n = n())

# Dada a tabela no link abaixo que representa uma competição entre Maria, 
# Teresa, Francisca e Joaquina, determine quem foi a vencedora. 
# https://docs.google.com/drawings/d/1-8biPvM3H8RaFFgLWttnnCuntSZ8lyKz8zvodemd5XY/edit?usp=sharing


# Escreva uma função chamada 'casosCompletos' que retorna a quantidade de casos 
# completos de um dado dataset .  A função ' casosCompletos' recebe dois 
# argumentos: 'df' e 'linhas'. Dado esses argumentos, a função retorna a 
# quantidade de casos completos. O protótipo dessa função é:  
#  casosCompletos<- function( df,  linhas=NA){}. Se nenhuma linha for definida, 
# então deve ser considerado todo o dataset. De posse dessa função, responda as 
# questões abaixo.
casosCompletos<- function( df,  linhas=NA){
  if (!is.na(linhas)){
    df <- df[linhas,]
  }
  teste1 <- df[rowSums(is.na(df))==0,]
  nrow(teste1) 
}

# Qual a quantidade de casos para o código a seguir? casosCompletos(df=airquality) 
casosCompletos(airquality)

# Qual a quantidade de casos completos para o código a seguir? Mas note que 
# "tb.csv" tem que está no diretório apontado pelo o RStudio. 
aux<-read.csv("entradas/tb.csv") 
casosCompletos(aux,1348:4954)

