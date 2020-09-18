# UFRPE
# CAD - Computação para Análise de Dados 2020.3
# set/2020
# Autor: Tito Kenzo 
# Exercício 10

setwd("D:/home/GoogleDrive/UFRPE/CAD 2020.3/exercicios")
getwd()

# Instale e carregue o pacote dplyr. Em seguida use o seguinte comando para 
# carregar os dados que irá trabalhar: df=data.frame(Theoph). Note que Wt: é o 
# peso do sujeito (kg); Dose: é a dose de teofilina administrada por via oral 
# ao indivíduo (mg / kg); Time: é o tempo desde a administração do medicamento 
# quando a amostra foi coletada (h); e conc: é concentração de teofilina na 
# amostra(mg / L). Responda as questões abaixo usando exclusivamente o pacote 
# dplyr. Note que ao submeter a resposta remova todos os espaços em branco. 
# Também use um dos padrões a seguir: filter(dataset,<<>>) ou 
# dataset%>%filter(<<>>).
install.packages("dplyr")
library(dplyr)
df = data.frame(Theoph)
View(df)
str(df)

# Qual o comando seleciona apenas a coluna Dose de df ?
df%>%select(Dose)
select(df,Dose)

# Qual o comando apresenta os dados para as doses maiores que 5 mg/kg ?
df%>%filter(Dose>5)

# Qual o comando seleciona as linhas de 10-20 ?Dica: use o "slice".
df%>%slice(10:20)

# Qual comando apresenta os dados para as doses maiores que 5 e cujo o tempo 
# desde a administração do medicamento (Time) é maior que a média do mesmo? 
# Use apenas um único comando.
df%>%filter(Dose>5 & Time>mean(df$Time))
filter(df,Dose>5,Time>mean(Time))

# Qual comando organizar df por peso (decrescente) ?
df%>%arrange(desc(Wt))
arrange(df,desc(Wt))

# Qual comando organizar df por peso (crescente) e tempo (decrescente) ?
arrange(df,Wt,desc(Time))

# Qual comando cria uma nova coluna chamada "tendencia" que é igual à 
# Time-mean(Time)?
mutate(df,tendencia=Time-mean(Time))

# Qual comando apresenta a maior concentração de teofilina ? Não use nenhum 
# nome para a coluna resultante
summarise(df,max(conc))

# Para os exercícios abaixo, usaremos dois conjuntos de dados relacionados aos
# tempos de atraso de vôos do Bureau of Transportation Statistics dos EUA 
# (X673598238_T_ONTIME_REPORTING e L_UNIQUE_CARRIERS.csv_). 
# Para carregar os dados é importante que vocês usem os seguintes argumentos: 
# quote="\"", sep = "," . Note que a extensão ".csv_" está correta. 
# O data frame do arquivo (X673598238_T_ONTIME_REPORTING)  possui apenas 
# informações da companhia aérea por código. No entanto, queremos saber os
# nomes das companhias aéreas. Assim, faça o merge dos datasets 
# "X673598238_T_ONTIME_REPORTING" e "L_UNIQUE_CARRIERS.csv_" através das
# colunas "OP_UNIQUE_CARRIER" e "Code". Em seguida, responda as questões abaixo.

dados1 <- read.csv("entradas/673598238_T_ONTIME_REPORTING.csv", 
                   quote="\"", 
                   sep = ",")

dados1 <- dados1[,1:5]
dados1$DEP_DELAY_NEW[which(is.na(dados1$DEP_DELAY_NEW))] <- 0


dados2 <- read.csv("entradas/L_UNIQUE_CARRIERS.csv_",
                   quote="\"", 
                   sep = ",")

dados <- merge(x = dados1, 
               y = dados2, 
               by.x = "OP_UNIQUE_CARRIER",
               by.y = "Code",
               all.x = TRUE, 
               all.y = FALSE)

# dados <- left_join(dados1, dados2, by = c("OP_UNIQUE_CARRIER"="Code"))
str(dados)
View(dados)

# Qual companhia teve o maior atraso ?
dados%>%filter(DEP_DELAY_NEW==max(DEP_DELAY_NEW))

# Qual companhia atrasa mais na média ?
media_atraso <- dados %>% 
  group_by(Description) %>%
  summarise(media=mean(DEP_DELAY_NEW)) %>%
  arrange(desc(media))

media_atraso %>% filter(media==max(media))
head(media_atraso)

# Qual companhia atrasa menos na média ?
media_atraso %>% filter(media==min(media))
tail(media_atraso)

# Qual companhia teve a maior proporção de atrasos ?
qtd_atrasos <- dados %>% 
  filter(DEP_DELAY_NEW>0) %>%
  group_by(Description) %>%
  summarise(atrasos = n())

qtd_total <- dados %>% 
  group_by(Description) %>%
  summarise(total = n())

dados4 <- merge(x = qtd_atrasos, y = qtd_total, by = "Description", all = T)
dados4$prop <- dados4$atrasos/dados4$total
dados4 %>% arrange(desc(prop))

dados %>%
  group_by(Description) %>%
  summarise(n = n(), atrasos = sum(DEP_DELAY_NEW>0)) %>%
  mutate(prop = atrasos / n ) %>%
  arrange(desc(prop))

# "Só uma dica: use variável OP_UNIQUE_CARRIER e obtenha as ocorrências para 
# cada companhia usando a função table(). E a proporção é uma porcentagem em 
# relação ao total de ocorrência. A questão pede a maior proporção."
table(dados$OP_UNIQUE_CARRIER, remove = dados$DEP_DELAY_NEW>0)


# Você está encarregado de analisar um conjunto de dados que contém casos de 
# tuberculose (TB) relatados entre 1995 e 2013, ordenados por país, idade e sexo.
# O recurso mais exclusivo desses dados é o seu sistema de codificação. 
# As colunas de três a vinte e três codificam quatro partes separadas de 
# informações em seus nomes de coluna: 
# (i) As três primeiras letras de cada coluna indicam se a coluna contém casos 
# novos ou antigos de TB. 
# (ii) As próximas duas letras descrevem os tipos de casos que estão sendo contados. 
# (iii) A sexta letra descreve o sexo dos pacientes com tuberculose. 
# Os números restantes descrevem a faixa etária dos pacientes com TB. 
# Carregue o conjunto de dados de http://stat405.had.co.nz/data/tb.csv como um
# novo dataframe chamado TB. 
# Observe que o conjunto de dados TB é desordenado de várias maneiras. 
# Primeiramente, mova os valores das colunas 3 até 23 para uma única coluna 
# chamada de “Informacao”. Dica use o gather (). 
# Divida os conteúdos da variável “informacao” em cada sublinhado(”_”). 
# Dica use o separate () e chame as colunas resultantes de "caso", "tipo” e 
# "sexofaixa”. Por fim, divida a variável que contém o sexo e a faixa etária 
# ("sexofaixa”) de modo a criar uma coluna de "sexo" e uma coluna de "faixa". 
# De posse do data frame resultante, responda as questões abaixo.
install.packages("tidyr")
library(tidyr)

fileurl <- "http://stat405.had.co.nz/data/tb.csv"
download.file(fileurl, destfile = "entradas/tb.csv", method = "auto")
arq <- "entradas/tb.csv"
txt <- read.csv2(arq,header = T, sep = ",", dec = ".")

df <- txt %>%  gather(key = "Informacao", value = "valor", 3:23)

df2 <- df %>% separate(col = "Informacao",
                       into = c("Caso","Tipo","SexoFaixa"),
                       sep = "_")
df3 <- df2 %>% separate(col = "SexoFaixa",
                        into = c("Sexo","Faixa"),
                        sep = 1)
str(df3)

# Qual foi a quantidade de casos para a Tailândia (TH) de pessoas do sexo 
# Masculino?
df3 %>%
  filter(Sexo == "m", iso2 == "TH") %>%
  summarise(sum(valor,na.rm = T))

# Qual a proporção de casos para os estados unidos (US) ? Não considerar 
# valores NAs.
df3 %>%
  group_by(iso2) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n,na.rm=T)) %>%
  filter(iso2 == "US")

# Qual a quantidade de casos para a faixa etária 2534 do sexo feminino?
df3 %>%
  filter(Faixa == "2534", Sexo == "f") %>%
  summarise(sum(valor, na.rm = T))

# Qual foi a quantidade de casos para a década de 2000 ? A década de 2000, 
# também referida como anos 2000, compreende o período de tempo entre 1 de 
# janeiro de 2000 e 31 de dezembro de 2009.
df3 %>%
  filter(year >= 2000, year <= 2009) %>%
  summarise(sum(valor, na.rm = T))
