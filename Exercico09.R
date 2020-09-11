# UFRPE
# CAD - Computação para Análise de Dados 2020.3
# set/2020
# Autor: Tito Kenzo 
# Exercício 09

setwd("D:/home/GoogleDrive/UFRPE/CAD 2020.3/exercicios")
getwd()

# Carregue o dataset do link abaixo. Esse dataset contém informações do peso do
# coração e corporal de 97 gatos adultos que foram usados para experimentos com
# a droga "digitalis". Com posse desses dados responda as perguntas abaixo. 
# Qual foi a média do peso dos gatos (Bwt) ? 
# https://www.dropbox.com/s/w4xv9urbowbig3s/catsM.csv?dl=0

fileurl <- "https://www.dropbox.com/s/w4xv9urbowbig3s/catsM.csv?dl=0"
download.file(fileurl, destfile = "entradas/catsM.csv", method = "curl")

arq <- "entradas/catsM.csv"
txt <- read.csv2(arq,header = T, sep = ",", dec = ".")
View(txt)
str(txt)

mean(txt$Bwt)

# Para as questões abaixo considere o dataset "Caracol_data_checked.csv".
# Note que para você obter esse dataset você precisa executar todos os passos 
# apresentado na aula de Manipulação Básica.
#*******************************************************************************
#*******************************************************************************
fileURL <- "https://www.dropbox.com/s/c6nhu4v7yq9pyto/Snail_feeding.csv?dl=1"
download.file(fileURL, destfile = "entradas/Snail_feeding.csv", method = "auto")
list.files("entradas/")
Caracol_data <- read.csv(file = "entradas/Snail_feeding.csv", 
                         header = T, 
                         strip.white = T, 
                         na.strings = "")
str(Caracol_data)
# remover/ignorar as colunas extra
Caracol_data <- Caracol_data[,1:7]

# verificar os níveis do fato Sex (male/female)
unique(Caracol_data$Sex)
levels(Caracol_data$Sex)

levels(Caracol_data$Sex) [2] <- "female"
levels(Caracol_data$Sex) [3] <- "male"
levels(Caracol_data$Sex) [3] <- "male"

levels(Caracol_data$Sex)
str(Caracol_data)

# converter um coluna numérica (Distance)
Caracol_data$Distance <- as.numeric(as.character(Caracol_data$Distance))
str(Caracol_data)

# corrigindo os NAs gerados na conversão
which(is.na(Caracol_data$Distance))
Caracol_data[682,"Distance"] <- 0.58
Caracol_data[755,"Distance"] <- 0.356452
which(is.na(Caracol_data$Distance))

str(Caracol_data)

# removendo dados duplicados
which(duplicated(Caracol_data))
index <- which(duplicated(Caracol_data))
Caracol_data <- Caracol_data[-index,]
which(duplicated(Caracol_data))

# estatísticas
summary(Caracol_data)

# corrigindo dados errados (Depth)
Caracol_data[which(Caracol_data$Depth > 2),]
Caracol_data[8,6] <- 1.62

summary(Caracol_data)

# ordenando vetores únicos
# sort(Caracol_data$Depth)
# ordenando dataframe
# Caracol_data[order(Caracol_data$Depth, Caracol_data$Temp), ]
# ordenando dataframe de forma decrescente
# Caracol_data[order(Caracol_data$Depth, Caracol_data$Temp, decreasing = T), ]

# exportação
write.csv(Caracol_data,
          file = "saidas/Caracol_data_checked.csv",
          row.names = FALSE)

View(Caracol_data)
#*******************************************************************************
#*******************************************************************************

# Qual a média da profundidade (depth) dos caracóis marinhos?
mean(Caracol_data$Depth)

# Qual foi a maior distância coletada para o Caracol Marinho Pequeno e Feminino?
dado <- Caracol_data[Caracol_data$Size=="small" & Caracol_data$Sex=="female", "Distance"]
max(dado, na.rm = T) # = 1

# Para as questões abaixo carregue os dados do dataset Sparrows.csv.

# O arquivo de dados Sparrows.csv contém medidas de asa, tarso, cabeça e bico 
# de duas espécies de pardal. 
# Use read.table(file = "Sparrows.csv", header = TRUE) para carregar os dados 
# para um objeto chamado Sparrows. Depois, use a função View () para exibir os
# dados. Note que algo deu errado! Todas as variáveis foram combinadas em uma
# grande coluna. Qual foi o problema ? 

Sparrows <- read.table(file = "entradas/Sparrows.csv", header = TRUE, sep = ",") 
View(Sparrows)
str(Sparrows)

# Qual o tamanho mínimo e máximo da cabeça da especie "SSTS" ?
max(Sparrows[Sparrows$Species == "SSTS","Head"], na.rm = T)
min(Sparrows[Sparrows$Species == "SSTS","Head"], na.rm = T)

# Durante a entrada de dados, três linhas foram inseridas duas vezes. 
# Quais são essas linhas duplicadas?
which(duplicated(Sparrows))

# Exiba os níveis do fator Sex. Veja que os níveis estão bagunçados. 
# Altere de modo que a variável sexo contenha apenas os níveis "Male" e "Female".
# Por exemplo, "Femal" deve ser "Female". 
# Após isso, determine qual é a media do tarso dos pardais femininos e masculinos.
levels(Sparrows$Sex)
levels(Sparrows$Sex) [1] <- "Female"
levels(Sparrows$Sex) [2] <- "Female"
levels(Sparrows$Sex) [3] <- "Male"

tapply(Sparrows$Tarsus, Sparrows$Sex, mean)

# Digite o comando que verifica quais linhas na variável Wing contém NAs. 
# Use a função which. 
# OBS.: Remova todos os espaços em branco.
which(is.na(Sparrows$Wing))

# Substitua todos os NAs da questão anterior pelos valores 59, 56.5 e 57 
# (nessa ordem). Qual a média das asas dos pardais ?
Sparrows[64,"Wing"] <- 59
Sparrows[250,"Wing"] <- 56.5
Sparrows[806,"Wing"] <- 57

mean(Sparrows$Wing)

# Qual o comando ordena o data frame pelas colunas Wing e Head e crie um novo 
# objeto chamado de Sparrows_Ordenado ?
Sparrows_Ordenado<-Sparrows[order(Sparrows$Wing,Sparrows$Head),]
