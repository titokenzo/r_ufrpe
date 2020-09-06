# UFRPE
# CAD - Computação para Análise de Dados 2020.3
# ago/2020
# Autor: Tito Kenzo 
# Exercício 03

setwd("D:/home/GoogleDrive/UFRPE/CAD 2020.3/exercicios")
getwd()

# Vetores aceitam apenas um tipo de dados. O que acontece com xx com o seguinte
# comando: xx <- c(TRUE, 2) ?
xx <- c(TRUE, 2)
xx

# Suponha que você tenha um vetor x <- 1:4 e y <- 2:3. O que é produzido pela
# expressão x * y?.
x <- 1:4 
y <- 2:3
x * y

# Dada a tabela no link abaixo que representa uma competição entre Maria e
# Joana, determine quem foi a vencedora. Dica: primeiro padronize os dados e
# depois calcule a média dos valores padronizados. 

csv <- read.csv2("entradas/tabela_abdominais.csv", sep=";", encoding = "UTF-8")
csv$MariaPadrao <- (csv$Maria - csv$Média) / csv$Desvio.Padrão
csv$JoanaPadrao <- (csv$Joana - csv$Média) / csv$Desvio.Padrão
csv
mediaMaria <- mean(csv$MariaPadrao)
mediaJoana <- mean(csv$JoanaPadrao)
mediaMaria
mediaJoana

# Para responder as questões abaixo, faça download do arquivo disponível em 
# https://www.dropbox.com/s/6t7b44acy7yfczu/vetor.RData?dl=1 e mova-o para o
# seu diretório. Em seguida leia-o com o comando load(“vetor.RData”). 
# O vetor01 possui 15.000 observações referentes às notas dos candidatos do
# concurso público para auxiliar administrativo da Prefeitura de São Longuinho.
# Os dados foram disponibilizados pela empresa organizadora do concurso.
# As notas variam de 0 a 10 e os candidatos que não realizaram a prova estão
# com NA nos respectivos campos. De posse dos dados, responda as perguntas
# abaixo.

load("entradas/vetor.RData")
vetor01
str(vetor01)
summary(vetor01)

# Qual foi a média das notas?
media <- mean(vetor01, na.rm=T)
round(media,3)

# Qual foi a mediana das notas? 
mediana <- median(vetor01, na.rm=T)
mediana

# Qual o desvio padrão das notas?
desvio <- sd(vetor01, na.rm=T)
desvio

# Qual foi a quantidade de faltosos? 
faltosos <- sum(is.na(vetor01),na.rm = F)
faltosos

# Qual foi percentual de faltosos?
presentes <- mean(is.na(vetor01))
presentes

# Quantos candidatos obtiveram notas maiores do que 7.00 e menores do que 8.00?
notas <- sum(vetor01>7 & vetor01<8, na.rm=T)
notas

# Quantos candidatos obtiveram notas maiores do que 9.00 ou menores do que 1.00?
notas2 <- sum(vetor01>9 | vetor01<1, na.rm=T)
notas2

# Quais os comandos removem os valores ausentes (NA) do vetor01 e guarda o 
# resultado no próprio vetor01? 

vetor01 <- vetor01[!is.na(vetor01)]
vetor01 <- !is.na(vetor01) # vetor de boolean
vetor01[!is.na(vetor01)] -> vetor01
vetor01 <- vetor01[-which(is.na(vetor01))]

faltosos <- sum(is.na(vetor01),na.rm = F)
faltosos
