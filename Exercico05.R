# UFRPE
# CAD - Computação para Análise de Dados 2020.3
# ago/2020
# Autor: Tito Kenzo 
# Exercício 05

setwd("D:/home/GoogleDrive/UFRPE/CAD 2020.3/exercicios")
getwd()

# Crie os vetores a seguir: ‘mouse.color' com os valores: ‘purple’, ‘red’, 
# ‘yellow’,‘brown’; ‘mouse.weight’ com os valores 23, 21, 18, 26. 
# Crie um data frame chamado de ‘mouse.info’ com esses vetores, formando 4 
# linhas e 2 colunas. Nomeie a primeira coluna para ‘colour’ e a segunda para 
# ‘weight’. De posse desse data frame, responda as perguntas abaixo. 
# OBS.: Ao submeter o comando, retire todos os espaço em branco.
mouse.color <- c('purple', 'red', 'yellow','brown')
mouse.weight <- c(23, 21, 18, 26)
mouse.info <- data.frame(mouse.color,mouse.weight)
names(mouse.info) <- c('colour','weight')
mouse.info

# Qual comando imprima a estrutura do data frame no console?
str(mouse.info)

# Qual comando imprima apenas a linha 3 no console? 
mouse.info[3,]

# Qual comando imprima apenas a coluna 1 no console?
mouse.info[,1]

# Qual comando imprima o item na linha 4 da coluna 1?
mouse.info[4,1]

# Para as questões abaixo use o dataset airquality já incluído no RStudio.
air <- airquality
str(air)

# Qual foi o valor mínimo de ozônio no mês de maio?
maio <- air[air$Month==5, c('Ozone')]
min(maio,na.rm=T)

# Extraia o subconjunto do data frame em que os valores de Ozônio estão acima 
# de 25 e os valores da temperatura (Temp) estão abaixo de 90. Qual é a média 
# do Solar.R nesse subconjunto?
sub <- air[air$Ozone>25 & air$Temp<90, c('Solar.R')]
mean(sub,na.rm=T)

# Qual a quantidade de casos completos no dataset airquality? Ou seja, a
# quantidade de observação (linhas) sem NAs.
total <- air[complete.cases(air),]
nrow(total)

# Carregue o arquivo genomes.csv numa variável chamada genomas através do 
# seguinte comando: 
# genomas <- as.data.frame(read.csv("https://www.dropbox.com/s/vgh6qk395ck86fp/genomes.csv?dl=1")). 
# De posse desse dado, responda as perguntas abaixo.
genomas <- as.data.frame(read.csv("https://www.dropbox.com/s/vgh6qk395ck86fp/genomes.csv?dl=1"))
str(genomas)

# Selecione os organismos com mais de 40 cromossomos.
sub_genomas <- genomas[genomas$Chromosomes>40,c('Organism')]
sub_genomas

# Selecione os organismos que contém plasmídeos e também possui mais de um 
# cromossomo.
sub_genomas <- genomas[genomas$Plasmids>0 & genomas$Chromosomes>1,c('Organism')]
View(sub_genomas)

# Carregue o arquivo cancer_stats.csv numa variável chamada cancer_stats 
# através do seguinte comando: 
# cancer_stats <- as.data.frame(read.csv("https://www.dropbox.com/s/g97bsxeuu0tajkj/cancer_stats.csv?dl=1")). 
# De posse desse dado, responda as perguntas abaixo.
cancer_stats <- as.data.frame(read.csv("https://www.dropbox.com/s/g97bsxeuu0tajkj/cancer_stats.csv?dl=1"))
str(cancer_stats)
View(cancer_stats)

# Para qual local do câncer (site) do sistema digestivo (Digestive System) 
# existem mais casos femininos do que masculinos?
casos <- cancer_stats[cancer_stats$Class=='Digestive System' & cancer_stats$Female.Cases>cancer_stats$Male.Cases,c('Site','Male.Cases','Female.Cases')]
casos

# Qual local do câncer tem a melhor taxa de sobrevivência para os homens?
cancer_stats$TaxaMortalidadeMasculina <- cancer_stats$Male.Deaths/cancer_stats$Male.Cases
taxa_min <- min(cancer_stats$TaxaMortalidadeMasculina,na.rm = T)
cancer_stats[cancer_stats$TaxaMortalidadeMasculina==taxa_min,]

# Qual local de câncer tem a pior taxa de sobrevivência para as mulheres?
cancer_stats$TaxaMortalidadeFeminina <- cancer_stats$Female.Deaths/cancer_stats$Female.Cases
taxa_max <- max(cancer_stats$TaxaMortalidadeFeminina,na.rm = T)
cancer_stats[cancer_stats$TaxaMortalidadeFeminina==taxa_max,]
