# UFRPE
# CAD - Computação para Análise de Dados 2020.3
# ago/2020
# Autor: Tito Kenzo 
# Aula 03 - Introdução ao R
# Exercício (PDF)

setwd("D:/home/GoogleDrive/UFRPE/CAD 2020.3/exercicios")
getwd()

# Crie um diretório de trabalho chamado: ”Meu RCurso”.

# Rode o RStudio e mude o seu diretório para ”Meu RCurso”.

# Use o R para os seguintes cálculos:
# 334+456, 900/45, 899-844 e 73*73
334+456
900/45
899-844
73*73

# Atribua o valor 133 para X e o valor 36 para Y.
x <- 133
y <- 36

# Adicione a soma de X e Y a Z e apresente o valor de Z na tela.
z <- x + y

# Calcule a raiz quadrada de Z e o logaritmo do resultado da raiz quadrada.
log(sqrt(z))

# Veja para quer o comando demo() serve. Na linha de comando, tente 
# demo(graphics).
demo(graphics)

# Veja os objetos criados usando a função ls().
ls()

# Delete alguns desses objetos usando rm().
rm(x)

# Delete todos os objetos rm(list = ls())
rm(list = ls())
