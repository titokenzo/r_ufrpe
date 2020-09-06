# UFRPE
# CAD - Computação para Análise de Dados 2020.3
# set/2020
# Autor: Tito Kenzo 
# Exercício 07

setwd("D:/home/GoogleDrive/UFRPE/CAD 2020.3/exercicios")
getwd()

# Qual o resultado da função abaixo
y <- 5
mult <- function(x,y){
  return (x*y)
}

mult(10)

# Qual o resultado da função abaixo.
y <- 5
mult <- function(x){
  return (x*y)
}

mult(10)

# Escreva uma função chamada 'prisoes' que calcula a soma das prisões em 
# estados americanos do dataset USArrests. A função 'prisoes' recebe dois 
# argumentos: 'estados' e 'tiposPrisoes'. Dado esses argumentos, a função 
# retorna o total de prisões. Se não existir nem o estado ou o tipo de prisão, 
# então uma mensagem de erro deve ser retornada. O protótipo dessa função é: 
# prisoes <- function(estados, tiposPrisoes){}. As seguintes mensagens precisam
# ser apresentadas caso o estado ou o tipo de prisão estejam inválidos: 
# "Estado Inválido" ou "Tipo de Prisão Inválida". De posse dessa função, 
# responda as questões abaixo.

prisoes <- function(estados,tiposPrisoes){
  #"Estado Inválido" "Tipo de Prisão Inválida"
  if(sum(tiposPrisoes %in% colnames(USArrests))<length(tiposPrisoes)){
    return(print("Tipo de Prisão Inválida"))
  }
  if(sum(estados %in% rownames(USArrests))<length(estados)){
    return(print("Estado Inválido"))
  }
  
  total_prisoes <- sum(apply(X = USArrests[estados,tiposPrisoes],MARGIN = 1, FUN = sum))
  if(length(estados)>1){
    return( paste("O total de prisoes dos estados",toString(estados),"é",toString(total_prisoes)) )
  }else{
    return( paste("O total de prisoes do estado",toString(estados),"é",toString(total_prisoes)) )
  }
}

prisoes(estados="Oregon", tiposPrisoes=c("Rape","Murder"))
prisoes(estados=c("Nevada","Washington"), tiposPrisoes=c("UrbanPop","Assault"))
prisoes(estados="Pernambuco", tiposPrisoes=c("Rape","Murder"))

prisoes(estados="Tennessee", tiposPrisoes=c("Rape","Murder")) 

# Qual é o total de prisões do código a seguir? 
prisoes(estados=c("California ","Miami", "Arizona"), tiposPrisoes=("Assault")) 

# Qual é o total de prisões do código a seguir? 
prisoes(estados=c("Pennsylvania","Mississippi", "Nebraska"), tiposPrisoes=c("Rape","UrbanPop","Assault"))

# Qual é o total de prisões do código a seguir? 
prisoes(estados=c("Vermont","Wisconsin", "Texas"), tiposPrisoes=c("Rape","Assalto"))

# Para responder as questões abaixo ver o documento "Detalhamento de Questões".
# Mais especificamente, Questão 'minhasNotas_1'.
conceito <- function(Opt,threshold,media){
  if(Opt!=3){
    return(ifelse(media>=threshold,"Acima da Média","Abaixo da Média"))
  }else{
    return(ifelse(media>=threshold,"Aprovado!!","Reprovado!"))
  }
}
minhasNotas_1 <- function(Exe_1=0,VA_1=0,Exe_2=0,Proj=0,VA_2=0,VA_3=0,Opt=1,threshold=7){
  #Opt = c(1,2,3) 1- VA_1; 2- VA_2; 3-Média Final (incluindo VA_3)
  erros <- sum(Exe_1>10 | Exe_1<0, na.rm = T)
  erros <- erros + sum(Exe_2>10 | Exe_2<0, na.rm = T)
  if(erros>0){
    return(print("Notas Inválidas!"))
  }

  if(length(VA_1)>1 | length(VA_2)>1 | length(Proj)>1 | length(VA_3)>1){
    return(print("Quantidade de Notas Inválida!"))
  }

  Exe_1[is.na(Exe_1)] <- 0
  Exe_2[is.na(Exe_2)] <- 0
  Exe_1 <- sum(Exe_1)/length(Exe_1)
  Exe_2 <- sum(Exe_2)/length(Exe_2)

  VA_1_na <- ifelse(is.na(VA_1),0,VA_1)
  VA_2_na <- ifelse(is.na(VA_2),0,VA_2)
  m_1VA = (Exe_1*5 + VA_1_na*5)/(5+5)
  m_2VA = (Exe_2*2 + Proj*5 + VA_2_na*3)/(2+5+3)
  
  if(Opt==1){
    if(is.na(VA_1)){
      return(print("Aluno não possui nota para a 1 VA"))
    }
    media = m_1VA
    return( paste("Média 1VA:",toString(media),"-",conceito(Opt,threshold,media)))
  }else if(Opt==2){
    if(is.na(VA_2)){
      return(print("Aluno não possui nota para a 2 VA"))
    }
    media = m_2VA
    return( paste("Média 2VA:",toString(media),"-",conceito(Opt,threshold,media)))
  }else if(Opt==3){
    menor = min(m_1VA,m_2VA,VA_3, na.rm = T)
    media = (m_1VA + m_2VA + VA_3 - menor)/2
    return( paste("Média Final:",toString(media),"-",conceito(Opt,threshold,media)))
  }else{
    return( print("Opção inválida") )
  }
  
  if(threshold>media){
    print("Passou")
  }else{
    print("reprovou")
  }
}
# TESTES:
minhasNotas_1(Exe_1=c(10,7,NA), VA_1=7, Exe_2=7, Proj=7, VA_2=7, VA_3=10, Opt=1, threshold=7)
minhasNotas_1(Exe_1=c(10,7,11), VA_1=7, Exe_2=7, Proj=7, VA_2=7, VA_3=10, Opt=1, threshold=7)
minhasNotas_1(Exe_1=c(10,7,NA), VA_1=7, Exe_2=c(7,9,10), Proj=7, VA_2=7, VA_3=10, Opt=2, threshold=8)
minhasNotas_1(Exe_1=NA, VA_1=NA, Exe_2=c(7,9,10), Proj=7, VA_2=7, VA_3=10, Opt=1, threshold=8)
minhasNotas_1(Exe_1=NA, VA_1=NA, Exe_2=c(7,9,10), Proj=7, VA_2=7, VA_3=10, Opt=3, threshold=8)
minhasNotas_1(Exe_1=NA, VA_1=NA, Exe_2=c(7,9,10), Proj=c(7,8), VA_2=7, VA_3=10, Opt=3, threshold=8)


# Qual o retorno da seguinte função? 
minhasNotas_1(Exe_1=c(10,9,7,1,11,10), VA_1=8, Exe_2=7, Proj=7, VA_2=7, VA_3=10, Opt=1, threshold=7)

minhasNotas_1(Exe_1=c(10,9,5,1,5,10), VA_1=8, Exe_2=7, Proj=7, VA_2=7, VA_3=10, Opt=1, threshold=7) 

minhasNotas_1(Exe_1=NA, VA_1=8, Exe_2=c(10,1,5), Proj=10, VA_2=8, VA_3=10, Opt=2, threshold=8)

minhasNotas_1(Exe_1=c(10,9,7,1,NA,NA), VA_1=NA, Exe_2=7, Proj=7, VA_2=7, VA_3=10, Opt=1, threshold=7)

minhasNotas_1(Exe_1=NA, VA_1=8, Exe_2=NA, Proj=10, VA_2=8, VA_3=10, Opt=2, threshold=8)

minhasNotas_1(Exe_1=c(5,8), VA_1=8, Exe_2=c(10,1,5), Proj=10, VA_2=8, VA_3=2, Opt=3, threshold=8)

minhasNotas_1(Exe_1=NA, VA_1=8, Exe_2=c(10,1,5), Proj=10, VA_2=8, VA_3=10, Opt=3, threshold=8) 

minhasNotas_1(Exe_1=c(5,8), VA_1=8, Exe_2=c(10,1,5), Proj=c(2,4), VA_2=8, VA_3=2, Opt=3, threshold=8)

minhasNotas_1(Exe_1=10, VA_1=NA, Exe_2=7, Proj=7, VA_2=7, VA_3=10, Opt=1, threshold=7) 

# Para responder as questões abaixo ver o documento "Detalhamento de Questões".
# Mais especificamente, Questão 'minhasNotas_2'.
conceito2 <- function(Opt,threshold,media){
  # “A”( Excelente) de 9,0 até 10,0; “B” (Bom) de 7,5 até 8,9;  
  # “C” (Regular) de 6,0 até 7,4; e  “D”( Reprovado) de 0,0 até 5,9. 
  if(media<6.0){
    conc = "D"
  }else if(media<7.5){
    conc = "C"
  }else if(media<9.0){
    conc = "B"
  }else{
    conc = "A"
  }
  
  if(Opt!="3"){
    return(paste(conc,"-",ifelse(conc<threshold,"Acima da Média",ifelse(conc==threshold,"Na Média","Abaixo da Média"))))
  }else{
    return(paste(conc,"-",ifelse(conc<=threshold,"Aprovado!!","Na Final!")))
  }
}
minhasNotas_2 <- function(Exe_1=0,VA_1=0,Exe_2=0,Proj=0,VA_2=0,VA_3=0,Opt=1,threshold="C"){
  #Opt = c(1,2,3) 1- VA_1; 2- VA_2; 3-Média Final (incluindo VA_3)
  erros <- sum(Exe_1>10 | Exe_1<0, na.rm = T)
  erros <- erros + sum(Exe_2>10 | Exe_2<0, na.rm = T)

  if(erros>0){
    return(print("Notas Inválidas!"))
  }

  if(length(VA_1)>1 | length(VA_2)>1 | length(Proj)>1 | length(VA_3)>1){
    return(print("Quantidade de Notas Inválida!"))
  }

  Exe_1[is.na(Exe_1)] <- 0
  Exe_2[is.na(Exe_2)] <- 0
  Exe_1 <- sum(Exe_1)/length(Exe_1)
  Exe_2 <- sum(Exe_2)/length(Exe_2)

  VA_1_na <- ifelse(is.na(VA_1),0,VA_1)
  VA_2_na <- ifelse(is.na(VA_2),0,VA_2)
  m_1VA = (Exe_1*5 + VA_1_na*5)/(5+5)
  m_2VA = (Exe_2*2 + Proj*5 + VA_2_na*3)/(2+5+3)
  
  if(Opt==1){
    if(is.na(VA_1)){
      return(print("Aluno não possui nota para a 1 VA"))
    }
    media = m_1VA
    return( paste("Média 1VA:",conceito2(Opt,threshold,media)))
  }else if(Opt==2){
    if(is.na(VA_2)){
      return(print("Aluno não possui nota para a 2 VA"))
    }
    media = m_2VA
    return( paste("Média 2VA:",conceito2(Opt,threshold,media)))
  }else if(Opt==3){
    menor = min(m_1VA,m_2VA,VA_3, na.rm = T)
    media = (m_1VA + m_2VA + VA_3 - menor)/2
    return( conceito2(Opt,threshold,media))
  }else{
    return( print("Opção inválida") )
  }
  
  if(threshold>media){
    print("Passou")
  }else{
    print("reprovou")
  }
}
# TESTES:
minhasNotas_2(Exe_1=8, VA_1=9, Exe_2=9, Proj=5, VA_2=4, VA_3=1, Opt=1, threshold="B")
minhasNotas_2(Exe_1=8, VA_1=9, Exe_2=9, Proj=5, VA_2=4, VA_3=1, Opt=1, threshold="A")
minhasNotas_2(Exe_1=NA, VA_1=c(9,1), Exe_2=9, Proj=5, VA_2=4, VA_3=1, Opt=1, threshold="A")
minhasNotas_2(Exe_1=NA, VA_1=9, Exe_2=9, Proj=5, VA_2=9, VA_3=10, Opt=3, threshold="C")
minhasNotas_2(Exe_1=NA, VA_1=9, Exe_2=9, Proj=5, VA_2=9, VA_3=10, Opt=3, threshold="A")

# Qual o retorno da seguinte função? 
minhasNotas_2(Exe_1=c(8,9,NA), VA_1=5, Exe_2=9, Proj=5, VA_2=4, VA_3=1, Opt=1, threshold="C") 

minhasNotas_2(Exe_1=c(8,9,10,9,9,8,6), VA_1=4, Exe_2=c(8,3,11,9,9,5), Proj=4, VA_2=4, VA_3=1, Opt=1, threshold="C")

minhasNotas_2(Exe_1=c(8,9,10,9,9,8,6), VA_1=7, Exe_2=c(8,3,10,9,9,5), Proj=8, VA_2=3, VA_3=1, Opt=3, threshold="B") 

minhasNotas_2(Exe_1=c(8,9,10,9,9,8,6), VA_1=7, Exe_2=c(8,3,10,9,9,5), Proj=8, VA_2=8, VA_3=1, Opt=2, threshold="B") 

minhasNotas_2(Exe_1=c(8,9,10,9,9,8,6), VA_1=7, Exe_2=c(8,3,10,9,9,5), Proj=5, VA_2=5, VA_3=10, Opt=4, threshold="B")
