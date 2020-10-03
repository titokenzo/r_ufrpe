setwd("D:/home/GoogleDrive/UFRPE/CAD 2020.3/exercicios")
getwd()
install.packages(c("readtext","tm","wordcloud","RColorBrewer","twitteR","syuzhet"))
library(readtext)
library(tm)
library(wordcloud)
library(stringr)
library(twitteR)
library(syuzhet)

### Carrega o arquivo com o texto "eu_tenho_um_sonho.txt"
crude = readtext("entradas/eu_tenho_um_sonho.txt")

### Trata os dados do texto para um formato mais "limpo"
crude <- Corpus(VectorSource(crude), readerControl = list(reader=readPlain, language='portuguese'))
crude <- tm_map(crude, content_transformer(removeNumbers))
crude <- tm_map(crude, tolower)
crude <- tm_map(crude, removePunctuation)
crude <- tm_map(crude, stripWhitespace)
crude <- tm_map(crude, removeWords, stopwords("portuguese"))

### Matriz de palavras e frequência
palavras <- as.matrix(TermDocumentMatrix(crude))
frequencia <- sort(rowSums(palavras), decreasing = T)
head(frequencia)

### Nuvem de palavras
wordcloud(crude, 
          min.freq = 3, 
          max.words = Inf, 
          random.order = F, 
          rot.per = 0.3, 
          colors = c("red","blue"))

### Palavras mais frequentes (>5)
crude.top5 <- subset(frequencia, frequencia>5)
barplot(crude.top5, 
        las=2, 
        main="Frequencia das Palavras", 
        xlab='Palavras', 
        ylab='Quantidade', 
        col=c("red","blue"))


consumer_key = "<>"
consumer_secret = "<>"
access_token = "<>"
access_secret = "<>"

### Conectando ao Twitter
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

### Fazendo a busca pela expressão #BlackLiveMatters
tweets <- searchTwitter("#BlackLiveMatters", n = 300, lang = "pt-br")

### Limpeza dos dados
tweets <- twListToDF(tweets)
crude <- paste(tweets$text, collapse = " ")
crude <- Corpus(VectorSource(crude))
crude <- tm_map(crude, tolower)
crude <- tm_map(crude, removePunctuation)
crude <- tm_map(crude, stripWhitespace)
crude <- tm_map(crude, removeWords, stopwords("portuguese"))

### Limpando expressões para deixar somente 'texto'
crude <- tm_map(crude, function(x) gsub("http[^[:space:]]*", "", x))
crude <- tm_map(crude, function(x) gsub("[^[:alpha:][:space:]]*", "", x))

### Matriz de palavras e frequência
palavras <- as.matrix(TermDocumentMatrix(crude))
frequencia <- sort(rowSums(palavras), decreasing = TRUE)

### Nuvem de Palavras com frequancia maior que 3
wordcloud(crude, 
          min.freq = 3,
          max.words = 50,
          random.order = F,
          rot.per = 0.3,
          colors = c("red","blue"))

### Análise de Sentimento
tweets <- tweets$text
sentimentos <- get_nrc_sentiment(tweets)
barplot(colSums(sentimentos), 
        las = 2, 
        col = rainbow(10), 
        ylab = "Contagem", 
        xlab = "sentimentos",
        main = "#BlackLiveMatters”")

## Questão 03



