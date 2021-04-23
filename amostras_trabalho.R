#install.packages("read.dbc")

library(read.dbc)

k <- read.dbc::read.dbc("~/Bruno/Estudo/UnB/Estatística Exploratoria/Trabalho/DNAC2016.dbc")


ESTADOS = c('AC', 'AL','AM', 'AP','BA','CE','DF','ES','GO',
            'MA','MG','MS','MT','PA','PB','PE','PI','PR',
            'RJ','RN','RO','RR','RS','SC','SE','TO','SP')


# Laço para obter o tamanho da população

tamanho = 0
for (estado in ESTADOS) {
  arquivo <- gsub("\\s", "", paste('DN',estado,"2016.dbc"))
  df <- read.dbc::read.dbc(arquivo)
  tamanho = tamanho +length(df$contador)
  print(paste(estado, 'carregado'))
  }

tamanho


#Laço para obter a amostra

x = k[(k$CODESTAB == 57019000029) & (!is.na(k$CODESTAB)),]

set.seed(1234)

elementos = 0
for (estado in ESTADOS) {
  arquivo <- gsub("\\s", "", paste('DN',estado,"2016.dbc"))
  y <- read.dbc::read.dbc(arquivo)
  y$contador = as.numeric(y$contador)
  tam_y = length(y$contador)
  amostra = 2000 * tam_y/tamanho
  if (amostra - round(amostra, digits = 0) > 0.5){
    amostra = round(amostra, digits = 0) + 1}
  else {
    amostra = round(amostra, digits = 0)  }
  if(estado == 'SP') {
    amostra = 2000 - elementos}
  cont_z = sample(y$contador,amostra)
  cont_z
  z = y[cont_z,]
  x = rbind(x,z)
  print(amostra)
  print(paste(arquivo, 'carregado'))
  elementos = elementos + amostra
  print(elementos)}





