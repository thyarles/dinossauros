#3)Descrever a variável peso do recém-nascidos da amostra.

# Importes para a solução da questão
grafico <- modules::use('R/grafico.R')
import('ggplot2')

# ------------------------------------------------------------------------------
#Classificação da variável
# ------------------------------------------------------------------------------

#A variável "peso" é uma variável quantitativa contínua cujos valores pertencem
#a um intervalo de números reais e que resultam de uma mensuração.

# ------------------------------------------------------------------------------
#Tabela de frequências
# ------------------------------------------------------------------------------
#Frequência ABSOLUTA
# ------------------------------------------------------------------------------

# Código original
# tPeso <- table(AMOSTRA$PESO)
tPeso <- table(round(AMOSTRA$PESO/1000, digits = 1))


#Como pode ser observado na tabela(REF. da tabela), existem poucas observaçoes
#iguais, isso deve-se ao fato da variável PESO ser contínua, e portanto para a
#construção da tabela de frequências precisamos ter o cuidado de dividirmos por
#classes(intervalos) da variável PESO.

#Cálculo dos extremos para ajudar na escolha da quantidade de classe(intervalos)
#e quais tamanhos de classes utilizaremos.
min(AMOSTRA$PESO)
max(AMOSTRA$PESO)

#Em Bussab, sugere-se a criação da tabela de frequências com uso de
#5 a 15 intervalos com mesma amplitude. Neste caso, adotou-se 10 classes com
#amplitude de 500, cada.

#cut - para criar as faixas
# tFreq <- table(cut(AMOSTRA$PESO, br=seq(0, 5000, 500)))
tFreq <-cut_interval(round(AMOSTRA$PESO/1000, digits = 1), n = 7)
tFreq

# ------------------------------------------------------------------------------
#Frequência RELATIVA
# ------------------------------------------------------------------------------
# Como estamos trabalhando com uma amostragem estratificada, o mais apropriado é
# gerar a tabela de frequências relativas


# Criando vetor com a variável PESO para dividirmos em classes.

classesPeso <- AMOSTRA[, 'PESO']

#Avaliando a quantidade de classe pelo método Sturges(k = 1 + 3,3log(n))

k=nclass.Sturges(AMOSTRA$PESO)
k

#Regra da Raiz Quadrada (k=sqrt(n))

k=sqrt(length(classesPeso))
k

#Regra da Potência de 2(Menor valor expoente da base 2 que tem resultado da
#potência menor que o tamanho da amostra)

#2^10=1024
#2^11=2048

k=10
k

# Gerando e nomeando os intervalos de classes

#Escolhi o método da potência para estabelecer a quantidade de classes.
#(valores ficaram arredondados e de fácil visualização e compreensão)

#tfreqRel <- prop.table(table(classesPeso1 <- cut(classesPeso,
#                         breaks=c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000),
#                         labels=c("0|-500", "500|-1000","1000|-1500","1500|-2000",
#                                  "2000|-2500", "2500|-3000", "3000|-3500",
#                                  "3500|-4000", "4000|-4500", "4500|-5000"),
#                         right=FALSE)))*100
tfreqRel <- prop.table(table(tFreq))
tfreqRel
#COMENTÁRIOS:
#labels - Nomes das classes
#Breaks - extremidades das classes
#right - False, indica que os intervalos estão abertos a direita.


#                 Transportar para EXCEL no formato padrão            #

# ------------------------------------------------------------------------------
#Histograma (Sem ggplot)
# ------------------------------------------------------------------------------

hist(AMOSTRA$PESO, xlab = "Peso",
     ylab = "Frequência",
     main = "Peso dos bebês nascidos em hospitais - Brasil - 2016",
     breaks = "Sturges",
     freq=FALSE,
     col="light blue" )
#     ylim = c(0, 1000),
#     xlim = c(0, 5500) ,
#     breaks = 8,



# ------------------------------------------------------------------------------
#Histograma (com ggplot)
# ------------------------------------------------------------------------------

#Histograma do PESO

#MODELO 1
ggplot(AMOSTRA) +
  aes(x = round(PESO/1000, digits = 1), fill = '') +
  scale_fill_brewer() +
  geom_histogram(bins = 7,
                 aes(y = ..density..),
                 show.legend = FALSE,
                 col = 'lightblue') +
  labs(x = 'Massa (kg)', y = NULL,
       title = 'Histograma do peso dos bebes nascidos em hospitais',
       subtitle = 'Registrados no Brasil em 2016',
       caption = 'Fonte: SINASC 2016') +
  stat_function(fun = dnorm, args = c(mean = mean(round(AMOSTRA$PESO/1000, digits = 1)),
                                      sd = sd(round(AMOSTRA$PESO/1000, digits = 1))))
  grafico$gravaEmDisco('q03-histogramaPesoHopitais')

# ------------------------------------------------------------------------------
#RAMO-E-FOLHAS
# ------------------------------------------------------------------------------
stem(AMOSTRA$PESO, scale = 1)
stem(AMOSTRA$PESO, scale = 10)
#Ramo-e-Folhas em duas escalas diferentes
# ------------------------------------------------------------------------------
#Análise:

# Com a distribuição de frequências e de Ramo-e-folhas obtém-se um resumo
# da variável Peso. A amplitude da amostra do Peso dos recém-nascido de 2016,
# está entre valores de 365g até 4800g, a maior concentração de informações está
# inserida na faixa  entre 3000 a 3500g. A seguir, analisaremos as informações
# das medidas Resumo do Peso dos nascidos vivos em 2016.

# ------------------------------------------------------------------------------
# MEDIDAS RESUMO
# ------------------------------------------------------------------------------
# MEDIDAS DE POSIÇÃO CENTRAL
# ------------------------------------------------------------------------------
#Média
resultMedia<-mean(AMOSTRA$PESO, trim=0, na.rm = FALSE)
#"trim" elimina, caso necessário, valores das extremidades.
#"na.rm" elimina valores NA's, que não é o caso desta variável(Se necessário na.rm=TRUE).

#Mediana
resultMediana<-median(AMOSTRA$PESO, na.rm = FALSE)
#"na.rm" elimina valores NA's, que não é o caso desta variável(Se necessário na.rm=TRUE).

#Moda
#Como o R não possui uma função nativa para o cálculo da Moda, criamos uma função no código
#que obtém o valor da Moda da variável PESO.

#Criando a Função
getmode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

#Atribuindo um vetor x

x <- AMOSTRA$PESO

#Cálculo da MODA pela função getmode
resultModa <- getmode(x)
cat(resultModa)

cat('\n  --> As medidas de posição central:\n')
cat('Média:',resultMedia)
cat('Mediana:',resultMediana)
cat('Moda:',resultModa)
cat('\n')

# ------------------------------------------------------------------------------
#MEDIDAS SEPARATRIZES
# ------------------------------------------------------------------------------

#Quartil
quantile(AMOSTRA$PESO)
#Decil
quantile(AMOSTRA$PESO, probs = seq(0, 1, 0.1))
#Percentil
quantile(AMOSTRA$PESO, probs = seq(0, 1, 0.01))
#Especificar
quantile(AMOSTRA$PESO, probs = 0.1)

# ------------------------------------------------------------------------------
#MEDIDAS DE DISPERSÃO
# ------------------------------------------------------------------------------

#Amplitude
range(AMOSTRA$PESO)
ampl <- max(AMOSTRA$PESO)-min(AMOSTRA$PESO)
ampl

#Desvio médio
DM <- sum(abs(AMOSTRA$PESO-median(AMOSTRA$PESO)))/length(AMOSTRA$PESO)
DM

#Desvio-padrão
Desv <- sd(AMOSTRA$PESO)
Desv

#Variância do Peso
varr <- var(AMOSTRA$PESO)
varr

# Coeficiente de variação
coefvar = Desv/resultMedia
coefvar

# Amplitude Interquartílica
#A diferença entre o terceiro e o primeiro quartis.

amplInt = quantile(AMOSTRA$PESO, probs = 0.75) - quantile(AMOSTRA$PESO, probs = 0.25)
amplInt

# ------------------------------------------------------------------------------
#MEDIDAS DE FORMA
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
#ASSIMETRIA
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
#Curtose
# ------------------------------------------------------------------------------

#ANÁLISE:

# As medidas de tendência central para os Pesos dos nascidos em 2016 apresentam
# valores bastante próximos, o que indica uma grande concentração da amostra na
# parte central dos dados.A mediana está levemente maior que a média, o que
# indica que as observações possuem uma assimetria a esquerda.

# ------------------------------------------------------------------------------
#BOXPLOT
# ------------------------------------------------------------------------------

boxplot(AMOSTRA$PESO,horizontal = TRUE, main="Peso dos recém-nascidos")



#DEVO RETIRAR OS DADOS DAS EXTREMIDADES??????

# ------------------------------------------------------------------------------
#
# ------------------------------------------------------------------------------
#Análise do PESO e SEXO
# ------------------------------------------------------------------------------

dados1 <- AMOSTRA[AMOSTRA$SEXO=="Masculino",]
dados2 <- AMOSTRA[AMOSTRA$SEXO=="Feminino",]

summary(dados1$PESO)
summary(dados2$PESO)

mean(dados1$PESO)
mean(dados2$PESO)

#require(tidyverse)

AMOSTRA %>%
  group_by(SEXO) %>%
  summarise(Media = mean(PESO), n = n(),
            Desvio_Padrao=sd(PESO),
            Mediana=quantile(PESO, 0.5) )


# ------------------------------------------------------------------------------
#DADOS A SEREM ANALISADOS
# ------------------------------------------------------------------------------
# Diagrama de dispers?o e Coeficiente de correla??o de Pearson

plot(dados$Idade,dados$Renda)

cor(dados$Idade,dados$Renda)

# Medidas descritivas para grupos

dados1<-dados[dados$Sexo=="Masculino",]
dados2<-dados[dados$Sexo=="Feminino",]

summary(dados1$Renda)
summary(dados2$Renda)

mean(dados1$Renda)
mean(dados2$Renda)

require(tidyverse)


dados %>%
  group_by(Sexo) %>%
  summarise(M?dia = mean(Renda), n = n(),
            Desvio_Padr?o=sd(Renda),
            Mediana=quantile(Renda, 0.5) )


ggplot(data = dados) +
  geom_histogram(mapping = aes(x = Renda), fill="blue", color="black")

ggplot(data = dados) +
  labs(x="Renda", y="Frequ?ncia relativa") +
  scale_x_discrete(limits = c(seq(0, 7, 0.5)))+
  geom_histogram(binwidth = 0.5,
                 mapping = aes(x = Renda, y=(..count..)/sum(..count..)), fill="blue", color="black")


ggplot(data = dados) +
  labs(x="Renda", y="Frequ?ncia relativa") +
  scale_x_discrete(limits = c(seq(0,7,0.5)))+
  geom_histogram(binwidth = 0.5,mapping = aes(x = Renda, y=(..count..)/sum(..count..)), fill="blue", color="black")+
  facet_wrap(~ Sexo, nrow = 2)

# Box-plot
ggplot(data = dados, mapping = aes(x = Sexo, y = Renda)) +
  geom_boxplot()

ggplot(data = dados, mapping = aes(x = Sexo, y = Renda)) +
  geom_boxplot() +
  coord_flip()


# Visualiza??o de duas variaveis quantitativa

# Diagrama de dispers?o
ggplot(data = dados) +
  geom_point(mapping = aes(x = Idade, y=Renda))

cor(dados$Idade, dados$Renda)


# com cores identificando grupos
ggplot(data = dados) +
  geom_point(mapping = aes(x = Idade, y=Renda, color = Sexo))



# dividindo em paineis
ggplot(data = dados) +
  geom_point(mapping = aes(x = Idade, y=Renda)) +
  facet_wrap(~ Sexo, nrow = 1)


# ------------------------------------------------------------------------------
#HISTOGRAMA DO BRUNO(Com ggplot)
# ------------------------------------------------------------------------------
#Colocar no padrão

#MODELO 1
ggplot(AMOSTRA) +
  aes(x = PESO) +
  geom_histogram(fill = 'lightblue',
                 col = 'black',
                 bins = 10,
                 alpha = 0.3,
                 aes(y=..density..)) +
  labs(title = 'Peso dos bebês nascidos em hospitais - Brasil - 2016',
       caption = 'Fonte: SISNAC XXX')
  stat_function(fun = dnorm, args = c(mean = mean(AMOSTRA$PESO),
                                      sd = sd(AMOSTRA$PESO)))


#MODELO2
ggplot(AMOSTRA) +
  aes(x = PESO) +
  geom_histogram(fill = 'lightblue',
                 col = 'black',
                 bins = 45,
                 alpha = 0.8,
                 aes(y=..density..)) +
  stat_function(fun = dnorm, args = c(mean = mean(AMOSTRA$PESO),
                                      sd = sd(AMOSTRA$PESO))) +
  labs(title = 'Peso dos bebês nascidos em hospitais - Brasil - 2016',
       caption = 'Fonte: SISNAC XXX')+
  geom_vline(aes(xintercept=mean(PESO,na.rm = T), color = 'mean'),
             show.legend = T )+
  geom_vline(aes(xintercept=median(PESO,na.rm = T), color='median'),
             show.legend = T)






