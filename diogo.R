library('ggplot2')
library('cowplot')


#3)Descrever a variável peso do recém-nascidos da amostra.

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

tPeso <- table(AMOSTRA$PESO)
print(tPeso)


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
tFreq <- table(cut(AMOSTRA$PESO, br=seq(0,5000, 500)))
tFreq
cat(tFreq)

# ------------------------------------------------------------------------------
#Frequência RELATIVA
# ------------------------------------------------------------------------------
# Como estamos trabalhando com uma amostragem estratificada, o mais apropriado é
# gerar a tabela de frequências relativas


# Criando vetor com a variável PESO para dividirmos em classes.

classesPeso <- AMOSTRA[, c(23)]

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

tfreqRel <- prop.table(table(classesPeso1 <- cut(classesPeso,
                         breaks=c(0,500,1000,1500,2000,2500,3000,3500,4000,4500,5000),
                         labels=c("0|-500", "500|-1000","1000|-1500","1500|-2000",
                                  "2000|-2500", "2500|-3000", "3000|-3500",
                                  "3500|-4000", "4000|-4500", "4500|-5000"),
                         right=FALSE)))*100
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
#BOXPLOT (UMA VARIÁVEL) sem ggplot
# ------------------------------------------------------------------------------

boxplot(AMOSTRA$PESO,
        horizontal = TRUE,
        main="Peso dos recém-nascidos",
        names=c("Peso"),
        xlabel="Peso")

# ------------------------------------------------------------------------------
#BOXPLOT (UMA VARIÁVEL)com ggplot
# ------------------------------------------------------------------------------

box1 <- ggplot(AMOSTRA, aes(x = PESO, y = "")) +
  geom_boxplot()

box2 <- ggplot(AMOSTRA, aes(x = PESO, y = "")) +
  geom_boxplot(width = 0.5, col = "black", fill = "gray")+ # boxplot
  # mostra a média por um ponto
  stat_summary(geom = "point", fun = mean) +
  # adiciona uma linha na média geral
  geom_hline(yintercept = resultMedia, linetype = "dashed")+
  labs(x = "Peso", y = "")

#Só imprimir o segundo modelo

plot_grid(box2, labels = c(""))



# ------------------------------------------------------------------------------
#PESO e SEXO
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

#   Criar uma tabela com as medidas resumo  #

# ------------------------------------------------------------------------------
#BOXPLOT com duas variáveis (Peso e sexo)
# ------------------------------------------------------------------------------

boxplot(dados1$PESO,dados2$PESO,
        horizontal = TRUE,
        main="Peso dos recém-nascidos",
        names=c("Masculino","Feminino"))
        #xlabel="Peso")
# ------------------------------------------------------------------------------
#ANÁLISE:
# A medidas resumo da variável PESO dividida por Gênero, mostra que crianças
# do sexo masculino tiveram uma maior quabtidade de nascimentos e valores levemente
# maiores para a as medidas de Média, mediana e Moda. Esse resultado é
# perfeitamente aceitável e de conhecimento comum na comunidade. (REFERÊNCIAS)






















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


ggplot(data = dados1) +
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
ggplot(data = dados1) +
  geom_point(mapping = aes(x = PESO, y=""))

# UTILIZAR NA QUESTÃO 4

cor(AMOSTRA$PESO, AMOSTRA$IDADEMAE)


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
                 bins = 43,
                 alpha = 0.8,
                 aes(y=..density..)) +
  stat_function(fun = dnorm, args = c(mean = mean(AMOSTRA$PESO),
                                      sd = sd(AMOSTRA$PESO))) +
  labs(title = 'Peso dos bebês nascidos em hospitais - Brasil - 2016',
       caption = 'Fonte: SISNAC XXX')+
  geom_vline(aes(xintercept=mean(PESO,na.rm = T), color = 'média'),
             show.legend = T )+
  geom_vline(aes(xintercept=median(PESO,na.rm = T), color='mediana'),
             show.legend = T)






