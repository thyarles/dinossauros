#3)Descrever a variável peso do recém-nascidos da amostra.

#A variável "peso" é uma variável quantitativa contínua. Bussab a define como sendo
#valores "cujos possíveis valor pertencem a um intervalo de números reais e que
# resultam de uma mensuração, como por exemplo estatura e peso
#(melhor seria dizer massa) de um indivíduo.


#peso <- table(AMOSTRA$PESO)
barplot(AMOSTRA$PESO)
#str(peso)
#str(AMOSTRA$PESO)
prop.table(AMOSTRA$PESO)   # outra forma de obter freq. rel.
view(AMOSTRA$PESO)

#Tabela de frequências

min(AMOSTRA$PESO)
max(AMOSTRA$PESO)

table(cut(AMOSTRA$PESO, br=seq(0,5500, 500)))


#Histograma
hist(AMOSTRA$PESO, xlab = "Peso (g)",
     ylab = "Frequência",
     main = "Histograma do Peso dos Recém-nascidos", ylim = c(0, 1000),
     xlim = c(0, 6000) , breaks = 8)

# **  Ramos e Folhas  **
stem(AMOSTRA$PESO, scale = 1)
args(stem)

boxplot(AMOSTRA$PESO)

#############################Tabelas
tpeso <- table(AMOSTRA$PESO)
prop.table(AMOSTRA$PESO)
#prop.table(tpeso)
addmargins(tpeso)
prop.table(tpeso)


#Medidas resumo
summary(AMOSTRA$PESO)

#Analisando o Peso pelo sexo
dados1 <- AMOSTRA[AMOSTRA$SEXO=="Masculino",]
dados2 <- AMOSTRA[AMOSTRA$SEXO=="Feminino",]

summary(dados1$PESO)
summary(dados2$PESO)

mean(dados1$PESO)
mean(dados2$PESO)

require(tidyverse)

AMOSTRA %>%
  group_by(SEXO) %>%
  summarise(Media = mean(PESO), n = n(),
            Desvio_Padrao=sd(PESO),
            Mediana=quantile(PESO, 0.5) )



#######Começar o estudo para gráficos ggplot












# ** Colocando rotulos na tabela - alternativa
# Informa os nomes das linhas e das colunas das tabelas
rownames(tabela0)<-c("M","F")
colnames(tabela0)<-c("Fundamental Incompleto", "Fundamental Completo", "Medio")
tabela0

# ** Trocando a posicao das variavel na tabela
tabela2 <- table(dados$g_Instru,dados$Sexo)
tabela2


#   Totais marginais (junto com a tabela)
addmargins(tpeso)

#   Total marginal  (sem a tabela)
margin.table(tpeso, 1)   # linha

# Alternativa xtabs
tabela3<-xtabs (~ dados$g_Instru+dados$Sexo, data=dados)
tabela3

addmargins(tabela3)
prop.table(tabela3,1)   # em relacaao ao total da linha

