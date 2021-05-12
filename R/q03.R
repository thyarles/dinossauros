# 3. Descrever a variável peso do recém-nascidos da amostra.
# ----------------------------------------------------------------------------

# Importes para a solução da questão
grafico <- modules::use('R/grafico.R')
suppressMessages(suppressWarnings(suppressPackageStartupMessages(
      import('ggplot2'))))
suppressMessages(suppressWarnings(suppressPackageStartupMessages(
  import('stats'))))

# Exporta função
export('resposta')

# Define função de resposta
resposta <- function(df) {

  # Define questão para salvar gráficos na pasta certa
  questao <- 'q03'

  # Med resumos ----
  tPeso <- paste(max(prop.table(table(df$PESO))*100), '%', sep = '')
  minPeso <- paste(round(min(df$PESO), digits = 2), 'gramas')
  maxPeso <- paste(round(max(df$PESO), digits = 2), 'gramas')
  grafico$msgB(paste('Freq máxima..:', tPeso))
  grafico$msgB(paste('Menor peso...:', minPeso))
  grafico$msgB(paste('Maior peso...:', maxPeso))

  # Faixas ----
 # tFreq <- table(cut(df$PESO, br=seq(0, 5000, 500), dig.lab = 4))
  tFreq <- table(cut(df$PESO, br=seq(250, 5000, 250), dig.lab = 4))
  grafico$msgB('Faixas')
  print(tFreq)

  # freq relativa ----

  # Criando vetor com a variável PESO para dividirmos em classes.
  classesPeso <- df[, 'PESO']

  # Avaliando a quantidade de classe pelo método Sturges(k = 1 + 3,3log(n))
  # k <- nclass.Sturges(df$PESO)

  # Regra da Raiz Quadrada (k=sqrt(n))
  # k <- sqrt(length(classesPeso))

  # Regra da Potência de 2(Menor valor expoente da base 2 que tem resultado da
  # potência menor que o tamanho da amostra)
  # 2^10=1024
  # 2^11=2048

  k <- 19
  grafico$msgB(paste('Número de classes para a variável PESO:', k))

  # Gerando e nomeando os intervalos de classes
  tfreqRel <- prop.table(table(tFreq))
  grafico$msgB('Frequência relativa')
  print(tfreqRel)

  # Histograma ----
  titulo <- 'Histograma do peso dos nascidos em hospitais'
  ggplot(df) +
    aes(x = PESO, fill = '') +
    scale_fill_brewer() +
    scale_y_continuous(labels = NULL) +
    geom_histogram(bins = 19,
                   aes(y = ..density..),
                   show.legend = FALSE,
                   col = 'lightblue') +
    labs(x = 'Massa (gramas)', y = 'Densidade',
         title = titulo,
         # subtitle = 'Registrados no Brasil em 2016',
         caption = 'Fonte: SINASC 2016') +
    stat_function(fun = dnorm, args = c(mean = mean(df$PESO),
                                        sd = sd(df$PESO)))
  grafico$gravaEmDisco(questao, titulo, altura = 7)

  # ramos e folhas ----
  # stem(df$PESO, scale = 1)
  # stem(df$PESO, scale = 10)

  # medidas-resumo

  # Média
  resultMedia <- mean(df$PESO, trim=0, na.rm = FALSE)
  grafico$msgB(paste('Medidas de posição central - MÉDIA dos pesos:', resultMedia))

  # Mediana
  resultMediana<-median(df$PESO, na.rm = FALSE)
  grafico$msgB(paste('Medidas de posição central - MEDIANA dos pesos:', resultMediana))

  # Moda
  getmode <- function(x) {
    uniqx <- unique(x)
    uniqx[which.max(tabulate(match(x, uniqx)))]
  }
  x <- df$PESO
  resultModa <- getmode(x)
  grafico$msgB(paste('Medidas de posição central - MODA dos pesos:', resultModa))


  # medidas separatrizes ----
  # Quartil
  grafico$msgB('Quartil dos pesos\n')
  print(quantile(df$PESO))
  # Decil
  grafico$msgB('Decil dos pesos\n')
  print(quantile(df$PESO, probs = seq(0, 1, 0.1)))
  # Percentil
  # grafico$msgB('Percentil dos pesos')
  # quantile(df$PESO, probs = seq(0, 1, 0.01))
  # Especificar
  # quantile(df$PESO, probs = 0.1)

  # medidas de dispersao ----

  # Amplitude
  # range(df$PESO)
  ampl <- max(df$PESO)-min(df$PESO)
  grafico$msgB(paste('Amplitude do peso:', ampl))

  # Desvio médio
  DM <- sum(abs(df$PESO-median(df$PESO)))/length(df$PESO)
  grafico$msgB(paste('Desvio médio do peso:', round(DM, digits = 2)))

  # Desvio-padrão
  Desv <- sd(df$PESO)
  grafico$msgB(paste('Desvio padrão do peso:', round(Desv, digits = 2)))

  # Variância do Peso
  varr <- var(df$PESO)
  grafico$msgB(paste('Variãncia do peso:', round(varr, digits = 2)))

  # Coeficiente de variação
  coefvar = Desv/resultMedia
  grafico$msgB(paste('Coeficiente de variação do peso:', round(coefvar, digits = 2)))

  # Amplitude Interquartílica
  # A diferença entre o terceiro e o primeiro quartis.
  amplInt = quantile(df$PESO, probs = 0.75) - quantile(df$PESO, probs = 0.25)
  grafico$msgB(paste('Aplitude interquartílica:', round(amplInt, digits = 2)))

  # BOXPLOT ----
  #boxplot(df$PESO,horizontal = TRUE, main="Peso dos recém-nascidos")
  titulo <- 'Peso dos recém-nascidos em hospitais'
  # Geração do gráfico com o número de partos por horas
  ggplot(df[!is.na(df$PARTO), ], aes(x = PESO, y = PARTO, fill = '')) +
    # Gráfico tipo boxplot
    geom_boxplot(show.legend = FALSE) +
    # Escala de cor leve
    scale_fill_brewer() +
    # Nomes dos eixos, título e subtítulo
    labs(x = '', y = '',
         title = titulo,
         # subtitle = 'Registrados no Brasil em 2016',
         caption = 'Fonte: SINASC 2016') +
    # Retira título da legenda e posiciona no topo
    theme(legend.title = element_blank(), legend.position = "top",
          strip.background = element_blank())
  # Grava figura em disco para uso no Word (veja no diretório png)
  grafico$gravaEmDisco(questao, titulo, escala = 1, altura = 5, largura = 12)
}
