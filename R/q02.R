# 2. Qual é o percentual de mães solteiras?
#    Descrever a variável estado civil das mães.
# ----------------------------------------------------------------------------

# Importes para a solução da questão
grafico <- modules::use('R/grafico.R')
suppressMessages(suppressWarnings(suppressPackageStartupMessages(
      import('ggplot2'))))

# Exporta função
export('resposta')

# Define função de resposta (rode df <- AMOSTRA para testar local)
resposta <- function(df) {

  # Define questão para salvar gráficos na pasta certa
  questao <- 'q02'

  # Cria novo atributo sem fatores
  df$ECA <- as.integer(df$ESTCIVMAE)

  # Unindo Viúva, Divorciada, Ignorado (Estado Civil Agrupado = ECA) no cód. 99
  df$ECA[df$ECA %in% c(3, 4, 6)] <- 99

  # Troca NAs por 99
  df$ECA[is.na(df$ECA)] <- 99

  # Reaplica fator
  df$ECA <- factor(df$ECA, levels = c(1, 2, 5, 99), labels = c('Solteira',
                                                               'Casada',
                                                               'União estável',
                                                               'Outros'))
  # Cria faixas de idade
  df$FAIXA <- cut(df$IDADEMAE,
                      breaks=c(13, 19, 24, 29, 34, 39, 44, 47),
                      labels=c("13 |- 18", "18 |- 23","23 |- 28","28 |- 33",
                               "33 |- 38", "38 |- 43", "43 |- 47"),
                      right=FALSE)

  # Plota gráfico
  titulo <- 'Percentual das mães por estado civil'
  ggplot(df, aes(x = ECA, fill='')) +
    # Aplica percentual calculado via GGPLOT
    geom_bar(aes(y = (..count..)/sum(..count..)), show.legend = FALSE) +
    # Aplica o tema de cores
    scale_fill_brewer() +
    # Troca densidade numérica pelo percentual
    scale_y_continuous(labels = scales::percent) +
    # Nomes dos eixos, título e subtítulo
    labs(x = NULL, y = NULL,
               title = titulo,
               # subtitle = 'Registrados no Brasil em 2016',
               caption = 'Fonte: SINASC 2016')
    # Grava figura em disco para uso no Word (veja no diretório png)
    grafico$gravaEmDisco(questao, titulo, altura = 6, largura = 10)

    # Plota gráfico
    titulo <- 'Percentual do estado civil das mães por faixa etária'
    ggplot(df, aes(x = FAIXA, fill=ECA)) +
      # Aplica percentual calculado via GGPLOT
      geom_bar(position = 'fill', aes(y = (..count..)/sum(..count..))) +
      # Aplica o tema de cores
      scale_fill_brewer() +
      # Adiciona percentual para ficar "humanizado"
      scale_y_continuous(labels = scales::percent) +
      # Nomes dos eixos, título e subtítulo
      labs(x = 'Faixa etária (anos)', y = NULL,
           title = titulo,

           # subtitle = 'Registrados no Brasil em 2016',
           caption = 'Fonte: SINASC 2016') +
      theme(legend.title = element_blank(), legend.position = "top")
      grafico$gravaEmDisco(questao, titulo, altura = 10)
}

# gerando tabela para cálculo da correlação de contingência
tab=table(df$FAIXA, df$ECA)

#definindo como matriz
tabmax=as.matrix(tab)


#Usamos a função chisq.test para obter o valor de qui-quadrado
# Com o valor de qui-quadrado, podemos obter o coeficiente de contingência
C_test <- (chisq.test(tabmax, correct = F))
X_Squared_eca <- C_test$statistic
C_p_i = (X_Squared_eca /(X_Squared_eca + 2000))**(1/2)

grafico$msgB(paste('CPI:', C_p_i))
grafico$msgB(paste('C* para as variáveis estado civil e idade:',
                   as.character(C_p_i/((1/2)^(1/2)))))
