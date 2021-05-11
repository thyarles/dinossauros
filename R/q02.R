# 2. Qual é o percentual de mães solteiras?
#    Descrever a variável estado civil das mães.
# ----------------------------------------------------------------------------

# Importes para a solução da questão
grafico <- modules::use('R/grafico.R')
suppressMessages(
  suppressWarnings(
    suppressPackageStartupMessages(
      import('ggplot2')
    )
  )
)

# Exporta função
export('resposta')



# Define função de resposta
resposta <- function(df) {

# Cria cópia da AMOSTRA para trabalhar local
df <- AMOSTRA

# Cria novo atributo sem fatores
df$ECA <- as.integer(AMOSTRA$ESTCIVMAE)

# Unindo Viúva, Divorciada, Ignorado (Estado Civil Agrupado = ECA) no cód. 99
df$ECA[df$ECA %in% c(3, 4, 6)] <- 99

# Troca NAs por 99
df$ECA[is.na(df$ECA)] <- 99

# Reaplica fator
df$ECA <- factor(df$ECA, levels = c(1, 2, 5, 99), labels = c('Solteira',
                                                                 'Casada',
                                                                 'União estável',
                                                                 'Outros'))
# Criando as faixas no df
df$FAIXA <- cut(df$IDADEMAE,
                    breaks=c(13,20, 25, 30, 35, 40, 47),
                    labels=c("13 |- 20", "20 |- 25","25 |- 30","30 |- 35",
                             "35 |- 40", "40 |- 46"),
                    right=FALSE)

# Plota gráfico
ggplot(df, aes(x = ECA, fill='')) +

# Aplica percentual calculado via GGPLOT
geom_bar(aes(y = (..count..)/sum(..count..)), show.legend = FALSE) +

# Aplica o tema de cores
scale_fill_brewer() +

# Adiciona percentual para ficar "humanizado"
scale_y_continuous(labels = scales::percent) +

# Nomes dos eixos, título e subtítulo
labs(x = 'Estado civil', y = NULL,
           title = 'Percentual do estado civil das mães',
           subtitle = 'Registrados no Brasil em 2016',
           caption = 'Fonte: SINASC 2016')

# Grava figura em disco para uso no Word (veja no diretório png)
grafico$gravaEmDisco('q02-estCivilPercentual')


pie(prop.table(tabestcivmae), col = c(1, 2, 3, 4, 5, 6), clockwise = TRUE)



# Gerando gráfico 2

    plot(estcivilmaefaixa)

    # Versão com GGPLOT
    ggplot(df, aes(x = FAIXA, fill=ECA)) +
      # Aplica percentual calculado via GGPLOT
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      # Aplica o tema de cores
      scale_fill_brewer() +
      # Adiciona percentual para ficar "humanizado"
      scale_y_continuous(labels = scales::percent) +
      # Nomes dos eixos, título e subtítulo
      labs(x = 'Faixa etária', y = NULL,
           title = 'Percentual do estado civil das mães por faixa etária',
           subtitle = 'Registrados no Brasil em 2016',
           caption = 'Fonte: SINASC 2016') +
      theme(legend.title = element_blank(), legend.position = "top")

    # Grava figura em disco para uso no Word (veja no diretório png)
    grafico$gravaEmDisco('q02-estCivilFaixaEtaria')


    # Grava figura em disco para uso no Word (veja no diretório png)
    grafico$gravaEmDisco('q02-estCivilFaixaEtariaSugestao')



