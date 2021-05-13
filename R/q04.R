# 4. Existe relação entre o peso do recém-nascido e idade da mãe?
#    A relação é forte?
# ----------------------------------------------------------------------------

# Importes para a solução da questão
grafico <- modules::use('R/grafico.R')
suppressMessages(suppressWarnings(suppressPackageStartupMessages(
  import('ggplot2'))))

# Exporta função
export('resposta')

# Define função de resposta
resposta <- function(df, Q01) {

  grafico$msgB('Produzindo gráficos para responder à questão...')
  questao <- 'q04'

  # box-plot parto/peso ----
  titulo <- 'Tipos de partos e peso'
  ggplot(df[!is.na(df$PARTO), ], aes(x = PARTO, y = PESO, fill = '')) +
    geom_boxplot(show.legend = FALSE) +
    scale_fill_brewer() +
    labs(x = '', y = '',
         title = titulo) +
    grafico$gravaEmDisco(questao, titulo)

  # box-plot esc/peso ----
  titulo <- 'Escolaridade da mãe e peso'
  ggplot(df[!is.na(df$ESCMAE) & df$ESCMAE != 'Ignorado', ],
         aes(x = ESCMAE, y = PESO, fill = '')) +
    geom_boxplot(show.legend = FALSE) +
    scale_fill_brewer() +
    labs(x = '', y = '',
         title = titulo) +
    grafico$gravaEmDisco(questao, titulo, largura = 15)

  # box-plot idade/peso ----
  titulo <- 'Idade da mãe e peso'
  ggplot(Q01[!is.na(Q01$GIDADE), ], aes(x = GIDADE, y = PESO, fill = '')) +
    geom_boxplot(show.legend = FALSE) +
    scale_fill_brewer() +
    labs(x = '', y = '',
         title = titulo) +
    grafico$gravaEmDisco(questao, titulo, largura = 15)

  # box-plot hora/peso ----
  titulo <- 'Hora de nascimento e peso'
  ggplot(Q01[!is.na(Q01$GHORANASC), ], aes(x = GHORANASC, y = PESO, fill = '')) +
    geom_boxplot(show.legend = FALSE) +
    scale_fill_brewer() +
    labs(x = '', y = '',
         title = titulo) +
    grafico$gravaEmDisco(questao, titulo, largura = 15)

  # box-plot estciv/peso ----
  titulo <- 'Estado civil e peso'
  ggplot(Q01[!is.na(Q01$ESTCIVMAE), ], aes(x = ESTCIVMAE, y = PESO, fill = '')) +
    geom_boxplot(show.legend = FALSE) +
    scale_fill_brewer() +
    labs(x = '', y = '',
         title = titulo) +
    grafico$gravaEmDisco(questao, titulo, largura = 15)

  # box-plot dia/peso ----
  titulo <- 'Dia e peso'
  ggplot(Q01[!is.na(Q01$DIA), ], aes(x = DIA, y = PESO, fill = '')) +
    geom_boxplot(show.legend = FALSE) +
    scale_fill_brewer() +
    labs(x = '', y = '',
         title = titulo) +
    grafico$gravaEmDisco(questao, titulo, largura = 15)

  # box-plot tdia/peso ----
  titulo <- 'Tipo dia e peso'
  ggplot(Q01[!is.na(Q01$CDIA), ], aes(x = CDIA, y = PESO, fill = '')) +
    geom_boxplot(show.legend = FALSE) +
    scale_fill_brewer() +
    labs(x = '', y = '',
         title = titulo) +
    grafico$gravaEmDisco(questao, titulo, largura = 15)

  # box-plot gestacao/peso ----
  titulo <- 'Tempo de gestação e peso'
  ggplot(df[!is.na(df$GESTACAO) & df$GESTACAO != 'Ignorado', ],
         aes(x = GESTACAO, y = PESO, fill = '')) +
    geom_boxplot(show.legend = FALSE) +
    scale_fill_brewer() +
    labs(x = '', y = '',
         title = titulo) +
    grafico$gravaEmDisco(questao, titulo, largura = 20)

  # box-plot gravidez/peso ----
  titulo <- 'Tipo de gravidez e peso'
  ggplot(df[!is.na(df$GRAVIDEZ), ], aes(x = GRAVIDEZ, y = PESO, fill = '')) +
    geom_boxplot(show.legend = FALSE) +
    scale_fill_brewer() +
    labs(x = '', y = '',
         title = titulo) +
    grafico$gravaEmDisco(questao, titulo, largura = 10)

  # box-plot raça/peso ----
  titulo <- 'Cor e peso'
  ggplot(df[!is.na(df$RACACOR), ], aes(x = RACACOR, y = PESO, fill = '')) +
    geom_boxplot(show.legend = FALSE) +
    scale_fill_brewer() +
    labs(x = '', y = '',
         title = titulo) +
    grafico$gravaEmDisco(questao, titulo, largura = 10)
}
