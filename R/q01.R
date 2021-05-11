# 1. Pode-se dizer que o número de partos varia entre os dias da semana?
#    Por que?
# ----------------------------------------------------------------------------

# Importes para a solução da questão com supressão de mensagens para deixar
# a saída mais limpa
suppressMessages(suppressWarnings(suppressPackageStartupMessages(
  grafico <- modules::use('R/grafico.R'))))
suppressMessages(suppressWarnings(suppressPackageStartupMessages(
  import('lubridate'))))
suppressMessages(suppressWarnings(suppressPackageStartupMessages(
  import('ggplot2'))))

# Exporta função
export('resposta')

# Define função de resposta
# Apenas para testar: df <- AMOSTRA
resposta <- function(df) {

  grafico$msgB('Criando data frames para gerar tabelas e gráficos...')

  # Redefine dataframe com tipo de parto por Dia da Semana sem NAs
  df <- data.frame(wday(df$DTNASC[!is.na(df$PARTO)]),
                        df$PARTO[!is.na(df$PARTO)],
                        df$ESCMAE[!is.na(df$PARTO)],
                        df$IDADEMAE[!is.na(df$PARTO)],
                        df$ESTCIVMAE[!is.na(df$PARTO)],
                        df$HORANASC[!is.na(df$PARTO)],
                        df$PESO[!is.na(df$PARTO)])

  # nome colunas ----
  # Ajuste no nome das colunas
  colnames(df) = c('DIA', 'PARTO', 'ESCMAE', 'IDADEMAE', 'ESTCIVMAE', 'HORANASC', 'PESO')

  # classe idades ----
  # Agrupamento das idades por classes
  df$GIDADE <- cut(df$IDADEMAE, breaks = c(13, 18, 28, 38, 48),
                                labels = c('13 a 17 anos',
                                           '18 a 27 anos',
                                           '28 a 37 anos',
                                           '38 a 48 anos'))

  # agrup est civil ----
  # Reagrupamento do estado civil Outros (divorciada, viúva, ignorado e NAs)
  df$ESTCIVMAE <- as.integer(df$ESTCIVMAE)
  df$ESTCIVMAE[df$ESTCIVMAE %in% c(3, 4, 6)] <- 99
  df$ESTCIVMAE[is.na(df$ESTCIVMAE)] <- 99
  df$ESTCIVMAE <- factor(df$ESTCIVMAE, levels = c(1, 2, 5, 99),
                         labels = c('Solteira', 'Casada', 'União estável', 'Outros'))

  # classe dias ----
  # Criação de dataframes para agrupamento dias úteis / fim de semana (CDIA)
  df$CDIA[df$DIA %in% c(1, 7)] <- 0  # Fins de semana
  df$CDIA[df$DIA %in% c(2: 6)] <- 1  # Dias úteis
  df$CDIA <- factor(df$CDIA, levels = 0:1, labels = c('Fim de semana',
                                                      'Dia útil'))

  # Configuração dos fatores para melhor idenbtificação dos dias da semana
  df$DIA <- factor(df$DIA, levels = 1:7, labels = c('Dom.', 'Seg.', 'Ter.',
                                                    'Qua.', 'Qui.', 'Sex.', 'Sáb.'))


  # classe pesos ----
  # Agrupamento dos pesos por classes
  df$PESO <- round(df$PESO/1000, digits = 1)
  df$GPESO <- cut(df$PESO, breaks = c(0.4, 1.5, 2, 2.5, 3, 3.5, 4.8),
                           labels = c('0,4 a 1,4',
                                      '1,5 a 1,9',
                                      '2,0 a 2,4',
                                      '2,5 a 2,9',
                                      '3,0 a 3,4',
                                      '3,5 a 4,8'))

  # classe horas ----
  # Agrupamento das horas de nascimento
  #7h |- 13h, 13h |- 19h, 19h |- 1h, 1h |- 7h
  df$GHORANASC <- as.integer(substr(df$HORANASC, start = 0, stop = 2))
  df$GHORANASC <- cut(df$GHORANASC, breaks = c(0, 6, 12, 18, 23),
                                    labels = c('0 às 6:59', '7 às 12:59',
                                               '13 às 18:59', '19 às 23:59'),
                                    include.lowest = TRUE)

  # g parto/dia ----
  # Geração do gráfico com o número de partos por dia da semana
  ggplot(df, aes(x = DIA, fill = PARTO)) +
    # Gráfico tipo barras
    geom_bar(position="dodge", show.legend = FALSE) +
    # Separa por tipo de parto para visualizar diferenças
    facet_grid(PARTO~ .) +
    # Escala de cor leve
    scale_fill_brewer() +
    # Nomes dos eixos, título e subtítulo
    labs(x = 'Dias da semana', y = 'N° partos',
         title = 'Número de partos por tipo/dia da semana',
         subtitle = 'Registrados no Brasil em 2016',
         caption = 'Fonte: SINASC 2016') +
    # Retira título da legenda e posiciona no topo
    theme(legend.title = element_blank(), legend.position = "top",
          strip.background = element_blank(),
          strip.text.x = element_blank()
        )
    # Grava figura em disco para uso no Word (veja no diretório png)
    grafico$gravaEmDisco('q01-partosDiaSemana')

    # g parto/diau ----
    # Geração do gráfico com o número de partos por dias úteis/fds
    ggplot(df, aes(x = CDIA, fill = DIA)) +
      # Gráfico tipo barras
      geom_bar(position="stack") +
      # Escala de cor leve
      scale_fill_brewer() +
      # Nomes dos eixos, título e subtítulo
      labs(x = '', y = 'N° partos',
           title = 'Número de partos por tipo/dias úteis',
           subtitle = 'Registrados no Brasil em 2016',
           caption = 'Fonte: SINASC 2016') +
      # Retira título da legenda e posiciona no topo
      theme(legend.title = element_blank())
    # Grava figura em disco para uso no Word (veja no diretório png)
    grafico$gravaEmDisco('q01-partosDiasUteis')

    # g parto/hora ----
    # Geração do gráfico com o número de partos por horas
    ggplot(df[!is.na(df$GHORANASC), ], aes(x = GHORANASC, fill = PARTO)) +
      # Gráfico tipo barras
      geom_bar(position="dodge", show.legend = FALSE) +
      # Separa por tipo de parto para visualizar diferenças
      facet_grid(PARTO~ .) +
      # Escala de cor leve
      scale_fill_brewer() +
      # Nomes dos eixos, título e subtítulo
      labs(x = 'Horário', y = 'N° partos',
           title = 'Número de partos por hora do dia',
           subtitle = 'Registrados no Brasil em 2016',
           caption = 'Fonte: SINASC 2016') +
      # Retira título da legenda e posiciona no topo
      theme(legend.title = element_blank(), legend.position = "top",
            strip.background = element_blank(),
            strip.text.x = element_blank()
      )
    # Grava figura em disco para uso no Word (veja no diretório png)
    grafico$gravaEmDisco('q01-partosHora')

    # g parto/t/i/e ----
    # Geração do gráfico com o número de partos por horas
    ggplot(df[!is.na(df$GIDADE), ],
      aes(x = DIA, fill = GIDADE)) +
      # Gráfico tipo barras
      geom_bar(position="stack", na.rm = TRUE) +
      facet_grid(PARTO~ .) +
      # Escala de cor leve
      scale_fill_brewer() +
      # Nomes dos eixos, título e subtítulo
      labs(x = 'Dia da semana', y = 'N° partos',
           title = 'Partos por tipo, idade e dia',
           subtitle = 'Registrados no Brasil em 2016',
           caption = 'Fonte: SINASC 2016') +
      # Retira título da legenda e posiciona no topo
      theme(legend.title = element_blank(), legend.position = "top",
            strip.background = element_blank(),
            strip.text.x = element_blank()
      )
    # Grava figura em disco para uso no Word (veja no diretório png)
    grafico$gravaEmDisco('q01-partoTipoIdadeEstCiv')

    # g pxxx ----
    # Geração do gráfico com o número de partos por horas
    ggplot(df[df$ESTCIVMAE != 'Outros' & !is.na(df$GHORANASC), ],
           aes(x = DIA, fill = PARTO)) +
      # Gráfico tipo barras
      geom_bar(position = "fill") +
      # Escala de cor leve
      scale_fill_brewer() +
      # Adiciona Grupo de hora e estado civil da mãe
      facet_grid(vars(GHORANASC), vars(ESTCIVMAE)) +
      # Troca densidade por percentual
      scale_y_continuous(labels = scales::percent) +
      # Nomes dos eixos, título e subtítulo
      labs(x = 'Dias da semana', y = '',
           title = 'Partos por tipo, idade, dia da semana e estado civil',
           subtitle = 'Registrados no Brasil em 2016',
           caption = 'Fonte: SINASC 2016') +
      # Retira título da legenda e posiciona no topo
      theme(legend.title = element_blank(), legend.position = "top",
            strip.background = element_blank())
    # Grava figura em disco para uso no Word (veja no diretório png)
    grafico$gravaEmDisco('q01-partoTipoIdadeSemanaCivil')

    # Impressão das tabelas ----
    grafico$mTab('Q01', 'Parto por dia da semana',
                 table(df$DIA, df$PARTO, exclude = 'Ignorado', useNA = NULL))
    grafico$mTab('Q01', 'Parto por tipo de dia na semana',
                 table(df$CDIA, df$PARTO, exclude = 'Ignorado', useNA = NULL))
    grafico$mTab('Q01', 'Parto por hora',
                 table(df$GHORANASC, df$PARTO, exclude = 'Ignorado', useNA = NULL))
    grafico$mTab('Q01', 'Parto por idade',
                 table(df$GIDADE, df$PARTO, exclude = 'Ignorado', useNA = NULL))
    grafico$mTab('Q01', 'Parto por Peso',
                 table(df$GPESO, df$PARTO, exclude = 'Ignorado', useNA = NULL))
    grafico$mTab('Q01', 'Parto por Estado civil da mãe',
                 table(df$ESTCIVMAE, df$PARTO, exclude = 'Ignorado', useNA = NULL))
    grafico$mTab('Q01', 'Parto por Escolariadade da mãe',
                 table(df$ESCMAE, df$PARTO, exclude = 'Ignorado', useNA = NULL))

}
