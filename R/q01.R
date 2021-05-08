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
resposta <- function(df) {

  grafico$msgB('Criando data frames para gerar tabelas e gráficos...')

  # Criação de dataframe com tipo de parto por Dia da Semana (PDS) sem NAs
  PDS <- data.frame(wday(df$DTNASC[!is.na(df$PARTO)]),
                    df$PARTO[!is.na(df$PARTO)])

  # Ajuste no nome das colunas
  colnames(PDS) = c('DIA', 'PARTO')

  # Criação de dataframes para agrupamento dias úteis / fim de semana (PDG)
  PDG <- PDS
  PDG$DIA[PDG$DIA %in% c(1, 7)] <- 0  # Fins de semana
  PDG$DIA[PDG$DIA %in% c(2: 6)] <- 1  # Dias úteis

  # Configuração dos fatores para melhor idenbtificação dos dias da semana
  PDS$DIA <- factor(PDS$DIA, levels = 1:7, labels = c('Dom.', 'Seg.', 'Ter.',
                                                      'Qua.', 'Qui.', 'Sex.', 'Sáb.'))
  # Configuração dos fatores para melhor idenbtificação de dias úteis / fds
  PDG$DIA <- factor(PDG$DIA, levels = 0:1, labels = c('Fim de semana',
                                                      'Dia útil'))

  # Geração do gráfico com o número de partos por dia da semana
  ggplot(as.data.frame(PDS), aes(x = DIA, fill = PARTO)) +
    # Gráfico tipo barras
    geom_bar(position="dodge") +
    # Escala de cor leve
    scale_fill_brewer() +
    # Nomes dos eixos, título e subtítulo
    labs(x = 'Dias da semana', y = 'N° partos',
         title = 'Número de partos por tipo/dia da semana',
         subtitle = 'Registrados no Brasil em 2016',
         caption = 'Fonte: SINASC 2016') +
    # Retira título da legenda e posiciona no topo
    theme(legend.title = element_blank(), legend.position = "top")
    # Grava figura em disco para uso no Word (veja no diretório png)
    grafico$gravaEmDisco('q01-partosDiaSemana')

    # Geração do gráfico com o número de partos por dias úteis/fds
    ggplot(as.data.frame(PDG), aes(x = DIA, fill = PARTO)) +
      # Gráfico tipo barras
      geom_bar(position="dodge") +
      # Escala de cor leve
      scale_fill_brewer() +
      # Nomes dos eixos, título e subtítulo
      labs(x = 'Tipo de dia', y = 'N° partos',
           title = 'Número de partos por tipo/dias úteis',
           subtitle = 'Registrados no Brasil em 2016',
           caption = 'Fonte: SINASC 2016') +
      # Retira título da legenda e posiciona no topo
      theme(legend.title = element_blank(), legend.position = "top")
    # Grava figura em disco para uso no Word (veja no diretório png)
    grafico$gravaEmDisco('q01-partosDiasUteis')

    # Impressão da tabela
    grafico$mTab('Q01', 'Parto por dia da semana', table(PDS$DIA, PDS$PARTO))
    grafico$mTab('Q01', 'Parto por tipo de dia na semana', table(PDG$DIA, PDG$PARTO))

}
