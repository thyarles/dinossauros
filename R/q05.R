# 5. Pode-se dizer que o tipo de parto está relacionado a seguintes variáveis?
#    a) Idade da mãe
#    b) Escolaridade da mãe
#    c) Raça ou cor da mãe
#    Se sim, como?
# ----------------------------------------------------------------------------

# Importes para a solução da questão
grafico <- modules::use('R/grafico.R')
import('ggplot2')

# Exporta função
export('resposta')

# Define função de resposta
resposta <- function(df) {

  parto <- df[,c('DTNASCMAE', 'ESCMAE', 'RACACORMAE', 'PARTO')]

  parto <- parto[(!is.na(parto$PARTO)) &
                          (!is.na(parto$DTNASCMAE))&
                          (!is.na(parto$ESCMAE)) &
                          (!is.na(parto$RACACORMAE)),]


  # Geração do gráfico com o número de partos por dias úteis/fds
  ggplot(parto) +
    # Gráfico tipo barras
    geom_bar(aes(x = ESCMAE, fill = PARTO), position = 'fill') +
    # Escala de cor leve
    scale_fill_brewer() +
    # Nomes dos eixos, título e subtítulo
    labs(x = 'Escolaridade da Mãe', y = 'N° partos',
         title = 'Número de partos pela escolaridade da mãe',
         subtitle = 'Registrados no Brasil em 2016',
         caption = 'Fonte: SINASC 2016') +
    # Retira título da legenda e posiciona no topo
    theme(legend.title = element_blank(), legend.position = "top")
  # Grava figura em disco para uso no Word (veja no diretório png)
  grafico$gravaEmDisco('q01-partosPorEscolaridade')
}
