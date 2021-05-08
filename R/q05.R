# 5. Pode-se dizer que o tipo de parto está relacionado a seguintes variáveis?
#    a) Idade da mãe
#    b) Escolaridade da mãe
#    c) Raça ou cor da mãe
#    Se sim, como?
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

  #1. Cria o dataframe parto como sub-dataframe de df e exclui os que não
  # interessam
  parto <- df[c('IDADEMAE', 'ESCMAE', 'RACACORMAE', 'PARTO')]

  parto <-  parto[(!is.na(parto$PARTO)) & (parto$PARTO != 'Ignorado') &
                  (!is.na(parto$IDADEMAE))& (parto$IDADEMAE != 'Ignorado') &
                  (!is.na(parto$ESCMAE)) & (parto$ESCMAE != 'Ignorado') &
                  (!is.na(parto$RACACORMAE))& (parto$RACACORMAE != 'Ignorado'),]


  parto <- droplevels(parto)

  # --- 2. Agrupando a variável idade

  parto$IDADE_AGRUP <- parto$IDADEMAE

  parto$IDADE_AGRUP[parto$IDADEMAE<20] <- '00 |- 20'
  parto$IDADE_AGRUP[(parto$IDADEMAE>=20) & (parto$IDADEMAE<25)] <- '20 |- 25'
  parto$IDADE_AGRUP[(parto$IDADEMAE>=25) & (parto$IDADEMAE<30)] <- '25 |- 30'
  parto$IDADE_AGRUP[(parto$IDADEMAE>=30) & (parto$IDADEMAE<35)] <- '30 |- 35'
  parto$IDADE_AGRUP[(parto$IDADEMAE>=35) & (parto$IDADEMAE<40)] <- '35 |- 40'
  parto$IDADE_AGRUP[parto$IDADEMAE>=40] <- '40 |- '

  # Geração do gráfico com o número de partos pela escolaridade da mãe
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


  #----- Gera o gráfico do número de partos a partir das idades das mães----
  # Geração do gráfico com o número de partos pela idade das mães
  ggplot(parto) +
    # Gráfico tipo barras
    geom_bar(aes(x = IDADEMAE, fill = PARTO), position = 'fill') +
    # Escala de cor leve
    scale_fill_brewer() +
    # Nomes dos eixos, título e subtítulo
    labs(x = 'Idade da Mãe', y = 'N° partos',
         title = 'Número de partos pela idade da mãe',
         subtitle = 'Registrados no Brasil em 2016',
         caption = 'Fonte: SINASC 2016') +
    # Retira título da legenda e posiciona no topo
    theme(legend.title = element_blank(), legend.position = "top")
  # Grava figura em disco para uso no Word (veja no diretório png)
  grafico$gravaEmDisco('q01-partosPorEscolaridade')




  #-- Gera o gráfico de pontos do tipo de partos a partir das idades das mães-
  # Geração do gráfico com o número de partos pela idade das mães
  tab_partos_idadeMAE <- table(parto$IDADEMAE,parto$PARTO )
  #addmargins(tab_partos_idade)
  #print(tab_partos_idadeMAE)
  provisorio <- data.frame(prop.table(tab_partos_idadeMAE,1))
  DF_parto_idade <- provisorio[provisorio$Var2 == 'Vaginal',]
  #Mudamos os nomes das variáveis
  names(DF_parto_idade)[names(DF_parto_idade) == 'Freq'] <- 'Vaginal'
  DF_parto_idade$Cesario <- provisorio$Freq[(provisorio$Var2 != 'Vaginal') &
                                              (provisorio$Var2 != 'Ignorado')]
  names(DF_parto_idade)[names(DF_parto_idade) == 'Var1'] <- 'Idade'
  DF_parto_idade$Var2<-NULL


  ggplot(DF_parto_idade) +
    # Gráfico de pontos - vermelhos são Cesários e azuis vaginais
    geom_point(aes(x = Idade, y = Cesario),color = 'red') +
    geom_point(aes(x = Idade, y = Vaginal), color = 'blue') +
    # Escala de cor leve
    scale_fill_brewer() +
    # Nomes dos eixos, título e subtítulo
    labs(x = 'Idade da Mãe', y = 'Proporção de partos',
         title = 'Tipos de parto pela idade da mãe',
         subtitle = 'Registrados no Brasil em 2016',
         caption = 'Fonte: SINASC 2016')
     #Retira título da legenda e posiciona no topo
    theme(legend.title = element_blank(), legend.position = "top")
  # Grava figura em disco para uso no Word (veja no diretório png)
  grafico$gravaEmDisco('q01-partosPorEscolaridade')


  #----- Gera o gráfico do número de partos a partir das idades agrupadas----
  # Geração do gráfico com o número de partos pela idade das mães
  ggplot(parto) +
    # Gráfico tipo barras
    geom_bar(aes(x = IDADE_AGRUP, fill = PARTO), position = 'fill') +
    # Escala de cor leve
    scale_fill_brewer() +
    # Nomes dos eixos, título e subtítulo
    labs(x = 'Idade da Mãe', y = 'N° partos',
         title = 'Número de partos por idade agrupada',
         subtitle = 'Registrados no Brasil em 2016',
         caption = 'Fonte: SINASC 2016') +
    # Retira título da legenda e posiciona no topo
    theme(legend.title = element_blank(), legend.position = "top")
  # Grava figura em disco para uso no Word (veja no diretório png)
  grafico$gravaEmDisco('q01-partosPorEscolaridade')


  #----- Gera o gráfico do número de partos a partir da cor das mães----
  # Geração do gráfico com o número de partos pela cor das mães
  ggplot(parto) +
    # Gráfico tipo barras
    geom_bar(aes(x = RACACORMAE, fill = PARTO), position = 'fill') +
    # Escala de cor leve
    scale_fill_brewer() +
    # Nomes dos eixos, título e subtítulo
    labs(x = 'Cor da Mãe', y = 'N° partos',
         title = 'Número de partos pela cor da mãe',
         subtitle = 'Registrados no Brasil em 2016',
         caption = 'Fonte: SINASC 2016') +
    # Retira título da legenda e posiciona no topo
    theme(legend.title = element_blank(), legend.position = "top")
  # Grava figura em disco para uso no Word (veja no diretório png)
  grafico$gravaEmDisco('q01-partosPorEscolaridade')



  #----- Geração das tabelas de contingência --------------------------------


    # Gerando tabela de contingência partos por idade
  tab_partos_idade <- table(parto$IDADE_AGRUP,parto$PARTO )
  #addmargins(tab_partos_idade)
  cat('\n  --> Criando tabela de partos por idade...\n')
  print((tab_partos_idade))


  # Proporção das idades das mães (Variável resposta) por parto (v. explicativa)
  print(prop.table(tab_partos_idade,2))#*100

  # Proporção dos partos (Variável resposta) por idade (v. explicativa)
  print(prop.table(tab_partos_idade,1))#*100


  # Tabela de contigência partos por escolaridade
  tab_partos_escolaridade <- table(parto$ESCMAE,parto$PARTO )
  #addmargins(tab_partos_escolaridade)
  print(tab_partos_escolaridade)
  print(prop.table(tab_partos_escolaridade,1))#*100


  # Tabela de contigência partos por cor
  tab_partos_cor <- table(parto$RACACORMAE,parto$PARTO )
  #addmargins(tab_partos_cor)
  print(tab_partos_cor)
  print(prop.table(tab_partos_cor,1))#*100


  }

#parto[parto$PARTO == 'Ignorado',]
#df[c('PARTO', 'IDADEMAE')][df$PARTO == 'Ignorado',]
