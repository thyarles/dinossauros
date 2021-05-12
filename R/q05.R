# 5. Pode-se dizer que o tipo de parto está relacionado a seguintes variáveis?
#    a) Idade da mãe
#    b) Escolaridade da mãe
#    c) Raça ou cor da mãe
#    Se sim, como?
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

  #1. Cria o dataframe parto como sub-dataframe de df e exclui os que não
  # interessam
  questao <- 'q05'
  parto <- df[c('IDADEMAE', 'ESCMAE', 'RACACORMAE', 'PARTO')]

  parto <- parto[(!is.na(parto$PARTO)) &
                  (!is.na(parto$IDADEMAE))&
                  (!is.na(parto$ESCMAE)) & (parto$ESCMAE != 'Ignorado') &
                  (!is.na(parto$RACACORMAE)),]


  # --- 2. Agrupando a variável idade

  parto$IDADE_AGRUP <- parto$IDADEMAE

  parto$IDADE_AGRUP[parto$IDADEMAE<18] <- '13 |- 18'
  parto$IDADE_AGRUP[(parto$IDADEMAE>=18) & (parto$IDADEMAE<23)] <- '18 |- 23'
  parto$IDADE_AGRUP[(parto$IDADEMAE>=23) & (parto$IDADEMAE<28)] <- '23 |- 28'
  parto$IDADE_AGRUP[(parto$IDADEMAE>=28) & (parto$IDADEMAE<33)] <- '28 |- 33'
  parto$IDADE_AGRUP[(parto$IDADEMAE>=33) & (parto$IDADEMAE<38)] <- '33 |- 38'
  parto$IDADE_AGRUP[(parto$IDADEMAE>=38) & (parto$IDADEMAE<43)] <- '38 |- 43'
  parto$IDADE_AGRUP[parto$IDADEMAE>=43] <- '43 |- '

  parto$IDADE_AGRUP_GRAF <- parto$IDADEMAE

  parto$IDADE_AGRUP_GRAF[parto$IDADEMAE<18] <- '13 a 18 anos'
  parto$IDADE_AGRUP_GRAF[(parto$IDADEMAE>=18) & (parto$IDADEMAE<23)] <- '18 a 23 anos'
  parto$IDADE_AGRUP_GRAF[(parto$IDADEMAE>=23) & (parto$IDADEMAE<28)] <- '23 a 28 anos'
  parto$IDADE_AGRUP_GRAF[(parto$IDADEMAE>=28) & (parto$IDADEMAE<33)] <- '28 a 33 anos'
  parto$IDADE_AGRUP_GRAF[(parto$IDADEMAE>=33) & (parto$IDADEMAE<38)] <- '33 a 38 anos'
  parto$IDADE_AGRUP_GRAF[(parto$IDADEMAE>=38) & (parto$IDADEMAE<43)] <- '38 a 43 anos'
  parto$IDADE_AGRUP_GRAF[parto$IDADEMAE>=43] <- '43 anos ou mais'


  proporcao_partos = round(length(parto[parto$PARTO != "Vaginal",5]) / length(parto$PARTO),5)
  grafico$msgB(paste("Os partos cirúrgicos representam",
              as.character((proporcao_partos*100)),
              "% dos partos no Brasil"))

  # Geração do gráfico com o número de partos pela escolaridade da mãe
  titulo <- 'Partos de acordo com a escolaridade da mãe'
  ggplot(parto) +
    # Gráfico tipo barras
    geom_bar(aes(x = ESCMAE, fill = PARTO), position = 'fill') +
    # Escala de cor leve
    scale_fill_brewer() +
    # Troca densidade por percentual
    scale_y_continuous(labels = scales::percent) +
    # Nomes dos eixos, título e subtítulo
    labs(x = 'Escolaridade da mãe', y = NULL,
         title = titulo,
         subtitle = 'Registrados no Brasil em 2016',
         caption = 'Fonte: SINASC 2016') +
    # Retira título da legenda e posiciona no topo
    theme(legend.title = element_blank(), legend.position = "top")
  # Grava figura em disco para uso no Word (veja no diretório png)
  grafico$gravaEmDisco(questao, titulo, altura = 7)


  #----- Gera o gráfico do número de partos a partir das idades das mães----
  # Geração do gráfico com o número de partos pela idade das mães
  titulo <- 'Partos de acordo com a idade da mãe'
  ggplot(parto) +
    # Gráfico tipo barras
    geom_bar(aes(x = IDADEMAE, fill = PARTO), position = 'fill') +
    # Escala de cor leve
    scale_fill_brewer() +
    # Troca densidade por percentual
    scale_y_continuous(labels = scales::percent) +
    # Nomes dos eixos, título e subtítulo
    labs(x = 'Idade da Mãe', y = NULL,
         title = titulo,
         subtitle = 'Registrados no Brasil em 2016',
         caption = 'Fonte: SINASC 2016') +
    # Retira título da legenda e posiciona no topo
    theme(legend.title = element_blank(), legend.position = "top")
  # Grava figura em disco para uso no Word (veja no diretório png)
  grafico$gravaEmDisco(questao, titulo, altura = 7)


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


  titulo <- 'Tipos de parto pela idade da mãe'
  ggplot(DF_parto_idade) +
    # Gráfico de pontos - vermelhos são Cesários e azuis vaginais
    geom_point(aes(x = Idade, y = Cesario), color = 'red') +
    geom_point(aes(x = Idade, y = Vaginal), color = 'blue') +
    # Escala de cor leve
    scale_fill_brewer() +
    # Troca densidade por percentual
    scale_y_continuous(labels = scales::percent) +
    # Nomes dos eixos, título e subtítulo
    labs(x = 'Idade da Mãe', y = 'Proporção de partos',
         title = 'Tipos de parto pela idade da mãe',
         subtitle = 'Registrados no Brasil em 2016',
         caption = 'Fonte: SINASC 2016') +
     #Retira título da legenda e posiciona no topo
    theme(legend.title = element_blank(), legend.position = "top")
  # Grava figura em disco para uso no Word (veja no diretório png)
  grafico$gravaEmDisco(questao, titulo, largura = 20, altura = 7)


  #----- Gera o gráfico do número de partos a partir das idades agrupadas----
  # Geração do gráfico com o número de partos pela idade das mães
  titulo <- 'Partos de acordo com idade agrupada'
  ggplot(parto) +
    # Gráfico tipo barras
    geom_bar(aes(x = IDADE_AGRUP_GRAF, fill = PARTO), position = 'fill') +
    # Escala de cor leve
    scale_fill_brewer() +
    # Troca densidade por percentual
    scale_y_continuous(labels = scales::percent) +
    # Nomes dos eixos, título e subtítulo
    labs(x = 'Idade da mãe', y = NULL,
         title = titulo,
         subtitle = 'Registrados no Brasil em 2016',
         caption = 'Fonte: SINASC 2016') +
    # Retira título da legenda e posiciona no topo
    theme(legend.title = element_blank(), legend.position = "top")
  # Grava figura em disco para uso no Word (veja no diretório png)
  grafico$gravaEmDisco(questao, titulo, altura = 7, largura = 20)

    #----- Gera o gráfico do número de partos a partir da cor das mães----
  # Geração do gráfico com o número de partos pela cor das mães
  titulo <- 'Partos de acordo com a cor da mãe'
  ggplot(parto) +
    # Gráfico tipo barras
    geom_bar(aes(x = RACACORMAE, fill = PARTO), position = 'fill') +
    # Escala de cor leve
    scale_fill_brewer() +
    # Troca densidade por percentual
    scale_y_continuous(labels = scales::percent) +
    # Nomes dos eixos, título e subtítulo
    labs(x = 'Cor da mãe', y = NULL,
         title = 'Número de partos pela cor da mãe',
         subtitle = 'Registrados no Brasil em 2016',
         caption = 'Fonte: SINASC 2016') +
    # Retira título da legenda e posiciona no topo
    theme(legend.title = element_blank(), legend.position = "top")
  # Grava figura em disco para uso no Word (veja no diretório png)
  grafico$gravaEmDisco(questao, titulo, altura = 7)


  #----- Geração das tabelas de contingência --------------------------------

  # Gerando tabela de contingência partos por idade
  tab_partos_idade <- table(parto$IDADE_AGRUP, parto$PARTO, exclude = 'Ignorado' )
  grafico$mTab('Q05', 'Contigência Partos por Idade', tab_partos_idade)

  # Proporção das idades das mães (Variável resposta) por parto (v. explicativa)
  grafico$mTab('Q05', 'Proporção das idades das mães',
               round(prop.table(tab_partos_idade,2)*100, digits = 3))

  # Proporção dos partos (Variável resposta) por idade (v. explicativa)
  grafico$mTab('Q05', 'Proporção dos partos por idade',
               round(prop.table(tab_partos_idade,1)*100, digits = 3))

  # Tabela de contigência partos por escolaridade
  tab_partos_escolaridade <- table(parto$ESCMAE,parto$PARTO, exclude = 'Ignorado')
  grafico$mTab('Q05', 'Contigência de partos por escolaridade',
               tab_partos_escolaridade)

  grafico$mTab('Q05', 'Proporção dos partos por idade proporcional',
               round(prop.table(tab_partos_escolaridade,1)*100, digits = 3))

  # Tabela de contigência partos por cor
  tab_partos_cor <- table(parto$RACACORMAE,parto$PARTO, exclude = 'Ignorado')

  grafico$mTab('Q05', 'Contigência de partos por cor',
               tab_partos_cor)

  grafico$mTab('Q05', 'Proporção de contigência de partos por cor',
               round(prop.table(tab_partos_cor,1)*100, digits = 3))


    #---- Cálculo dos coeficientes de contingencias ------------

  #Calcula o coeficiente de contingências - parto / idade
  #Criamos uma matriz associada à tabela de contingência
  mat_partos_idade <- as.matrix(tab_partos_idade)
  #grafico$msgB(paste('Coeficiente de contigências parto/idade:', mat_partos_idade))


  #Usamos a função chisq.test para obter o valor de qui-quadrado
  # Com o valor de qui-quadrado, podemos obter o coeficiente de contingência
  C_test <- (chisq.test(mat_partos_idade, correct = F))
  X_Squared_parto_idade <- C_test$statistic
  C_p_i = (X_Squared_parto_idade /(X_Squared_parto_idade + 2000))**(1/2)
  grafico$msgB(paste('CPI:', C_p_i))
  grafico$msgB(paste('C* para as variáveis tipo de parto e idade:',
              as.character(C_p_i/((1/2)^(1/2)))))

  #Calcula o coeficiente de contingências - parto / escolaridade
  #Criamos uma matriz associada à tabela de contingência

  mat_partos_esc <- as.matrix(tab_partos_escolaridade)

  #Usamos a função chisq.test para obter o valor de qui-quadrado
  # Com o valor de qui-quadrado, podemos obter o coeficiente de contingência
  C_test <- (chisq.test(mat_partos_esc, correct = F))
  X_Squared_parto_esc <- C_test$statistic
  C_p_e = (X_Squared_parto_esc /(X_Squared_parto_esc + 2000))**(1/2)
  grafico$msgB(paste('CPE:', C_p_e))
  grafico$msgB(paste('C* para as variáveis tipo de parto e escolaridade:',
              as.character(C_p_e/((1/2)^(1/2)))))


  #Calcula o coeficiente de contingências - parto / cor
  #Criamos uma matriz associada à tabela de contingência

  mat_partos_cor <- as.matrix(tab_partos_cor)

  #Usamos a função chisq.test para obter o valor de qui-quadrado
  # Com o valor de qui-quadrado, podemos obter o coeficiente de contingência
  C_test <- (chisq.test(mat_partos_cor, correct = T))
  X_Squared_parto_cor <- C_test$statistic
  C_p_c = (X_Squared_parto_cor /(X_Squared_parto_cor + 2000))**(1/2)
  grafico$msgB(paste('CPC:', C_p_c))
  grafico$msgB(paste('C* para as variáveis tipo de parto e Raça:',
              as.character(C_p_c/((1/2)^(1/2)))))

}



