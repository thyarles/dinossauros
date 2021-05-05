# Este código é o principal para execução da atividade proposta nas discplinas
# Estatística Exploratória e Computaão em Estatística da UnB, 2020/2.

# Docentes
# > Estatatística Exploratória....: Maria Teresa Leão Costa (mtleao@unb.br)
# > Computação em Estatística 1...: Luis Gustavo Vinha (luisvinha@unb.br)

# Discentes
# > 202043666 Bruno Marques Ribeiro
# > 202046003 Charles Henrique Gonçalves Santos
# > xxxxxxxxx Diogo Pereira Almeida
# > xxxxxxxxx Rafael Ribeiro Araújo

# Instala caso necessário e carrega pacotes para a solução do problema
# ------------------------------------------------------------------------------
print('Carregando e/ou instalando bibliotecas necessárias...')
  if (!require('read.dbc')) install.packages('read.dbc')    # Leitura de DBC
  if (!require('foreign')) install.packages('foreign')      # Leitura de DBF
  if (!require('modules')) install.packages('modules')      # Leitura dos módulos
  if (!require('ggplot2')) install.packages('ggplot2')      # Plotagem de gráficos
  if (!require('lubridate')) install.packages('lubridate')  # Conversão de datas
  #if (!require('stargazer')) install.packages('stargazer')  # Geração de tabelas
  #library(stargazer)
  library(lubridate)
  library(ggplot2)


# Ativa os nossos módulos no diretório R
# ------------------------------------------------------------------------------
print('Carregando módulos na pasta R...')
  sinasc <- modules::use('R')


# Leitura das tabelas auxiliares
# ------------------------------------------------------------------------------
print('Carregando tabelas auxiliares...')
  UF        <- sinasc$tabelaAuxiliar$uf()
  PAIS      <- sinasc$tabelaAuxiliar$pais()
  OCUPACAO  <- sinasc$tabelaAuxiliar$ocupacao()
  CAPCID    <- sinasc$tabelaAuxiliar$capCid()
  CID       <- sinasc$tabelaAuxiliar$cid()
  MUNICIPIO <- sinasc$tabelaAuxiliar$municipio()


# Calcula número de observações por UF
# ------------------------------------------------------------------------------
print('Carregando e calculando observações por UF, por favor, aguarde...')
  for (uf in UF$SIGLA_UF) {
    UF$NUM_OBS_SINASC[UF$SIGLA_UF == uf] <- sinasc$tabelaDados$total(uf)
  }


# Calcula número de amostras para cada UF
# ------------------------------------------------------------------------------
print('Calculando amostra proporcional para cada UF (2000 no total)...')
  UF$AMOSTRA = as.integer(round(prop.table(UF$NUM_OBS_SINASC)*2000))


# Tabela para formatação e uso no relatório
# ------------------------------------------------------------------------------
cat('\n  --> Número de observações totais e da amostra por UF\n´', '\n')
  # Define dataframe auxiliar para ordenação
  print(UF[, c('SIGLA_UF', 'NUM_OBS_SINASC', 'AMOSTRA')])
  cat('\n')


# Configura SEED para 1234 para que o estudo seja repetido em outras máquinas
# ------------------------------------------------------------------------------
print('Configrando o SEED para 1234...')
  set.seed(1234)


# Amostra os dados considerando os nascidos em hospitais no dataframe AMOSTRA
# ------------------------------------------------------------------------------
print('Salvando amostras de cada UF no dataframe AMOSTRAS...')
  for (uf in UF$SIGLA_UF) {
    # Pega o valor da amostra calculada no dataframe UF
    numAmostras = UF$AMOSTRA[UF$SIGLA_UF == uf]
    # Processa amostra
    df <- sinasc$tabelaDados$amostra(uf, numAmostras)
    # Verifica se o dataframe AMOSTRA ja existe... se não, trata diferente
    if (exists('AMOSTRA')) {
      # Nas demais, adiciona-se linhas ao existente
      AMOSTRA <- rbind(AMOSTRA, df)
    } else {
      # Na primeira, cria-se o dataframe
      AMOSTRA <- df
    }
  }


# Limpa dataframes e variáveis não necessárias nos próximos passos
# ------------------------------------------------------------------------------
print('Limpando e otimizando memória...')
  # Limpa variáveis e dataframes locais
  remove(uf, numAmostras, df)
  # Limpa dataframes dos Estados
  remove(list = as.character(UF$SIGLA_UF))


# Formata campos de acordo com a Estrutura do SINASC que acompanhou o BD
# ------------------------------------------------------------------------------
print('Formatando campos da amostra de acordo com dicionário de dados...')
  AMOSTRA <- sinasc$dicionario$aplicaEstrutura(AMOSTRA)


# Configurações do tema do GGPLOT2
# ------------------------------------------------------------------------------
print('Configurando o tema do GGPLOT para um padrão do trabalho...')
  sinasc$grafico$configuraTema()


################################################################################
# Resposta aos itens
################################################################################

# 1. Pode-se dizer que o número de partos varia entre os dias da semana?
#    Por que?
# ----------------------------------------------------------------------------
cat('\n\n##### Gerando dados para a resposta da Questão 01...\n')
  sinasc <- modules::use('R')
  sinasc$q01$resposta(AMOSTRA)


# 2. Qual é o percentual de mães solteiras?
#    Descrever a variável estado civil das mães.
# ----------------------------------------------------------------------------
cat('\n\n##### Gerando dados para a resposta da Questão 02...\n')
  sinasc <- modules::use('R')
  sinasc$q02$resposta(AMOSTRA)


# 3. Descrever a variável peso do recém-nascidos da amostra.
# ----------------------------------------------------------------------------
cat('\n\n##### Gerando dados para a resposta da Questão 03...\n')
  sinasc <- modules::use('R')
  sinasc$q03$resposta(AMOSTRA)


# 4. Existe relação entre o peso do recém-nascido e idade da mãe?
#    A relação é forte?
# ----------------------------------------------------------------------------
cat('\n\n##### Gerando dados para a resposta da Questão 04...\n')
  sinasc <- modules::use('R')
  sinasc$q04$resposta(AMOSTRA)


# 5. Pode-se dizer que o tipo de parto está relacionado a seguintes variáveis?
#    a) Idade da mãe
#    b) Escolaridade da mãe
#    c) Raça ou cor da mãe
#    Se sim, como?
# ----------------------------------------------------------------------------
cat('\n\n##### Gerando dados para a resposta da Questão 05...\n')
sinasc <- modules::use('R')
sinasc$q05$resposta(AMOSTRA)




# Código do Bruno para aproveitamento nos gráficos
# Descomentar para testar
#-----------------------------------------------------------
#
#
# amostra_sem_NA <- AMOSTRA[,c('DTNASCMAE', 'ESCMAE', 'RACACORMAE', 'PARTO')]
#
#
#
# amostra_sem_NA <- amostra_sem_NA[(!is.na(amostra_sem_NA$PARTO)) &
#                                    (!is.na(amostra_sem_NA$DTNASCMAE))&
#                                    (!is.na(amostra_sem_NA$ESCMAE)) &
#                                    (!is.na(amostra_sem_NA$RACACORMAE)),]
#
#
#
#
# ggplot(data = amostra_sem_NA) +
#   labs(x ='Tipo de parto', y ='Quantidade de partos')+
#   geom_bar(mapping = aes(x = PARTO, fill=PARTO))
#
#
#
# #library(ggplot2)
# library(tidyverse)
# library(moments)
#
#
#
# ggplot(data = amostra_sem_NA) +
#   labs(x ='Tipo de parto', y ='Quantidade de partos')+
#   geom_bar(mapping = aes(x = PARTO, fill=PARTO))
#
#
# ggplot(data = amostra_sem_NA) +
#   labs(x ='Escolaridade da Mãe', y ='Quantidade de pessoas por classe') +
#   geom_bar(mapping = aes(x = ESCMAE, fill=ESCMAE)) +
#   coord_flip()
#
# ggplot(data = amostra_sem_NA) +
#   labs(x ='Identidade Racial', y ='Quantidade de pessoas por Raça') +
#   geom_bar(mapping = aes(x = RACACORMAE, fill=RACACORMAE))
#
#
#
# ggplot(data = amostra_sem_NA) +
#   labs(x ='Tipo de Parto', y ='Frequencia relativa') +
#   geom_bar(mapping = aes(x = ESCMAE, fill = PARTO), position = 'fill')
#
#
#
#
# #write.csv(amostra_sem_NA, file = 'amostra_sem_NA.csv', fileEncoding = 'UTF-8')
#
