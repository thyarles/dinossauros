# Este código é o principal para execução da atividade proposta nas discplinas
# Estatística Exploratória e Computaão em Estatística da UnB, 2020/2.

# Docentes
# > Estatatística Exploratória....: Maria Teresa Leão Costa (mtleao@unb.br)
# > Computação em Estatística 1...: Luis Gustavo Vinha (luisvinha@unb.br)

# Discentes
# > 202043666 Bruno Marques Ribeiro
# > 202046003 Charles Henrique Gonçalves Santos
# > 202046621 Diogo Pereira Almeida
# > 202046541 Rafael Ribeiro Araújo

# modulos-----------------------------------------------------------------------
# Ativa os nossos módulos no diretório R
  print('Carregando e/ou instalando bibliotecas necessárias...')
  if (!require('modules')) install.packages('modules')      # Leitura dos módulos
  if (!require('read.dbc')) install.packages('read.dbc')    # Leitura de DBC
  if (!require('foreign')) install.packages('foreign')      # Leitura de DBF
  if (!require('ggplot2')) install.packages('ggplot2')      # Plotagem de gráficos
  if (!require('lubridate')) install.packages('lubridate')  # Conversão de datas
  sinasc <- modules::use('R')

# Carrega pacotes globais para plotagem de gráficos com supressão de mensagens
  suppressMessages(suppressWarnings(suppressPackageStartupMessages(
    library('ggplot2'))))

# tab aux-----------------------------------------------------------------------
# Leitura das tabelas auxiliares
  cat('\014', '\n ### Início da execução.\n',
      '-----------------------------------------------------------------')
  sinasc$grafico$msgA('Carregando tabelas auxiliares')
  UF        <- sinasc$tabelaAuxiliar$uf()
  PAIS      <- sinasc$tabelaAuxiliar$pais()
  OCUPACAO  <- sinasc$tabelaAuxiliar$ocupacao()
  CAPCID    <- sinasc$tabelaAuxiliar$capCid()
  CID       <- sinasc$tabelaAuxiliar$cid()
  MUNICIPIO <- sinasc$tabelaAuxiliar$municipio()

# n obs-----------------------------------------------------------------------
# Calcula número de observações por UF
  sinasc$grafico$msgA('Carregando e calculando observações por UF')
  for (uf in UF$SIGLA_UF) {
    UF$NUM_OBS_SINASC[UF$SIGLA_UF == uf] <- sinasc$tabelaDados$total(uf)
  }

# n amost-----------------------------------------------------------------------
# Calcula número de amostras para cada UF
  sinasc$grafico$msgA('Calculando amostra proporcional para cada UF')
  if (!exists('AMOSTRA')) {
    # Calcula somente se já não existe o dataframe AMOSTRA
    UF$AMOSTRA = as.integer(round(prop.table(UF$NUM_OBS_SINASC)*2000))
  } else {
    sinasc$grafico$msgB('Proporções já calculadas!')
  }

# tab amost---------------------------------------------------------------------
# Imprime tabela para formatação e uso no relatório
  sinasc$grafico$tab('Número de observações totais e da amostra por UF',
    UF[, c('SIGLA_UF', 'NUM_OBS_SINASC', 'AMOSTRA')])

# seed--------------------------------------------------------------------------
# Configura SEED para 1234 para que o estudo seja repetido em outras máquinas
  sinasc$grafico$msgA('Configrando o SEED para 1234')
  set.seed(1234)

# amostra-----------------------------------------------------------------------
# Amostra os dados considerando os nascidos em hospitais no dataframe AMOSTRA
  sinasc$grafico$msgA('Salvando amostras de cada UF no dataframe AMOSTRAS')
  for (uf in UF$SIGLA_UF) {
    # Pega o valor da amostra calculada no dataframe UF
      numAmostras = UF$AMOSTRA[UF$SIGLA_UF == uf]
    # Processa amostra
      df <- sinasc$tabelaDados$amostra(uf, numAmostras)
    # Verifica se o dataframe AMOSTRA ja existe... se não, trata diferente
      if (exists('AMOSTRA')) {
        # Verifica se o AMOSTRA está completo e saipara otimizar processamento
          if (dim(AMOSTRA)[1] == 2000) {
            sinasc$grafico$msgB('AMOSTRA já existente!')
            break
          }
        # Nas demais, adiciona-se linhas ao existente
          AMOSTRA <- rbind(AMOSTRA, df)
      } else {
        # Na primeira, cria-se o dataframe
          AMOSTRA <- df
      }
  }

# limpeza-----------------------------------------------------------------------
# Limpa dataframes e variáveis não necessárias nos próximos passos
  sinasc$grafico$msgA('Limpando dados para otimizar memória')
  # Limpa variáveis e dataframes locais
  remove(uf, numAmostras, df)
  # Limpa dataframes dos Estados
  remove(list = as.character(UF$SIGLA_UF))

# formata-----------------------------------------------------------------------
# Formata campos de acordo com a Estrutura do SINASC que acompanhou o BD
  sinasc$grafico$msgA('Formatando AMOSTRA de acordo com dicionário de dados')
  AMOSTRA <- sinasc$dicionario$aplicaEstrutura(AMOSTRA)

# ggplot------------------------------------------------------------------------
# Configurações do tema do GGPLOT2
  sinasc$grafico$msgA('Configurando tema GGPLOT para manter padrão')
  sinasc$grafico$configuraTema()

# questao1---------------------------------------------------------------------------
# 1. Pode-se dizer que o número de partos varia entre os dias da semana?
#    Por que?
  sinasc$grafico$msgA('Gerando dados para a QUESTÃO 01')
  Q01 <- sinasc$q01$resposta(AMOSTRA) # Salva Q01 para uso na questao 4

# questao2----------------------------------------------------------------------
# 2. Qual é o percentual de mães solteiras?
#    Descrever a variável estado civil das mães.
  sinasc$grafico$msgA('Gerando dados para a QUESTÃO 02')
  sinasc$q02$resposta(AMOSTRA)

# questao3----------------------------------------------------------------------
# 3. Descrever a variável peso do recém-nascidos da amostra.
  sinasc$grafico$msgA('Gerando dados para a QUESTÃO 03')
  sinasc$q03$resposta(AMOSTRA)

# questao4----------------------------------------------------------------------
# 4. Existe relação entre o peso do recém-nascido e idade da mãe?
#    A relação é forte?
  sinasc$grafico$msgA('Gerando dados para a QUESTÃO 04')
  sinasc$q04$resposta(AMOSTRA, Q01)

# qiestao5----------------------------------------------------------------------
# 5. Pode-se dizer que o tipo de parto está relacionado a seguintes variáveis?
#    a) Idade da mãe
#    b) Escolaridade da mãe
#    c) Raça ou cor da mãe
#    Se sim, como?
  sinasc$grafico$msgA('Gerando dados para a QUESTÃO 05')
  sinasc$q05$resposta(AMOSTRA)

# final----------------------------------------------------------------------
# Mensagem de sucesso
  cat('\n\n ### Script executado com sucesso!\n\n')
