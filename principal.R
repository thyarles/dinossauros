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

# Instala caso necessário e carrega pacotes para a solução do problema
# ------------------------------------------------------------------------------
print('Carregando e/ou instalando bibliotecas necessárias...')
  if (!require('read.dbc')) install.packages('read.dbc') # Leitura de DBC
  if (!require('foreign')) install.packages('foreign')   # Leitura de DBF
  if (!require('modules')) install.packages('modules')   # Leitura dos módulos


# Ativa os nossos módulos no diretório R
# ------------------------------------------------------------------------------
print('Carregando módulos na pasta R...')
  sinasc <- modules::use("R")


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
print('Calculando observações por UF...')
  for (uf in UF$SIGLA_UF) {
    UF$NUM_OBS_SINASC[UF$SIGLA_UF == uf] <- sinasc$tabelaDados$total(uf)
  }


# Calcula número de amostras para cada UF
# ------------------------------------------------------------------------------
print('Calculando amostra proporcional para cada UF (2000 no total)...')
  UF$AMOSTRA = as.integer(round(prop.table(UF$NUM_OBS_SINASC)*2000))


# Configura SEED para 1234 para que o estudo seja repetido em outras máquinas
# ------------------------------------------------------------------------------
print('Configurando o SEED para 1234...')
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


# Imprime resultados (apagar, só para conferência momentânea)
# ------------------------------------------------------------------------------
sinasc$tabelaDados$verifica(AMOSTRA)

sinasc$graficos$boxplot(10, 20)
