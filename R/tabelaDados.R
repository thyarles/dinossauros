# Módulo para leitura das tabelas de dados fornecidas pelo SISNAC
#
# DNAC2016.dbc  DNDF2016.dbc  DNMT2016.dbc  DNRJ2016.dbc  DNSE2016.dbc
# DNAL2016.dbc  DNES2016.dbc  DNPA2016.dbc  DNRN2016.dbc  DNSP2016.dbc
# DNAM2016.dbc  DNGO2016.dbc  DNPB2016.dbc  DNRO2016.dbc  DNTO2016.dbc
# DNAP2016.dbc  DNMA2016.dbc  DNPE2016.dbc  DNRR2016.dbc
# DNBA2016.dbc  DNMG2016.dbc  DNPI2016.dbc  DNRS2016.dbc
# DNCE2016.dbc  DNMS2016.dbc  DNPR2016.dbc  DNSC2016.dbc

# Leitura da library
import('read.dbc')

# Contabiliza o total de observações por UF
export('total')
total <- function(uf) {
  soma <- dim(read.dbc(paste('dados/DN', uf, '2016.dbc', sep = '')))[1]
  print(paste(uf, 'tem', soma,  'registros.'))
  return(soma)
}

# Pega amostras para determinada UF
# De acordo com o comando da questão considerar apenas nascidos em hospitais
# A Estrutura do SINASC diz que o LOCNASC é:
# Local de ocorrência do nascimento, conforme a tabela:
#   9: Ignorado
#   1: Hospital
#   2: Outro Estab Saúde
#   3: Domicílio
#   4: Outros
# Logo, apenas nos interessa amostras dos registros cuso campo LOCNASC = 1
export('amostra')
amostra <- function(uf, amostra) {
  # Le a base de dados
  df <- read.dbc(paste('dados/DN', uf, '2016.dbc', sep = ''))
  # Filtra apenas os nascidos em hospitais, sem NAs
  df <- subset(df, df$LOCNASC == 1)
  # Faz a amostragem
  df <- df[sample(df$contador, amostra),]
  # Mostra progresso
  print(paste(uf, '- processando', amostra, 'amostras.'))
  # Retorna o resultado
  return(df)
}

# Contabiliza o total de observações por UF
export('fator')
fator <- function(df, niveis, classes) {
  return(factor(df, label = classes, levels = niveis))
}

