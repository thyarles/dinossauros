# Este código é o principal para execução da atividade proposta nas discplinas
# Estatística Exploratória e Computaão em Estatística da UnB, 2020/2.

# Docentes
# > Estatatística Exploratória....: Maria Teresa Leão Costa (mtleao@unb.br)
# > Computação em Estatística 1...: Luis Gustavo Vinha (luisvinha@unb.br)

# Discentes
# > xxxxxxxxx Bruno Marques Ribeiro
# > 202046003 Charles Henrique Gonçalves Santos
# > xxxxxxxxx Diogo Pereira Almeida
# > xxxxxxxxx Rafael Ribeiro Araújo




# Instala caso necessário e carrega pacotes para a solução do problema
# ------------------------------------------------------------------------------

# Leitura de arquivos do tipo DBC
if (!require('read.dbc')) install.packages('read.dbc')

# Leitura de arquivos do tipo DBF
if (!require('foreign')) install.packages('foreign')

# Leitura das funções que desenvolvemos para resolver o problema
if (!require('modules')) install.packages('modules')




# Ativa os nossos módulos no diretório R
# ------------------------------------------------------------------------------

sinasc <- modules::use("R")




# Leitura das tabelas auxiliares
# ------------------------------------------------------------------------------

UF        <- sinasc$tabelaAuxiliar$uf()
PAIS      <- sinasc$tabelaAuxiliar$pais()
OCUPACAO  <- sinasc$tabelaAuxiliar$ocupacao()
CAPCID    <- sinasc$tabelaAuxiliar$capCid()
CID       <- sinasc$tabelaAuxiliar$cid()
MUNICIPIO <- sinasc$tabelaAuxiliar$municipio()




# Calcula número de observações por UF e define quantidaedes das amostras
# ------------------------------------------------------------------------------

# Cálculo de observações por UF
for (uf in UF$SIGLA_UF) {
  UF$NUM_OBS_SINASC[UF$SIGLA_UF == uf] <- sinasc$tabelaDados$total(uf)
}

# Cálculo da amostra proporcional para cada UF (2000 no total)
UF$AMOSTRA = as.integer(round(prop.table(UF$NUM_OBS_SINASC)*2000))

# Verifica dados e efetua limpeza de variáveis desnecessárias
cat('Somatório do número de amostras por UF:', sum(UF$AMOSTRA))

# Remove a variável uf para não confundir com o dataframe UF
remove(uf)




# Amostra os dados considerando os nascidos em hospitais no dataframe AMOSTRA
# ------------------------------------------------------------------------------

# Cria dataframe vazio e configura a raíz
set.seed(1234)

# Para cada UF, colete amostras e adicione no dataframe AMOSTRA
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

# Remove variáveis temporárias
remove(uf, numAmostras, df)

# Verifica amostra
str(AMOSTRA)
head(AMOSTRA)




# Correção dos dados do dataframe AMOSTRA
# ------------------------------------------------------------------------------
sinasc$tabelaDados$fator(AMOSTRA$ORIGEM, 1:3, c('A', 'B', 'C'))
