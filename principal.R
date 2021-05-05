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
print('Gerando dados para a resposta do item 1...')

  # 1. Pode-se dizer que o número de partos varia entre os dias da semana?
  #    Por que?
  # ----------------------------------------------------------------------------

  # Criação de dataframe com tipo de parto por Dia da Semana (PDS) sem NAs
  PDS <- data.frame(wday(AMOSTRA$DTNASC[!is.na(AMOSTRA$PARTO)]),
                    AMOSTRA$PARTO[!is.na(AMOSTRA$PARTO)])

  # Ajuste no nome das colunas
  colnames(PDS) = c('DIA', 'PARTO')

  # Configuração dos fatores para melhor idenbtificação dos dias da semana
  PDS$DIA <- factor(PDS$DIA, levels = 1:7, labels = c('Dom.', 'Seg.', 'Ter.',
                                              'Qua.', 'Qui.', 'Sex.', 'Sáb.'))

  # Geração do gráfico com o número de partos por dia da semana
  ggplot(as.data.frame(PDS), aes(x = DIA, fill = PARTO)) +
    # Gráfico tipo barras
    geom_bar(position="dodge") +
    # Nomes dos eixos, título e subtítulo
    labs(x = 'Dias da semana', y = 'Num. de partos',
         title = 'Número de partos por tipo/dia da semana',
         subtitle = 'Registrados no Brasil em 2016',
         caption = 'Fonte: SINASC 2016',
         tag = 'Fig. 1') +
    # Retira título da legenda e posiciona no topo
    theme(legend.title = element_blank(), legend.position = "top")

  # Grava figura em disco para uso no Word (veja no diretório png)
  sinasc$grafico$gravaEmDisco()

  # A tabela ainda não achei um jeito de fazer automático, os dados são esses:
  table(PDS$DIA, droplevels(PDS$PARTO))

  # Remove PDS
  remove(PDS)


ggplot(as.data.frame(PDS), aes(fill = PDS)) +
  geom_col(position = 'dodge') +
  labs(x = 'Idade dos alunos', y = 'Quantidade de alunos', fill = 'Consome inalantes?') +
  geom_text(aes(label = Freq), colour = 'white', size = 3, vjust = 1.5, position = position_dodge(.9))



ggplot(data = amostra_sem_NA) +
  labs(x ='Tipo de parto', y ='Quantidade de partos')+
  geom_bar(mapping = aes(x = PARTO, fill=PARTO))



ggplot(AMOSTRA$PARTO)


#-----------------------------------------------------------

amostra_sem_NA <- AMOSTRA[,c('DTNASCMAE', 'ESCMAE', 'RACACORMAE', 'PARTO')]
  amostra_sem_NA <- amostra_sem_NA[(!is.na(amostra_sem_NA$PARTO)) &
                                     (!is.na(amostra_sem_NA$DTNASCMAE))&
                                     (!is.na(amostra_sem_NA$ESCMAE)) &
                                     (!is.na(amostra_sem_NA$RACACORMAE)),]


  amostra_sem_NA$ESCMAE <- factor(amostra_sem_NA$ESCMAE,
       label = c('Nenhuma','1 a 3 anos','4 a 7 anos',
                 '8 a 11 anos','12 e mais', 'Ignorado'),
       level= c(1,2,3,4,5,9))
  amostra_sem_NA$RACACORMAE <- factor(amostra_sem_NA$RACACORMAE,
        label = c('Branca','Preta','Amarela','Parda','Indígena'),
        level= 1:5)

  amostra_sem_NA$PARTO <- factor(amostra_sem_NA$PARTO,
                          label = c('Vaginal','Cesáreo','Ignorado'),
                          level= c(1,2,9))


library(ggplot2)
library(tidyverse)
library(moments)



ggplot(data = amostra_sem_NA) +
  labs(x ='Tipo de parto', y ='Quantidade de partos')+
  geom_bar(mapping = aes(x = PARTO, fill=PARTO))


ggplot(data = amostra_sem_NA) +
  labs(x ='Escolaridade da Mãe', y ='Quantidade de pessoas por classe') +
  geom_bar(mapping = aes(x = ESCMAE, fill=ESCMAE)) +
  coord_flip()

ggplot(data = amostra_sem_NA) +
  labs(x ='Identidade Racial', y ='Quantidade de pessoas por Raça') +
  geom_bar(mapping = aes(x = RACACORMAE, fill=RACACORMAE))



ggplot(data = amostra_sem_NA) +
  labs(x ='Tipo de Parto', y ='Frequencia relativa') +
  geom_bar(mapping = aes(x = ESCMAE, fill = PARTO), position = 'fill')




#write.csv(amostra_sem_NA, file = 'amostra_sem_NA.csv', fileEncoding = 'UTF-8')

