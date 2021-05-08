# 2. Qual é o percentual de mães solteiras?
#    Descrever a variável estado civil das mães.
# ----------------------------------------------------------------------------


#DÚVIDAS:
#- GERAR GRÁFICO 1 NO GGPLOT
#- GERAR GRÁFICO 2 (NÃO CONSIGO NENHUM DELES)

df=AMOSTRA
# Importes para a solução da questão
grafico <- modules::use('R/grafico.R')
import('ggplot2')

# Exporta função
export('resposta')

# Define função de resposta
resposta <- function(df) {

cat('\n  --> Criando data frames para gerar tabelas e gráficos...\n')


# Gerando tabela com os dados de estado civil da mãe
tabestcivmae <- table(AMOSTRA$ESTCIVMAE)
tabestcivmae

# Calculando a proporção
prop.table(tabestcivmae)*100

# Gerando informações que relacionem estado civil e idade

idadeestcivilmae=AMOSTRA[, c(7, 6)]

# Gerando os quartis
quantile(idadeestcivilmae$IDADEMAE, probs = seq(0, 1, 0.2))

# Dividindo em 5 classes (5 é o mínimo, de acordo com a apresentação da professora)
idadeestcivilmae$FAIXA <- cut(idadeestcivilmae$IDADEMAE,
                              breaks=c(13,20, 25, 30, 35, 40, 47),
                      labels=c("13 |- 20", "21 |- 25","26 |- 30","31 |- 35",
                               "36 |- 40", "41 |- 46"),
                      right=FALSE)

estcivilmaefaixa=idadeestcivilmae[,c(1,3)]
table(estcivilmaefaixa)

# Gerando gráfico 1
barplot(prop.table(tabestcivmae), col = c(1, 2, 3, 4, 5, 6),
        xlab = "Estado Civil",
        ylab = "Percentual", ylim = c(0, 0.5))


# Versão no GGPLOT
# Cria cópia da AMOSTRA para trabalhar local
df <- AMOSTRA
# Cria novo atributo sem fatores
df$ECA <- as.integer(AMOSTRA$ESTCIVMAE)
# Unindo Viúva, Divorciada, Ignorado (Estado Civil Agrupado = ECA) no cód. 99
df$ECA[df$ECA %in% c(3, 4, 6)] <- 99
# Troca NAs por 99
df$ECA[is.na(df$ECA)] <- 99
# Reaplica fator
df$ECA <- factor(df$ECA, levels = c(1, 2, 5, 99), labels = c('Solteira',
                                                             'Casada',
                                                             'União estável',
                                                             'Outros'))
# Criando as faixas no df
df$FAIXA <- cut(df$IDADEMAE, breaks=c(13, 20, 25, 30, 35, 40, 47),
                labels=c("13 |- 20", "21 |- 25","26 |- 30","31 |- 35",
                         "36 |- 40", "41 |- 46"),
                right=FALSE)

# Plota gráfico
ggplot(df, aes(x = ECA, fill='')) +
  # Aplica percentual calculado via GGPLOT
  geom_bar(aes(y = (..count..)/sum(..count..)), show.legend = FALSE) +
  # Aplica o tema de cores
  scale_fill_brewer() +
  # Adiciona percentual para ficar "humanizado"
  scale_y_continuous(labels = scales::percent) +
  # Nomes dos eixos, título e subtítulo
  labs(x = 'Estado civil', y = NULL,
       title = 'Percentual do estado civil das mães',
       subtitle = 'Registrados no Brasil em 2016',
       caption = 'Fonte: SINASC 2016')
# Grava figura em disco para uso no Word (veja no diretório png)
grafico$gravaEmDisco('q02-estCivilPercentual')


pie(prop.table(tabestcivmae), col = c(1, 2, 3, 4, 5, 6), clockwise = TRUE)

# GGplot não tem gráfico circular... vamos para o 'gato'
ggplot(df, aes(x = '', y = ECA, fill = ECA)) +
  # Aplica percentual calculado via GGPLOT
  geom_bar(stat = 'identity', width = 1) +
  # Gato, mudar o sistema de coordenadas
  coord_polar('y', start = 0) +
  # Aplica o tema de cores
  scale_fill_brewer() +
  theme(
    legend.title = element_blank(), legend.position = "right",
    axis.text = element_blank(), axis.title = element_blank(),
    axis.ticks.length = unit(0, "pt"), axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL, axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL, axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL, legend.box = NULL,
    line = element_blank(), rect = element_blank()
  ) +
  # Nomes dos eixos, título e subtítulo
  labs(x = NULL, y = NULL,
       title = 'Percentual do estado civil das mães',
       subtitle = 'Registrados no Brasil em 2016',
       caption = 'Fonte: SINASC 2016')
# Grava figura em disco para uso no Word (veja no diretório png)
grafico$gravaEmDisco('q02-estCivilPercentualPolar')


## NÃO ESTOU CONSEGUINDO GERAR NO GGPLOT !!!

# Gerando gráfico 2

plot(estcivilmaefaixa)

# Versão com GGPLOT
ggplot(df, aes(x = FAIXA, fill=ECA)) +
  # Aplica percentual calculado via GGPLOT
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  # Aplica o tema de cores
  scale_fill_brewer() +
  # Adiciona percentual para ficar "humanizado"
  scale_y_continuous(labels = scales::percent) +
  # Nomes dos eixos, título e subtítulo
  labs(x = 'Faixa etária', y = NULL,
       title = 'Percentual do estado civil das mães por faixa etária',
       subtitle = 'Registrados no Brasil em 2016',
       caption = 'Fonte: SINASC 2016') +
  theme(legend.title = element_blank(), legend.position = "top")

# Grava figura em disco para uso no Word (veja no diretório png)
grafico$gravaEmDisco('q02-estCivilFaixaEtaria')


# RELATÓRIO
# Estado civil das mães
#
# Os dados do Sinasc também trazem informações relevantes sobre o estado civil das
# mães de 2016. Quase metade delas (44,44%) são solteiras, enquanto as casadas
# contabilizam 30,1% e as conviventes em regime de união estável representam
# cerca de 1/4 do total (24,41%). Por fim, as divorciadas são 0,86% das mães e as
# viúvas somam 0,15%.
#
# INSERIR GRÁFICO GERAL (1)
#
# Os números também mostram que a proporção de solteiras varia de acordo com a
# idade: elas são a maioria nas classes etárias mais baixas (dos 13 aos 24 anos),
# enquanto o percentual de casadas aumenta com a idade. Esses dados refletem a
# tendência de formação das famílias brasileiras, que passou a ter mais de 1
# milhão de lares liderados por mulheres solteiras entre 2005 e 2015 (IBGE apud
# Velasco, 2017).
#
# INSERIR GRÀFICO DE LINHAS (2)
#
# REF https://g1.globo.com/economia/noticia/em-10-anos-brasil-ganha-mais-de-1-milhao-de-familias-formadas-por-maes-solteiras.ghtml




# COMANDO DO Q1
#
#   # Criação de dataframe com tipo de parto por Dia da Semana (PDS) sem NAs
#   PDS <- data.frame(wday(df$DTNASC[!is.na(df$PARTO)]),
#                     df$PARTO[!is.na(df$PARTO)])
#
#   # Ajuste no nome das colunas
#   colnames(PDS) = c('DIA', 'PARTO')
#
#   # Criação de dataframes para agrupamento dias úteis / fim de semana (PDG)
#   PDG <- PDS
#   PDG$DIA[PDG$DIA %in% c(1, 7)] <- 0  # Fins de semana
#   PDG$DIA[PDG$DIA %in% c(2: 6)] <- 1  # Dias úteis
#
#   # Configuração dos fatores para melhor idenbtificação dos dias da semana
#   PDS$DIA <- factor(PDS$DIA, levels = 1:7, labels = c('Dom.', 'Seg.', 'Ter.',
#                                                       'Qua.', 'Qui.', 'Sex.', 'Sáb.'))
#   # Impressão da tabela
#   grafico$geraTabela('Parto por dia da semana', table(PDS$DIA, PDS$PARTO))
#
#   # Configuração dos fatores para melhor idenbtificação de dias úteis / fds
#   PDG$DIA <- factor(PDG$DIA, levels = 0:1, labels = c('Fim de semana',
#                                                       'Dia útil'))
#   # Impressão da tabela
#   grafico$geraTabela('Parto por tipo de dia na semana', table(PDG$DIA, PDG$PARTO))
#
#   # Geração do gráfico com o número de partos por dia da semana
#   ggplot(as.data.frame(PDS), aes(x = DIA, fill = PARTO)) +
#     # Gráfico tipo barras
#     geom_bar(position="dodge") +
#     # Escala de cor leve
#     scale_fill_brewer() +
#     # Nomes dos eixos, título e subtítulo
#     labs(x = 'Dias da semana', y = 'N° partos',
#          title = 'Número de partos por tipo/dia da semana',
#          subtitle = 'Registrados no Brasil em 2016',
#          caption = 'Fonte: SINASC 2016') +
#     # Retira título da legenda e posiciona no topo
#     theme(legend.title = element_blank(), legend.position = "top")
#   # Grava figura em disco para uso no Word (veja no diretório png)
#   grafico$gravaEmDisco('q01-partosDiaSemana')
#
#   # Geração do gráfico com o número de partos por dias úteis/fds
#   ggplot(as.data.frame(PDG), aes(x = DIA, fill = PARTO)) +
#     # Gráfico tipo barras
#     geom_bar(position="dodge") +
#     # Escala de cor leve
#     scale_fill_brewer() +
#     # Nomes dos eixos, título e subtítulo
#     labs(x = 'Tipo de dia', y = 'N° partos',
#          title = 'Número de partos por tipo/dias úteis',
#          subtitle = 'Registrados no Brasil em 2016',
#          caption = 'Fonte: SINASC 2016') +
#     # Retira título da legenda e posiciona no topo
#     theme(legend.title = element_blank(), legend.position = "top")
#   # Grava figura em disco para uso no Word (veja no diretório png)
#   grafico$gravaEmDisco('q01-partosDiasUteis')

  # Colocar o código da resposta aqui
  # Pode-se gerar variávieis, mas elas não vão persistir além desse módulo
  # Não dá para usar uma variável criada aqui na próxima resposta
  # Olhem o q01.R para exemplo

}

