# Este arquivo tem a função de corrigir os tipos de dados e aplicar o dicionário,
# conforme arquitetura do SINASC no PDF 'Estrutura_SINASC_para_CD.pdf'

# Percebemos que nem todos os campos listados na Estrurua do SINASC para CD-ROM
# batem, talvez porque os dados se referem à ultima versão e nós estamos trabalhando
# com a de 2016. Por exemplo, o campo Declaração (SEQ 01) é inexistente; existe
# um campo chamado ORIGEM que não está listado na Estrutura e assim por diante.

# Na importação todos os dados vieram como FATORES, talvez uma anomalia desse DBC,
# por isso é necessário converter tudo para numérico e string antes de operar

# Recebe dataframe para aplicar limpaza, devolve datraframe transformado
export('aplicaEstrutura')
aplicaEstrutura <- function(df) {

  # Remove todos os fatores que vieram errados na improtacão do DBC
  df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)

  # Campo contador: C(08)
  df$contador <- as.integer(df$contador)

  # Campo Origem: Não tem na estrutura
  # Esse campo não existe na estrutura, vamos apenas converter para numérico
  df$ORIGEM <- as.integer(df$ORIGEM)

  # LOCNASC: Fator
  df$LOCNASC <- factor(df$LOCNASC, levels = c(1:4, 9), labels = c('Hospital',
                                                                  'Outro Estab Saúde',
                                                                  'Domicilio',
                                                                  'Outros',
                                                                  'Ignorado'))
  # IDADEMAE: Inteiro
  df$IDADEMAE <- as.integer(df$IDADEMAE)

  # ESTCIVMAE: Fator
  df$ESTCIVMAE <- factor(df$ESTCIVMAE, levels = c(1:5, 9), labels = c('Solteira',
                                                                      'Casada',
                                                                      'Viúva',
                                                                      'Divorciada',
                                                                      'União estável',
                                                                      'Ignorado'))

  # ESCMAE: Fator
  df$ESCMAE <- factor(df$ESCMAE, levels = c(1:5, 9), labels = c('Nenhuma',
                                                                '1 a 3 anos',
                                                                '4 a 7 anos',
                                                                '8 a 11 anos',
                                                                '12 e mais',
                                                                'Ignorado'))

  # QTDFILVIVO: Inteiro
  df$QTDFILVIVO <- as.integer(df$QTDFILVIVO)

  # QTDFILMORT: Inteiro
  df$QTDFILMORT <- as.integer(df$QTDFILMORT)

  # GESTACAO: Fator
  df$GESTACAO <- factor(df$GESTACAO, levels = c(1:6, 9), labels = c('Menos de 22 semanas',
                                                                    '22 a 27 semanas',
                                                                    '28 a 31 semanas',
                                                                    '32 a 36 semanas',
                                                                    '37 a 41 semanas',
                                                                    '42 semanas e mais',
                                                                    'Ignorado'))

  # GRAVIDEZ: Fator
  df$GRAVIDEZ <- factor(df$GRAVIDEZ, levels = c(1:3, 9), labels = c('Única',
                                                                    'Dupla',
                                                                    'Tripla e mais',
                                                                    'Ignorado'))

  # PARTO: Fator
  df$PARTO <- factor(df$PARTO, levels = c(1:2, 9), labels = c('Vaginal',
                                                              'Cesáreo',
                                                              'Ignorado'))

  # CONSULTAS: Fator
  df$CONSULTAS <- factor(df$CONSULTAS, levels = c(1:4, 9), labels = c('Nenhuma',
                                                                      'de 1 a 3',
                                                                      'de 4 a 6',
                                                                      '7 e mais',
                                                                      'Ignorado'))

  # DTNASC: Data
  df$DTNASC <- as.Date(df$DTNASC, format = '%d%m%Y')

  # HORANASC: Não tem na arquitetura
  # Vamos precisar converter?

  # SEXO: fator
  df$SEXO <- factor(df$SEXO, levels = c(0:2), labels = c('Ignorado',
                                                         'Masculino',
                                                         'Feminino'))

  # RACACOR: fator
  df$RACACOR <- factor(df$RACACOR, levels = c(1:5), labels = c('Branca',
                                                               'Preta',
                                                               'Amarela',
                                                               'Parda',
                                                               'Indígena'))

  # PESO: Inteiro (gramas)
  df$PESO <- as.integer(df$PESO)

  # IDANOMAL: fator (anomalia)
  df$IDANOMAL <- factor(df$IDANOMAL, levels = c(1:2), labels = c('Sim',
                                                                 'Não'))

  # DTCADASTRO: Data
  df$DTCADASTRO <- as.Date(df$DTCADASTRO, format = '%d%m%Y')

  # DTRECEBIM: Data
  df$DTRECEBIM <- as.Date(df$DTRECEBIM, format = '%d%m%Y')

  # DTRECEBIM: Data
  df$DTRECORIGA <- as.Date(df$DTRECORIGA, format = '%d%m%Y')

  # DTNASCMAE: Data
  df$DTNASCMAE <- as.Date(df$DTNASCMAE, format = '%d%m%Y')

  # RACACORMAE: fator
  df$RACACORMAE <- factor(df$RACACORMAE, levels = c(1:5), labels = c('Branca',
                                                                     'Preta',
                                                                     'Amarela',
                                                                     'Parda',
                                                                     'Indígena'))

  # ESCMAE2010: Fator
  df$ESCMAE2010 <- factor(df$ESCMAE2010, levels = c(1:5, 9), labels = c('Nenhuma',
                                                                        '1 a 3 anos',
                                                                        '4 a 7 anos',
                                                                        '8 a 11 anos',
                                                                        '12 e mais',
                                                                        'Ignorado'))

  # Retorna BD formatado
  return(df)
}
