# Módulo responsável pela geração dos gráficos

# Importes para a solução da questão com supressão de alertas para deixar
# a saída mais limpa
suppressMessages(suppressWarnings(suppressPackageStartupMessages(
  import('ggplot2'))))
suppressMessages(suppressWarnings(suppressPackageStartupMessages(
  import('stats'))))

# Configura tema para todos os gráficos
export('configuraTema')
configuraTema <- function() {
  theme_set(
    theme_classic() %+replace%
      theme(
        plot.title = element_text(face = 'bold',
                                  hjust = .5,
                                  margin = margin(0, 0, 5, 0),
                                  size = rel(1)),
        plot.subtitle = element_text(family = 'Playfair',
                                     hjust = .5,
                                     margin = margin(0, 0, 10, 0),
                                     size = rel(0.8)),
        plot.tag = element_text(size = rel(0.5),
                                vjust = 1),
        axis.title.x = element_text(vjust = -2),
        axis.title.y = element_text(vjust = 3, angle = 90)
  ))
}

# Salva gráfico na pasta png
export('gravaEmDisco')
gravaEmDisco <- function(questao, titulo, altura = NA, largura = NA, escala = 1.0) {
  # Monta caminho
  diretorio <- paste('png/', questao, sep = '')
  # Cria diretório
  dir.create(diretorio, showWarnings = FALSE, recursive = TRUE, mode = "0777")
  # Nome do arquivo
  arquivo <- paste(titulo,  '.png', sep = '')
  # Mostra mensagem
  msgB(paste('Salvando gráfico', titulo, 'no diretório', diretorio, '...'))
  suppressMessages(suppressWarnings(
    ggsave(
      filename = arquivo,
      plot = last_plot(),
      device = NULL,
      path = diretorio,
      scale = escala,
      width = largura,
      height = altura,
      units = 'cm',
      #units = c("in", "cm", "mm"),
      dpi = 300,
      limitsize = TRUE
    )
  ))

}

# Imprime tabela com totalizadores
export('mTab')
mTab <- function(questao, titulo, tabela) {
  cat ('\n ----> Tabela:', questao, '-', titulo)
  cat ('\n -----------------------------------------------------------------')
  print(addmargins(tabela))
  cat (' -----------------------------------------------------------------')
}

# Formata impressão de tabela
export('tab')
tab <- function(titulo, df) {
  cat ('\n ----> Tabela:', titulo)
  cat ('\n -----------------------------------------------------------------\n')
  print(df, row.names = FALSE)
  cat (' -----------------------------------------------------------------')
}

# Formata mensagens do tipo A
export('msgA')
msgA <- function(titulo) {
  cat ('\n\n', ' # ', titulo, '...', sep = '')
}

# Formata mensagens do tipo B
export('msgB')
msgB <- function(titulo) {
  cat ('\n', '   --> ', titulo, sep = '')
}
