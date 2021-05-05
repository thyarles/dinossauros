# Módulo responsável pela geração dos gráficos

# Importes para a solução da questão
import('ggplot2')
import('stats')

# Configura tema para todos os gráficos
export('configuraTema')
configuraTema <- function() {
  theme_set(
    theme_classic() %+replace%
      theme(
        plot.title = element_text(face = 'bold',
                                  hjust = .5,
                                  margin = margin(0, 0, 5, 0),
                                  size = 14),
        plot.subtitle = element_text(family = 'Playfair',
                                     hjust = .5,
                                     margin = margin(0, 0, 10, 0)),
        plot.tag = element_text(size = rel(0.8),
                                vjust = 1)
  ))
}

# Salva gráfico na pasta png
export('gravaEmDisco')
gravaEmDisco <- function(arquivo) {
  ggsave(
    filename = paste('png/', arquivo, '.png', sep = ''),
    plot = last_plot(),
    device = NULL,
    path = NULL,
    scale = 1,
    width = NA,
    height = NA,
    units = c("in", "cm", "mm"),
    dpi = 300,
    limitsize = TRUE,
  )
}

# Salva gráfico na pasta png
export('geraTabela')
geraTabela <- function(titulo, tabela) {
  cat ('\n  >>>', titulo, '\n')
  print(addmargins(tabela))
  cat ('\n')
}
