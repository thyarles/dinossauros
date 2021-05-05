# Módulo responsável pela geração dos gráficos

# Leitura da library
import('ggplot2')

# Gráfico
export('boxplot')
boxplot <- function(x, y) {
  ggplot(as.data.frame(tabAlcool), aes(x = Idade, y = Freq, fill = Alcool_vida)) +
    geom_col(position = "dodge") +
    labs(x = "Idade dos alunos", y = "Quantidade de alunos", fill = "Consome álcool?") +
    geom_text(aes(label = Freq), colour = "white", size = 3, vjust = 1.5, position = position_dodge(.9))
}


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
gravaEmDisco <- function() {
  ggsave(
    'png/figura-%03d.png',
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
