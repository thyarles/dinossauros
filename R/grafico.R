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

