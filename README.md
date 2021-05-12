# SINASC 2016

    [1] "Carregando e/ou instalando bibliotecas necessárias..."

     ### Início da execução.
     -----------------------------------------------------------------

     # Carregando tabelas auxiliares...

     # Carregando e calculando observações por UF...
       --> AC tem 15773 observações.
       --> AL tem 48164 observações.
       --> AM tem 76703 observações.
       --> AP tem 15521 observações.
       --> BA tem 199830 observações.
       --> CE tem 126246 observações.
       --> DF tem 43340 observações.
       --> ES tem 53413 observações.
       --> GO tem 95563 observações.
       --> MA tem 110493 observações.
       --> MG tem 253520 observações.
       --> MS tem 42432 observações.
       --> MT tem 53531 observações.
       --> PA tem 137681 observações.
       --> PB tem 56083 observações.
       --> PE tem 130733 observações.
       --> PI tem 46986 observações.
       --> PR tem 155066 observações.
       --> RJ tem 219129 observações.
       --> RN tem 45366 observações.
       --> RO tem 26602 observações.
       --> RR tem 11376 observações.
       --> RS tem 141411 observações.
       --> SC tem 95313 observações.
       --> SE tem 32218 observações.
       --> SP tem 601437 observações.
       --> TO tem 23870 observações.

     # Calculando amostra proporcional para cada UF...
     ----> Tabela: Número de observações totais e da amostra por UF
     -----------------------------------------------------------------
     SIGLA_UF NUM_OBS_SINASC AMOSTRA
           AC          15773      11
           AL          48164      34
           AM          76703      54
           AP          15521      11
           BA         199830     140
           CE         126246      88
           DF          43340      30
           ES          53413      37
           GO          95563      67
           MA         110493      77
           MG         253520     177
           MS          42432      30
           MT          53531      37
           PA         137681      96
           PB          56083      39
           PE         130733      91
           PI          46986      33
           PR         155066     109
           RJ         219129     153
           RN          45366      32
           RO          26602      19
           RR          11376       8
           RS         141411      99
           SC          95313      67
           SE          32218      23
           SP         601437     421
           TO          23870      17
     -----------------------------------------------------------------

     # Configrando o SEED para 1234...

     # Salvando amostras de cada UF no dataframe AMOSTRAS...

     # Limpando dados para otimizar memória...

     # Formatando AMOSTRA de acordo com dicionário de dados...

     # Configurando tema GGPLOT para manter padrão...

     # Gerando dados para a QUESTÃO 01...
       --> Criando data frames para gerar tabelas e gráficos...
       --> Salvando gráfico Partos por tipo e dia da semana no diretório png/q01 ...
       --> Salvando gráfico Partos por tipo de dia no diretório png/q01 ...
       --> Salvando gráfico Partos por turno no diretório png/q01 ...
       --> Salvando gráfico Partos por tipo, idade e dia no diretório png/q01 ...
       --> Salvando gráfico Partos por tipo, idade, dia e estado civil no diretório png/q01 ...
     ----> Tabela: Q01 - Parto por dia da semana
     -----------------------------------------------------------------      
           Vaginal Cesáreo  Sum
      Dom.     125     103  228
      Seg.     122     172  294
      Ter.     140     187  327
      Qua.     108     189  297
      Qui.     137     169  306
      Sex.     138     161  299
      Sáb.     132     116  248
      Sum      902    1097 1999
     -----------------------------------------------------------------
     ----> Tabela: Q01 - Parto por tipo de dia na semana
     -----------------------------------------------------------------               
                    Vaginal Cesáreo  Sum
      Fim de semana     257     219  476
      Dia útil          645     878 1523
      Sum               902    1097 1999
     -----------------------------------------------------------------
     ----> Tabela: Q01 - Parto por hora
     -----------------------------------------------------------------             
                  Vaginal Cesáreo  Sum
      0 às 6:59       246      94  340
      7 às 12:59      242     421  663
      13 às 18:59     232     377  609
      19 às 23:59     181     203  384
      Sum             901    1095 1996
     -----------------------------------------------------------------
     ----> Tabela: Q01 - Parto por idade
     -----------------------------------------------------------------              
                   Vaginal Cesáreo  Sum
      13 a 17 anos     175      96  271
      18 a 27 anos     463     506  969
      28 a 37 anos     246     443  689
      38 a 48 anos      17      52   69
      Sum              901    1097 1998
     -----------------------------------------------------------------
     ----> Tabela: Q01 - Parto por Peso
     -----------------------------------------------------------------           
                Vaginal Cesáreo  Sum
      0,4 a 1,4      14      21   35
      1,5 a 1,9      13      25   38
      2,0 a 2,4      57      69  126
      2,5 a 2,9     263     242  505
      3,0 a 3,4     387     460  847
      3,5 a 4,8     167     280  447
      Sum           901    1097 1998
     -----------------------------------------------------------------
     ----> Tabela: Q01 - Parto por Estado civil da mãe
     -----------------------------------------------------------------               
                    Vaginal Cesáreo  Sum
      Solteira          445     438  883
      Casada            181     417  598
      União estável     263     221  484
      Outros             13      21   34
      Sum               902    1097 1999
     -----------------------------------------------------------------
     ----> Tabela: Q01 - Parto por Escolariadade da mãe
     -----------------------------------------------------------------             
                  Vaginal Cesáreo  Sum
      Nenhuma           5       7   12
      1 a 3 anos       34      16   50
      4 a 7 anos      199     131  330
      8 a 11 anos     562     648 1210
      12 e mais        86     285  371
      Sum             886    1087 1973
     -----------------------------------------------------------------

     # Gerando dados para a QUESTÃO 02...
       --> Salvando gráfico Percentual das mães por estado civil no diretório png/q02 ...
       --> Salvando gráfico Percentual do estado civil das mães por faixa etária no diretório png/q02 ...
       --> CPI: 0.318577770777836
       --> C* para as variáveis estado civil e idade: 0.450537004104602

     # Gerando dados para a QUESTÃO 03...
       --> Freq máxima..: 1.05%
       --> Menor peso...: 365 gramas
       --> Maior peso...: 4800 gramas
       --> Faixas
      (250,500]   (500,750]  (750,1000] (1000,1250] (1250,1500] (1500,1750] 
              2           6           7           5          12          14 
    (1750,2000] (2000,2250] (2250,2500] (2500,2750] (2750,3000] (3000,3250] 
             22          39          70         160         294         454 
    (3250,3500] (3500,3750] (3750,4000] (4000,4250] (4250,4500] (4500,4750] 
            395         289         143          62          18           7 
    (4750,5000] 
              1 

       --> Número de classes para a variável PESO: 19
       --> Frequência relativatFreq
             1          2          5          6          7         12         14 
    0.05263158 0.05263158 0.05263158 0.05263158 0.10526316 0.05263158 0.05263158 
            18         22         39         62         70        143        160 
    0.05263158 0.05263158 0.05263158 0.05263158 0.05263158 0.05263158 0.05263158 
           289        294        395        454 
    0.05263158 0.05263158 0.05263158 0.05263158 

       --> Salvando gráfico Histograma do peso dos nascidos em hospitais no diretório png/q03 ...
       --> Medidas de posição central - MÉDIA dos pesos: 3173.9125
       --> Medidas de posição central - MEDIANA dos pesos: 3207.5
       --> Medidas de posição central - MODA dos pesos: 3100
       --> Quartil dos pesos
         0%     25%     50%     75%    100% 
     365.00 2910.00 3207.50 3516.25 4800.00 

       --> Decil dos pesos
        0%    10%    20%    30%    40%    50%    60%    70%    80%    90%   100% 
     365.0 2554.5 2835.0 2985.0 3100.0 3207.5 3330.0 3455.0 3600.0 3800.0 4800.0 

       --> Amplitude do peso: 4435
       --> Desvio médio do peso: 403.66
       --> Desvio padrão do peso: 556.09
       --> Variãncia do peso: 309237.5
       --> Coeficiente de variação do peso: 0.18
       --> Aplitude interquartílica: 606.25
       --> Salvando gráfico Peso dos recém-nascidos em hospitais no diretório png/q03 ...

     # Gerando dados para a QUESTÃO 04...
       --> Produzindo gráficos para responder à questão...
       --> Salvando gráfico Tipos de partos e peso no diretório png/q04 ...
       --> Salvando gráfico Escolaridade da mãe e peso no diretório png/q04 ...
       --> Salvando gráfico Idade da mãe e peso no diretório png/q04 ...
       --> Salvando gráfico Hora de nascimento e peso no diretório png/q04 ...
       --> Salvando gráfico Estado civil e peso no diretório png/q04 ...
       --> Salvando gráfico Dia e peso no diretório png/q04 ...
       --> Salvando gráfico Tipo dia e peso no diretório png/q04 ...
       --> Salvando gráfico Tempo de gestação e peso no diretório png/q04 ...
       --> Salvando gráfico Tipo de gravidez e peso no diretório png/q04 ...
       --> Salvando gráfico Cor e peso no diretório png/q04 ...

     # Gerando dados para a QUESTÃO 05...
       --> Os partos cirúrgicos representam 55.414 % dos partos no Brasil
       --> Salvando gráfico Partos de acordo com a escolaridade da mãe no diretório png/q05 ...
       --> Salvando gráfico Partos de acordo com a idade da mãe no diretório png/q05 ...
       --> Salvando gráfico Tipos de parto pela idade da mãe no diretório png/q05 ...
       --> Salvando gráfico Partos de acordo com idade agrupada no diretório png/q05 ...
       --> Salvando gráfico Partos de acordo com a cor da mãe no diretório png/q05 ...
     ----> Tabela: Q05 - Contigência Partos por Idade
     -----------------------------------------------------------------          
               Vaginal Cesáreo  Sum
      13 |- 18     118      58  176
      18 |- 23     240     220  460
      23 |- 28     211     245  456
      28 |- 33     137     235  372
      33 |- 38     105     209  314
      38 |- 43      28      67   95
      43 |-          1      10   11
      Sum          840    1044 1884
     -----------------------------------------------------------------
     ----> Tabela: Q05 - Proporção das idades das mães
     -----------------------------------------------------------------          
               Vaginal Cesáreo     Sum
      13 |- 18  14.048   5.556  19.604
      18 |- 23  28.571  21.073  49.644
      23 |- 28  25.119  23.467  48.586
      28 |- 33  16.310  22.510  38.820
      33 |- 38  12.500  20.019  32.519
      38 |- 43   3.333   6.418   9.751
      43 |-      0.119   0.958   1.077
      Sum      100.000 100.001 200.001
     -----------------------------------------------------------------
     ----> Tabela: Q05 - Proporção dos partos por idade
     -----------------------------------------------------------------          
               Vaginal Cesáreo     Sum
      13 |- 18  67.045  32.955 100.000
      18 |- 23  52.174  47.826 100.000
      23 |- 28  46.272  53.728 100.000
      28 |- 33  36.828  63.172 100.000
      33 |- 38  33.439  66.561 100.000
      38 |- 43  29.474  70.526 100.000
      43 |-      9.091  90.909 100.000
      Sum      274.323 425.677 700.000
     -----------------------------------------------------------------
     ----> Tabela: Q05 - Contigência de partos por escolaridade
     -----------------------------------------------------------------             
                  Vaginal Cesáreo  Sum
      Nenhuma           5       7   12
      1 a 3 anos       32      15   47
      4 a 7 anos      187     125  312
      8 a 11 anos     541     621 1162
      12 e mais        75     276  351
      Sum             840    1044 1884
     -----------------------------------------------------------------
     ----> Tabela: Q05 - Proporção dos partos por idade proporcional
     -----------------------------------------------------------------             
                  Vaginal Cesáreo     Sum
      Nenhuma      41.667  58.333 100.000
      1 a 3 anos   68.085  31.915 100.000
      4 a 7 anos   59.936  40.064 100.000
      8 a 11 anos  46.558  53.442 100.000
      12 e mais    21.368  78.632 100.000
      Sum         237.614 262.386 500.000
     -----------------------------------------------------------------
     ----> Tabela: Q05 - Contigência de partos por cor
     -----------------------------------------------------------------          
               Vaginal Cesáreo  Sum
      Branca       235     461  696
      Preta         53      54  107
      Amarela        2       5    7
      Parda        545     519 1064
      Indígena       5       5   10
      Sum          840    1044 1884
     -----------------------------------------------------------------
     ----> Tabela: Q05 - Proporção de contigência de partos por cor
     -----------------------------------------------------------------          
               Vaginal Cesáreo     Sum
      Branca    33.764  66.236 100.000
      Preta     49.533  50.467 100.000
      Amarela   28.571  71.429 100.000
      Parda     51.222  48.778 100.000
      Indígena  50.000  50.000 100.000
      Sum      213.090 286.910 500.000
     -----------------------------------------------------------------
       --> CPI: 0.203520174954769
       --> C* para as variáveis tipo de parto e idade: 0.287820991637579
       --> CPE: 0.236710726363532
       --> C* para as variáveis tipo de parto e escolaridade: 0.334759519582493
       --> CPC: 0.161934320599145
       --> C* para as variáveis tipo de parto e Raça: 0.229009712404984

     ### Script executado com sucesso!
