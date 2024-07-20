# Creditos
## Titulo: Calculando a rotatividade dos servidores comissionados desde 2013
## Scrip R por: Daniel Marques Vieira, mestrando em Ciencia Politica pela Universidade de Brasilia

# Pacotes----

pacman::p_load("tidyverse",
               "haven",
               "lubridate",         
               "janitor",
               "readxl",
               "stringr", 
               "magrittr",
               "srvyr",
               "survey",
               "ggplot2",
               "dplyr",
               "scales",
               "readr",
               "rmarkdown",
               "officer",
               "knitr",
               "purrr",
               "tools")

setwd("C:/Users/danie/Documentos/Mestrado/Pre_Projeto/Dados/Servidores/Cadastros")

# Organizando os dados----

## Obtencao:----
## Os dados dos servidores publicos sao disponibilizados no Portal da Tranparencia
## no formato CSV, com um arquivo referente a cada mes. Baixei os arquivos um por um,
## coloquei na mesma pasta e alterei o titulo dos arquivos para corresponder ao formato
## de data AAAAMMDD, que poderia ser lido e interpretado pelo R

## Criando um objeto com o endereco da pasta dos arquivos .csv: 

pastadoscsvs <- "./csvs"

## Listando os arquivos em outro objetos:
arquivos_csv <- list.files(path = pastadoscsvs, pattern = "\\.csv$", full.names = TRUE)

## Lendo os arquivos csv:----
### Ler todos os arquivos CSV, adicionando uma coluna de identificacao "origem".
### Assim, posso identificar a qual mes aqueles registro diz respeito.
### Em seguida, converto todas as colunas para "character", o que permite maior
### manipulacao dos dados. Por fim, armazeno em uma lista de data frames

lista_mensal <- arquivos_csv %>%
  map(~ read_csv2(.x, locale = locale(encoding = "ISO-8859-1")) %>%
        ### Remover linhas onde SIGLA_FUNCAO ? "-1" ou "-11" (excluindo os servidores
        ### nao comissionados):
        filter(SIGLA_FUNCAO != "-1" & SIGLA_FUNCAO != "-11") %>% 
        ### Adicionando a coluna 'origem' sem extensao do tipo de arquivo,
        ### para identificar o mes a que o dado se refere:
        mutate(origem = tools::file_path_sans_ext(basename(.x)),  
               ID_AND_CARGO = paste(NOME, CPF, SIGLA_FUNCAO, NIVEL_FUNCAO, COD_ORG_LOTACAO, sep = "_"))) %>%
        ### Mantenho apenas as colunas criadas 'ID_AND_CARGO' e 'origem, que
        ### cont?m apenas os dados que vou precisar. Reduzo assim a necessidade de
        ### processamento computacional a cada operacao
        map(~ select(., ID_AND_CARGO, origem))  

## Nomeia cada data frame na lista de acordo com o nome do arquivo de origem, ou seja, a data
names(lista_mensal) <- arquivos_csv %>%
  map(~ tools::file_path_sans_ext(basename(.x)))

## Adiciona colunas correspondentes aos nomes das databases originais e preenche com 1
lista_mensal <- lista_mensal %>%
  imap(~ mutate(.x, !!.y := 1)) %>%
  map(~ select(., -origem))  # Remove a coluna 'origem'

## Unindo as databases:----
## Uno com "full_join", que preserva todos os dados de servido-cargo. 
## Com o procedimento anterior, cada database
## originar? uma coluna, marcada com "1" para indicar a presen?a do servidor-cargo
## no quadro de funcionarios comissionados. Quando um determinado servidor-cargo
## nao aparecer em determinado mes, sera preenchido com "0"
comissionados_mes_a_mes <- reduce(lista_mensal, full_join, by = "ID_AND_CARGO") %>%
  replace(is.na(.), 0)  ## Substitui NA por 0

# Calculando o indice de rotatividade----

## Inicializa um vetor para armazenar os resultados parciais. Etapa burocratica
resultados <- c()
n_comissionados <- c()

## Itera sobre as colunas, comecando da segunda ate a ultima
for(i in 2:ncol(comissionados_mes_a_mes)) {
  col_name <- colnames(comissionados_mes_a_mes)[i]
  prev_col_name <- if (i == 2) NULL else colnames(comissionados_mes_a_mes)[i - 1]
  
  if (is.null(prev_col_name)) {
    ## Para a primeira coluna, prev_col_name ? NULL, entao nao ha comparacao
    count_equal_rows <- NA
    count_comissionados <- comissionados_mes_a_mes %>%
      filter(.[[col_name]] == 1) %>%
      nrow()
  } else {
    ## Conta as linhas onde os valores sao iguais na coluna atual e na coluna imediatamente anterior,
    ## ignorando os casos onde ambos os valores sao 0
    count_equal_rows <- comissionados_mes_a_mes %>%
      filter(.[[prev_col_name]] == .[[col_name]] & .[[prev_col_name]] != 0) %>%
      nrow()
    
    ## Conta o numero de linhas marcadas com "1" na coluna atual
    count_comissionados <- comissionados_mes_a_mes %>%
      filter(.[[col_name]] == 1) %>%
      nrow()
  }
  
  ## Adiciona os resultados aos vetores
  resultados <- c(resultados, count_equal_rows)
  n_comissionados <- c(n_comissionados, count_comissionados)
  
  ## Imprime o resultado para a coluna atual
  print(paste("Numero de linhas onde os valores sao iguais entre", prev_col_name, "e", col_name, ":", count_equal_rows))
  print(paste("Numero de linhas marcadas com '1' na coluna", col_name, ":", count_comissionados))
}

## Cria a tabela de resultados com a nova coluna 'n_comissionados', para "n?mero
## efetivo de comissionados em determinado mes"
calculo_rotatividade <- data.frame(mes = colnames(comissionados_mes_a_mes)[2:ncol(comissionados_mes_a_mes)], 
                                   inalterados = resultados,
                                   n_comissionados = n_comissionados)

## Conceituacao e formula:----
## Adiciona a coluna 'rotatividade'. Aqui aplica-se o conceito da rotatividade
## baseada na perman?ncia, emprestado do ramo dos Recursos Humanos, mas aplicado
## para calcular rotatividade de servidores comissionados por Lopez, Bugarin e Bugarin (2015)
calculo_rotatividade <- calculo_rotatividade %>%
  mutate(rotatividade = if_else(!is.na(lag(n_comissionados)) & !is.na(inalterados),
                                (lag(n_comissionados) - inalterados) / lag(n_comissionados),
                                NA_real_))

## Converte os valores da coluna "mes" para data
calculo_rotatividade <- calculo_rotatividade %>%
    mutate(mes = ymd(mes))

## Adiciona a coluna 'percent_rotat' como percentual da coluna 'rotatividade'
calculo_rotatividade <- calculo_rotatividade %>%
  mutate(percent_rotat = rotatividade * 100)

## Salva o dataframe como um arquivo CSV
write_csv(calculo_rotatividade, "calculo_rotatividade.csv")

## Exibe a tabela de resultados
print(calculo_rotatividade)

## Uma leve olhada nos dados mostra que as mudancas ocorridas em um mes sao computadas
## apenas no mes seguinte. Portanto, optei por adiantar os dados de rotatividade
## para que correspondam ao mes em que efetivamente ocorreram.
## Para preservar a base de dados original, optei por criar uma nova com a alteracao

calculo_rotatividade_b <- calculo_rotatividade %>%
  mutate(rotatividade = lead(rotatividade),
         percent_rotat = lead(percent_rotat))

## Filtrar fora os ultimos valores que agora sao NA
calculo_rotatividade_b <- calculo_rotatividade_b %>%
  filter(!is.na(rotatividade))

write_csv(calculo_rotatividade_b, "calculo_rotatividade_b.csv") #disponível no Github


# E possivel retomar daqui: 
calculo_rotatividade_b <- read_csv("calculo_rotatividade_b.csv") %>%
  mutate(mes = ymd(mes))

# Grafico de rotatividade.----


# Criar um dataframe auxiliar para os eventos
eventos <- data.frame(
  data = as.Date(c("2014-10-01", "2018-10-01", "2022-10-01", "2016-10-01", "2020-10-01",
                   "2015-01-01", "2019-01-01", "2023-01-01", "2017-01-01", "2021-01-01")),
  evento = factor(c("Elei??es gerais", "Elei??es gerais", "Elei??es gerais", "Elei??es municipais", "Elei??es municipais",
                    "Posse (Geral)", "Posse (Geral)", "Posse (Geral)", "Posse (Municipal)", "Posse (Municipal)"),
                  levels = c("Elei??es gerais", "Posse (Geral)", "Elei??es municipais", "Posse (Municipal)"))
)

# Grafico indicando a correlacao (esperada) entre rotatividade e eleicoes
ggplot(calculo_rotatividade_b, 
       aes(x = mes, group = 1, y = rotatividade, label = rotatividade), stat = "identity") +
  geom_line() +
  theme_minimal() +
  labs(y= "% Rotatividade", x = "Mes") +
  ggtitle("Rotatividade em cargos comissionados no Executivo Federal\nJan/2013 a Mar/2023, considerando eleições") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  scale_y_continuous(labels = percent) + 
  scale_x_date(date_breaks = "6 month", limits = as.Date(c("2013-02-01", "2024-03-01")),
               date_labels = "%b %y") +
  geom_vline(data = eventos, aes(xintercept = data, color = evento, linetype = evento), size = 1, alpha = 0.5) +
  scale_color_manual(name = "Eventos",
                     values = c("Eleições gerais" = "orange", "Posse (Geral)" = "orange", 
                                "Eleições municipais" = "cyan", "Posse (Municipal)" = "cyan"),
                     labels = c("Eleições gerais", "Posse (Geral)", "Eleições municipais", "Posse (Municipal)")) +
  scale_linetype_manual(name = "Eventos",
                        values = c("Eleições gerais" = "solid", "Posse (Geral)" = "dotted", 
                                   "Eleições municipais" = "solid", "Posse (Municipal)" = "dotted")) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "dotted", "solid", "dotted"), size = 1)),
         linetype = "none")



# Grafico de tendencia mostrando o crescimento da rotatividade
ggplot(calculo_rotatividade_b, 
       aes(x = mes, group = 1, y = rotatividade, label = rotatividade), stat = "identity") +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "solid", size = 0.5, alpha = 0.5) +  # Adiciona a linha de tendência
  theme_minimal() +
  labs(y= "% Rotatividade", x = "Mes") +
  ggtitle("Tendência linear - Rotatividade em cargos comissionados\nno Executivo Federal - Jan/2013 a Mar/2023") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = percent) + 
  scale_x_date(date_breaks = "6 month", limits = as.Date(c("2013-02-01", "2024-03-01")),
               date_labels = "%b %y")

# Demarcando meses antes e depois do afastamento de Dilma Rousseff
calculo_rotatividade_b <- calculo_rotatividade_b %>%
  mutate(ano = year(mes),
         pos_imp = if_else(mes > ymd("2016-08-31"), 1, 0))

## Calculando média e intervalo de confiança

dados_agrupados <- calculo_rotatividade_b %>%
  group_by(ano) %>%
  summarise(
    media_rotatividade = mean(rotatividade, na.rm = TRUE),
    sd_rotatividade = sd(rotatividade, na.rm = TRUE),
    n = n(),
    erro_padrao = sd_rotatividade / sqrt(n),
    ic_inferior = media_rotatividade - qt(0.975, df = n - 1) * erro_padrao,
    ic_superior = media_rotatividade + qt(0.975, df = n - 1) * erro_padrao
 
## Gráfico de densidade

ggplot(calculo_rotatividade_b, aes(x = percent_rotat, fill = factor(pos_imp, labels = c("Pré-Afastamento", "Pós-Afastamento")))) +
  geom_density(alpha = 0.6, color = NA) +
  geom_vline(data = summary_stats, aes(xintercept = mean_percent_rotat, color = factor(pos_imp, labels = c("Pré-Afastamento", "Pós-Afastamento"))), linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("#1F77B4", "#FF7F0E")) +
  scale_color_manual(values = c("#1F77B4", "#FF7F0E")) + 
  labs(
    title = "Distribuição de densidade da rotatividade antes\ne depois do afastamento de Dilma",
    x = "Percentual de Rotatividade",
    y = "Densidade",
    fill = "Período",
    color = "Período"
  ) +
  theme_minimal()

# Grafico de pontos

ggplot(calculo_rotatividade_b, aes(y = percent_rotat, x = factor(pos_imp, labels = c("Pré-Afastamento", "Pós-Afastamento")), color = factor(pos_imp))) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("#1F77B4", "#FF7F0E")) + # cores mais escuras
  labs(
    title = "Rotatividade mensal antes\ne depois do afastamento de Dilma",
    y = "Percentual de Rotatividade",
    x = "Período",
    color = "Grupo"
  ) +
  theme_minimal()

# Testando a correlação (teste t)

t_test_result <- t.test(percent_rotat ~ pos_imp, data = calculo_rotatividade_b)

tidy_t_test <- tidy(t_test_result)

kable(tidy_t_test, format = "html") %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed"))

tidy_t_test %>%
  gt() %>%
  tab_header(
    title = "Resultados do Teste t de Student",
    subtitle = "Comparação das médias de percent_rotat por pos_imp"
  )
