#############################################################
#Bibliotecas                                                #
#############################################################
#usado na função insert
install.packages("R.utils")
#usado na geração do gráfico de correlação
install.packages("ggcorrplot")
#usada nos gráficos
install.packages("ggplot2")
install.packages("ggridges")
#usado para tabelas
install.packages("kableExtra")
install.packages("httr")
install.packages("purrr")
install.packages("xml2")
install.packages("rvest")
install.packages("janitor")
install.packages("lubridate")


library(R.utils)
library(ggplot2)
library(ggcorrplot)
library(ggridges)
library(scales)
library(gridExtra)
library(grid)
library(gtable)
library(knitr)
library(kableExtra)
library(magrittr)
library(httr)
library(purrr)
library(xml2)
library(rvest)
library(janitor)
library(lubridate)



#############################################################
#Funções                                                    #
#############################################################

# scraper_CETESB
# ==============
#
# Permite exportar dados do sistema de dados de qualidade do ar da CETESB: Qualar
# https://qualar.cetesb.sp.gov.br/qualar/exportaDados.do?method=filtrarEstacoes
# A funcao, faz uma requisicaoo via POST ao sistema Qualar e converte a resposta
# em um dataframe
# 
# Dados de entrada:
# - station: Codigo da estacao em que se quer coletar os dados.
#            As id's das estacoes, estao disponiveis em
#            https://github.com/williamorim/Rpollution-blog/blob/master/content/blog/data/station_ids.csv
#
#            Ex: P. D.Pedro II, Campinas-Taquaral, Campinas-Centro,
#                Campinas V.Unicao, Cubatao...
#
# - parameter: Codigo do tipo de medicao que se quer coletar.
#              As id's das medicoes estao disponiveis em:
#              https://github.com/williamorim/Rpollution-blog/blob/master/content/blog/data/param_ids.csv
#
#              Exemplo: O3 (Ozonio), CO (Monoxido de Carbono), RADUV (Radiacao Ultra-violeta), ...
#
# - start_time: Data inicial do periodo em que se quer coletar os dados no formato dd/mm/ano
#               Exemplo: "04/03/2018"
#
# - end_time: Data final do perioodo e em que se quer coletar os dados no formato dd/mm/ano
#             Exemplo: "05/03/2018"
#
# - type: E o tipo de dado. Media Horaria (P) ou Media Movel (M)
#
# - login: login do usuario
#
# - password: senha do usuario
#
# - invalidData: Indica se queremos coletar dados invalidos "on" ou vazio
#
# - network: Tipo de rede Manual (M), Automatico (A) ou Passivo (P)

scraper_CETESB <- function(station, parameter, start_time, end_time, type = "P", login, password, invalidData = "on", network = "A") {
  
  # Get and save the cookie
  res <- GET("https://qualar.cetesb.sp.gov.br/qualar/home.do")
  my_cookie <- cookies(res)$value %>% purrr::set_names(cookies(res)$name)
  
  # login to the Qualar
  url <- "https://qualar.cetesb.sp.gov.br/qualar/autenticador"
  
  res <- POST(
    url, 
    body = list(
      cetesb_login = login,
      cetesb_password = password,
      enviar = "OK"
    ), 
    encode = "form",
    set_cookies(my_cookie)
  )
  
  # Use REST POST to request station data
  url <- "https://qualar.cetesb.sp.gov.br/qualar/exportaDados.do"
  
  res <- POST(
    url,
    query = list(method = "pesquisar"),
    body = list(
      irede = network,
      dataInicialStr  = start_time,
      dataFinalStr = end_time,
      cDadosInvalidos = invalidData,
      iTipoDado = type,
      estacaoVO.nestcaMonto = station,
      parametroVO.nparmt = parameter
    ),
    encode = "form",
    set_cookies(my_cookie)
  )
  
  content(res) %>% 
    rvest::html_table(fill = TRUE) %>%
    extract2(2)
}



# TratamentoDados_CETESB
# ======================
# Tratamentos dos dados obtidos da CETESB (Qualar)

tratamentoDados_CETESB <- function(dataFrame,col_medicao) {
  
  
  # remove colunas vazias
  dataFrame <- dataFrame %>%
    janitor::remove_empty("cols")
  
  # Acertar o nome das colunas
  col_names <- as.character(dataFrame[1,])
  
  # Remove acentuacao das colunas
  col_names <- iconv(col_names,from="UTF-8",to="ASCII//TRANSLIT")
  
  dataFrame <- dataFrame %>% 
    magrittr::set_colnames(col_names) %>% 
    dplyr::slice(-1)
  
  # substitui o caracter virgula por ponto para representar casas decimais
  dataFrame[ , col_medicao] <- gsub(",",".",dataFrame[ , col_medicao])
  
  # Transformar a coluna da Media Horaria em numerico
  dataFrame[ , col_medicao ] <- as.numeric(dataFrame[ , col_medicao])
  
  return(dataFrame)
}



# consecutive
# ===========
# Funcao que verifica a ocorrencia de k valores consecutivos identicos no vetor
# A função retorna um vetor de valores logicos onde TRUE é a posicao em que seu valor é igual
# a k valores consecutivos posteriores ou anteriores dentro de vector.
#
# Dados de entrada:
# - vector: vetor em que se quer analisar
#
# - k: númerero inteiro que indica a quantidade de valores consecutivos

consecutive <- function(vector , k = 1) {
  n <- length(vector)
  result <- logical(n)
  
  for (i in (1+k):n)
    if (all(vector [(i-k):(i-1)] == vector[i]))
      result[i] <- TRUE
  for (i in 1:(n-k))
    if (all(vector [(i+1):(i+k)] == vector[i]))
      result[i] <- TRUE
  
  return(result)
}




# grafico_aggregado_mes
# =====================
# Gera um grafico de linha para o conjunto de dados da CETESB
# Dados de entrada:
# - vetorData: Vetor que contem as datas
#
# - vetorMediaHoraria: Vetor que contem as medidas a serem plotadas no grafico
#
# - parametroMedido: Nome do parametro medido (ex: O3, CO..)
#
grafico_aggregado_mes <- function(vetorData,vetorMediaHoraria,parametroMedido,nomeLabel) {
  # Pega a lista de meses do dataframe
  lista_meses <- month(as.POSIXct(as.character(vetorData), format = '%d/%m/%Y'))
  # agrega as medicoes por mes
  perMonth <- aggregate(vetorMediaHoraria,list(lista_meses),mean)
  # acerta o nome das colunas
  colnames(perMonth) <- c("Mes",parametroMedido)
  
  perMonth$Mes <- factor(month.abb[perMonth$Mes], levels = month.abb , ordered = TRUE)
  
  # executa um grafico de linha
  g <- ggplot(perMonth, aes_string(x = "Mes", y = parametroMedido, group = 1)) + geom_line (aes(colour = nomeLabel))
  return(g)
}



# Agrega_mediaMensal
# ==================
# 
# A funcao agrega os valores dos meses das medicoes da CETESB, calcula a media e retorna um dataframe
#
# Dados de entrada:
# - vetorData: Vetor que contem as datas
#
# - vetorMedidas: Vetor que contem as medidas
#
# - nomeMedida: Legenda da medida para se colocar no grafico (Ex: CO, O3..)


Agrega_mediaMensal <- function(vetorData,vetorMedidas,nomeMedida) {
  # Pega a lista de meses do dataframe
  lista_meses <- month(as.POSIXct(as.character(vetorData), format = '%d/%m/%Y'))
  
  Aggregate_perMonth <- aggregate(vetorMedidas,list(lista_meses),mean)
  colnames(Aggregate_perMonth) <- c("Mes",nomeMedida)
  
  return (Aggregate_perMonth)
  
}



# Add_period8H
# ==================
# 
# A funcao agrega os valores dos meses das medicoes da CETESB, calcula a media e retorna um dataframe
# Cria uma coluna periodo que compreende os valores
# periodo = 1 - Entre 1h e 8h
# periodo = 2 - Entre 9h e 16h
# perido  = 3 - Apos 17h
#
# Dados de entrada:
# - dataFrame: o data frame que se quer tratar
#
# - valorColunaMedida: valor da coluna que contem a horas para agrupamento do periodo

Add_period8H <- function (dataFrame,valorColunaMedida) {
  
  n <- length(dataFrame[ ,valorColunaMedida])
  
  periodo <- rep(NA,n)
  
  
  for (i in 1:n) {
    if (dataFrame[i,valorColunaMedida] >= 1 & dataFrame[i,valorColunaMedida] <= 8) {
      periodo[i] <- 1
    } else {
      if (dataFrame[i,valorColunaMedida] >= 9 & dataFrame[i,valorColunaMedida] <= 16) {
        periodo[i] <- 2
      } else {
        periodo[i] <- 3
      }
    }
  }
  
  dataFrame <- cbind(dataFrame,periodo)
}



# estação_do_ano
# ==============
# A função retorna a estação do ano referente a data de entrada (x)
#
# Dados de entrada:
# - x: data (independente do ano) 
estação_do_ano <- function(x) {
  verão <- as.Date("2012-12-22", format = "%Y-%m-%d") # verão
  outono <- as.Date("2012-03-20",  format = "%Y-%m-%d") # outono
  inverno <- as.Date("2012-06-21",  format = "%Y-%m-%d") # inverno
  primavera <- as.Date("2012-09-22",  format = "%Y-%m-%d") # primavera
  
  #converte a data de qualquer ano para uma data de 2012 que foi ano bissexto
  x <- as.Date(strftime(x, format="2012-%m-%d"))
  
  ifelse (x >= verão | x < outono, "verão",
          ifelse (x >= outono & x < inverno, "outono",
                  ifelse (x >= inverno & x < primavera, "inverno", "primavera")))
}



# remove_outlier
# ==============
# Funcao para remover o valor outlier do dataframe. A funcao retorna o data frame sem os valores outliers
#
# Dados de entrada:
# - dataFrame: Nome do data frame 
#
# - vetor: vetor do data frame que contem o valor outlier
#
# - valor_outlier: Valor outlier que se quer remover do data frame dataFrame
remove_outlier <- function(df,vetor, valor_outlier){
  df <- df[vetor < valor_outlier, ]
  return (df)
}




#############################################################
#Coleta de dados Cepagri                                    #
#############################################################
#cria uma conexão com base de dados que será utilizada na análise
con <- url("https://www.ic.unicamp.br/~zanoni/cepagri/cepagri.csv")

#inclui os nomes das colunas do arquivo que será lido
names <- c("horario", "temperatura", "vento", "umidade", "sensacao")

#carrega em cepagri a base de dados linhas, completando com NA as linhas nas 
#quais o número de observações é menor que o número esperado de colunas.
cepagri <- read.table(con, header = FALSE, fill = TRUE, sep = ";", col.names = names)

#armazena apenas os dados do intervalo de 01/01/2015 a 31/12/2018
cepagri[ , 1] <- as.POSIXct(as.character(cepagri[ , 1]), format = '%d/%m/%Y-%H:%M')
cepagri <- cepagri[cepagri[, 1] >= "2015-01-01" & cepagri[, 1] < "2019-01-01",]




#############################################################
#Coleta de dados Cetesb                                     #
#############################################################
#https://www.rpollution.com/blog/scraper-cetesb/

### =================================================== ###
### Coletando e tratando dados da camada de Ozonio (O3) ###
### =================================================== ###

# Station: Campinas-Taquaral, 276
# Parameter: Ozonio (O3), 63
# Periodo: 2017
Campinas_taquaral_O3_2017 <- scraper_CETESB(station = "276",
                                            parameter = "63", 
                                            start = "01/01/2017", 
                                            end = "31/12/2017", 
                                            type = "P", 
                                            login = "******************", 
                                            password = "*******", 
                                            invalidData = "on", 
                                            network = "A")

# Station: Campinas-Taquaral, 276
# Parameter: Ozonio (O3), 63
# Periodo: 2018
Campinas_taquaral_O3_2018 <- scraper_CETESB(station = "276",
                                            parameter = "63", 
                                            start = "01/01/2018", 
                                            end = "31/12/2018", 
                                            type = "P", 
                                            login = "******************", 
                                            password = "*******", 
                                            invalidData = "on", 
                                            network = "A")



### ============================================================ ###
### Coletando e tratando dados de Radiacao Ultra-violeta (RADUV) ###
### ============================================================ ###

# Station: Campinas-Taquaral, 276
# Parameter: Radiacao Ultra-violeta (RADUV), 56
# Periodo: 2017
Campinas_taquaral_RADUV_2017 <- scraper_CETESB(station = "276",
                                               parameter = "56", 
                                               start = "01/01/2017", 
                                               end = "31/12/2017", 
                                               type = "P", 
											   login = "******************", 
											   password = "*******", 
                                               invalidData = "on", 
                                               network = "A")



# Station: Campinas-Taquaral, 276
# Parameter: Radiacao Ultra-violeta (RADUV), 56
# Periodo: 2018
Campinas_taquaral_RADUV_2018 <- scraper_CETESB(station = "276",
                                               parameter = "56", 
                                               start = "01/01/2018", 
                                               end = "31/12/2018", 
                                               type = "P", 
											   login = "******************", 
											   password = "*******", 
                                               invalidData = "on", 
                                               network = "A")



### ====================================================== ###
### Coletando e tratando dados de Monoxido de Carbono (CO) ###
### ====================================================== ###

# Station: Campinas-Centro, 89
# Parameter: Monoxido de Carbono (CO), 16
# Periodo: 2018

Campinas_centro_CO_2018 <- scraper_CETESB(station = "89",
                                          parameter = "16", 
                                          start = "01/01/2018", 
                                          end = "31/12/2018", 
                                          type = "P", 
                                          login = "******************", 
                                          password = "*******",  
                                          invalidData = "on", 
                                          network = "A")




####################################################
#Preparação dos dados CEPAGRI                      #
####################################################
#obtém a classe de cada coluna
lapply(cepagri, class)

#tranforma fator em numerico, NAs serão introduzidos por coerção
cepagri[ , 2] <- as.numeric(levels(cepagri[ , 2]))[cepagri[ , 2]]

#remove linhas que contém N/A
cepagri <- cepagri[complete.cases(cepagri), ]

#analisar valores max e min para verficar se há outlier na variação de temperatura
min(cepagri[, 2])
max(cepagri[, 2])

#analisar valores max e min para verficar se há outlier na variação do vento
min(cepagri[, 3])
max(cepagri[, 3])

#analisar valores max e min para verficar se há outlier na variação da umidade
min(cepagri[, 4])
max(cepagri[, 4])

#analisar valores max e min para verficar se há outlier na variação da sensação térmica
min(cepagri[, 5])
max(cepagri[, 5])
#remove as linhas com valores acima de 90 para sensação térmica
cepagri <- remove_outlier(cepagri, cepagri$sensacao, 90)
max(cepagri[, 5])

#remove repetição consecutiva das 4 medições com mais de uma hora
diference <- rowSums(diff(as.matrix(cepagri[2:5])))
diference <- insert(diference, ats=1, NA)
#cria coluna auxiliar
cepagri$diference <- replace(diference, diference!=0, NA)
repconsec <- rle(cepagri$diference)
#cria vetor ordenado por mais repetidos
maiorrep <- sort((repconsec$lengths), decreasing = TRUE)
#6 medições (1 hora)
while(maiorrep[1] > 6) {
  #início de remoção de valores consecutivos repetidos
  start <- sum(repconsec$lengths[1:which(repconsec$lengths == maiorrep[1])[1]]) - maiorrep[1] + 1
  #remove linhas com valores consecutivos repetidos
  cepagri <- cepagri[-(start:(start+maiorrep[1]-1)), ]
  #verifica repetição consecutiva de valores
  repconsec <- rle(cepagri$diference)
  #cria vetor ordenado por mais repetidos
  maiorrep <- sort((repconsec$lengths), decreasing = TRUE)
}
#remove coluna auxiliar
cepagri <- cepagri[,-6]



###################################################
#Preparação dos dados CETESB                      #
###################################################
### Tratamento dos dados O3 2017 e 2018 ###
### =================================== ###


Campinas_taquaral_O3_2017 <- tratamentoDados_CETESB (Campinas_taquaral_O3_2017,10)
Campinas_taquaral_O3_2018 <- tratamentoDados_CETESB (Campinas_taquaral_O3_2018,10)


# Foram indentificados 3 outliers utilizando a funcao summary. Nas medicoes de 2017 e 2018, os valores
# 759.00 / 2017, 386.00 / 2017 e 431.00 / 2018 foram discrepantes do conjunto de medidas
# summary(Campinas_taquaral_O3_2017$`Media Horaria`)
# summary(Campinas_taquaral_O3_2018$`Media Horaria`)
Campinas_taquaral_O3_2017 <- remove_outlier(Campinas_taquaral_O3_2017,Campinas_taquaral_O3_2017[ ,10],386.00)
Campinas_taquaral_O3_2018 <- remove_outlier(Campinas_taquaral_O3_2018,Campinas_taquaral_O3_2018[ ,10],431.00)


# Utilizando a funcao consecutive, nao forma identificados sequencias de valores
# sum(consecutive(Campinas_taquaral_O3_2017$`Media Horaria`,10))
# sum(consecutive(Campinas_taquaral_O3_2018$`Media Horaria`,12))



### Tratamento dos dados RADUV 2017 e 2018 ###
### ====================================== ###

Campinas_taquaral_RADUV_2017 <- tratamentoDados_CETESB (Campinas_taquaral_RADUV_2017,10)
Campinas_taquaral_RADUV_2018 <- tratamentoDados_CETESB (Campinas_taquaral_RADUV_2018,10)


# Foram identificados valores outliers atraves da funcao summary nas medicoes de RADUV de 2018. Valores
# medidos acima de 61 foram disprepandes do resto do conjunto de dados
# summary(Campinas_taquaral_RADUV_2017$`Media Horaria`)
# summary(Campinas_taquaral_RADUV_2018$`Media Horaria`)
Campinas_taquaral_RADUV_2018 <- remove_outlier(Campinas_taquaral_RADUV_2018,Campinas_taquaral_RADUV_2018[ ,10],61.00)


# Nao detectamos sequencia de valores discrepantes do conjunto de dados de 2017 atraves da funcao consecutive
# sum(consecutive(Campinas_taquaral_RADUV_2017$`Media Horaria`,15))

# Ja para as medicos de 2018 atraves da funcao consecutive conseguimos indentificar que a partir
# de 05/11/2018 as medicoes de RADUV cairam pra zero abruptamente seguindo assim ate o final do ano
# Portanto utilizaremos o filtro abaixo para filtrar valores sequencias erroneos
# sum(consecutive(Campinas_taquaral_RADUV_2018$`Media Horaria`,18))

filtro <- consecutive(Campinas_taquaral_RADUV_2018$`Media Horaria`,18)
Campinas_taquaral_RADUV_2018 <- Campinas_taquaral_RADUV_2018[!filtro, ]



### Tratamento dos dados de CO de 2018 ###
### ================================== ###
Campinas_centro_CO_2018 <- tratamentoDados_CETESB (Campinas_centro_CO_2018,10)

# Nao foram identificados valores outliers atraves da funcao summary #
# summary(Campinas_centro_CO_2018$`Media Horaria`)

# Utilizando a funcao consecutive, nao foram identificados valores sequencias discrepantes
# do conjunto de dados
# sum(consecutive(Campinas_centro_CO_2018$`Media Horaria`,12))



### Tratamento dos dados de CO e UR da tabela CETESB + cepagri ###
### ========================================================== ###

# remve linhas com NA
CETESB_CEPAGRI_UR_CO_2018 <- CETESB_CEPAGRI_UR_CO_2018[complete.cases(CETESB_CEPAGRI_UR_CO_2018), ]

# Nao forma identificados valores outlies nem de UR e nem de CO #
# summary(CETESB_CEPAGRI_UR_CO_2018$umidade)
# summary(CETESB_CEPAGRI_UR_CO_2018$`Media Horaria`)

# Nao forma identificados valores sequencias discrepandos do conjunto de dados atraves da funcao consecutive
# sum(consecutive(CETESB_CEPAGRI_UR_CO_2018$`Media Horaria`,10))
# sum(consecutive(CETESB_CEPAGRI_UR_CO_2018$umidade,10))

# Agrega as medicoes por meses
lista_meses <- month(as.POSIXct(CETESB_CEPAGRI_UR_CO_2018$horario, format = '%d/%m/%Y %H:%M:%S'))

CETESB_CEPAGRI_CO_2018_perMonth <- aggregate(CETESB_CEPAGRI_UR_CO_2018$`Media Horaria`,list(lista_meses),mean)
colnames(CETESB_CEPAGRI_CO_2018_perMonth) <- c("Mes","CO")

CETESB_CEPAGRI_UR_2018_perMonth <- aggregate(CETESB_CEPAGRI_UR_CO_2018$umidade,list(lista_meses),mean)
colnames(CETESB_CEPAGRI_UR_2018_perMonth) <- c("Mes","UR")




#############################################################
#Análises                                                   #
#############################################################
#variáveis auxiliares para agregação
dia <- format(cepagri$horario,"%Y-%m-%d")
mes <- as.numeric(format(cepagri$horario,"%m"))
mes <- factor(month.abb[mes], levels = month.abb, ordered = TRUE)



###############################################################
### Análise Monóxido de Carbono (CO) com Umidade relativa do ar
################################################################


### =================================================================================== ###
### Relacionando Monoxido e Carbono (CO) CETESB com Umidade Relativa do Ar (UR) cepagri ###
### =================================================================================== ###

Campinas_centro_CO_2018_comDatas <- Campinas_centro_CO_2018
n <- nrow(Campinas_centro_CO_2018)

# cria um vetor de datas
horario <- as.POSIXct(rep(0,(n)),origin = "1582-10-14")

# preenche o vetor com a juncao das colunas de data hora da tabela da CETESB
for (i in c(1:n)) {
  horario[i] <- as.POSIXct(as.character(paste(Campinas_centro_CO_2018[i, 4],Campinas_centro_CO_2018[i, 5])), format = '%d/%m/%Y %H:%M')
}

Campinas_centro_CO_2018_comDatas <- cbind(Campinas_centro_CO_2018_comDatas,horario)

# Junta os valores de medicoes do cepagri e da CETESB
CETESB_CEPAGRI_UR_CO_2018 <- merge(x=Campinas_centro_CO_2018_comDatas[ ,c("horario", "Nome Parametro", "Unidade de Medida", "Media Horaria")], y=cepagri[ ,c("horario","umidade")], by = "horario", all.x = TRUE)




### Analise de correlacao RADUV, O3, UR e CO ###
### ======================================== ###

# remove a ultima linha das medicoes de O3, CO e UR devido aos dados invalidos das medicoes de RADUV, para poder correlacionar
Campinas_taquaral_O3_2018_perMonth_corr <- Campinas_taquaral_O3_2018_perMonth[1:11, ]
CETESB_CEPAGRI_CO_2018_perMonth_corr <- CETESB_CEPAGRI_CO_2018_perMonth[1:11, ]
CETESB_CEPAGRI_UR_2018_perMonth_corr <- CETESB_CEPAGRI_UR_2018_perMonth[1:11, ] 

Campinas_taquaral_corr <- data.frame(Campinas_taquaral_RADUV_2018_perMonth$RADUV,Campinas_taquaral_O3_2018_perMonth_corr$O3,CETESB_CEPAGRI_CO_2018_perMonth_corr$CO,CETESB_CEPAGRI_UR_2018_perMonth_corr$UR)

colnames(Campinas_taquaral_corr) <- c("RADUV","O3","CO","UR")

# tracar a correlacao
ggcorrplot(cor(Campinas_taquaral_corr[1:4]), hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 4, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlograma de RADUV O3 CO UR", 
           ggtheme=theme_bw)



###################################################################
### Raios Ultravioleta com camada de ozônio (O3)                  #
###################################################################


### Analise RADUV e O3 ###
### ================== ###

### O3 ###
# Cria um grafico de linhas de 2017
graf_O3 <- grafico_aggregado_mes (Campinas_taquaral_O3_2017$Data,Campinas_taquaral_O3_2017$`Media Horaria`,"O3","2017")
graf_O3


# Cria uma tabela de medicoes mensais do ano de 2018
Campinas_taquaral_O3_2018_perMonth <-  Agrega_mediaMensal (Campinas_taquaral_O3_2018$Data,Campinas_taquaral_O3_2018$`Media Horaria`,"O3")

# adicionar o grafico de 2018
graf_O3 + geom_line(data=Campinas_taquaral_O3_2018_perMonth, aes(x = Mes, y =O3, colour="2018")) + labs(colour = "Legenda:") +
  ggtitle("Analise de Variacao de Ozonio (2017 e 2018)") +
  theme(plot.title = element_text(hjust = 0.5))



### RADUV ###
graf_RADUV <- grafico_aggregado_mes (Campinas_taquaral_RADUV_2017$Data,Campinas_taquaral_RADUV_2017$`Media Horaria`,"RADUV","2017")
graf_RADUV


# Cria uma tabela de medicoes mensais do ano de 2018
Campinas_taquaral_RADUV_2018_perMonth <-  Agrega_mediaMensal (Campinas_taquaral_RADUV_2018$Data,Campinas_taquaral_RADUV_2018$`Media Horaria`,"RADUV")

# adicionar o grafico de 2018
graf_RADUV + geom_line(data=Campinas_taquaral_RADUV_2018_perMonth, aes(x = Mes, y =RADUV, colour="2018")) + labs(colour = "Legenda:") +
  ggtitle("Analise de Variacao de Radiacao Ultra-violeta (2017 e 2018)") +
  theme(plot.title = element_text(hjust = 0.5))



### Tablea de Qualidade do ar e O3 de 2018 ###
### ====================================== ###

# filtrar medicoes de agosto e setembro (Mes critico para O3)
filtro_agosto_setembro <- month(as.POSIXct(as.character(Campinas_taquaral_O3_2018$Data), format = '%d/%m/%Y')) == 8 | month(as.POSIXct(as.character(Campinas_taquaral_O3_2018$Data), format = '%d/%m/%Y')) == 9

indices <- which(filtro_agosto_setembro)
Campinas_taquaral_O3_m8_m9_2018 <- Campinas_taquaral_O3_2018[indices, ]



# agrupamento de periodos de 8 horas para calculo da media.
Campinas_taquaral_O3_m8_m9_2018$Hora <- hour(as.POSIXct(as.character(Campinas_taquaral_O3_m8_m9_2018$Hora), format = '%H:%M'))
Campinas_taquaral_O3_m8_m9_2018$Data <- as.POSIXct(as.character(Campinas_taquaral_O3_m8_m9_2018$Data), format = '%d/%m/%Y')


# Cria uma coluna periodo que compreende os valores
# periodo = 1 - Entre 1h e 8h
# periodo = 2 - Entre 9h e 16h
# perido  = 3 - Apos 17h
Campinas_taquaral_O3_m8_m9_2018 <- Add_period8H (Campinas_taquaral_O3_m8_m9_2018,5)


Campinas_taquaral_O3_m8_m9_2018_Quali <- aggregate(Campinas_taquaral_O3_m8_m9_2018$`Media Horaria`,list(Campinas_taquaral_O3_m8_m9_2018$Data,Campinas_taquaral_O3_m8_m9_2018$periodo),mean)[order(aggregate(Campinas_taquaral_O3_m8_m9_2018$`Media Horaria`, by = list(Campinas_taquaral_O3_m8_m9_2018$Data,Campinas_taquaral_O3_m8_m9_2018$periodo), mean)[1]),]

# corrige o nome das colunas
colnames(Campinas_taquaral_O3_m8_m9_2018_Quali) <- c("Data","periodo","O3-8h")

n <- nrow(Campinas_taquaral_O3_m8_m9_2018_Quali)
Qualidade_Ar <- rep(NA,n)


# Classificacao dos niveis de O3 de acordo os padroes de qualidade do ar da CETESB:
# https://cetesb.sp.gov.br/ar/padroes-de-qualidade-do-ar/
# 0 - 100: N1 - BOA
# >100 - 130: N2 - MODERADA
# >130 - 160: N3 - RUIM
# >160 - 200: N4 - MUITO RUIM
# >200: N5 - PESSIMA

for (i in 1:n) {
  if (Campinas_taquaral_O3_m8_m9_2018_Quali[i,3] <= 100) {
    Qualidade_Ar[i] <- "N1 - BOA"
  } else {
    if (Campinas_taquaral_O3_m8_m9_2018_Quali[i,3] > 100 & Campinas_taquaral_O3_m8_m9_2018_Quali[i,3] <= 130) {
      Qualidade_Ar[i] <- "N2 - MODERADA"
    } else {
      if (Campinas_taquaral_O3_m8_m9_2018_Quali[i,3] > 130 & Campinas_taquaral_O3_m8_m9_2018_Quali[i,3] <= 160) {
        Qualidade_Ar[i] <- "N3 - RUIM"
      } else {
        if (Campinas_taquaral_O3_m8_m9_2018_Quali[i,3] > 160 & Campinas_taquaral_O3_m8_m9_2018_Quali[i,3] <= 200) {
          Qualidade_Ar[i] <- "N4 - MUITO RUIM"
        } else {
          if (Campinas_taquaral_O3_m8_m9_2018_Quali[i,3] > 200) {
            Qualidade_Ar[i] <- "N5 - PESSIMA"
          }
        }
      }
    }
  }
}

Campinas_taquaral_O3_m8_m9_2018_Quali <- cbind(Campinas_taquaral_O3_m8_m9_2018_Quali,Qualidade_Ar)


# Filtra as mediões com qualidae de ar N2 - Moderada para fazer a tabela
Campinas_taquaral_O3_m8_m9_2018_Quali_filt <- Campinas_taquaral_O3_m8_m9_2018_Quali[Campinas_taquaral_O3_m8_m9_2018_Quali$Qualidade_Ar == "N2 - MODERADA", ]

# Classifica os periodos em:
# periodo = 1 -> Manha
# periodo = 2 -> Tarde
# periodo = 3 -> Noite
Campinas_taquaral_O3_m8_m9_2018_Quali_filt$periodo <- as.character(Campinas_taquaral_O3_m8_m9_2018_Quali_filt$periodo)

Campinas_taquaral_O3_m8_m9_2018_Quali_filt$periodo[Campinas_taquaral_O3_m8_m9_2018_Quali_filt$periodo ==  "1"] <- "Manha"
Campinas_taquaral_O3_m8_m9_2018_Quali_filt$periodo[Campinas_taquaral_O3_m8_m9_2018_Quali_filt$periodo ==  "2"] <- "Tarde"
Campinas_taquaral_O3_m8_m9_2018_Quali_filt$periodo[Campinas_taquaral_O3_m8_m9_2018_Quali_filt$periodo ==  "3"] <- "Noite"


rownames(Campinas_taquaral_O3_m8_m9_2018_Quali_filt) <- NULL

# Tabela
kable(Campinas_taquaral_O3_m8_m9_2018_Quali_filt) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)





### Tabela de Qualidade do ar e CO de 2018 ###
### ====================================== ###

# filtrar medicoes de setembro (Mes critico para CO)
filtro_setembro <- month(as.POSIXct(as.character(Campinas_centro_CO_2018$Data), format = '%d/%m/%Y')) == 9
indices <- which(filtro_setembro)
Campinas_centro_CO_2018_sept <- Campinas_centro_CO_2018[indices, ]


# agrupamento de periodos de 8 horas para calculo da media.
Campinas_centro_CO_2018_sept$Hora <- hour(as.POSIXct(as.character(Campinas_centro_CO_2018_sept$Hora), format = '%H:%M'))
Campinas_centro_CO_2018_sept$Data <- as.POSIXct(as.character(Campinas_centro_CO_2018_sept$Data), format = '%d/%m/%Y')


# Cria uma coluna periodo que compreende os valores
# periodo = 1 - Entre 1h e 8h
# periodo = 2 - Entre 9h e 16h
# perido  = 3 - Apos 17h
Campinas_centro_CO_2018_sept <- Add_period8H (Campinas_centro_CO_2018_sept,5)

Campinas_centro_CO_2018_sept_Quali <- aggregate(Campinas_centro_CO_2018_sept$`Media Horaria`,list(Campinas_centro_CO_2018_sept$Data,Campinas_centro_CO_2018_sept$periodo),mean)[order(aggregate(Campinas_centro_CO_2018_sept$`Media Horaria`, by = list(Campinas_centro_CO_2018_sept$Data,Campinas_centro_CO_2018_sept$periodo), mean)[1]),]


# corrige o nome das colunas
colnames(Campinas_centro_CO_2018_sept_Quali) <- c("Data","periodo","CO-8h")

n <- nrow(Campinas_centro_CO_2018_sept_Quali)
Qualidade_Ar_CO <- rep(NA,n)

# Classificacao dos niveis de CO de acordo os padroes de qualidade do ar da CETESB:
# https://cetesb.sp.gov.br/ar/padroes-de-qualidade-do-ar/
# 0 - 9: N1 - BOA
# >9 - 11: N2 - MODERADA
# >11 - 13: N3 - RUIM
# >13 - 15: N4 - MUITO RUIM
# >15: N5 - PESSIMA
for (i in 1:n) {
  if (Campinas_centro_CO_2018_sept_Quali[i,2] <= 9) {
    Qualidade_Ar_CO[i] <- "N1 - BOA"
  } else {
    if (Campinas_centro_CO_2018_sept_Quali[i,2] > 9 & Campinas_centro_CO_2018_sept_Quali[i,2] <= 11) {
      Qualidade_Ar_CO[i] <- "N2 - MODERADA"
    } else {
      if (Campinas_centro_CO_2018_sept_Quali[i,2] > 11 & Campinas_centro_CO_2018_sept_Quali[i,2] <= 13) {
        Qualidade_Ar_CO[i] <- "N3 - RUIM"
      } else {
        if (Campinas_centro_CO_2018_sept_Quali[i,2] > 13 & Campinas_centro_CO_2018_sept_Quali[i,2] <= 15) {
          Qualidade_Ar_CO[i] <- "N4 - MUITO RUIM"
        } else {
          if (Campinas_centro_CO_2018_sept_Quali[i,2] > 15) {
            Qualidade_Ar_CO[i] <- "N5 - PESSIMA"
          }
        }
      }
    }
  }
}

Campinas_centro_CO_2018_sept_Quali <- cbind(Campinas_centro_CO_2018_sept_Quali,Qualidade_Ar_CO)

# Classifica os periodos em:
# periodo = 1 -> Manha
# periodo = 2 -> Tarde
# periodo = 3 -> Noite
Campinas_centro_CO_2018_sept_Quali$periodo <- as.character(Campinas_centro_CO_2018_sept_Quali$periodo)

Campinas_centro_CO_2018_sept_Quali$periodo[Campinas_centro_CO_2018_sept_Quali$periodo ==  "1"] <- "Manha"
Campinas_centro_CO_2018_sept_Quali$periodo[Campinas_centro_CO_2018_sept_Quali$periodo ==  "2"] <- "Tarde"
Campinas_centro_CO_2018_sept_Quali$periodo[Campinas_centro_CO_2018_sept_Quali$periodo ==  "3"] <- "Noite"

# Pega as mediçoes do 20 primeiros dias mais criticos para criar a tabela
Campinas_centro_CO_2018_sept_Quali <- Campinas_centro_CO_2018_sept_Quali[1:20, ]

rownames(Campinas_centro_CO_2018_sept_Quali) <- NULL


# Cria tabela
kable(Campinas_centro_CO_2018_sept_Quali) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)




                                              
###################################################################
### Sensação térmica mínima 2016
###################################################################

sensação2016 <- subset(cepagri, cepagri$horario > "2016-01-01" & cepagri$horario < "2016-12-31")
muitofrio <- subset(sensação2016, sensação2016$sensacao < -5)
extremo <- muitofrio[muitofrio$sensacao == min(muitofrio$sensacao),]
sensação2016 <- aggregate(sensação2016[, 5], list(format(sensação2016$horario,"%Y-%m-%d")), min)[order(aggregate(sensação2016[, 5], list(format(sensação2016$horario,"%Y-%m-%d")), min)[1]),]
colnames(sensação2016) <- c("dia", "sensação")
sensação2016$dia <- as.Date(sensação2016$dia)
sensação2016$estação = as.factor(sapply(sensação2016$dia, estação_do_ano))

ggplot(sensação2016, aes(x=estação, y=sensação)) +
  geom_violin(alpha=0.5, color="gray") +
  geom_jitter(alpha=0.5, aes(color=estação), position = position_jitter(width = 0.1)) + 
  coord_flip() +
  ggtitle("Sensação térmica mínima 2016") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("sensação térmica (\u00B0C)") + 
  theme(legend.position = "none") +
  theme(axis.title.y = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(hjust = 0.5)) 


names(extremo)[1] <- "data/hora"
aux <- extremo$temperatura
extremo[,2] <- extremo$sensacao
extremo[,5] <- aux
names(extremo)[2:5] <- c("sensação (\u00B0C)", "vento (km/h)", "umidade (%)", "temperatura (\u00B0C)")
extremo <- extremo[,-6]


rownames(extremo) <- NULL


kable(extremo) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)




###################################################################
### Análise da umidade abaixo de 30% (2015 a 2018)
###################################################################

minimadiaria <- aggregate(cepagri[, 2:5], list(dia), min)[order(aggregate(cepagri[, 2:5], list(dia), min)[1]),]
colnames(minimadiaria)[1]<-c("dia")
minimadiaria$dia <- as.Date(minimadiaria$dia)
minimadiaria$estação = as.factor(sapply(minimadiaria$dia, estação_do_ano))

#número de dias em que a umidade ficou abaixo de 30% (estado de atenção)
length(subset(minimadiaria$umidade, minimadiaria$umidade < 30))

ggplot(minimadiaria, aes(x=dia, y=umidade, color=estação)) + 
  geom_point() +
  guides(colour = guide_legend(override.aes = list(size=4))) +
  scale_color_manual(values=c("salmon","green","blue","purple"), breaks = c("primavera", "verão", "outono", "inverno")) +
  geom_text(data=minimadiaria, aes(dia, umidade, label=floor(umidade)), size=4) +
  ggtitle("Valores mínimos diários de umidade") +
  xlab("ano") +
  ylab("umidade (%)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(axis.title.y = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 30) +
  scale_x_date(date_breaks = "4 months" , date_labels = "%b-%y") +
  theme(axis.text.x = element_text(angle=45,vjust=0.5))


#Umidade estado de emergência abaixo de 12% em set e out/2017
umidademuitobaixa <- subset(cepagri, cepagri$horario > "2017-09-01" & cepagri$horario < "2017-10-30" & cepagri$umidade < 12)
umidademuitobaixa <- aggregate(umidademuitobaixa$umidade, list(format(umidademuitobaixa$horario,"%Y-%m-%d")), min)
colnames(umidademuitobaixa) <- c("data", "umidade (%)")
umidademuitobaixa$data <- as.Date(umidademuitobaixa$data)

umidademuitobaixa$data <- format(umidademuitobaixa$data, "%d-%m-%Y")
kable(umidademuitobaixa) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)




####################################################################
### Análise da variação da temperatura(2015 e 2018)
####################################################################

ggplot(cepagri, aes(x=mes, y=temperatura, group=mes)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8) + 
  facet_wrap(~ format(cepagri$horario,"%Y", ncol=1)) +
  ylab("temperatura \u00B0C") + xlab("mês") + 
  ggtitle("Variação da temperatura (2015 a 2018)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle=45)) +
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"))



###################################################################
### Análise da velocidade máxima dos ventos em 2015
###################################################################

densventomax <- subset(cepagri, cepagri$horario > "2014-12-31" & cepagri$horario < "2016-01-01")
densventomax <- aggregate(densventomax, list(format(densventomax$horario,"%Y-%m-%d")), max)
densventomax <- densventomax[,-2]
colnames(densventomax)[1]<-"dia"
densventomax$dia <- as.Date(densventomax$dia)
mes <- as.numeric(format(densventomax$dia,"%m"))
mes <- factor(month.abb[mes], levels = month.abb, ordered = TRUE)
densventomax <- cbind(densventomax, mes)

ggplot(densventomax, aes(x=vento, y=mes)) +
  geom_density_ridges_gradient(aes(fill = ..x..), scale = 0.9, size = 0.3) +
  scale_fill_gradientn(colours = c("#F0F921FF", "#CC4678FF", "#0D0887FF"), name = "vel. (km/h)") +
  ggtitle("Velocidade máxima do vento 2015") +
  ylab("mês") +
  xlab("velocidade do vento (km/h)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_text(hjust = 0.5))


###################################################################
### Análise do vento acima de 100 km/h
###################################################################

muitovento <- subset(cepagri, cepagri$vento > 100)
muitovento <- aggregate(muitovento, list(format(muitovento$horario,"%Y-%m-%d")), max)
muitovento <- muitovento[,-1]
muitovento$horario <- format(muitovento$horario, "%d-%m-%Y %H:%M")
names(muitovento)[1] <- "data/hora"
aux <- muitovento$temperatura
muitovento[,2] <- muitovento$vento
muitovento[,3] <- aux
names(muitovento)[2:5] <- c("vento (km/h)", "temperatura (\u00B0C)", "umidade (%)", "sensação (\u00B0C)")

kable(muitovento) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)