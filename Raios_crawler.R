install.packages(c("rvest", "httr", "lubridate", "dplyr"))
library("rvest")
library("httr")
library("lubridate")
library("dplyr")

robozinho <- function(mesinicial, mesfinal, local='brasil'){
  r <- GET("http://www.zeus.iag.usp.br/estatisticas.php", 
           query = list(regiao = local, start_date = mesinicial, end_date= mesfinal)
  )
  
  base<- r %>%
    content('text') %>%
    read_html() %>%
    html_node('table') %>%
    html_table()
  
  base
  Mes = rep(mesinicial,27)
  Mes = as.data.frame(Mes)
  base = cbind(base, Mes)
}


data_hoje = ymd(Sys.Date())
ano_hoje = year(data_hoje)
mes_hoje = month(data_hoje)


if(mes_hoje == 1){
  datapadrao = dmy(paste(01, 12 ,ano_hoje - 1, sep = "-")) #sempre pega realizado do Mes anterior fechado
}else{
  datapadrao = dmy(paste(01, mes_hoje - 1, ano_hoje, sep = "-")) #sempre pega realizado do Mes anterior fechado
}


saida <- function(m_inicial, m_final, Ano){
  Dados=robozinho(m_inicial, m_final)
  write.table(Dados, file="raios.csv", row.names = F,sep = ",", append = T, col.names=FALSE)
}


while(datapadrao >= dmy("01-01-2013") & datapadrao < data_hoje ){
  ano = year(datapadrao)
  for(mes in seq(1:12)){
    if(mes %in% c(1,3,5,7,8,10,12)){
      data.inicial = dmy(paste(01, mes, ano, sep = "-"))
      data.final = dmy(paste(31, mes, ano, sep = "-"))
      data.inicial = format(data.inicial, "%d/%m/%Y")
      data.final = format(data.final, "%d/%m/%Y")
      Ano = year(data.final)
      saida(data.inicial, data.final, Ano)
    }
    else if(mes %in% c(4,6,9,11)){
      data.inicial = dmy(paste(01, mes, ano, sep = "-"))
      data.final = dmy(paste(30, mes, ano, sep = "-"))
      data.inicial = format(data.inicial, "%d/%m/%Y")
      data.final = format(data.final, "%d/%m/%Y")
      Ano = year(data.final)
      saida(data.inicial, data.final, Ano)
    }
    else{
      data.inicial = dmy(paste(01, mes, ano, sep = "-"))
      data.final = dmy(paste(28, mes, ano, sep = "-"))
      data.inicial = format(data.inicial, "%d/%m/%Y")
      data.final = format(data.final, "%d/%m/%Y")
      Ano = year(data.final)
      saida(data.inicial, data.final, Ano)
    }
  }
  datapadrao = datapadrao - years(1)
}
  

Dados = read.csv("raios.csv")
colnames(Dados) <- c("UF", "Raios","Porcentagem", "Mes")

Dados <- Dados %>%
         select(UF, Raios, Mes)

Dados$UF = as.character(Dados$UF)
Dados$Mes = dmy(as.character(Dados$Mes))

write.table(Dados, "raios.csv", row.names = F, sep = ",")
