# IMPORTANTE: 
# Qte base utilizadas: 2
# Bases: Latitude&longitude.csv e raios.csv (da parte 1)

install.packages(c("dplyr", "lubridate", "RColorBrewer", "ggplot2"))
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(ggplot2)


lat = read.csv("C:\\Users\\guilherme.c.martins\\Desktop\\Latitude&longitute.csv", stringsAsFactors = F) #leitura do banco de dados latitudinais do Brasil
raios = read.csv("C:\\Users\\guilherme.c.martins\\Documents\\raios.csv", stringsAsFactors = F,
                 colClasses = c("character","integer", "Date"))  #leitura do crawler Raios da parte 1

data_hoje = ymd(Sys.Date())
ano_hoje = year(data_hoje)
mes_hoje = month(data_hoje)


if(mes_hoje == 1){
  datapadrao = dmy(paste(01, 12 ,ano_hoje - 1, sep = "-")) #sempre pega realizado do Mes anterior fechado
}else{
  datapadrao = dmy(paste(01, mes_hoje - 1, ano_hoje, sep = "-")) #sempre pega realizado do Mes anterior fechado
}

raios.ultimo.mes <- raios %>%
  mutate(Raios_milhares = round(Raios/1000, 3)) %>%
  filter(Mes == datapadrao) 

#Estabelecendo o ponto de corte
vetor.corte = as.vector(quantile(raios.ultimo.mes$Raios_milhares, c(.2, .4, .6, .8)))

raios.ultimo.mes <- raios.ultimo.mes %>%
  mutate(Corte = cut(Raios_milhares, breaks = c(0, vetor.corte[1], vetor.corte[2],
                                                vetor.corte[3], vetor.corte[4],
                                                round(max(raios.ultimo.mes$Raios_milhares)))))

dados.mapa <- left_join(lat, raios.ultimo.mes, by = "UF")

#Construindo o Mapa de raios para Brasil

p <- ggplot() +
  geom_polygon(data = dados.mapa, aes(x = long, y = lat, group = group,
                                      fill = Corte, color = "black")) +
  scale_fill_manual(values=c("#fef0d9", "#fdcc8a", "#fc8d59", "#e34a33", "#b30000"), name = "Incidência de raios \n(milhares)") + xlab("") + ylab("") +guides(colour=FALSE) 

p + theme(
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank())
