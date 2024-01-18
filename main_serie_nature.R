#IMPORT DELLE LIBRARY
library("ggplot2")
library("dplyr")
#IMPORT DELLE MIE FUNZIONI
source("util.R")
source("analisi_serie_storiche.R")

ifm_natura <- read.csv("./data/incidenti_data_cause.csv")

seq_date <- seq(as.Date("2001/01/01"), as.Date("2022/12/01"), by="month")

### INVESTIMENTO PEDONE
ifm_pedoni <- ifm_natura %>%
  arrange(Anno, Mese) %>%
  filter(NaturaIncidente == "Investimento pedone") %>%
  select(Incidenti, Feriti, Morti)
ifm_pedoni$date <- seq_date

## INCIDENTE
plot_pedoni_incidenti <- ggplot(ifm_pedoni, aes(x=date, y=Incidenti))+
  geom_line() +
  labs( x="Data", y="Incidenti")
print(plot_pedoni_incidenti)
save_plot(plot_pedoni_incidenti, "serie_storica_pedoni/incidenti_andamento")
# calcolo il stl
stl_pedoni_incidenti <- calculate_stl(ifm_pedoni$Incidenti)
plot_stl(stl_pedoni_incidenti, "Incidenti", 
         to_save=TRUE, filename = "serie_storica_pedoni/incidenti")


## FERITI
plot_pedoni_feriti <- ggplot(ifm_pedoni, aes(x=date, y=Feriti))+
  geom_line() +
  labs( x="Data", y="Feriti")

print(plot_pedoni_feriti)
save_plot(plot_pedoni_feriti, "serie_storica_pedoni/feriti_andamento")
stl_pedoni_feriti <- calculate_stl(ifm_pedoni$Feriti)
plot_stl(stl_pedoni_feriti, "Feriti",
         to_save=TRUE, filename="serie_storica_pedoni/feriti")

## MORTI
plot_pedoni_morti <- ggplot(ifm_pedoni, aes(x=date, y=Morti))+
  geom_line() +
  labs( x="Data", y="Morti")

print(plot_pedoni_morti)
save_plot(plot_pedoni_morti, "serie_storica_pedoni/morti_andamento")
stl_pedoni_morti <- calculate_stl(ifm_pedoni$Morti)
plot_stl(stl_pedoni_morti, "Morti",
         to_save=TRUE, filename="serie_storica_pedoni/morti")

### SCONTRO FRONTALE - LATERALE
ifm_frontale_laterale <- ifm_natura %>%
  arrange(Anno, Mese) %>%
  filter(NaturaIncidente == "Scontro frontale - laterale") %>%
  select(Incidenti, Feriti, Morti)

ifm_frontale_laterale$date <- seq_date


## INCIDENTE
plot_frontale_laterale_incidenti <- ggplot(ifm_frontale_laterale, aes(x=date, y=Incidenti))+
  geom_line() +
  labs( x="Data", y="Incidenti")
print(plot_frontale_laterale_incidenti)
save_plot(plot_frontale_laterale_incidenti, "serie_storica_frontale_laterale/incidenti_andamento")
# calcolo il stl
stl_frontale_laterale_incidenti <- calculate_stl(ifm_frontale_laterale$Incidenti)
plot_stl(stl_frontale_laterale_incidenti, "Incidenti", 
         to_save=TRUE, filename = "serie_storica_frontale_laterale/incidenti")


## FERITI
plot_frontale_laterale_feriti <- ggplot(ifm_frontale_laterale, aes(x=date, y=Feriti))+
  geom_line() +
  labs( x="Data", y="Feriti")

print(plot_frontale_laterale_feriti)
save_plot(plot_frontale_laterale_feriti, "serie_storica_frontale_laterale/feriti_andamento")
stl_frontale_laterale_feriti <- calculate_stl(ifm_frontale_laterale$Feriti)
plot_stl(stl_frontale_laterale_feriti, "Feriti",
         to_save=TRUE, filename="serie_storica_frontale_laterale/feriti")

## MORTI
plot_frontale_laterale_morti <- ggplot(ifm_frontale_laterale, aes(x=date, y=Morti))+
  geom_line() +
  labs( x="Data", y="Morti")

print(plot_frontale_laterale_morti)
save_plot(plot_frontale_laterale_morti, "serie_storica_frontale_laterale/morti_andamento")
stl_frontale_laterale_morti <- calculate_stl(ifm_frontale_laterale$Morti)
plot_stl(stl_frontale_laterale_morti, "Morti",
         to_save=TRUE, filename="serie_storica_frontale_laterale/morti")
