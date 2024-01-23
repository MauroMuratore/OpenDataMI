#IMPORT DELLE LIBRARY
library("ggplot2")
library("dplyr")
#IMPORT DELLE MIE FUNZIONI
source("util.R")
source("analisi_serie_storiche.R")


dir.create("image/serie_storica_totale", showWarnings = FALSE)

ifm_totale <- read.csv("./data/incidenti_data_totale.csv")
ifm_totale.avg <- ifm_totale %>% 
  group_by(Anno) %>%
  summarise(Incidenti=mean(Incidenti),
            Feriti=mean(Feriti),
            Morti=mean(Morti))

seq_date <- seq(as.Date("2001/01/01"), as.Date("2022/12/01"), by="month")

## INCIDENTI ##
# DATO #
incidenti <- ifm_totale %>% select(Incidenti)
incidenti$date <- seq_date
incidenti.avg <- ifm_totale.avg[,"Incidenti"]
incidenti.avg$date <- seq(as.Date("2001/07/01"), as.Date("2022/07/1)"), by="year")
incidenti.plot_series <- ggplot(incidenti, aes(x=date, y=Incidenti)) +
  geom_line() +
  geom_line(inherit.aes = FALSE, data = incidenti.avg, aes(x=date, y=Incidenti),
            color="#AA3333", linewidth=2)+
  labs( x="Data", y="Incidenti")

print(incidenti.plot_series)
save_plot(incidenti.plot_series, "serie_storica_totale/incidenti_andamento")

# calcolo il stl
incidenti.stl <- calculate_stl(ifm_totale$Incidenti)

plot_stl(stl_dataframe = incidenti.stl, "Incidenti",
         to_save=TRUE, filename = "serie_storica_totale/incidenti")

# CALCOLO SCARTI INTERQUARTILE
incidente.iqr <- IQR(incidenti$Incidenti)
incidente.stl_iqr <- calculate_iqr(incidenti.stl, incidente.iqr)


## FERITI ##
feriti <- ifm_totale %>% select(Feriti)
feriti$date <- seq_date
feriti.avg <- ifm_totale.avg[,"Feriti"]
feriti.avg$date <- seq(as.Date("2001/07/01"), as.Date("2022/07/1)"), by="year")

feriti.plot_series <- ggplot(feriti, aes(x=date, y=Feriti))+
  geom_line() +
  geom_line(inherit.aes = FALSE, data = feriti.avg, aes(x=date, y=Feriti),
            color="#AA3333", linewidth=2)+
  labs( x="Data", y="Feriti")

print(feriti.plot_series)
save_plot(feriti.plot_series, "serie_storica_totale/feriti_andamento")

# calcolo il stl
feriti.stl <- calculate_stl(ifm_totale$Feriti)

plot_stl(stl_dataframe = feriti.stl, "Feriti",
         to_save=TRUE, filename = "serie_storica_totale/feriti")

# CALCOLO SCARTI INTERQUARTILE
feriti.iqr <- IQR(feriti$Feriti)
feriti.stl_iqr <- calculate_iqr(feriti.stl, feriti.iqr)

## MORTI ##
morti <- ifm_totale %>% select(Morti)
morti$date <- seq_date
morti.avg <- ifm_totale.avg[,"Morti"]
morti.avg$date <- seq(as.Date("2001/07/01"), as.Date("2022/07/1)"), by="year")

morti.plot_series <- ggplot(morti, aes(x=date, y=Morti))+
  geom_line() +
  geom_line(inherit.aes = FALSE, data = morti.avg, aes(x=date, y=Morti),
            color="#AA3333", linewidth=2)+
  labs( x="Data", y="Morti")

print(morti.plot_series)
save_plot(morti.plot_series, "serie_storica_totale/morti_andamento")

# calcolo il stl
morti.stl <- calculate_stl(ifm_totale$Morti)

plot_stl(stl_dataframe = morti.stl, "Morti",
         to_save=TRUE, filename = "serie_storica_totale/morti")

# CALCOLO SCARTI INTERQUARTILE
morti.iqr<- IQR(morti$Morti)
morti.stl_iqr <- calculate_iqr(morti.stl, morti.iqr)



