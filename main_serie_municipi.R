#IMPORT DELLE LIBRARY
library("ggplot2")
library("dplyr")
#IMPORT DELLE MIE FUNZIONI
source("util.R")
source("analisi_serie_storiche.R")

 ifm_municipi_data <- read.csv("./data/incidenti_data_zone.csv", sep = ";")
 pop_municipi <-
   read.csv("./data/residenti_per_municipi_serie_storica.csv", sep = ";")

 ifm_municipi_data <- ifm_municipi_data  %>% na.omit() %>% arrange(Anno, Mese)
 ifm_municipi_data$Municipio = as.factor(ifm_municipi_data$Municipio)
 
 ifm_municipi_anno <- ifm_municipi_data %>%
   group_by(Anno, Municipio) %>%
   summarise(
     Incidenti = sum(Incidenti),
     Feriti = sum(Feriti),
     Morti = sum(Morti)
   )
 colnames(pop_municipi)[colnames(pop_municipi) == "MUNICIPIO"] <-
   "Municipio"
 pop_municipi$Municipio <- as.factor(pop_municipi$Municipio)

 ifm_pop_municipi <- ifm_municipi_anno %>%
   inner_join(pop_municipi, by = c("Municipio", "Anno")) %>%
   mutate(
     Feriti = Feriti * 1000 / Residenti,
     Incidenti = Incidenti * 1000 / Residenti,
     Morti = Morti * 100000 / Residenti
   )

 ifm_pop_municipi$Municipio <- as.factor(ifm_pop_municipi$Municipio)

 ifm_pop_media <- ifm_municipi_anno %>%
   inner_join(pop_municipi, by = c("Municipio", "Anno")) %>%
   group_by(Anno) %>%
   summarise(Incidenti=1000*sum(Incidenti)/sum(Residenti),
             Feriti=1000*sum(Feriti)/sum(Residenti),
             Morti=100000*sum(Morti)/sum(Residenti)
   )

 p <- ggplot(ifm_pop_municipi, aes(x=Anno, y=Morti, color=Municipio)) +
   geom_line() +
   geom_line(data=ifm_pop_media, aes(x=Anno, y=Morti),
             color="red", size=1.2)

print(p)


ifm_m3_data <- ifm_municipi_data %>%
  filter(Municipio == 3)
seq_date <- seq(as.Date("2001/01/01"), as.Date("2022/12/01"), by="month")
morti_m3 <- ifm_m3_data[,"Morti"]
morti_m3$date <- seq_date
p_morti_m3 <- ggplot(morti_m3, aes(x=date, y=Morti)) +geom_line()
print(p_morti_m9)


# ifm_m9_data <- ifm_municipi_data %>%
#   filter(Municipio == 9)
# seq_date <- seq(as.Date("2001/01/01"), as.Date("2021/12/01"), by="month")
# feriti_m9 <- ifm_m9_data[,"Feriti"]
# feriti_m9$date <- seq_date
# p_feriti_m9 <- ggplot(feriti_m9, aes(x=date, y=Feriti)) +geom_line()
# print(p_feriti_m9)
# 
# stl_feriti_m9 <- calculate_stl(ifm_m9_data$Feriti)
# plot_stl(stl_feriti_m9, "Feriti")

