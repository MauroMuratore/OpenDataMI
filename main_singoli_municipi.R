library(dplyr)
library(ggplot2)

source("util.R")


tema <-  theme(
  legend.position = "bottom",
  legend.key.width = unit(0.4, "cm"),
  legend.text = element_text(size=8),
  legend.title = element_text(size=10),
  )

ifm_municipi <- read.csv("./data/incidenti_data_municipio.csv") %>%
  group_by(Anno, Municipio) %>%
  summarise(Incidenti = sum(Incidenti),
            Feriti = sum(Feriti),
            Morti = sum(Morti))

ifm_municipi$Municipio <- as.factor(ifm_municipi$Municipio)
ifm_municipi.totale <- ifm_municipi %>%
  group_by(Anno) %>%
  summarise(Incidenti = sum(Incidenti),
            Feriti = sum(Feriti),
            Morti = sum(Morti))

incidenti.plot_abs <- ggplot(ifm_municipi, aes(Anno, Incidenti, color=Municipio)) +
  geom_line() + 
  tema

feriti.plot_abs <- ggplot(ifm_municipi, aes(Anno, Feriti, color=Municipio)) +
  geom_line() + 
  tema

morti.plot_abs <- ggplot(ifm_municipi, aes(Anno, Morti, color=Municipio)) +
  geom_line() + 
  tema

print(incidenti.plot_abs)
print(feriti.plot_abs)
print(morti.plot_abs)

ifm_municipi.percentage <- ifm_municipi %>%
  inner_join(ifm_municipi.totale, by=c("Anno"), suffix = c("", ".totale")) %>%
  group_by(Anno,Municipio) %>%
  summarise(Incidenti = Incidenti*100/Incidenti.totale,
            Feriti=Feriti*100/Feriti.totale,
            Morti=Morti*100
            /Morti.totale)
incidenti.plot_rel <- ggplot(ifm_municipi.percentage, aes(Anno, Incidenti, color=Municipio)) +
  geom_line() + 
  tema

feriti.plot_rel <- ggplot(ifm_municipi.percentage, aes(Anno, Feriti, color=Municipio)) +
  geom_line() + 
  tema

morti.plot_rel <- ggplot(ifm_municipi.percentage, aes(Anno, Morti, color=Municipio)) +
  geom_line() + 
  tema

print(incidenti.plot_rel)
print(feriti.plot_rel)
print(morti.plot_rel)

save_plot(incidenti.plot_abs, "municipi/incidenti_abs")
save_plot(incidenti.plot_rel, "municipi/incidenti_rel")
save_plot(feriti.plot_abs, "municipi/feriti_abs")
save_plot(feriti.plot_rel, "municipi/feriti_rel")
save_plot(morti.plot_abs, "municipi/morti_abs")
save_plot(morti.plot_rel, "municipi/morti_rel")