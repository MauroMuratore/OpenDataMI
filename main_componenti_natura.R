#IMPORT DELLE LIBRARY
library("ggplot2")
library("dplyr")
#IMPORT DELLE MIE FUNZIONI
source("util.R")

tema <-  theme(
  legend.position = "right",
  legend.key.size = unit(0.2, "cm"),
  legend.text = element_text(size=6),
  legend.title = element_text(size=8))

ifm <- read.csv("./data/incidenti_data_cause.csv")
ifm$NaturaIncidente <- as.factor(ifm$NaturaIncidente)

ifm_anno <- ifm %>% group_by(Anno) %>%
  summarise(Feriti=sum(Feriti), Incidenti=sum(Incidenti), Morti=sum(Morti))

ifm_natura_anno <- ifm %>% 
  group_by(Anno, NaturaIncidente) %>%
  summarise(Feriti=sum(Feriti), Incidenti=sum(Incidenti), Morti=sum(Morti))

p_bar_incidenti <- ggplot(data=ifm_natura_anno, 
                          aes(x=Anno, y=Incidenti, fill=NaturaIncidente)) +
  geom_bar(stat="identity") +
  geom_text(data=ifm_anno, size=2, vjust=-0.5,
            aes(x=Anno, y=Incidenti,
                label=Incidenti, fill=NULL)) +
  tema

p_bar_feriti <- ggplot(data=ifm_natura_anno, 
                          aes(x=Anno, y=Feriti, fill=NaturaIncidente)) +
  geom_bar(stat="identity") +
  geom_text(data=ifm_anno, size=2, vjust=-0.5,
            aes(x=Anno, y=Feriti,
                label=Feriti, fill=NULL)) +
  tema

p_bar_morti <- ggplot(data=ifm_natura_anno, 
                          aes(x=Anno, y=Morti, fill=NaturaIncidente)) +
  geom_bar(stat="identity") +
  geom_text(data=ifm_anno, size=2, vjust=-0.5,
            aes(x=Anno, y=Morti,
                label=Morti, fill=NULL)) +
  tema

print(p_bar_incidenti)
print(p_bar_feriti)
print(p_bar_morti)

save_plot(p_bar_incidenti,"componenti_natura/totale/incidenti")
save_plot(p_bar_feriti,"componenti_natura/totale/feriti")
save_plot(p_bar_morti,"componenti_natura/totale/morti")