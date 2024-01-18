#IMPORT DELLE LIBRARY
library("ggplot2")
library("dplyr")
#IMPORT DELLE MIE FUNZIONI
source("util.R")

ifm <- read.csv("./data/incidenti_data_cause.csv")
ifm$NaturaIncidente <- as.factor(ifm$NaturaIncidente)

ifm_natura_anno <- ifm %>% 
  group_by(Anno, NaturaIncidente) %>%
  summarise(Feriti=sum(Feriti), Incidenti=sum(Incidenti), Morti=sum(Morti))

tema <-  theme(
  legend.position = "right",
  legend.key.size = unit(0.2, "cm"),
  legend.text = element_text(size=6),
  legend.title = element_text(size=8))

p_line_incidenti <- ggplot(ifm_natura_anno, aes(x=Anno, y=Incidenti, 
                                                color=NaturaIncidente)) +
  geom_line() +
  scale_color_discrete(name="Natura Incidente") +
  tema

p_line_feriti <- ggplot(ifm_natura_anno, aes(x=Anno, y=Feriti, 
                                                color=NaturaIncidente)) +
  geom_line() + 
  scale_color_discrete(name="Natura Incidente") +
  tema

p_line_morti <- ggplot(ifm_natura_anno, aes(x=Anno, y=Morti, 
                                                color=NaturaIncidente))+
  geom_line() + 
  scale_color_discrete(name="Natura Incidente") +
  tema

print(p_line_incidenti)
print(p_line_feriti)
print(p_line_morti)

fm_rapporto <- ifm_natura_anno %>%
  group_by(Anno, NaturaIncidente) %>%
  summarise(Feriti = Feriti/Incidenti, Morti=Morti*100/Incidenti)

sum_rapporto <- ifm_natura_anno %>%
  group_by(Anno) %>%
  summarise(Feriti = sum(Feriti)/sum(Incidenti), Morti=sum(Morti)*100/sum(Incidenti))

p_rapporto_feriti <- ggplot(fm_rapporto, aes(x=Anno, y=Feriti, 
                                             color=NaturaIncidente)) +
  geom_line() + 
  ylab("Feriti ogni incidente")+
  scale_color_discrete(name="Natura Incidente") +
  geom_line(data = sum_rapporto, aes(x=Anno, y=Feriti, linetype="Media"), 
            color = "red", linewidth=1.2) +
  scale_linetype_manual(name="" ,guide = "legend",
                        labels=c("Media"), values = c("dashed")) +
  tema
  

p_rapporto_morti <- ggplot(fm_rapporto, aes(x=Anno, y=Morti, color=NaturaIncidente)) +
  geom_line() +
  ylab("Morti ogni 1000 incidenti") +
  scale_color_discrete(name="Natura Incidente") +
  geom_line(data = sum_rapporto, aes(x=Anno, y=Morti, linetype="Media"),
            color= "red", linewidth=1.2) +
  scale_linetype_manual(name="" ,guide = "legend",
                        labels=c("Media"), values = c("dashed")) +
  tema

print(p_rapporto_feriti)
print(p_rapporto_morti)
# save_plot(p_line_incidenti,
#           "componenti_natura/singole_nature/incidenti")
# save_plot(p_line_feriti, "componenti_natura/singole_nature/feriti")
# save_plot(p_line_morti, "componenti_natura/singole_nature/morti")
# 
save_plot(p_rapporto_morti,
          "componenti_natura/singole_nature/rapporto_morti")
save_plot(p_rapporto_feriti,
          "componenti_natura/singole_nature/rapporto_feriti")

