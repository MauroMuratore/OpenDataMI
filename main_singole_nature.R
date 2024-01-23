library(dplyr)
library(ggplot2)


tema <-  theme(
  legend.position = "bottom",
  legend.key.size = unit(0.2, "cm"),
  legend.text = element_text(size=6),
  legend.title = element_text(size=8))

ifm_nature.original <- read.csv("data/incidenti_data_cause.csv")
ifm_nature.sum_anno <- ifm_nature.original %>%
  group_by(Anno, NaturaIncidente) %>%
  summarise(Incidenti = sum(Incidenti),
            Feriti = sum(Feriti),
            Morti= sum(Morti)) 

ifm_totale <- ifm_nature.sum_anno %>%
  group_by(Anno) %>%
  summarise(Incidenti = sum(Incidenti),
            Feriti = sum(Feriti),
            Morti= sum(Morti)) 

ifm_nature.percentage <- ifm_nature.sum_anno %>%
  inner_join(y = ifm_totale, by=c("Anno"), suffix = c("", ".Totale")) %>%
  group_by(Anno, NaturaIncidente) %>%
  summarise(Incidenti=Incidenti*100/Incidenti.Totale,
            Feriti=Feriti*100/Feriti.Totale,
            Morti=Morti*100/Morti.Totale)

p_inc_tot <- ggplot(ifm_nature.sum_anno, aes(Anno, Incidenti, color=NaturaIncidente)) +
  geom_line() + 
  labs(color="Natura Incidente") +
  tema

p_fer_tot <- ggplot(ifm_nature.sum_anno, aes(Anno, Feriti, color=NaturaIncidente)) +
  geom_line() + 
  labs(color="Natura Incidente") +
  tema

p_mor_tot <- ggplot(ifm_nature.sum_anno, aes(Anno, Morti, color=NaturaIncidente)) +
  geom_line()+ 
  labs(color="Natura Incidente") +
  tema

p_inc_rel <- ggplot(ifm_nature.percentage, aes(x=Anno, Incidenti, color=NaturaIncidente))+
  geom_line() +
  ylab("Distribuzione percentuale degli incidenti")+ 
  labs(color="Natura Incidente") +
  tema

p_fer_rel <- ggplot(ifm_nature.percentage, aes(x=Anno, Feriti, color=NaturaIncidente))+
  geom_line()+
  ylab("Distribuzione percentuale dei feriti")+ 
  labs(color="Natura Incidente") +
  tema

p_mor_rel <- ggplot(ifm_nature.percentage, aes(x=Anno, Morti, color=NaturaIncidente))+
  geom_line()+
  ylab("Distribuzione percentuale dei morti")+ 
  labs(color="Natura Incidente") +
  tema

print(p_inc_rel)
print(p_inc_tot)
print(p_fer_rel)
print(p_fer_tot)
print(p_mor_rel)
print(p_mor_tot)


ifm_nature.tassi <- ifm_nature.sum_anno %>%
  group_by(Anno, NaturaIncidente) %>%
  summarise(Feribilita = Feriti/Incidenti,
            Mortalita = Morti*1000/Incidenti)

ifm_totale.tassi <- ifm_totale %>%
  group_by(Anno) %>%
  summarise(Feribilita = Feriti/Incidenti,
            Mortalita = Morti*1000/Incidenti)

p_feribilita_natura <- ggplot(ifm_nature.tassi, aes(Anno, Feribilita, color=NaturaIncidente)) +
  geom_line() + 
  ylab("Feriti ogni incidente")+
  scale_color_discrete(name="Natura Incidente") +
  geom_line(data = ifm_totale.tassi, aes(x=Anno, y=Feribilita, linetype="Media"), 
            color = "red", linewidth=1.2) +
  scale_linetype_manual(name="" ,guide = "legend",
                        labels=c("Media"), values = c("dashed")) +
  tema

p_mortalita_natura <- ggplot(ifm_nature.tassi, aes(Anno, Mortalita, color=NaturaIncidente)) +
  geom_line() + 
  ylab("Morti ogni 1000 incidenti")+
  scale_color_discrete(name="Natura Incidente") +
  geom_line(data = ifm_totale.tassi, aes(x=Anno, y=Mortalita, linetype="Media"), 
            color = "red", linewidth=1.2) +
  scale_linetype_manual(name="" ,guide = "legend",
                        labels=c("Media"), values = c("dashed")) +
  tema

print(p_mortalita_natura)
print(p_feribilita_natura)

dir.create("image/componenti_natura/singole_nature", showWarnings = FALSE)

save_plot(p_inc_tot, "componenti_natura/singole_nature/incidenti_tot")
save_plot(p_fer_tot, "componenti_natura/singole_nature/feriti_tot")
save_plot(p_mor_tot, "componenti_natura/singole_nature/morti_tot")
save_plot(p_inc_rel, "componenti_natura/singole_nature/incidenti_rel")
save_plot(p_fer_rel, "componenti_natura/singole_nature/feriti_rel")
save_plot(p_mor_rel, "componenti_natura/singole_nature/morti_rel")