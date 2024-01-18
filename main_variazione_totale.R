#IMPORT DELLE LIBRARY
library("ggplot2")
library("dplyr")
#IMPORT DELLE MIE FUNZIONI
source("util.R")
source("variazione_totale.R")


#LETTURA
ifm_totale <- read.csv("./data/incidenti_data_totale.csv")

ifm_totale.sum_anno <- ifm_totale %>%
  group_by(Anno) %>%
  summarise(Incidenti=sum(Incidenti),
    Feriti=sum(Feriti),
    Morti=sum(Morti))

# CALCOLO VARIAZIONE RISPETTO ALL'ANNO BASE
ifm_totale.delt_ref <- data.frame(
  Anno = c(2001:2022),
  Incidenti = f_delt_ref(ifm_totale.sum_anno$Incidenti),
  Feriti = f_delt_ref(ifm_totale.sum_anno$Feriti),
  Morti = f_delt_ref(ifm_totale.sum_anno$Morti)
)
colnames(ifm_totale.delt_ref) <- c("Anno", "Incidenti", "Feriti", "Morti")

plot_delt_ref_incidenti <- ggplot(ifm_totale.delt_ref, aes(x=Anno, y=Incidenti))+
  geom_bar(stat="identity", fill="#FF9955") +
  geom_text(size=2, vjust=-0.5, aes(label=sprintf("%0.1f%%", Incidenti)))
print(plot_delt_ref_incidenti)
plot_delt_ref_feriti <- ggplot(ifm_totale.delt_ref, aes(x=Anno, y=Feriti))+
  geom_bar(stat="identity", fill="#FF9955") +
  geom_text(size=2, vjust=-0.5, aes(label=sprintf("%0.1f%%", Feriti)))
print(plot_delt_ref_feriti)
plot_delt_ref_morti <- ggplot(ifm_totale.delt_ref, aes(x=Anno, y=Morti))+
  geom_bar(stat="identity", fill="#FF9955") +
  geom_text(size=2, vjust=-0.5, aes(label=sprintf("%0.1f%%", Morti)))
print(plot_delt_ref_morti)

# VARIAZIONE RISPETTO ALL'ANNO PRECEDENTE
ifm_totale.delt_prec<- data.frame(
  Anno = c(2001:2022),
  Incidenti = f_delt_prev(ifm_totale.sum_anno$Incidenti),
  Feriti = f_delt_prev(ifm_totale.sum_anno$Feriti),
  Morti = f_delt_prev(ifm_totale.sum_anno$Morti)
)
colnames(ifm_totale.delt_prec
        ) <- c("Anno", "Incidenti", "Feriti", "Morti")

plot_delt_prev_incidenti <- ggplot(ifm_totale.delt_prec
                                  , aes(x=Anno, y=Incidenti))+
  geom_bar(stat="identity", aes(fill = Incidenti > 0 )) +
  scale_fill_manual(guide="none", breaks =c(TRUE, FALSE), values = c("#77FF77","#FF7777")) +
  geom_text(aes(label = sprintf("%0.1f%%", Incidenti),
                vjust = ifelse(Incidenti > 0, -1, 1)),
            size=2)
plot_delt_prev_feriti <- ggplot(ifm_totale.delt_prec
                              , aes(x=Anno, y=Feriti))+
  geom_bar(stat="identity", aes(fill = Feriti > 0 )) +
  scale_fill_manual(guide="none", breaks =c(TRUE, FALSE), values = c("#77FF77","#FF7777")) +
  geom_text(aes(label = sprintf("%0.1f%%", Feriti),
                vjust = ifelse(Feriti > 0, -1, 1)),
            size=2)
plot_delt_prev_morti <- ggplot(ifm_totale.delt_prec
                              , aes(x=Anno, y=Morti))+
  geom_bar(stat="identity", aes(fill = Morti > 0 )) +
  scale_fill_manual(guide="none", breaks =c(TRUE, FALSE), values = c("#77FF77","#FF7777")) +
  geom_text(aes(label = sprintf("%0.1f%%", Morti),
                vjust = ifelse(Morti > 0, -1, 1)),
            size=2)

print(plot_delt_prev_incidenti)
print(plot_delt_prev_feriti)
print(plot_delt_prev_morti)


ifm_totale.tassi <- ifm_totale.sum_anno %>%
  summarise(Feribilita=Feriti/Incidenti,
            Mortalita=Morti*1000/Incidenti)

plot_feribilita <- ggplot(ifm_totale.tassi, aes(Anno, Feribilita))

plot_mortalita <- ggplot(ifm_totale.tassi, aes(Anno, Mortalita))

# save_plot(plot_delt_ref_incidenti, "variazione_totale/incidenti_ref")
# save_plot(plot_delt_ref_feriti, "variazione_totale/feriti_ref")
# save_plot(plot_delt_ref_morti, "variazione_totale/morti_ref")
# save_plot(plot_delt_prev_incidenti, "variazione_totale/incidenti_prev")
# save_plot(plot_delt_prev_feriti, "variazione_totale/feriti_prev")
# save_plot(plot_delt_prev_morti, "variazione_totale/morti_prev")