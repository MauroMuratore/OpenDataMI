# SCRIPT PER IL SET UP DEGLI SCRIPT SUCCESSIVI
# 
# IMPORTO LE LIBRERIE
library("dplyr")
library("ggmap")
print("Librerie caricate")
#
# SCARICO I DATI
#
# DATASET NATURE
# if(FALSE){
dir.create("./data", showWarnings = FALSE)
df_nature.url <- "https://dati.comune.milano.it/dataset/771063f3-c80c-4cbc-a79e-37b98116d830/resource/9287a395-3427-47da-8635-4755cbf4fbdc/download/ds176_trafficotrasporti_incidenti_stradali_persone_infortunate_mese_natura_incidente_2001-2022.csv"
df_nature.file <- "./data/incidenti_data_cause_original.csv"
download.file(df_nature.url, df_nature.file)
ifm_nature.original <- read.csv(df_nature.file, sep = ";")
ifm_nature <- ifm_nature.original %>%
  arrange(Anno, Mese)
ifm_nature$NaturaIncidente[ifm_nature$NaturaIncidente == "Scontro frontale-laterale"] = "Scontro frontale - laterale"
ifm_nature.file <- "./data/incidenti_data_cause.csv"
write.csv(x = ifm_nature, file = ifm_nature.file)
print("Dataset nature incidenti pulito e scaricato")
# }
#
# DATASET MUNICIPI
# if(FALSE){
df_municipi.url <- "https://dati.comune.milano.it/dataset/9f7bcc9c-20a4-4e48-a7cd-99b15ed11102/resource/45b317ca-710a-4a9e-b81b-f380c24d8398/download/ds177_trafficotrasporti_incidenti_stradali_persone_infortunate_mese_zona_2001-2022.csv"
df_municipi.file <- "./data/incidenti_data_municipio_original.csv"
download.file(df_municipi.url, df_municipi.file)
ifm_municipi.original <- read.csv(df_municipi.file, sep=";")
ifm_municipi <- ifm_municipi.original %>%
  arrange(Anno, Mese)
ifm_municipi.file <- "./data/incidenti_data_municipio.csv"
write.csv(x = ifm_municipi, file = ifm_municipi.file)
print("Dataset municipi incidenti pulito e scaricato")
# }
#
# GEOJSON MUNICIPI
# if(FALSE){
geo_municipi.url <- "https://dati.comune.milano.it/dataset/36ba21c2-8b48-43ce-bbe1-e236a8a49ff6/resource/99ecd085-0b04-4fb2-a66e-9795694d4fc4/download/ds379_municipi_label.geojson"
geo_municipi.file <- "./data/geo_municipi.geojson"
download.file(geo_municipi.url, geo_municipi.file)
print("Geojson dei municipi scaricato")
# }
#
# CONTROLLO DELLA SOMMA DEGLI INCIDENTI E SCRITTURA DEL DATASET
# if(FALSE){
ifm_totale.nature <- ifm_nature %>%
  group_by(Anno, Mese) %>%
  summarise(Incidenti=sum(Incidenti),
      Feriti=sum(Feriti),
      Morti=sum(Morti))
ifm_totale.municipi <- ifm_municipi %>%
  group_by(Anno, Mese) %>%
  summarise(Incidenti=sum(Incidenti),
      Feriti=sum(Feriti),
      Morti=sum(Morti))
if(all.equal(ifm_totale.nature, ifm_totale.municipi)){
  ifm_totale.file <- "./data/incidenti_data_totale.csv"
  write.csv(ifm_totale.nature, ifm_totale.file)
  print("Dataset ifm totali salvato")
}
# }
#
# DATASET RESIDENTI
df_residenti.url <- "https://dati.comune.milano.it/dataset/04c124f6-c5b9-4821-b0f1-cb8a782e6723/resource/44f15901-b8f8-493f-b707-a31dd4f5fa42/download/residenti_per_municipi_serie_storica.csv"
df_residenti.file <- "./data/residenti_per_municipi_serie_storica.csv"
download.file(df_residenti.url, df_residenti.file)