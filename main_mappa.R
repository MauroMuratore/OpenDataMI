#IMPORT DELLE LIBRARY
library("ggplot2")
library("dplyr")
library("ggmap")
library("st")
library("geojsonio")
library("osmdata")
#IMPORT DELLE MIE FUNZIONI
source("util.R")


tema <-  theme(
  legend.position = "bottom",
  legend.key.height = unit(0.2, "cm"),
  legend.key.width = unit(1, "cm"),
  legend.text = element_text(size=6),
  legend.title = element_text(size=8))

f_correggi_municipi <- function(dataset){
  for(data in dataset)
    if(data==2){
      data=8
    }
    else if(data==3){
      data=9
    }
    else if(data==4){
      data=2
    }
    else if(data==5){
      data=3
    }
    else if(data==6){
      data=4
    }
    else if(data==8){
      data=6
    }
    else if(data==9){
      data=5
    }
  return(data)
    
}


 bb_milano <- getbb("Milano")
 bb_milano["x", "min"] <- 9.035
 bb_milano["y", "min"] <- 45.385
 bb_milano["x", "max"] <- 9.280
 bb_milano["y", "max"] <- 45.540
 map_milano <- get_stadiamap(bb_milano, zoom = 12) %>% ggmap()
 df_municipi.geojson <- geojson_read("data/geo_municipi.geojson", what="sp")
 
 df_municipi <- fortify(df_municipi.geojson)
 colnames(df_municipi)[colnames(df_municipi) == 'id'] <- 'Municipio'
 df_municipi <- df_municipi %>%
   mutate(Municipio = mapply(f_correggi_municipi, Municipio))
 
 ifm_municipi.anno_mese <- read.csv("data/incidenti_data_municipio.csv")
 ifm_municipi <- ifm_municipi.anno_mese %>% na.omit() %>%
   group_by(Anno,Municipio) %>%
   summarise(Feriti=sum(Feriti), 
             Incidenti=sum(Incidenti), 
             Morti=sum(Morti)
             )
 ifm_municipi$Municipio <- as.factor(ifm_municipi$Municipio)
 residenti_municipi <- read.csv("data/residenti_per_municipi_serie_storica.csv",
                                header = TRUE, sep=";")
 colnames(residenti_municipi)[colnames(residenti_municipi)=="MUNICIPIO"]<-"Municipio"
 residenti_municipi$Municipio <- as.factor(residenti_municipi$Municipio)
 residenti_municipi <- residenti_municipi %>% filter(Anno > 2000)
 ifm_residenti_municipi <- ifm_municipi %>%
   inner_join(residenti_municipi, by=c("Municipio", "Anno")) %>%
   mutate(Feriti=Feriti*1000/Residenti,
          Incidenti=Incidenti*1000/Residenti,
        Morti=Morti*100000/Residenti) %>%
   mutate(Residenti=NULL)

for(anno in 2001:2022){
  ifm_anno <- ifm_residenti_municipi %>% filter(Anno==anno)
  df_municipi_per_residenti <- df_municipi %>% mutate(Municipio=as.factor(Municipio)) %>%
    inner_join(ifm_anno, by="Municipio")

  map_milano_feriti <- map_milano +
    geom_polygon(data=df_municipi_per_residenti,aes(x=long, y=lat, group=group, fill=Feriti), 
                 color="black",alpha=.7, inherit.aes = FALSE) +
    scale_fill_gradientn(colours = c("blue", "green", "yellow", "red", "darkred"),
                         limits=c(0,30), name="Feriti ogni \n1.000 residenti") +
    tema
  
  map_milano_incidenti <- map_milano +
    geom_polygon(data=df_municipi_per_residenti,aes(x=long, y=lat, group=group, fill=Incidenti), 
                 color="black",alpha=.7, inherit.aes = FALSE) +
    scale_fill_gradientn(colours = c("blue", "green", "yellow", "red", "darkred"),
                         limits=c(0,25), name="Incidenti ogni \n1.000 residenti") +
    tema

  map_milano_morti <- map_milano +
    geom_polygon(data=df_municipi_per_residenti,aes(x=long, y=lat, group=group, fill=Morti), 
                 color="black",alpha=.7, inherit.aes = FALSE) +
    scale_fill_gradientn(colours = c("blue", "green", "yellow", "red", "darkred"),
                         limits=c(0,15), name="Morti ogni \n100.000 residenti") +
    tema
  file_feriti <- paste("mappa/feriti",anno , sep="_")
  file_incidenti <- paste("mappa/incidenti",anno , sep="_")
  file_morti <- paste("mappa/morti",anno , sep="_")
  # print(map_milano_feriti)
  # print(map_milano_incidenti)
  # print(map_milano_morti)
  
  save_plot(name_file = file_incidenti, p_plot = map_milano_incidenti)
  save_plot(name_file = file_feriti, p_plot = map_milano_feriti)
  save_plot(name_file = file_morti, p_plot = map_milano_morti)
}
 
