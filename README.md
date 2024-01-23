# Setup 
Prima di tutto installare le seguenti librerie
- dplyr
- stm
- osmdata
poi registrare la propria API key di stadiamaps e infine lanciare lo script main_setup.R

# Script
Elenco dei vari script con le relative funzioni:
- main_componenti_natura.R -> crea l'istogramma per gli incidenti, feriti e morti suddivisi per natura
- main_mappa.R -> crea le mappe degli incidenti, feriti e morti dal 2001 al 2022 suddivisi per municipio
- main_serie_totale.R -> crea i grafici dell'andamento, del trend, della stagionalità e del residuo per incidenti, feriti e morti
- main_singole_nature.R -> crea i grafici degli incidenti, feriti e morti suddivisi per natura
- main_singoli_municipi.R -> crea i grafici degli incidenti, feriti e morti suddivisi per municipi
- main_variazione_totale.R -> crea gli istogrammi delle variazioni di incidenti, feriti e morti rispetto al 2001 e all'anno base
