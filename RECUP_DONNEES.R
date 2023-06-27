requirements <- readLines("requirements_R.txt")
for (package in requirements) {
  install.packages(package)
}
library(aws.s3)
library(dplyr)
library(readr)
bucket <- "projet-funathon"
path_data <- "2023/sujet3/diffusion"

description_indiv <- s3read_using(read_delim, object = paste(path_data, "description-indiv.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
habitudes_indiv <- s3read_using(read_delim, object = paste(path_data, "habitudes-indiv.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
actphys_sedent <- s3read_using(read_delim, object = paste(path_data, "actphys-sedent.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
fpq <- s3read_using(read_delim, object = paste(path_data, "fpq.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)

# Imc moyen par niveau de diplôme

imc_par_diplome <- description_indiv %>% group_by(diplome_PR) %>% summarise(imc_moyen=mean(imc,na.rm=TRUE))

ggplot(data=description_indiv,aes(x=diplome_interv))+ 
  geom_histogram(binwidth=1,color="black",fill="darkred")+
  labs(title="Histogramme des niveaux de diplôme",  # cette fois on rajoute des légendes
       x="Code du niveau de diplôme",
       y="Nombre d'individus")
description_indiv <- description_indiv %>% mutate(categorie_diplome=case_when(diplome_interv==1 ~ "Aucun diplôme, n'a jamais été scolarisé",
                                                                              diplome_interv==2 ~ "Aucun diplôme, scolarité s'est arrêtée à l'école primaire",
                                                                              diplome_interv==3 ~ "Aucun diplôme, scolarité s'est arrêtée au collège",
                                                                              diplome_interv==4 ~ "Aucun diplôme, scolarité s'est arrêtée au delà du collège",
                                                                              diplome_interv==5 ~ "Aucun diplôme, sans précision",
                                                                              diplome_interv==6 ~ "CEP",
                                                                              diplome_interv==7 ~ "CAP, BEP, BEPC, brevet élémentaire, brevet de compagnon",
                                                                              diplome_interv==8 ~ "Baccalauréat technologique ou professionnel,\nBrevet professionnel ou de technicien,\nBEA, BEC, BEI, BEH, capacité en droit",
                                                                              diplome_interv==9 ~ "Baccalauréat général",
                                                                              diplome_interv==10 ~ "Diplôme de 1er cycle universitaire (Bac +3, licence),\nBTS, DUT, DEST, DEUG, diplôme des professions\nsociales ou de la santé, d'infirmier",
                                                                              diplome_interv==11 ~ "Diplôme de 2ème cycle universitaire (Bac+4, Bac+5),\nMaster, Maîtrise, diplôme d'ingénieur,\nd'une grande école",
                                                                              diplome_interv==12 ~ "Diplôme de 3ème cycle universitaire (>Bac+5, doctorat),\ndiplôme de vétérinaire, médecin, pharmacien",
                                                                              diplome_interv==13 ~ "Refus",
                                                                              diplome_interv==14 ~ "Ne sait pas"))
# Tableau des fréquences de chaque catégorie de diplome
counts_diplome <- description_indiv %>% group_by(categorie_diplome) %>% summarise(n=n())

# Graphique en barres horizontales
ggplot(data=counts_diplome,aes(x=categorie_diplome,y=n))+
  geom_histogram(stat="identity")+
  coord_flip()+
  labs(title="Histogramme des niveaux de diplôme",
       x="Libelle du niveau de diplôme",
       y="Nombre d'individus")

description_indiv <- description_indiv %>% mutate(categorie_agglo = case_when(agglo_5cl==1 ~ "Rural",
                                                                              agglo_5cl==2 ~ "2000 - 19 999 hab",
                                                                              agglo_5cl==3 ~ "20 000 - 99 999 hab",
                                                                              agglo_5cl==4 ~ "+ 100 000 hab",
                                                                              agglo_5cl==5 ~ "Paris"))
counts_agglo <- description_indiv %>% group_by(categorie_agglo) %>% summarise(n=n())

# Générer le graphique en barres horizontales
ggplot(data=counts_agglo,aes(x=categorie_agglo,y=n))+
  geom_histogram(stat="NOMEN")+
  coord_flip()+ 
  labs(title="Histogramme des types d'agglomération",
       x="Type d'agglomération",
       y="Nombre d'individus")



description_indiv <- description_indiv %>% mutate(categorie_ruc=case_when(RUC_4cl==1 ~ "<900 €/mois/UC",
                                                                          RUC_4cl==2 ~ "[900-1 340[ €/mois/UC",
                                                                          RUC_4cl==3 ~ "[1 340-1 850[ €/mois/U",
                                                                          RUC_4cl==4 ~ ">=1 850 €/mois/UC"))

counts_RUC <- description_indiv %>% group_by(RUC_4cl) %>% summarise(n=n())

ggplot(data=counts_RUC,aes(x=RUC_4cl,y=n))+
  geom_histogram(stat="identity")+
  coord_flip()+ 
  labs(title="Histogramme des niveaux de revenus",
       x="Niveau de revenus",
       y="Nombre d'individus")

ponderations<-description_indiv %>% select(NOIND,pond_indiv_adu_pop2,pond_indiv_enf_pop2)

infos_indiv <- description_indiv %>%
  select(NOIND,sex_PS,tage_PS,region_adm_12cl,agglo_5cl)
table(habitudes$tage_PS)
habitudes<-habitudes_indiv %>%
  left_join(ponderations) %>% 
  left_join(infos_indiv) %>% 
  mutate(POND=if_else(tage_PS %in% c("7","8","9"),
                      pond_indiv_adu_pop2,
                      pond_indiv_enf_pop2),
         AGES=if_else(tage_PS %in% c("7","8","9"),
                      "ADULTE",
                      "ENFANT"))  


count_pers<- habitudes %>% group_by(AGES) %>% summarise(POND=sum(POND))
adultes <-infos_indiv %>% filter(tage_PS %in% c("7","8","9"))
enfants <-infos_indiv %>% filter(!(tage_PS %in% c("7","8","9")))                         

if_else(tage_PS %in% c("7","8","9"),
                      pond_indiv_adu_pop2,
                      pond_indiv_enf_pop2)

url <- "https://minio.lab.sspcloud.fr/projet-cartiflette/diffusion/shapefiles-test1/year%3D2022/administrative_level%3DREGION/crs%3D4326/FRANCE_ENTIERE%3Dmetropole/vectorfile_format%3D%27geojson%27/provider%3D%27IGN%27/source%3D%27EXPRESS-COG-CARTO-TERRITOIRE%27/raw.geojson"
region <- sf::st_read(url)
# Passons le fonds de carte dans le système de coordonnées de référence utilisé pour la FRance, Lambert 93 (code : 2154) au lieu de WGS 84
region <- region %>% st_transform(2154)
mf_base(region)
tab_corr <-data.frame(reg=c(84,32,84,93,93,93,84,44,76,44,76,76,93,28,84,75,75,24,75,27,53,75,75,27,84,28,24,53,94,94,76,76,76,75,76,53,24,24,84,27,75,24,84,84,52,24,76,75,76,52,28,44,44,52,44,44,53,44,27,32,32,28,32,84,75,76,76,44,44,84,27,27,52,84,84,11,28,11,11,75,32,76,76,93,93,52,75,75,44,27,27,11,11,11,11,11,01,02,03,04,06),
                    dep=c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","21","22","23","24","25","26","27","28","29","2A","2B","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","971","972","973","974","976"))
lib_reg<-description_indiv %>% select(region_adm_12cl)%>% unique() %>% 
  mutate(region_recode=case_when(
    region_adm_12cl==1 ~ "ILE-DE-FRANCE",
    region_adm_12cl==2 ~ "NORMANDIE",
    region_adm_12cl==3 ~ "CENTRE-VAL DE LOIRE",
    region_adm_12cl==4 ~ "PAYS DE LA LOIRE",
    region_adm_12cl==5 ~ "BRETAGNE",
    region_adm_12cl==6 ~ "HAUTS-DE-FRANCE",
    region_adm_12cl==7 ~ "GRAND EST",
    region_adm_12cl==8 ~ "BOURGOGNE-FRANCHE-COMTE",
    region_adm_12cl==9 ~ "AUVERGNE-RHONE-ALPES",
    region_adm_12cl==10 ~ "PROVENCE-ALPES-COTE D'AZUR",
    region_adm_12cl==11 ~ "OCCITANIE",
    region_adm_12cl==12 ~ "NOUVELLE-AQUITAINE",)) %>% unique()

compte_region<-habitudes %>% group_by(region_adm_12cl) %>%
  summarise(POND=sum(POND)) %>% left_join(lib_reg)

consoBIO_ad<-habitudes %>% filter(NOIND %in% adultes$NOIND) %>% select(region_adm_12cl,consommation_bio,POND) %>% group_by(region_adm_12cl,consommation_bio) %>%
  summarise_all(sum, na.rm=T) %>% left_join(lib_reg)

 cons_bio2_ad<- pivot_wider(consoBIO_ad,
                         names_from = consommation_bio,
                         values_from = POND) %>%
   rename("NON"="0","OUI"="1","MQ"="NA") %>%
   mutate(partBIO_Adult=OUI/(NON+OUI)*100,
          partNA_AD=(MQ/(NON+OUI+MQ)*100) )%>%
   left_join(lib_reg)

 
 #carte de la consommation en Bio de adultes 
 carteB<-left_join(region,cons_bio2_ad,by=c("NOM_M"="region_recode"))
 carteB<-st_as_sf(carteB)
 carteB<-mutate(carteB,label=round(partBIO_Adult),1)
library(mapsf)
 mf_base(carteB)
 mf_choro(carteB,var="partBIO_Adult", breaks = "jenks",nbreaks = 5)
 mf_label(carteB,var="label",col="black",cex=0.8,halo=T)
 mf_title("Part des adultes déclarant consommer du bio",pos="center")
 
 
 test<-habitudes %>% mutate(nb = rowSums(is.na(.))) %>% select(NOIND,nb)

library(survey)
