#Read dataset from step 3 and fix types
ER <- read.csv("~/Desktop/Tironcino/Projects/Database azienda/ER.csv")

#If the variable names are not these, change them to these
cols.num <- c("ricavi_vendite", "dipendenti", "utile_netto", "longitudine", "latitudine")
cols.fact <- c("provincia", "regione", "forma_giuridica", "attivita_principale",
               "startup_innovativa", "settore_codice", "settore_liv3", "settore_liv2", 
               "settore_liv1", "region")
cols.string <- c("ragione_sociale", "CF", "indirizzo", "CAP", "telefono", "website", 
                 "descrizione_attivita", "principali_prodotti_servizi", "comune")

ER[cols.num] <- sapply(ER[cols.num],as.numeric)
ER[cols.fact] <- sapply(ER[cols.fact],as.factor)
ER[cols.string] <- sapply(ER[cols.string],as.character)

#Manage strange regions
ER[is.na(ER$regione), "regione"] <- "Unknown"
ER[(ER$regione != "EMILIA-ROMAGNA") &
          (ER$regione != "Unknown")
        , "regione"] <- "Estero"
ER[is.na(ER$provincia), "provincia"] <- "Unknown"

#Manage regions from outside ER
ER[(ER$provincia != "Bologna") &
          (ER$provincia != "Parma") &
          (ER$provincia != "Modena") &
          (ER$provincia != "Forlì-Cesena") &
          (ER$provincia != "Rimini") &
          (ER$provincia != "Reggio nell'Emilia") &
          (ER$provincia != "Ravenna") &
          (ER$provincia != "Ferrara") &
          (ER$provincia != "Piacenza") &
          (ER$provincia != "Unknown")
          , "provincia"]<- "Estero"

#This is the key part. Rename it to these names so that the function map_data will recongnize them
ER$region <- ER$provincia
ER$region[ER$region == "Forlì-Cesena"] <- "Forli'" #rename
ER$region[ER$region == "Reggio nell'Emilia"] <- "Reggio Emilia" #rename
ER$region[ER$region == "Rimini"] <- "Forli'" #not in Italy


#Now, write the CSV. The database is almost ready to be merged with the app
