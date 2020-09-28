# Proyecto-Bedu
Análisis de Índice Delictivo
##Intalacion de packetes
install.packages("tidyverse")
install.packages("readxl")
install.packages("readr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("data.table")
install.packages("rgdal", type = "binary")
install.packages("rgeos", type = "binary")
install.packages("maptools")
install.packages("sf")
install.packages("RColorBrewer")
install.packages("classInt")
install.packages("rgdal")
install.packages("tmap")
install.packages("sp")
install.packages("xlsx")

##Librerias
library(readxl)
library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(data.table)
library(rgdal)
library(rgeos)
library(maptools)
library(sf)
library(RColorBrewer)
library(classInt)
library(rgdal)
library(sp)
library(xlsx)


##Obtener los directorios
getwd()
setwd("C:/Users/alejandro.zambrano/Desktop/Introduccion-a-Base-de-Datos/Sesion-03/Postwork-03/Municipal-Delitos-2015-2020_ene2020")

##Se le el archivo en xl y se nombra DB_CRIMINALIDAD
DB_CRIMINALIDAD<-read_csv("Criminalidad-2015-2020.csv")

##Checar los titulos
names(DB_CRIMINALIDAD)

##Se selecciona solo las variables que se usaran
DB_CRIMINALIDAD <-select(DB_CRIMINALIDAD, "Fecha" = "Ano", "CVE_ENT"  = "Clave_Ent", "Ent" = "Entidad", "CVE_MUN" = "Cve. Municipio", "Mun" = "Municipio", "Delito" = "Tipo de delito", "Delito_sub" = "Subtipo de delito", "Modalidad",  "ENE" = "Enero", "FEB" = "Febrero", "MAR" = "Marzo", "ABR" = "Abril", "MAY" = "Mayo", "JUN" = "Junio",  "JUL" = "Julio", "AGO" = "Agosto",  "SEP" = "Septiembre",  "OCT" = "Octubre", "NOV" =  "Noviembre", "DIC" = "Diciembre")

##Se hace un doble check de los nombres de la base 
names(DB_CRIMINALIDAD)

##Agregamos un 0 a la variable "Clave_Ent", solo de los numero que van del 1 al 9
DB_CRIMINALIDAD<- DB_CRIMINALIDAD %>%
    mutate_at(c("CVE_ENT"),as.character) %>%
  mutate("CVE_ENT"=ifelse(nchar(CVE_ENT)==1, paste0("0",CVE_ENT), CVE_ENT))

##se limpia la base de la variable Clave_Mun
DB_CRIMINALIDAD<- DB_CRIMINALIDAD %>%
  mutate_at(c("CVE_MUN"),as.character) %>%
  mutate("CVE_MUN"=ifelse(nchar(CVE_MUN)==4, substr(CVE_MUN,2,4), CVE_MUN))

DB_CRIMINALIDAD<- DB_CRIMINALIDAD %>%
  mutate_at(c("CVE_MUN"),as.character) %>%
  mutate("CVE_MUN"=ifelse(nchar(CVE_MUN)==5, substr(CVE_MUN,3,5), CVE_MUN))
        

##Pegar clave_mun en clave_ent solo en los que tengas 3 digitos, se nombra concat
DB_CRIMINALIDAD<- mutate(DB_CRIMINALIDAD, CVEGEO = paste0(CVE_ENT, CVE_MUN))
  
##Se coloca la variable concat despues de Clave_mun
DB_CRIMINALIDAD <-select(DB_CRIMINALIDAD, "Fecha", "CVE_ENT", "Ent", "CVE_MUN", "CVEGEO", "MUN" = "Mun", "Delito", "Delito_sub", "Modalidad", "ENE", "FEB", "MAR", "ABR", "MAY", "JUN",  "JUL", "AGO",  "SEP",  "OCT", "NOV", "DIC")


#Solo nos interesa ciertas variables, por eso sacamos los valores unicos de delitos
unique(DB_CRIMINALIDAD$Delito)

##Filtrar variables que deseamos análizar
DB_CRIMINALIDAD<- DB_CRIMINALIDAD %>%
  filter(Delito %in% c("Homicidio", "Feminicidio", "Secuestro", "Abuso sexual", "Robo", "Allanamiento de morada"))

##Se elijen los sub-delitos
unique(DB_CRIMINALIDAD$Delito_sub)

#Filtran los sub-delitos
DB_CRIMINALIDAD <- DB_CRIMINALIDAD %>%
  filter(Delito_sub %in% c("Homicidio doloso", "Homicidio culposo", "Feminicidio", "Abuso sexual", "Secuestro", "Robo a casa habitacion", "Robo de vehiculo automotor", "Robo a negocio", "Allanamiento de morada"))

##Se carga la base shp
setwd("C:/Users/alejandro.zambrano/Desktop/Introduccion-a-Base-de-Datos/Sesion-03/Postwork-03")

shape_ent<-  st_read("00ent.shp")
view(shape_ent)
shape_ent$CVEGEO <- as.character(shape_ent$CVEGEO)
shape_ent$CVE_ENT <- as.character(shape_ent$CVE_ENT) 
  
shape_mun<- st_read("00mun.shp")
view(shape_mun)
shape_mun$CVEGEO <- as.character(shape_mun$CVEGEO)
shape_mun$CVE_ENT <- as.character(shape_mun$CVE_ENT)
shape_mun$CVE_MUN <- as.character(shape_mun$CVE_MUN)

##Se carga la base de poblacion
setwd("C:/Users/alejandro.zambrano/Desktop/Introduccion-a-Base-de-Datos/Sesion-03/Postwork-03/Población")

Pob1<-read_csv("base_municipios_final_datos_01.csv")
Pob2<- read_csv("base_municipios_final_datos_02.csv")

#Se limpia las bases de población 
names(Pob1)
Pob1_sex <-select(Pob1, "CVE_MUN" = "CLAVE", "CVE_ENT" = "CLAVE_ENT", "Fecha" = "FECHA", "SEXO" ,"Pob" = "POB")
Pob1 <-select(Pob1, "CVE_MUN" = "CLAVE", "CVE_ENT" = "CLAVE_ENT", "Fecha" = "FECHA", "Pob" = "POB")


names(Pob2)
Pob2_sex <-select(Pob2, "CVE_MUN" = "CLAVE", "CVE_ENT" = "CLAVE_ENT", "Fecha" = "FECHA", "SEXO" ,"Pob" = "POB")
Pob2 <-select(Pob2, "CVE_MUN" = "CLAVE", "CVE_ENT" = "CLAVE_ENT", "Fecha" = "FECHA", "Pob" = "POB")

Pob <- rbind(Pob1, Pob2)
Pob_sex <- rbind(Pob1_sex, Pob2_sex)
names(Pob)
names(Pob_sex)

##Se trasnforma las variable Cve_ent y Cve_mun, con la finalidad de hacer el join
##Agregamos un 0 a la variable "Clave_Ent", solo de los numero que van del 1 al 9
Pob<- Pob %>%
  mutate_at(c("CVE_ENT"),as.character) %>%
  mutate("CVE_ENT"=ifelse(nchar(CVE_ENT)==1, paste0("0",CVE_ENT), CVE_ENT))

Pob_sex<- Pob_sex %>%
  mutate_at(c("CVE_ENT"),as.character) %>%
  mutate("CVE_ENT"=ifelse(nchar(CVE_ENT)==1, paste0("0",CVE_ENT), CVE_ENT))

##se limpia la base de la variable Clave_Mun
Pob<- Pob %>%
  mutate_at(c("CVE_MUN"),as.character) %>%
  mutate("CVE_MUN"=ifelse(nchar(CVE_MUN)==4, substr(CVE_MUN,2,4), CVE_MUN))

Pob<- Pob %>%
  mutate_at(c("CVE_MUN"),as.character) %>%
  mutate("CVE_MUN"=ifelse(nchar(CVE_MUN)==5, substr(CVE_MUN,3,5), CVE_MUN))


Pob_sex<- Pob_sex %>%
  mutate_at(c("CVE_MUN"),as.character) %>%
  mutate("CVE_MUN"=ifelse(nchar(CVE_MUN)==4, substr(CVE_MUN,2,4), CVE_MUN))

Pob_sex<- Pob_sex %>%
  mutate_at(c("CVE_MUN"),as.character) %>%
  mutate("CVE_MUN"=ifelse(nchar(CVE_MUN)==5, substr(CVE_MUN,3,5), CVE_MUN))


##Pegar clave_mun en clave_ent solo en los que tengas 3 digitos, se nombra concat
Pob<- mutate(Pob, CVEGEO = paste0(CVE_ENT, CVE_MUN))
Pob_sex<- mutate(Pob_sex, CVEGEO = paste0(CVE_ENT, CVE_MUN))

##Se coloca la variable CVEGEO al principio
Pob <- select(Pob, "Fecha", "CVEGEO", "CVE_ENT", "CVE_MUN", "Pob")
Pob_sex <- select(Pob_sex, "Fecha", "CVEGEO", "CVE_ENT", "CVE_MUN", "SEXO", "Pob")

##Se filtran solo las mujeres en la base de población y sexo para hacer la tasa de feminicidios
Pob_sex <- filter(Pob_sex, SEXO == "Mujeres")
unique(Pob_sex$SEXO)

##Se filtran solo los años a usar POB
Pob_2015 <- filter(Pob, Fecha == 2015)
Pob_2016 <- filter(Pob, Fecha == 2016)
Pob_2017 <- filter(Pob, Fecha == 2017)
Pob_2018 <- filter(Pob, Fecha == 2018)  
Pob_2019 <- filter(Pob, Fecha == 2019)

##Se filtran solo los años a usar POB_SEX
Pob_2015_SEXO <- filter(Pob_sex, Fecha == 2015)
Pob_2016_SEXO <- filter(Pob_sex, Fecha == 2016)
Pob_2017_SEXO <- filter(Pob_sex, Fecha == 2017)
Pob_2018_SEXO <- filter(Pob_sex, Fecha == 2018)  
Pob_2019_SEXO <- filter(Pob_sex, Fecha == 2019)
  
#Se obtendra cada base por año tanto de población como de criminalidad 
DB_CRIMI_2015<- filter(DB_CRIMINALIDAD, DB_CRIMINALIDAD$Fecha==2015)
DB_CRIMI_2016<- filter(DB_CRIMINALIDAD, DB_CRIMINALIDAD$Fecha==2016)
DB_CRIMI_2017<- filter(DB_CRIMINALIDAD, DB_CRIMINALIDAD$Fecha==2017)
DB_CRIMI_2018<- filter(DB_CRIMINALIDAD, DB_CRIMINALIDAD$Fecha==2018)
DB_CRIMI_2019<- filter(DB_CRIMINALIDAD, DB_CRIMINALIDAD$Fecha==2019)

##Se hace una tabla de estados y una de municipios de las bases de criminalidad
#Se agrupan y se suman por Fecha, entidad, municipio y Delito
#2015
DB_CRIMI_2015_MUN <- 
  DB_CRIMI_2015 %>% 
  tbl_df() %>%
  group_by(Fecha, CVEGEO, CVE_ENT, Ent, CVE_MUN, MUN, Delito) %>%
  select(ENE:DIC) %>%
  gather(key = "mes", value = "numero", ENE:DIC) %>%
  summarise(Total.Delitos = sum(numero)) %>%
  ungroup()


#2016
DB_CRIMI_2016_MUN <- 
  DB_CRIMI_2016 %>% 
  tbl_df() %>%
  group_by(Fecha, CVEGEO, CVE_ENT, Ent, CVE_MUN, MUN, Delito) %>%
  select(ENE:DIC) %>%
  gather(key = "mes", value = "numero", ENE:DIC) %>%
  summarise(Total.Delitos = sum(numero)) %>%
  ungroup()

#2017
DB_CRIMI_2017_MUN <- 
  DB_CRIMI_2017 %>% 
  tbl_df() %>%
  group_by(Fecha, CVEGEO, CVE_ENT, Ent, CVE_MUN, MUN, Delito) %>%
  select(ENE:DIC) %>%
  gather(key = "mes", value = "numero", ENE:DIC) %>%
  summarise(Total.Delitos = sum(numero)) %>%
  ungroup()

#2018
DB_CRIMI_2018_MUN <- 
  DB_CRIMI_2018 %>% 
  tbl_df() %>%
  group_by(Fecha, CVEGEO, CVE_ENT, Ent, CVE_MUN, MUN, Delito) %>%
  select(ENE:DIC) %>%
  gather(key = "mes", value = "numero", ENE:DIC) %>%
  summarise(Total.Delitos = sum(numero)) %>%
  ungroup()

#2019
DB_CRIMI_2019_MUN <- 
  DB_CRIMI_2019 %>% 
  tbl_df() %>%
  group_by(Fecha, CVEGEO, CVE_ENT, Ent, CVE_MUN, MUN, Delito) %>%
  select(ENE:DIC) %>%
  gather(key = "mes", value = "numero", ENE:DIC) %>%
  summarise(Total.Delitos = sum(numero)) %>%
  ungroup()

#Se agrupan y se suman por Fecha, entidad, y Delito
#2015
DB_CRIMI_2015_ENT <- 
  DB_CRIMI_2015 %>% 
  tbl_df() %>%
  group_by(Fecha, CVE_ENT, Ent, Delito) %>%
  select(ENE:DIC) %>%
  gather(key = "mes", value = "numero", ENE:DIC) %>%
  summarise(Total.Delitos = sum(numero)) %>%
  ungroup()


#2016
DB_CRIMI_2016_ENT <- 
  DB_CRIMI_2016 %>% 
  tbl_df() %>%
  group_by(Fecha, CVE_ENT, Ent, Delito) %>%
  select(ENE:DIC) %>%
  gather(key = "mes", value = "numero", ENE:DIC) %>%
  summarise(Total.Delitos = sum(numero)) %>%
  ungroup()

#2017
DB_CRIMI_2017_ENT <- 
  DB_CRIMI_2017 %>% 
  tbl_df() %>%
  group_by(Fecha, CVE_ENT, Ent, Delito) %>%
  select(ENE:DIC) %>%
  gather(key = "mes", value = "numero", ENE:DIC) %>%
  summarise(Total.Delitos = sum(numero)) %>%
  ungroup()

#2018
DB_CRIMI_2018_ENT <- 
  DB_CRIMI_2018 %>% 
  tbl_df() %>%
  group_by(Fecha, CVE_ENT, Ent, Delito) %>%
  select(ENE:DIC) %>%
  gather(key = "mes", value = "numero", ENE:DIC) %>%
  summarise(Total.Delitos = sum(numero)) %>%
  ungroup()

#2019
DB_CRIMI_2019_ENT <- 
  DB_CRIMI_2019 %>% 
  tbl_df() %>%
  group_by(Fecha, CVE_ENT, Ent, Delito) %>%
  select(ENE:DIC) %>%
  gather(key = "mes", value = "numero", ENE:DIC) %>%
  summarise(Total.Delitos = sum(numero)) %>%
  ungroup()


##Se hace una tabla de estados y una de municipios de las bases de Pob
#Se agrupan y se suman por Fecha, entidad, y Poblacion
#2015
Pob_2015_ENT <- 
  Pob_2015 %>% 
  tbl_df() %>%
  group_by(Fecha, CVE_ENT) %>%
  summarise(Total.Pob = sum(Pob)) %>%
  ungroup()

#2015_SEXO
Pob_2015_ENT_SEXO <- 
  Pob_2015_SEXO %>% 
  tbl_df() %>%
  group_by(Fecha, CVE_ENT, SEXO) %>%
  summarise(Total.Pob = sum(Pob)) %>%
  ungroup()

#2016
Pob_2016_ENT <- 
  Pob_2016 %>% 
  tbl_df() %>%
  group_by(Fecha, CVE_ENT) %>%
  summarise(Total.Pob = sum(Pob)) %>%
  ungroup()

#2016_SEXO
Pob_2016_ENT_SEXO <- 
  Pob_2015_SEXO %>% 
  tbl_df() %>%
  group_by(Fecha, CVE_ENT, SEXO) %>%
  summarise(Total.Pob = sum(Pob)) %>%
  ungroup()

#2017
Pob_2017_ENT <- 
  Pob_2017 %>% 
  tbl_df() %>%
  group_by(Fecha, CVE_ENT) %>%
  summarise(Total.Pob = sum(Pob)) %>%
  ungroup()

#2017_SEXO
Pob_2017_ENT_SEXO <- 
  Pob_2017_SEXO %>% 
  tbl_df() %>%
  group_by(Fecha, CVE_ENT, SEXO) %>%
  summarise(Total.Pob = sum(Pob)) %>%
  ungroup()

#2018
Pob_2018_ENT <- 
  Pob_2018 %>% 
  tbl_df() %>%
  group_by(Fecha, CVE_ENT) %>%
  summarise(Total.Pob = sum(Pob)) %>%
  ungroup()

#2018_SEXO
Pob_2018_ENT_SEXO <- 
  Pob_2018_SEXO %>% 
  tbl_df() %>%
  group_by(Fecha, CVE_ENT, SEXO) %>%
  summarise(Total.Pob = sum(Pob)) %>%
  ungroup()

#2019
Pob_2019_ENT <- 
  Pob_2019 %>% 
  tbl_df() %>%
  group_by(Fecha, CVE_ENT) %>%
  summarise(Total.Pob = sum(Pob)) %>%
  ungroup()

#2019_SEXO
Pob_2019_ENT_SEXO <- 
  Pob_2019_SEXO %>% 
  tbl_df() %>%
  group_by(Fecha, CVE_ENT, SEXO) %>%
  summarise(Total.Pob = sum(Pob)) %>%
  ungroup()

##Join ENTIDAD
# Se realizan los joins por fecha, a nivel ENT, con shp y la base de criminalidad
#Base_2015_ENT <- left_join(shape_ent, DB_CRIMI_2015_ENT, by = "CVE_ENT" )
#Base_2016_ENT <- left_join(shape_ent, DB_CRIMI_2016_ENT, by = "CVE_ENT" )
#Base_2017_ENT <- left_join(shape_ent, DB_CRIMI_2017_ENT, by = "CVE_ENT" )
#Base_2018_ENT <- left_join(shape_ent, DB_CRIMI_2018_ENT, by = "CVE_ENT" )
#Base_2019_ENT <- left_join(shape_ent, DB_CRIMI_2019_ENT, by = "CVE_ENT" )


# Se realizan los joins por fecha, a nivel ENT, con shp y la base de POB
#Base_2015_ENT <- left_join(Base_2015_ENT, Pob_2015_ENT, by = "CVE_ENT" )
#Base_2016_ENT <- left_join(Base_2016_ENT, Pob_2016_ENT, by = "CVE_ENT" )
#Base_2017_ENT <- left_join(Base_2017_ENT, Pob_2017_ENT, by = "CVE_ENT" )
#Base_2018_ENT <- left_join(Base_2018_ENT, Pob_2018_ENT, by = "CVE_ENT" )
#Base_2019_ENT <- left_join(Base_2019_ENT, Pob_2019_ENT, by = "CVE_ENT" )

# Se realizan los joins por fecha, a nivel ENT, con shp y la base de criminalidad
Base_2015_ENT <- left_join(DB_CRIMI_2015_ENT, Pob_2015_ENT, by = "CVE_ENT" )
Base_2016_ENT <- left_join(DB_CRIMI_2016_ENT, Pob_2016_ENT, by = "CVE_ENT" )
Base_2017_ENT <- left_join(DB_CRIMI_2017_ENT, Pob_2017_ENT, by = "CVE_ENT" )
Base_2018_ENT <- left_join(DB_CRIMI_2018_ENT, Pob_2016_ENT, by = "CVE_ENT" )
Base_2019_ENT <- left_join(DB_CRIMI_2019_ENT, Pob_2016_ENT, by = "CVE_ENT" )



#Se selecciona la información que estan en las base de enteidad
#Base_2015_ENT <- select(Base_2015_ENT, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "Total.Pob", "geometry" )
#Base_2016_ENT <- select(Base_2016_ENT, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "Total.Pob", "geometry" )
#Base_2017_ENT <- select(Base_2017_ENT, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "Total.Pob", "geometry" )
#Base_2018_ENT <- select(Base_2018_ENT, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "Total.Pob", "geometry" )
#Base_2019_ENT <- select(Base_2019_ENT, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "Total.Pob", "geometry" )

#Se selecciona la información que estan en las base de enteidad
Base_2015_ENT <- select(Base_2015_ENT, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "Total.Pob")
Base_2016_ENT <- select(Base_2016_ENT, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "Total.Pob")
Base_2017_ENT <- select(Base_2017_ENT, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "Total.Pob")
Base_2018_ENT <- select(Base_2018_ENT, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "Total.Pob")
Base_2019_ENT <- select(Base_2019_ENT, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "Total.Pob")


##Join ENTIDAD_SEXO
# Se realizan los joins por fecha, a nivel ENT, con shp y la base de criminalidad
#Base_2015_ENT_SEXO <- left_join(shape_ent, DB_CRIMI_2015_ENT, by = "CVE_ENT" )
#Base_2016_ENT_SEXO <- left_join(shape_ent, DB_CRIMI_2016_ENT, by = "CVE_ENT" )
#Base_2017_ENT_SEXO <- left_join(shape_ent, DB_CRIMI_2017_ENT, by = "CVE_ENT" )
#Base_2018_ENT_SEXO <- left_join(shape_ent, DB_CRIMI_2018_ENT, by = "CVE_ENT" )
#Base_2019_ENT_SEXO <- left_join(shape_ent, DB_CRIMI_2019_ENT, by = "CVE_ENT" )

Base_2015_ENT_SEXO <- left_join(DB_CRIMI_2015_ENT, Pob_2015_ENT_SEXO, by = "CVE_ENT")
Base_2016_ENT_SEXO <- left_join(DB_CRIMI_2016_ENT, Pob_2016_ENT_SEXO, by = "CVE_ENT")
Base_2017_ENT_SEXO <- left_join(DB_CRIMI_2017_ENT, Pob_2017_ENT_SEXO, by = "CVE_ENT")
Base_2018_ENT_SEXO <- left_join(DB_CRIMI_2018_ENT, Pob_2018_ENT_SEXO, by = "CVE_ENT")
Base_2019_ENT_SEXO <- left_join(DB_CRIMI_2019_ENT, Pob_2019_ENT_SEXO, by = "CVE_ENT")


# Se realizan los joins por fecha, a nivel ENT, con shp y la base de POB
#Base_2015_ENT_SEXO <- left_join(Base_2015_ENT_SEXO, Pob_2015_ENT_SEXO, by = "CVE_ENT" )
#Base_2016_ENT_SEXO <- left_join(Base_2016_ENT_SEXO, Pob_2016_ENT_SEXO, by = "CVE_ENT" )
#Base_2017_ENT_SEXO <- left_join(Base_2017_ENT_SEXO, Pob_2017_ENT_SEXO, by = "CVE_ENT" )
#Base_2018_ENT_SEXO <- left_join(Base_2018_ENT_SEXO, Pob_2018_ENT_SEXO, by = "CVE_ENT" )
#Base_2019_ENT_SEXO <- left_join(Base_2019_ENT_SEXO, Pob_2019_ENT_SEXO, by = "CVE_ENT" )

#Se selecciona la información que estan en las base de enteidad
#Base_2015_ENT_SEXO <- select(Base_2015_ENT_SEXO, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "SEXO" ,"Total.Pob", "geometry" )
#Base_2016_ENT_SEXO <- select(Base_2016_ENT_SEXO, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "SEXO" ,"Total.Pob", "geometry" )
#Base_2017_ENT_SEXO <- select(Base_2017_ENT_SEXO, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "SEXO" ,"Total.Pob", "geometry" )
#Base_2018_ENT_SEXO <- select(Base_2018_ENT_SEXO, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "SEXO" ,"Total.Pob", "geometry" )
#Base_2019_ENT_SEXO <- select(Base_2019_ENT_SEXO, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "SEXO" ,"Total.Pob", "geometry" )

Base_2015_ENT_SEXO <- select(Base_2015_ENT_SEXO, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "SEXO" ,"Total.Pob")
Base_2016_ENT_SEXO <- select(Base_2016_ENT_SEXO, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "SEXO" ,"Total.Pob")
Base_2017_ENT_SEXO <- select(Base_2017_ENT_SEXO, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "SEXO" ,"Total.Pob")
Base_2018_ENT_SEXO <- select(Base_2018_ENT_SEXO, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "SEXO" ,"Total.Pob")
Base_2019_ENT_SEXO <- select(Base_2019_ENT_SEXO, "CVE_ENT", "FECHA" = "Fecha.x", "ENT" = "Ent", "Delito", "Total.Delitos", "SEXO" ,"Total.Pob")


#Join MUNICIPIO
##Se hace el join para la base criminalidad de municipios y shp
#Base_2015_MUN <- left_join(shape_mun, DB_CRIMI_2015_MUN, by = "CVEGEO" )
#Base_2016_MUN <- left_join(shape_mun, DB_CRIMI_2016_MUN, by = "CVEGEO" )
#Base_2017_MUN <- left_join(shape_mun, DB_CRIMI_2017_MUN, by = "CVEGEO" )
#Base_2018_MUN <- left_join(shape_mun, DB_CRIMI_2018_MUN, by = "CVEGEO" )
#Base_2019_MUN <- left_join(shape_mun, DB_CRIMI_2019_MUN, by = "CVEGEO" )

Base_2015_MUN <- left_join(DB_CRIMI_2015_MUN, Pob_2015, by = "CVEGEO" )
Base_2016_MUN <- left_join(DB_CRIMI_2016_MUN, Pob_2016, by = "CVEGEO" )
Base_2017_MUN <- left_join(DB_CRIMI_2017_MUN, Pob_2017, by = "CVEGEO" )
Base_2018_MUN <- left_join(DB_CRIMI_2018_MUN, Pob_2018, by = "CVEGEO" )
Base_2019_MUN <- left_join(DB_CRIMI_2019_MUN, Pob_2019, by = "CVEGEO" )

##Se hace el join para la base criminalidad de municipios y Pob
#Base_2015_MUN <- left_join(DB_CRIMI_2015_MUN, Pob_2015, by = "CVEGEO" )
#Base_2016_MUN <- left_join(DB_CRIMI_2016_MUN, Pob_2016, by = "CVEGEO" )
#Base_2017_MUN <- left_join(DB_CRIMI_2017_MUN, Pob_2017, by = "CVEGEO" )
#Base_2018_MUN <- left_join(DB_CRIMI_2018_MUN, Pob_2018, by = "CVEGEO" )
#Base_2019_MUN <- left_join(DB_CRIMI_2019_MUN, Pob_2019, by = "CVEGEO" )

#Se selecciona la información que estan en las base de municipio
#Base_2015_MUN <- select(Base_2015_MUN,"Fecha" = "Fecha.x", "CVEGEO", "CVE_ENT" = "CVE_ENT.x", "ENT" = "Ent", "CVE_MUN" = "CVE_MUN.x", "MUN","Delito", "Total.Delitos", "POB" = "Pob")
#Base_2016_MUN <- select(Base_2016_MUN,"Fecha" = "Fecha.x", "CVEGEO", "CVE_ENT" = "CVE_ENT.x", "ENT" = "Ent", "CVE_MUN" = "CVE_MUN.x", "MUN","Delito", "Total.Delitos", "POB" = "Pob")
#Base_2017_MUN <- select(Base_2017_MUN,"Fecha" = "Fecha.x", "CVEGEO", "CVE_ENT" = "CVE_ENT.x", "ENT" = "Ent", "CVE_MUN" = "CVE_MUN.x", "MUN","Delito", "Total.Delitos", "POB" = "Pob")
#Base_2018_MUN <- select(Base_2018_MUN,"Fecha" = "Fecha.x", "CVEGEO", "CVE_ENT" = "CVE_ENT.x", "ENT" = "Ent", "CVE_MUN" = "CVE_MUN.x", "MUN","Delito", "Total.Delitos", "POB" = "Pob")
#Base_2019_MUN <- select(Base_2019_MUN,"Fecha" = "Fecha.x", "CVEGEO", "CVE_ENT" = "CVE_ENT.x", "ENT" = "Ent", "CVE_MUN" = "CVE_MUN.x", "MUN","Delito", "Total.Delitos", "POB" = "Pob")

Base_2015_MUN <- select(Base_2015_MUN, "Fecha" = "Fecha.x", "CVEGEO", "CVE_ENT" = "CVE_ENT.x", "ENT" = "Ent", "CVE_MUN" = "CVE_MUN.x", "MUN","Delito", "Total.Delitos", "POB" = "Pob")
Base_2016_MUN <- select(Base_2016_MUN, "Fecha" = "Fecha.x", "CVEGEO", "CVE_ENT" = "CVE_ENT.x", "ENT" = "Ent", "CVE_MUN" = "CVE_MUN.x", "MUN","Delito", "Total.Delitos", "POB" = "Pob")
Base_2017_MUN <- select(Base_2017_MUN, "Fecha" = "Fecha.x", "CVEGEO", "CVE_ENT" = "CVE_ENT.x", "ENT" = "Ent", "CVE_MUN" = "CVE_MUN.x", "MUN","Delito", "Total.Delitos", "POB" = "Pob")
Base_2018_MUN <- select(Base_2018_MUN, "Fecha" = "Fecha.x", "CVEGEO", "CVE_ENT" = "CVE_ENT.x", "ENT" = "Ent", "CVE_MUN" = "CVE_MUN.x", "MUN","Delito", "Total.Delitos", "POB" = "Pob")
Base_2019_MUN <- select(Base_2019_MUN, "Fecha" = "Fecha.x", "CVEGEO", "CVE_ENT" = "CVE_ENT.x", "ENT" = "Ent", "CVE_MUN" = "CVE_MUN.x", "MUN","Delito", "Total.Delitos", "POB" = "Pob")

#Join MUNICIOIO_SEXO
##Se hace el join para la base criminalidad de municipios y shp
#Base_2015_MUN_SEXO <- left_join(shape_mun, DB_CRIMI_2015_MUN, by = "CVEGEO" )
#Base_2016_MUN_SEXO <- left_join(shape_mun, DB_CRIMI_2016_MUN, by = "CVEGEO" )
#Base_2017_MUN_SEXO <- left_join(shape_mun, DB_CRIMI_2017_MUN, by = "CVEGEO" )
#Base_2018_MUN_SEXO <- left_join(shape_mun, DB_CRIMI_2018_MUN, by = "CVEGEO" )
#Base_2019_MUN_SEXO <- left_join(shape_mun, DB_CRIMI_2019_MUN, by = "CVEGEO" )

Base_2015_MUN_SEXO <- left_join(DB_CRIMI_2015_MUN, Pob_2015_SEXO, by = "CVEGEO" )
Base_2016_MUN_SEXO <- left_join(DB_CRIMI_2016_MUN, Pob_2016_SEXO, by = "CVEGEO" )
Base_2017_MUN_SEXO <- left_join(DB_CRIMI_2017_MUN, Pob_2017_SEXO, by = "CVEGEO" )
Base_2018_MUN_SEXO <- left_join(DB_CRIMI_2018_MUN, Pob_2018_SEXO, by = "CVEGEO" )
Base_2019_MUN_SEXO <- left_join(DB_CRIMI_2019_MUN, Pob_2019_SEXO, by = "CVEGEO" )

##Se hace el join para la base criminalidad de municipios y Pob
#Base_2015_MUN_SEXO <- left_join(DB_CRIMI_2015_MUN, Pob_2015_SEXO, by = "CVEGEO" )
#Base_2016_MUN_SEXO <- left_join(DB_CRIMI_2016_MUN, Pob_2016_SEXO, by = "CVEGEO" )
#Base_2017_MUN_SEXO <- left_join(DB_CRIMI_2017_MUN, Pob_2017_SEXO, by = "CVEGEO" )
#Base_2018_MUN_SEXO <- left_join(DB_CRIMI_2018_MUN, Pob_2018_SEXO, by = "CVEGEO" )
#Base_2019_MUN_SEXO <- left_join(DB_CRIMI_2019_MUN, Pob_2019_SEXO, by = "CVEGEO" )

#Se selecciona la información que estan en las base de municipio
Base_2015_MUN_SEXO <- select(Base_2015_MUN_SEXO, "Fecha" = "Fecha.x", "CVEGEO", "CVE_ENT" = "CVE_ENT.x", "ENT" = "Ent", "CVE_MUN" = "CVE_MUN.x", "MUN","Delito", "Total.Delitos", "SEXO", "POB" = "Pob")
Base_2016_MUN_SEXO <- select(Base_2016_MUN_SEXO, "Fecha" = "Fecha.x", "CVEGEO", "CVE_ENT" = "CVE_ENT.x", "ENT" = "Ent", "CVE_MUN" = "CVE_MUN.x", "MUN","Delito", "Total.Delitos", "SEXO", "POB" = "Pob")
Base_2017_MUN_SEXO <- select(Base_2017_MUN_SEXO, "Fecha" = "Fecha.x", "CVEGEO", "CVE_ENT" = "CVE_ENT.x", "ENT" = "Ent", "CVE_MUN" = "CVE_MUN.x", "MUN","Delito", "Total.Delitos", "SEXO", "POB" = "Pob")
Base_2018_MUN_SEXO <- select(Base_2018_MUN_SEXO, "Fecha" = "Fecha.x", "CVEGEO", "CVE_ENT" = "CVE_ENT.x", "ENT" = "Ent", "CVE_MUN" = "CVE_MUN.x", "MUN","Delito", "Total.Delitos", "SEXO", "POB" = "Pob")
Base_2019_MUN_SEXO <- select(Base_2019_MUN_SEXO, "Fecha" = "Fecha.x", "CVEGEO", "CVE_ENT" = "CVE_ENT.x", "ENT" = "Ent", "CVE_MUN" = "CVE_MUN.x", "MUN","Delito", "Total.Delitos", "SEXO", "POB" = "Pob")

##Se unen las bases de datos
Base_ENT <- rbind(Base_2015_ENT, Base_2016_ENT, Base_2017_ENT, Base_2018_ENT, Base_2019_ENT)
Base_ENT_SEXO <- rbind(Base_2015_ENT_SEXO, Base_2016_ENT_SEXO, Base_2017_ENT_SEXO, Base_2018_ENT_SEXO, Base_2019_ENT_SEXO)
Base_MUN <- rbind(Base_2015_MUN, Base_2016_MUN, Base_2017_MUN, Base_2018_MUN, Base_2019_MUN)
Base_MUN_SEXO <- rbind(Base_2016_MUN_SEXO, Base_2017_MUN_SEXO, Base_2018_MUN_SEXO, Base_2019_MUN_SEXO)

rm(Base_2015_ENT, Base_2015_ENT_SEXO, Base_2015_MUN, Base_2015_MUN_SEXO,
   Base_2016_ENT, Base_2016_ENT_SEXO, Base_2016_MUN, Base_2016_MUN_SEXO,
   Base_2017_ENT, Base_2017_ENT_SEXO, Base_2017_MUN, Base_2017_MUN_SEXO,
   Base_2018_ENT, Base_2018_ENT_SEXO, Base_2018_MUN, Base_2018_MUN_SEXO,
   Base_2019_ENT, Base_2019_ENT_SEXO, Base_2019_MUN, Base_2019_MUN_SEXO,
   DB_CRIMI_2015, DB_CRIMI_2015_ENT, DB_CRIMI_2015_MUN, 
   DB_CRIMI_2016, DB_CRIMI_2016_ENT, DB_CRIMI_2016_MUN, 
   DB_CRIMI_2017, DB_CRIMI_2017_ENT, DB_CRIMI_2017_MUN, 
   DB_CRIMI_2018, DB_CRIMI_2018_ENT, DB_CRIMI_2018_MUN,
   DB_CRIMI_2019, DB_CRIMI_2019_ENT, DB_CRIMI_2019_MUN,
   DB_CRIMINALIDAD, Pob, Pob1, Pob1_sex, Pob2, Pob2_sex,
   Pob_2015, Pob_2015_ENT, Pob_2015_ENT_SEXO, Pob_2015_SEXO,
   Pob_2016, Pob_2016_ENT, Pob_2016_ENT_SEXO, Pob_2016_SEXO,
   Pob_2017, Pob_2017_ENT, Pob_2017_ENT_SEXO, Pob_2017_SEXO,
   Pob_2018, Pob_2018_ENT, Pob_2018_ENT_SEXO, Pob_2018_SEXO,
   Pob_2019, Pob_2019_ENT, Pob_2019_ENT_SEXO, Pob_2019_SEXO)

##Tasa de homicidios
#Entidad
Base_ENT_HOM <- filter(Base_ENT, Delito == "Homicidio")

Base_ENT_HOM <- Base_ENT_HOM %>%
  mutate(T.HOM = (Total.Delitos / Total.Pob)*100000 )

write.csv(Base_ENT_HOM, file = "Base_ENT_HOM.csv")

##write.xlsx2( Base_ENT_HOM, file = "Base_ENT_HOM.xlsx", sheetName="Sheet1",
            ##col.names = TRUE, row.names = TRUE, append = FALSE)

#Feminicidios
#Entidad
Base_ENT_SEXO_HOM <- filter(Base_ENT_SEXO, Delito == 'Feminicidio')

Base_ENT_SEXO_HOM <- Base_ENT_SEXO_HOM %>%
  mutate(T.Feminicisio = (Total.Delitos / Total.Pob)*100000 )

write.csv(Base_ENT_SEXO_HOM, file = "Base_ENT_Feminicidio.csv")
