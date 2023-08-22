####################################
#SE COPIA SCRIPT DE TABLERO https://github.com/dnme-minturdep/turismo_internacional/blob/main/global.R
####################################

library(tidyverse) 
library(glue) 
library(lubridate) 
library(data.table)
library(shiny)
library(plotly)
library(waiter)
library(shinycssloaders)
library (readxl)									
library(herramientas)
#library(comunicacion)

# language en DT::

options(DT.options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))

#color
cols_arg2 <- c("#EE3D8F", # "(rosa)"
               "#50B8B1", # celeste "
               "#F7941E", # naranja 
               "FFD100", #amarillo
               "#D7DF23", #verde amarillo
               "#9283BE") #violeta

#saco notacion cientifica
options(scipen = 999)

# datos turismo internacional ####

datos <- readRDS("/srv/DataDNMYE/turismo_internacional/turismo_internacional_visitantes.rds")

datos <- datos %>%
  rename(year = 'anio', 
         casos = casos_ponderados)  

#datos para graficos####

#data para serie, por pais_agrupado y destino

data_graficos <- datos [tipo_visitante == "Turistas", .(turistas = sum(casos)), 
                        by = .(year, mes, pais_agrupado, destino_agrup, 
                               turismo_internac)] 

#completo meses faltantes.

data_graficos <- data.table(complete (data_graficos, 
                                      expand(data_graficos, year, mes, 
                                             nesting(destino_agrup, 
                                                     pais_agrupado, 
                                                     turismo_internac)),
                                      fill = list(turistas = 0)))

# elimino meses posteriores al ultimo, que se completaron por nesting.

mes_ult_nro <- as_tibble(datos[nrow(datos),mes])
year_ult <- as_tibble(datos[nrow(datos),year])

data_graficos <- data_graficos %>%
  filter ((year < as.numeric(year_ult)) | (year == as.numeric(year_ult) 
                                           & mes <= as.numeric(mes_ult_nro))) %>% 
  mutate (periodo = dmy(as.character(glue("01/{mes}/{year}"))))%>% 
  rename (turismo = turismo_internac)

# acumulado por destino:

data_grafico_ac_pais <- data_graficos %>%
  filter (year == as.numeric(year_ult)) %>% 
  mutate(pais_destino = case_when(turismo == "Receptivo" ~ pais_agrupado,
                                  turismo == "Emisivo" ~ destino_agrup)) %>% 
  group_by(turismo,pais_destino) %>% 
  summarise(turistas = round(sum(turistas))) %>% 
  ungroup() 
# acumulado por via. 

data_grafico_ac_via <- datos %>%
  filter (year == as.numeric(year_ult), tipo_visitante == "Turistas") %>% 
  rename(turismo = turismo_internac) %>% 
  group_by(turismo, via) %>% 
  summarise(turistas = round(sum(casos))) %>% 
  ungroup() 

data_grafico_ac_total <- data_grafico_ac_via %>%
  group_by(turismo) %>% 
  summarise(turistas = round(sum(turistas))) %>% 
  ungroup() %>% 
  mutate(via = "Total")

#agrego total a via. 
data_grafico_ac_via <- bind_rows(data_grafico_ac_via, data_grafico_ac_total)

# datos para tabla ####

#mes de numero a texto.

datos <- datos[, mes := .(fcase(mes == 1 ,"Enero", mes == 2 ,"Febrero", 
                                mes == 3 ,"Marzo", mes == 4 ,"Abril", 
                                mes == 5 ,"Mayo",    mes == 6 ,"Junio",
                                mes == 7 ,"Julio", mes == 8 ,"Agosto",  
                                mes == 9 ,"Septiembre", mes == 10 ,"Octubre",
                                mes == 11 ,"Noviembre", mes == 12 ,"Diciembre"))] 						

Mes_ult <- as_tibble(datos[nrow(datos),2])

#reordeno niveles

datos$mes<- factor(datos$mes, levels = c("Enero",	"Febrero",	"Marzo", "Abril",	
                                         "Mayo",	"Junio",	"Julio",	"Agosto",	
                                         "Septiembre", "Octubre",	"Noviembre",	
                                         "Diciembre"), 
                   ordered = TRUE)	

## RECEPTIVO 

data_receptivo <-  datos[turismo_internac == "Receptivo", ] 
data_receptivo <- data_receptivo[, .(turistas = sum(casos)), 
                                 by = .(year, trim, mes, tipo_visitante, via,
                                        pais_agrupado, pais, 
                                        paso_publ, prov, limita, ruta_natural, 
                                        sexo, grupoetario)] 
#####################################################################################################
#SE GENERA LA DATA PARA INDEC
##############################

data_pais_limitrofes <- data_receptivo %>% 
  filter(year==2017 & tipo_visitante=="Turistas") %>% 
  group_by(year,pais) %>% 
  summarise(turistas=sum(turistas)) %>% 
  filter(pais%in% c("Brasil",
                    "Chile",
                    "Paraguay",
                    "Uruguay",
                    "Bolivia")) %>% 
  #pivot_wider(names_from = via, values_from = turistas) %>% 
  mutate(pais=factor(pais, levels = c("Brasil",
                          "Chile",
                          "Paraguay",
                          "Uruguay",
                          "Bolivia"),
         ordered = TRUE)) %>% 
  arrange(pais)


data_pais_eeuu_canada <- data_receptivo %>% 
  filter(year==2022 & tipo_visitante=="Turistas") %>% 
  group_by(year,pais,via) %>% 
  summarise(turistas=sum(turistas)) %>% 
  filter(pais%in% c("Estados Unidos",
                    "Canadá"
  )) %>% 
  pivot_wider(names_from = via, values_from = turistas) %>% 
  mutate(pais=factor(pais, levels = c("Estados Unidos",
                                      "Canadá"),
                     ordered = TRUE)) %>% 
  arrange(pais)


data_pais_resto_America <- data_receptivo %>% 
  filter(year==2022 & tipo_visitante=="Turistas") %>% 
  group_by(year,pais,via) %>% 
  summarise(turistas=sum(turistas)) %>% 
  filter(pais%in% c("Perú", "Colombia","Venezuela","México","Ecuador")) %>% 
  pivot_wider(names_from = via, values_from = turistas) %>% 
  mutate(pais=factor(pais, levels = c("Perú", "Colombia","Venezuela","México","Ecuador"),
                     ordered = TRUE)) %>% 
  arrange(pais)


data_pais_europa <- data_receptivo %>% 
  filter(year==2022 & tipo_visitante=="Turistas") %>% 
  group_by(year,pais,via) %>% 
  summarise(turistas=sum(turistas)) %>% 
  filter(pais%in% c("España",
                    "Alemania",
                    "Francia",
                    "Reino Unido",
                    "Italia"
  )) %>% 
  pivot_wider(names_from = via, values_from = turistas) %>% 
  mutate(pais=factor(pais, levels = c("España",
                                      "Alemania",
                                      "Francia",
                                      "Reino Unido",
                                      "Italia"),
                     ordered = TRUE)) %>% 
  arrange(pais)


data_pais_resto_mundo <- data_receptivo %>% 
  filter(year==2022 & tipo_visitante=="Turistas") %>% 
  group_by(year,pais,via) %>% 
  summarise(turistas=sum(turistas)) %>% 
  filter(pais%in% c("Australia","Israel","China","Corea del Sur","Japón")) %>% 
  pivot_wider(names_from = via, values_from = turistas) %>% 
  mutate(pais=factor(pais, levels = c("Australia","Israel","China","Corea del Sur","Japón"),
                     ordered = TRUE)) %>% 
  arrange(pais)

################################PARA LA CATEGORIA "OTROS"###################################

paises_a_excluir_resto_mundo <- c("Australia","Israel","China","Corea del Sur","Japón")

OTROS_resto_mundo <- data_receptivo %>% 
  filter(pais_agrupado=="Resto del mundo" & tipo_visitante=="Turistas") %>% 
  subset(!(pais%in%paises_a_excluir_resto_mundo)) %>% 
  group_by(year,via) %>%
  summarise(turistas=sum(turistas)) %>% 
  pivot_wider(names_from = via, values_from = turistas)

paises_a_excluir_europa <- c("España",
                               "Alemania",
                               "Francia",
                               "Reino Unido",
                               "Italia")

OTROS_europa <- data_receptivo %>% 
  filter(pais_agrupado=="Europa" & tipo_visitante=="Turistas") %>% 
  subset(!(pais%in%paises_a_excluir_europa)) %>% 
  group_by(year,via) %>%
  summarise(turistas=sum(turistas))
  pivot_wider(names_from = via, values_from = turistas)

  
  
paises_a_excluir_america <- c("Perú", "Colombia","Venezuela","México","Ecuador")
  
OTROS_america <- data_receptivo %>% 
    filter(pais_agrupado=="Resto de América" & tipo_visitante=="Turistas") %>% 
    subset(!(pais%in%paises_a_excluir_america)) %>% 
    group_by(year,via) %>%
    summarise(turistas=sum(turistas)) %>% 
  pivot_wider(names_from = via, values_from = turistas)


  