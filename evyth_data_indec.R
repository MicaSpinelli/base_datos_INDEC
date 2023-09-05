library(tidyverse)
library(herramientas)
library(evyth)

#                  Se crean script para info de anuarios INDEC                   ~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


serie_evyth_indec <- b_evyth %>% 
  filter(anio != 2011) %>% 
  filter(arg_o_ext == 1) %>% 
  mutate(pondera = ifelse(is.na(pondera),w_adelanto,pondera),
         mes = as.numeric(substr(Mes,5,6))) %>% 
  left_join(ipc_empalme, by = c("mes","anio")) %>% 
  group_by(anio,trimestre, tipo_visitante) %>% 
  summarise(gasto_nominal = sum(pondera * gasto_pc,na.rm=T),
            pernoct = sum(pondera*px07,na.rm=T),
            cantidad= sum(pondera,na.rm=T)) %>% 
  crear_etiqueta("tipo_visitante") %>% 
  pivot_wider(names_from = tipo_visitante, values_from = c(3:5))


write_file_srv(serie_evyth_indec,"/srv/DataDNMYE/economia2/bases_para_indec/serie_evyth_INDEC.csv")

b_evyth_indec <- read_file_srv("/srv/DataDNMYE/evyth/base_trabajo/evyth_base_de_trabajo.parquet")

serie_evyth_indec_apertura <- b_evyth_indec %>% 
  select(anio, arg_o_ext, tipo_visitante, w_adelanto,pondera, region_origen, region_destino, px10_1, px08_agrup, px09, gasto_pc,px07) %>% 
  filter(anio >= 2014) %>% 
  filter(arg_o_ext == 1, tipo_visitante == 1) %>% 
  crear_etiqueta(c("region_origen", "region_destino", "px10_1", "px08_agrup", "px09"))


total_tur_anio <- serie_evyth_indec_apertura %>% 
  group_by(anio) %>% 
  summarise(gasto_nominal = sum(pondera * gasto_pc,na.rm=T),
            pernoct = sum(pondera*px07,na.rm=T),
            tur= sum(pondera,na.rm=T))

write_file_srv(total_tur_anio,"/srv/DataDNMYE/economia2/bases_para_indec/total_tur_anio.csv")

region_origen <- serie_evyth_indec_apertura %>% 
  group_by(anio, region_origen) %>% 
  summarise(gasto_nominal = sum(pondera * gasto_pc,na.rm=T),
            pernoct = sum(pondera*px07,na.rm=T),
            tur= sum(pondera,na.rm=T)) %>% 
  mutate(part_tur=(tur/sum(tur))*100,
         part_gasto=(gasto_nominal/sum(gasto_nominal))*100,
         part_pernoct=(pernoct/sum(pernoct))*100) %>% 
  select(anio, region_origen, part_tur, part_gasto, part_pernoct) %>% 
  filter(anio==2022)

region_destino <- serie_evyth_indec_apertura %>% 
  group_by(anio, region_destino) %>% 
  summarise(gasto_nominal = sum(pondera * gasto_pc,na.rm=T),
            pernoct = sum(pondera*px07,na.rm=T),
            tur= sum(pondera,na.rm=T)) %>% 
  mutate(part_tur=(tur/sum(tur))*100,
         part_gasto=(gasto_nominal/sum(gasto_nominal))*100,
         part_pernoct=(pernoct/sum(pernoct))*100) %>% 
  select(anio, region_destino, part_tur, part_gasto, part_pernoct) %>% 
  filter(anio==2022)

motivo <- serie_evyth_indec_apertura %>% 
  group_by(anio,px10_1) %>% 
  summarise(gasto_nominal = sum(pondera * gasto_pc,na.rm=T),
            pernoct = sum(pondera*px07,na.rm=T),
            tur= sum(pondera,na.rm=T)) %>% 
  mutate(part_tur=(tur/sum(tur))*100,
         part_pernoct=(pernoct/sum(pernoct))*100,
         part_gasto=(gasto_nominal/sum(gasto_nominal))*100)%>% 
  select(anio, px10_1, part_tur, part_pernoct,part_gasto) %>% 
  filter(anio==2022)

transporte <- serie_evyth_indec_apertura %>% 
  group_by(anio,px09) %>% 
  summarise(gasto_nominal = sum(pondera * gasto_pc,na.rm=T),
            pernoct = sum(pondera*px07,na.rm=T),
            tur= sum(pondera,na.rm=T)) %>% 
  mutate(part_tur=(tur/sum(tur))*100,
         part_pernoct=(pernoct/sum(pernoct))*100,
         part_gasto=(gasto_nominal/sum(gasto_nominal))*100)%>% 
  select(anio, px09, part_tur, part_pernoct,part_gasto) %>% 
  filter(anio==2022)

alojamiento <- serie_evyth_indec_apertura %>% 
  group_by(anio,px08_agrup) %>% 
  summarise(gasto_nominal = sum(pondera * gasto_pc,na.rm=T),
            pernoct = sum(pondera*px07,na.rm=T),
            tur= sum(pondera,na.rm=T)) %>% 
  mutate(part_tur=(tur/sum(tur))*100,
         part_pernoct=(pernoct/sum(pernoct))*100,
         part_gasto=(gasto_nominal/sum(gasto_nominal))*100)%>% 
  select(anio, px08_agrup, part_tur, part_pernoct,part_gasto) %>% 
  filter(anio==2022)

