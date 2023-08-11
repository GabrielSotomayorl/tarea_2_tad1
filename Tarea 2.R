#Limpiar el ambiente
gc()
rm(list=ls())

if (!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(tidyverse,
               readxl, 
               chilemapas
               )

#1a
base <- read_excel("Hantavirus_chile.xlsx")

nrow(base)
base %>% 
  filter(duplicated(base)==T)
colnames(base)
#1b

 base %>%
  mutate(anio = year(fecha_notificacion)) %>%
  group_by(anio, sexo) %>%
  tally() %>%
  group_by(anio) %>%
  mutate(percentage = n/sum(n) * 100)
 
 base %>%
   mutate(anio = year(fecha_notificacion)) %>%
   group_by(anio, edad_cat) %>%
   tally() %>%
   group_by(anio) %>%
   mutate(percentage = n/sum(n) * 100)
 
#1c
base %>%
   mutate(quin = cut(year(fecha_notificacion),c(1995,1999,2004,2009,2014,2022),include.lowest = T))  %>%
   group_by(region_residencia) %>%
   count() %>%
   arrange(-n) 

 base %>%
   mutate(quin = cut(year(fecha_notificacion),c(1995,1999,2004,2009,2014,2022),include.lowest = T)) %>%
   group_by(quin, region_residencia) %>%
   count() %>%
   group_by(quin) %>%
   summarize(region_max = region_residencia[which.max(n)],
             max_cases = max(n)) %>%
   ungroup()

 base %>%
   mutate(quin = cut(year(fecha_notificacion),c(1995,1999,2004,2009,2014,2022),include.lowest = T))  %>%
   group_by(quin, region_residencia) %>%
   count() %>%
   arrange(region_residencia) %>%
   filter(region_residencia == "Region de Los Lagos") 
 
 #comunas
 base %>%
   filter(region_residencia == "Region de Los Lagos") %>%
   group_by(comuna_residencia) %>%
   count() %>%
   arrange(-n)
 
 
 #1d
 
base<- base %>%
   mutate(difdias= as.numeric(difftime(fecha_notificacion, fecha_primeros_sintomas, units = "days"))) 


base %>% 
   summarize(
     mean = mean(difdias, na.rm = TRUE),
     median = median(difdias, na.rm = TRUE),
     sd = sd(difdias, na.rm = TRUE),
     q1 = quantile(difdias, 0.25, na.rm = TRUE),
     q3 = quantile(difdias, 0.75, na.rm = TRUE),
     min = min(difdias, na.rm = TRUE),
     max = max(difdias, na.rm = TRUE)
   )
 
ggplot(base, aes(x = difdias)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Days between First Symptoms and Notification",
       x = "Number of Days",
       y = "Frequency") +
  theme_minimal()



#Bonus 

mapa_reg<-mapa_comunas %>% 
  generar_regiones() %>% 
  mutate(codigo_region = as.numeric(codigo_region))

basem<-base %>%
  mutate(quin = cut(year(fecha_notificacion),c(1995,2004,2014,2022),include.lowest = T))  %>%
  group_by(quin,region_residencia) %>%
  count() %>% 
  ungroup() %>%
  complete(quin, region_residencia, fill = list(n = 0)) %>% 
  arrange(region_residencia) %>% 
  mutate(codigo_region = case_when(
    region_residencia == "DESCONOCIDO" ~ NA_integer_,
    region_residencia == "Region Metropolitana de Santiago" ~ 13,
    region_residencia == "Region de Antofagasta" ~ 2,
    region_residencia == "Region de Atacama" ~ 3,
    region_residencia == "Region de Aysen del General Carlos Ibanez del Campo" ~ 11,
    region_residencia == "Region de Coquimbo" ~ 4,
    region_residencia == "Region de Los Lagos" ~ 10,
    region_residencia == "Region de Los Rios" ~ 14,
    region_residencia == "Region de Magallanes y la Antartica Chilena" ~ 12,
    region_residencia == "Region de Valparaiso" ~ 5,
    region_residencia == "Region de la Araucania" ~ 9,
    region_residencia == "Region del Biobio" ~ 8,
    region_residencia == "Region del Libertador General Bernardo OHiggins" ~ 6,
    region_residencia == "Region del Maule" ~ 7,
    region_residencia == "Region del Nuble" ~ 16
  )) 


rtar<-data.frame(basem$quin[1:3],"Region de Tarapaca",0, 1)
rari<-data.frame(basem$quin[1:3],"Region de Arica y Parinacota",0, 15)
colnames(rtar)<-colnames(basem)
colnames(rari)<-colnames(basem)
basem<- bind_rows(basem, rtar,rari)


mapa<-left_join(mapa_reg, basem, by = "codigo_region")

paleta <- c("darkred", "white")
for (i in levels(mapa$quin)) {
  periodo <- gsub("\\[|\\]","" ,i) %>%   # Elimina los paréntesis cuadrados
    gsub("\\(", "" ,.) %>%  # Elimina el paréntesis abierto
    gsub("\\)", "" ,.) %>%  # Elimina el paréntesis cerrado
    gsub(",", "-" ,.) # Reemplaza la coma por un guion
  
  g <- ggplot(mapa[mapa$quin == i,]) + 
    geom_sf(aes(fill = n, geometry = geometry)) +
    xlim(95,66) +
    theme_minimal(base_size = 13)+
    geom_sf_label(aes(label = paste0(gsub("Region de ", "", region_residencia), ": ", n),
                      geometry = geometry),
                  hjust=1.22, 
                  size = 2.5, 
                  nudge_x = 0.5, 
                  nudge_y = 0.5) +
    scale_fill_gradientn(colours = rev(paleta), name = "número de casos") +
    labs(title=paste0("Casos de antavirus por región en
    el periodo ", periodo), caption= "Fuente: Elaboración propia a partir de registros 
       administrativos del Ministerio de Salud.")
  print(g)
}

