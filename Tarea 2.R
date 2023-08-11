#Limpiar el ambiente
gc()
rm(list=ls())

if (!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(tidyverse,
               readxl, 
               chilemapas,
               kableExtra,
               gridExtra
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
  mutate(porcentaje = round(n/sum(n) * 100,1)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = sexo, 
    values_from = c(n, porcentaje),
    names_glue = "{.value}_{sexo}") %>% 
  arrange(anio)  %>% 
  select(anio, mujer ,porcentaje_mujer , hombre ,porcentaje_hombre) %>% 
  replace_na(list(mujer = 0, porcentaje_mujer = 0, hombre = 0, porcentaje_hombre = 0)) %>% 
  kable(format = "html", booktabs = TRUE, 
        col.names = c("Año", "N mujeres","% Mujeres", "N hombres", "% Hombres")) %>%
  kable_styling(full_width = FALSE, latex_options = "striped", font_size = 12) %>%
  save_kable(file = "tabla1b1.html")
 
base %>%
  mutate(anio = year(fecha_notificacion)) %>%
  group_by(anio, edad_cat) %>%
  tally() %>%
  group_by(anio) %>%
  mutate(porcentaje = round(n/sum(n) * 100,1)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = edad_cat, 
    values_from = c(n, porcentaje),
    names_glue = "{.value}_{edad_cat}") %>% 
  arrange(anio)  %>% 
  select("anio", 
         "n_0-4", "porcentaje_0-4", 
         "n_5-9", "porcentaje_5-9",
         "n_10-14", "porcentaje_10-14", 
         "n_15-19", "porcentaje_15-19",
         "n_20-24", "porcentaje_20-24", 
         "n_25-29", "porcentaje_25-29",
         "n_30-34", "porcentaje_30-34",
         "n_35-39", "porcentaje_35-39",
         "n_40-44", "porcentaje_40-44",
         "n_45-49", "porcentaje_45-49",
         "n_50-54", "porcentaje_50-54",
         "n_55-59", "porcentaje_55-59",
         "n_60-64", "porcentaje_60-64",
         "n_65-69", "porcentaje_65-69",
         "n_70-74", "porcentaje_70-74",
         "n_75-79", "porcentaje_75-79",
         "n_+80", "porcentaje_+80") %>% 
  mutate(across(starts_with(c("n_", "porcentaje_")), ~ifelse(is.na(.), 0, .))) %>% 
  kable(format = "html", booktabs = TRUE,
        col.names = c("anio", 
                      "0-4", "%",
                      "5-9","%",
                      "10-14", "%",
                      "15-19", "%",
                      "20-24", "%",
                      "25-29", "%",
                      "30-34", "%",
                      "35-39", "%",
                      "40-44", "%",
                      "45-49", "%",
                      "50-54", "%",
                      "55-59", "%",
                      "60-64", "%",
                      "65-69", "%",
                      "70-74", "%",
                      "75-79", "%",
                      "+80", "%")) %>%
  kable_styling(full_width = FALSE, latex_options = "striped", font_size = 12) %>%
  save_kable(file = "tabla1b2.html")
 
#1c
base %>%
   mutate(quin = cut(year(fecha_notificacion),c(1995,1999,2004,2009,2014,2022),include.lowest = T))  %>%
   group_by(region_residencia,quin) %>%
   count() %>%
   arrange(-n) 

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
   summarize(regiomax = region_residencia[which.max(n)],
             max_cases = max(n)) %>%
   ungroup()

 base %>%
   mutate(quin = cut(year(fecha_notificacion),c(1995,1999,2004,2009,2014,2022),include.lowest = T))  %>%
   group_by(quin, region_residencia) %>%
   count() %>%
   arrange(region_residencia) %>%
   filter(region_residencia == "Region de Los Lagos") %>% 
   mutate(quin= gsub("\\[|\\]","" ,quin) %>%
              gsub("\\(", "" ,.) %>%
              gsub("\\)", "" ,.) %>%
              gsub(",", "-" ,.)) %>% 
   kable(format = "html", booktabs = TRUE, 
         col.names = c("Período", "Región","N")) %>%
   kable_styling(full_width = FALSE, latex_options = "striped", font_size = 12) %>%
   save_kable(file = "tabla1c1.html")
 
 
 
 #comunas

 base %>%
   filter(region_residencia == "Region de Los Lagos") %>%
   mutate(comuna =  tolower(iconv(comuna_residencia, from = 'UTF-8', to = 'ASCII//TRANSLIT'))) %>% 
   group_by(comuna) %>%
   count() %>%
   arrange(-n) %>% 
   head(10) %>% 
   kable(format = "html", booktabs = TRUE, 
         col.names = c("Comuna","N")) %>%
   kable_styling(full_width = FALSE, latex_options = "striped", font_size = 12) %>%
   save_kable(file = "tabla1c2.html")
 
 
 #1d
 
base<- base %>%
   mutate(difdias= as.numeric(difftime(fecha_notificacion, fecha_primeros_sintomas, units = "days"))) 


base %>% 
   summarize(
     mean = round(mean(difdias, na.rm = TRUE),2),
     median = median(difdias, na.rm = TRUE),
     sd = round(sd(difdias, na.rm = TRUE),2),
     q1 = quantile(difdias, 0.25, na.rm = TRUE),
     q3 = quantile(difdias, 0.75, na.rm = TRUE),
     min = min(difdias, na.rm = TRUE),
     max = max(difdias, na.rm = TRUE)
   ) %>% 
  kable(format = "html", booktabs = TRUE,
        col.names = c("Media","Mediana","SD","Q1","Q2","Mínimo","Máximo")) %>%
  kable_styling(full_width = FALSE, latex_options = "striped", font_size = 12) %>%
  save_kable(file = "tabla1d1.html")


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

min_var <- min(mapa$n, na.rm = TRUE)
max_var <- max(mapa$n, na.rm = TRUE)

paleta <- c("darkred", "white")
graficos <- list() # Inicializa una lista vacía para almacenar los gráficos


for (i in seq_along(levels(mapa$quin))) {
  periodo <- gsub("\\[|\\]","" ,levels(mapa$quin)[i]) %>%
    gsub("\\(", "" ,.) %>%
    gsub("\\)", "" ,.) %>%
    gsub(",", "-" ,.)
  
  g <- ggplot(mapa[mapa$quin == levels(mapa$quin)[i],]) + 
    geom_sf(aes(fill = n, geometry = geometry)) +
    xlim(95,66) +
    theme_minimal(base_size = 13)+
    geom_sf_label(aes(label = paste0(gsub("Region de ", "", region_residencia), ": ", n),
                      geometry = geometry),
                  hjust=1.22, 
                  size = 2.5, 
                  nudge_x = 0.5, 
                  nudge_y = 0.5) +
    scale_fill_gradientn(colours = rev(paleta), name = "N", limits = c(min_var, max_var)) +
    labs(title=periodo) +
    ylab(NULL)+ xlab(NULL) +
    theme(legend.position = "bottom")
  
  graficos[[i]] <- g
}

grid.arrange(grobs = graficos, ncol = 3, top = "Casos de antavirus por región según años")

