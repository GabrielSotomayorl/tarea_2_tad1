# Tarea 2

#Primero, debemos limpiar el ambiente:

gc()
rm(list=ls())

if (!require(pacman)) install.packages("pacman")

# Instalamos los paquetes que usaremos en la tarea:

pacman::p_load(tidyverse,
               readxl, 
               chilemapas,
               kableExtra,
               gridExtra,
               dplyr,
               ggplot2,
               WDI,
               patchwork
               )
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Ejercicio 1

# 1.A) Importe su tabla de datos e indique cuántas infecciones por Hantavirus se
# han registrado a la fecha. Explique cuál es la unidad de análisis de estos 
# datos ¿La tabla está compuesta por personas únicas o hay duplicados? 
# Explique que sería un duplicado en este caso.

# Importamos la tabla de datos con la que vamos a trabajar:
base <- read_excel("Hantavirus_chile.xlsx")

# El número de infecciones por hantavirus registrada se calcula contando el numero
# de filas que tenemos en la base de datos:

nrow(base)

# Al revisar la base de datos podemos notar que no existe una clave principal
# que identifique claramente a cada individuo diagnosticado con Hantavirus, por
# lo tanto debemos considerar la fila completa como clave primaria para poder
# considerar cada caso como un contagio.

# Ahora, para revisar si existen duplicados, contaremos el número de filas pero 
# tomando en cuenta solo valores únicos con el siguiente comando:

n_distinct(base)

# Podemos notar que existiría un dato duplicado. Para saber cuál es, debemos
# realizar el siguiente código:

base %>% 
  filter(duplicated(base)==T) %>% 
  View()

# Como podemos confirmar, la tabla tiene dos observaciones iguales. Este valor podría 
# ser considerado como duplicado debido a que la probabilidad
# de que 2 individuos cuenten con la misma información es muy baja.

#-------------------------------------------------------------------------------------------------------------------------------

# 1.B) Construya dos tablas con los porcentajes de 1) infecciones de hantavirus por 
# año desagregado por sexo y 2) infecciones de hantavirus por año desagregado por
# grupo etarios2. ¿Qué le podría comentar al Ministerio de Salud respecto a sus 
# resultados?

# Para la primera tabla tomaremos la base de datos y agruparemos por año (tomado de la fecha
# de notificación) y el sexo, para contabilizar el número de contagios con la columna diagnóstico
# y aplicamos un calcula para convertir este último también en porcentaje.
# Luego pasaremos la tabla a formato ancho para facilitar su  visualización y la exportaremos como html

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
  select(anio, n_mujer ,porcentaje_mujer , n_hombre ,porcentaje_hombre) %>% 
  replace_na(list(n_mujer = 0, porcentaje_mujer = 0, n_hombre = 0, porcentaje_hombre = 0)) %>% 
  kable(format = "html", booktabs = TRUE, 
        col.names = c("Año", "N mujeres","% Mujeres", "N hombres", "% Hombres")) %>%
  kable_styling(full_width = FALSE, latex_options = "striped", font_size = 12) %>%
  save_kable(file = "tabla1b1.html")

# Además, aplicaremos visualización con un gráfico de líneas para identificar la cantidad de casos 
# por género que hubo durante los periodos señalados. Para ello usaremos el paquete ggplot.

G1 <- ggplot(base %>%
               group_by(Año=year(fecha_notificacion), Sexo=sexo) %>%
               summarize(N = length(diagnostico)) %>%
               mutate("%" = (N / sum(N)) * 100)
             , aes(x = Año, y = N, color=Sexo)) +
  geom_line() +
  geom_point() +
  labs(title = paste("Infecciones de hantavirus por año desagregado por sexo"),
       x = "Año",
       y = "N") +
  scale_color_manual(values = c("hombre"="blue", "mujer"="red"))

# Visualizamos el gráfico 1:

print(G1)

# Para la segunda tabla, haremos un proceso similar en donde tomaremos la base de datos,
# agruparemos por año y grupo etario, contabilizaremos los casos según el número de 
# observaciones en la columna diagnóstico y también aplicaremos un comando para generar
# el porcentaje de esa cifra.
# Luego pasaremos la tabla a formato ancho para facilitar su  visualización y la exportaremos como html



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
        col.names = c("Año", 
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

#creamos versión resumida de la varaible de tramos para facilitar la presentación de la tabla.
base <- base %>%
  mutate(edad_cat2 = case_when(
    edad_cat %in% c("0-4", "5-9") ~ "0-9",
    edad_cat %in% c("10-14", "15-19") ~ "10-19",
    edad_cat %in% c("20-24", "25-29", "30-34", "35-39") ~ "20-39",
    edad_cat %in% c("40-44", "45-49", "50-54", "55-59") ~ "40-59",
    TRUE ~ "60+"
  ))

base %>%
  mutate(anio = year(fecha_notificacion)) %>%
  group_by(anio, edad_cat2) %>%
  tally() %>%
  group_by(anio) %>%
  mutate(porcentaje = round(n/sum(n) * 100,1)) %>%
  ungroup() %>%
  pivot_wider(
    names_from = edad_cat2, 
    values_from = c(n, porcentaje),
    names_glue = "{.value}_{edad_cat2}") %>% 
  arrange(anio)  %>% 
  select("anio", 
         "n_0-9", "porcentaje_0-9",
         "n_10-19", "porcentaje_10-19",
         "n_20-39", "porcentaje_20-39",
         "n_40-59", "porcentaje_40-59",
         "n_60+", "porcentaje_60+") %>% 
  mutate(across(starts_with(c("n_", "porcentaje_")), ~ifelse(is.na(.), 0, .))) %>% 
  kable(format = "html", booktabs = TRUE,
        col.names = c("Año", 
                      "0-9", "%",
                      "10-19","%",
                      "20-39", "%",
                      "40-59", "%",
                      "60+", "%")) %>%
  kable_styling(full_width = FALSE, latex_options = "striped", font_size = 12) %>%
  save_kable(file = "tabla1b2.2.html")


# Diseñaremos también un gráfico de líneas para poder visualizar mejor el comportamiento de 
# la variable en los rangos etarios y cómo les afecta el virus. Asignamos un color a cada 
# rango.

G2 <- ggplot(base %>%
               group_by(Año=year(fecha_notificacion), edad_cat) %>%
               summarize(N= length(diagnostico)) %>%
               mutate("%" = (N / sum(N)) * 100), aes(x = Año, y = N, color=edad_cat)) +
  geom_line() +
  geom_point() +
  labs(title = paste("Infecciones de hantavirus por año desagregado por grupo etario"),
       x = "Año",
       y = "N")+   
  scale_color_manual(values = c("+80"="blue", "0-4"="red", "10-14"="yellow", 
                                "15-19"="black", "20-24"="pink", "25-29"="green", "30-34"="brown", 
                                "35-39"="purple", "40-44"="magenta", "45-49"="lightblue", "50-54"="grey", 
                                "55-59"="darkblue", "5-9"="darkred", "60-64"="darkgreen", "65-69"="darkorange",
                                "70-74"="darkmagenta", "75-79"="aquamarine4"))

# Visualizamos el gráfico 2:

print(G2)



#--------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1.C)

# Para generar una tabla de casos por región en periodos agrupados en los periodos señalados
# primero crearemos los tramos señalados 
base<- base %>%
  mutate(quin = cut(year(fecha_notificacion),c(1995,1999,2004,2009,2014,2022),include.lowest = T)) 
base$quin<-factor(base$quin, levels(base$quin), labels = c("1995-1999" ,
                                                           "2000-2004", "2005-2009" 
                                                           ,"2010-2014", "2015-2022"))

base %>%
  group_by(region_residencia,quin) %>%
  count() %>%
  arrange(region_residencia) %>% 
  kable(format = "html", booktabs = TRUE, 
        col.names = c("Región", "Periodo","N")) %>%
  kable_styling(full_width = FALSE, latex_options = "striped", font_size = 12) %>%
  save_kable(file = "tabla1c1.html")


# Ahora, para poder identificar cual es la región con la mayor cantidad de casos durante estos periodos,
# debemos sumar la recurrencia de aquellas zonas, pero sin separar por años y ordenar de manera descendiente 
# para encontrar rápidamente la región con más casos. Tal como se ve a continuación:

base %>%
  mutate(quin = cut(year(fecha_notificacion),c(1995,1999,2004,2009,2014,2022),include.lowest = T))  %>%
  group_by(region_residencia) %>%
  count() %>%
  arrange(-n) %>% 
  kable(format = "html", booktabs = TRUE, 
        col.names = c("Región", "N")) %>%
  kable_styling(full_width = FALSE, latex_options = "striped", font_size = 12) %>%
  save_kable(file = "tabla1c2.html")


base %>%
  mutate(quin = cut(year(fecha_notificacion),c(1995,1999,2004,2009,2014,2022),include.lowest = T)) %>%
  group_by(quin, region_residencia) %>%
  count() %>%
  group_by(quin) %>%
  summarize(regiomax = region_residencia[which.max(n)],
            max_cases = max(n)) %>%
  ungroup()

base %>%
  group_by(quin, region_residencia) %>%
  count() %>%
  arrange(region_residencia) %>%
  filter(region_residencia == "Region de Los Lagos") %>% 
  kable(format = "html", booktabs = TRUE, 
        col.names = c("Período", "Región","N")) %>%
  kable_styling(full_width = FALSE, latex_options = "striped", font_size = 12) %>%
  save_kable(file = "tabla1c3.html")


# Al hacer un zoom en la Región de Los Lagos, con mayor cantidad de casos. Para esto 
# debemos filtrar solo los casos que corresponden a la regiuón de los lagos, 
# y luego agruparlos por comuna. La variable comuna tiene problemas de registro
# al tener las mismas comunas con distintas mayusuculas/minusculas. Homogenizamos esto para que se 
# agrupen ocrrectamente yu seleccionamos las 10 comunas con más casos. Luego exportamos en 
# formato html

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
  save_kable(file = "tabla1c4.html")


#--------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1.D) 
# Primero agregaremos la nueva columna que señalara la diferencia de días que existe desde que
# se comienza con los síntomas hasta que la entidad médica es notificada del contagio.

base<- base %>%
  mutate(difdias= as.numeric(difftime(fecha_notificacion,fecha_primeros_sintomas, units = "days"))) %>%
  arrange(desc(difdias))

# Luego, realizamos un análisis descriptivo de la misma para poder comprender mejor el 
# comportamiento de esta nueva variable.

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
        col.names = c("Media","Mediana","SD","Q1","Q3","Mínimo","Máximo")) %>%
  kable_styling(full_width = FALSE, latex_options = "striped", font_size = 12) %>%
  save_kable(file = "tabla1d1.html")

# Luego graficamos para poder tener una mejor visualización de la variable y su
# comportamiento: 

p1 <- ggplot(base, aes(x = difdias)) +
  geom_density(fill = "blue", alpha = 0.5) +
  xlab("Diferencia de días") +
  ggtitle("Gráfico de densidad de la diferencia de días")

p2 <- ggplot(base[scale(base$difdias)<1&scale(base$difdias)>-1,], aes(x = difdias)) +
  geom_density(fill = "blue", alpha = 0.5) +
  xlab("Diferencia de días") +
  ggtitle("Gráfico de densidad de la diferencia de días (sin atipicos)")

grid.arrange(p1, p2, ncol = 1)

base %>%
  mutate(scaled_difdias = scale(difdias)) %>%
  filter(scaled_difdias > 1 | difdias < 0) %>%
  select(fecha_notificacion, fecha_primeros_sintomas, difdias) %>%
  arrange(difdias) %>%
  filter(difdias > 250|difdias< -250) %>% 
   kable(format = "html", booktabs = TRUE) %>%
  kable_styling(full_width = FALSE, latex_options = "striped", font_size = 12) %>%
  save_kable(file = "tabla1d2.html")

#-----------------------------------------------------------------------------

# Ejercicio 2

gdp <- WDI(
  country = "all",
  indicator = "NY.GDP.PCAP.PP.KD",
  start = 2015,
  end = 2020,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "es") %>%
  filter(region=="Latin America & Caribbean" & year==2020)

print(gdp)

glimpse(gdp)

mean(gdp$NY.GDP.PCAP.PP.KD, na.rm = TRUE)

muestreo <- function(v, m, n, replace = TRUE) {
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  vector_promedios <- c()
  for(i in 1:m) {
    muestra <- sample(x = v,
                      size = n,
                      replace = TRUE)
    vector_promedios[i] <- mean(muestra, na.rm=TRUE)
  }
  promedio <- mean(vector_promedios, na.rm=TRUE)
  g1 <- ggplot(data = NULL, aes(x = 1:m, y = vector_promedios)) +
    geom_point(color = "darkblue") + geom_line(color = "blue") +
    geom_hline(yintercept = mean(v, na.rm=TRUE), color = "red", lwd = 1) + #Promedio real
    geom_hline(yintercept = promedio, color = "green", lwd = 1) #Gran media
  g2 <- ggplot(data = NULL, aes(x = vector_promedios)) +
    geom_histogram(bins = 100, fill="darkblue") +
    geom_vline(xintercept = mean(v, na.rm=TRUE), color = "red", lwd = 1) +
    geom_vline(xintercept = promedio, color = "green", lwd = 1)
  grafico <- g1/g2
  print(paste0("Promedios de ", m, " muetras de tamaño ", n, ":"))
  print(vector_promedios)
  print(paste0("Promedio de promedios (Gran media): ", promedio))
  print(paste0("Promedio 'verdadero': ", mean(v, na.rm=TRUE)))
  print(grafico)
}

muestreo(v=gdp$NY.GDP.PCAP.PP.KD, m=200, n=5, replace = TRUE)

#-------------------------------------------------------------------------
# Bonus 

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
  periodo <- c("1995-2004","2005-2014","2015-2022")[i]
  
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
