library(readxl)
library(tidyverse)
base <- read_excel("Hantavirus_chile.xlsx")

nrow(base)
nrow(distinct(base))

base %>% 
  filter(duplicated(base)==T)


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
 


 base %>%
   mutate(quin = cut(year(fecha_notificacion),c(1995,1999,2004,2009,2014,2022)))  %>%
   group_by(quin, region_residencia) %>%
   tally() %>%
   arrange(region_residencia) %>% 
   print(n =1000)
 