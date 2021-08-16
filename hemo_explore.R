
# paquetes
library(tidyverse)
library(compareGroups)

# leer base actualizada
hem <- readxl::read_excel("data-raw/hemdb-visitas-20210812.xlsx") %>% 
  rename(diagnostico=diagostico) %>% 
  mutate(temperatura_celsius=as.numeric(temperatura_celsius)) %>% 
  mutate(fiebre_2_ultimos_dias=if_else(fiebre_2_ultimos_dias=="sí",
                                       "si",
                                       fiebre_2_ultimos_dias))

# visualizacion rapida de variables
hem %>% glimpse()

# ver patron de valores perdidos
hem %>% naniar::vis_miss()

# solo hay datos adicionales para positivos
hem %>% 
  # count(group)
  filter(!is.na(temperatura_celsius)) %>% 
  count(group)

# ¿como categorizar diagnostico de parasitemia?
hem %>% 
  filter(!is.na(temperatura_celsius)) %>% 
  count(diagnostico,sort = T) %>% 
  epihelper::print_inf()

hem %>% 
  filter(!is.na(temperatura_celsius)) %>% 
  mutate(diagnostico=fct_infreq(diagnostico)) %>% 
  janitor::tabyl(diagnostico,group) %>% 
  epihelper::adorn_ame(denominator = "row")

# tablas descriptivas -----------------------------------------------------

# __ vivax ----------------------------------------------------------------

hem %>% 
  filter(!is.na(temperatura_celsius)) %>% 
  filter(group=="pviv") %>% 
  select(group,num.visita,
         temperatura_celsius:densidad_parasitaria_gametocitos) %>% 
  compareGroups(formula = num.visita ~ .,data = .) %>% 
  createTable(show.all = T, 
              show.n = F, 
              show.p.trend = T, sd.type = 2,
              digits = 1)

# __ falciparum -----------------------------------------------------------

hem %>% 
  filter(!is.na(temperatura_celsius)) %>% 
  filter(group=="pfal") %>% 
  select(group,num.visita,
         temperatura_celsius:densidad_parasitaria_gametocitos) %>% 
  compareGroups(formula = num.visita ~ .,data = .) %>% 
  createTable(show.all = T, 
              show.n = F, 
              show.p.trend = T, sd.type = 2,
              digits = 1)
