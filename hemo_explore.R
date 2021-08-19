
# paquetes
library(tidyverse)
library(compareGroups)

theme_set(theme_bw())

# leer base actualizada
hem <- readxl::read_excel("data-raw/hemdb-visitas-20210812.xlsx") %>% 
  rename(diagnostico=diagostico) %>% 
  mutate(temperatura_celsius=as.numeric(temperatura_celsius)) %>% 
  mutate(fiebre_2_ultimos_dias=if_else(fiebre_2_ultimos_dias=="sí",
                                       "si",
                                       fiebre_2_ultimos_dias)) %>% 
  mutate(cruces_n=case_when(
    str_detect(diagnostico,pattern = "\\+\\+\\+") ~ 3L,
    str_detect(diagnostico,pattern = "\\+\\+") ~ 2L,
    str_detect(diagnostico,pattern = "\\+") ~ 1L,
    str_detect(diagnostico,pattern = "Neg") ~ NA_integer_,
    TRUE ~ 0L
  )) %>% 
  mutate(cruces_n=as.factor(cruces_n))

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
  # mutate(cruces_n=case_when(
  #   str_detect(diagnostico,pattern = "\\+\\+\\+") ~ 3L,
  #   str_detect(diagnostico,pattern = "\\+\\+") ~ 2L,
  #   str_detect(diagnostico,pattern = "\\+") ~ 1L,
  #   str_detect(diagnostico,pattern = "Neg") ~ NA_integer_,
  #   TRUE ~ 0L
  # )) %>% 
  # tail(25) %>% 
  # select(-n) %>% 
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
         temperatura_celsius:densidad_parasitaria_gametocitos,cruces_n) %>% 
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
         temperatura_celsius:densidad_parasitaria_gametocitos,cruces_n) %>% 
  compareGroups(formula = num.visita ~ .,data = .) %>% 
  createTable(show.all = T, 
              show.n = F, 
              show.p.trend = T, sd.type = 2,
              digits = 1)

# figuras -----------------------------------------------------------------


# temperatura -------------------------------------------------------------


hem %>% 
  filter(group!="control") %>% 
  ggplot(aes(x = num.visita,
             y = temperatura_celsius, 
             group = new.code,
             color = group)) +
  geom_line() +
  facet_wrap(~group)

hem %>% 
  filter(group!="control") %>% 
  ggplot(aes(x = num.visita,
             y = temperatura_celsius, 
             #group = new.code,
             color = group)) +
  geom_violin() +
  ggbeeswarm::geom_quasirandom(method = "tukeyDense",
                               dodge.width = 0.9)


# trofosoitos -------------------------------------------------------------

hem %>% 
  filter(group!="control") %>% 
  ggplot(aes(x = num.visita,
             y = densidad_parasitaria_trofozoitos, 
             group = new.code,
             color = group)) +
  geom_line() +
  facet_wrap(~group)

hem %>% 
  filter(group!="control") %>% 
  ggplot(aes(x = num.visita,
             y = densidad_parasitaria_trofozoitos, 
             #group = new.code,
             color = group)) +
  geom_violin() +
  ggbeeswarm::geom_quasirandom(method = "tukeyDense",
                               dodge.width = 0.9)


# cruces ------------------------------------------------------------------

hem %>% 
  filter(group!="control") %>% 
  mutate(cruces_n=fct_rev(cruces_n)) %>% 
  ggplot(aes(x = num.visita,
             # y = cruces_n, 
             # group = new.code,
             fill = cruces_n)) +
  # geom_bar(position = position_fill()) +
  geom_bar() +
  facet_wrap(~group) +
  colorspace::scale_fill_discrete_sequential(palette = "Light Grays", 
                                             rev=FALSE) +
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

