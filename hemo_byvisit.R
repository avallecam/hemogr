
# objetivo ----------------------------------------------------------------

#' explorar distribucion
#' generar tablas epi por grupo (ctr,viv,fal)
#' estimar diferencia promedio entre controles y casos en la primera visita
#' 
#' referencia bibliografica principal
#' vittingoff - capitulo 7 - repeated measurements
#' modelo tabla 7.16
#' 
#' recomendacion
#' set.seed(33)

# packages ------------------------------------------------------------------

library(readxl)
library(tidyverse)
library(zoo)
library(lubridate)
library(compareGroups)
library(broom)
library(rlang)
library(geepack)
library(geeM)
# library(avallecam)
if(!require(pacman)) install.packages("pacman")
pacman::p_load_gh('avallecam/epihelper')

rm(list = ls())
theme_set(theme_bw())


# 00_ IMPORTAR ------------------------------------------------------------

hem <- read_rds("data/hemdb.rds")

#hem %>% visdat::vis_dat()
hem %>% glimpse()
hem %>% count(group)
hem %>% count(new.code,group,sort = T) %>% count(group,n) #visitas para casos

# CORE: tiempo al momento de visita -----------------------------------------
#fuente: https://stackoverflow.com/questions/32312925/time-difference-in-years-with-lubridate
#library(lubridate)

hem %>% 
  # group_by(new.code) %>% 
  # #mutate(diff_fecha=fecha - lag(fecha)) %>% 
  # mutate(diff_fecha= interval(min(fecha), fecha)/days(1) ) %>% 
  # ungroup() %>% 
  filter(group=="pviv") %>% 
  select(new.code,num.visita,fecha,diff_fecha) %>% 
  group_by(num.visita) %>% 
  skimr::skim(diff_fecha)

#' las visitas no estan equitativamente separadas en tiempo
#' 0, 7, 28

# outcome distribution ----------------------------------------------------

hem %>% 
  select(hto.:plaqueta) %>% 
  skimr::skim()

#' abaston, basofil, monocit
#' may require negative binomial dsitribution
hem %>% count(abaston.)

# trend plot -------------------------------------------------------------

# __ persona-fecha_visita ------------------------------------------------
hem %>% 
  filter(group!="control") %>% 
  select(-fecha,-num.visita, -edad, -sexo) %>% 
  gather(key,value,-new.code,-group, -diff_fecha) %>% 
  mutate(value=as.numeric(value)) %>% 
  ggplot(aes(diff_fecha,value)) +
  #geom_line() +
  geom_point(aes(colour=group),alpha=0.5
             #position = position_jitter()
             ) +
  #geom_boxplot(aes(group=diff_fecha),alpha=0) +
  facet_wrap(~key,scales = "free_y")

# differences -------------------------------------------------------------

hem_diff_plot <- hem %>% 
  filter(group!="control") %>% 
  select(-edad, -sexo, -fecha,-diff_fecha) %>% 
  pivot_longer(cols = -c(new.code,group,num.visita),
               names_to="outcome",
               values_to="value") %>% 
  pivot_wider(names_from = num.visita,
              values_from = value) %>% 
  janitor::clean_names() %>% 
  rowwise() %>% 
  mutate(diff_2_1=sum(c(x2,-x1),na.rm = T)) %>% 
  mutate(diff_3_2=sum(c(x3,-x2),na.rm = T)) %>% 
  mutate(diff_3_1=sum(c(x3,-x1),na.rm = T)) %>% 
  ungroup() %>% 
  select(-x1,-x2,-x3) %>% 
  pivot_longer(cols = -c(new_code,group,outcome),
               names_to="interval",
               values_to="value") %>% 
  
  rename(key=outcome) %>% 
  mutate(key_2=case_when(
    key == "hto." ~ "Hematocrit~('%')",
    key == "leuco." ~ "White~blood~cells~(10^{3}/mm^{3})",
    # key == "abaston." ~ "Neutrophils~(band~cells)~('%')",
    key == "abaston." ~ "Band~cells~('%')",
    key == "segment." ~ "Neutrophils~('%')",
    key == "eosinof." ~ "Eosinophils~('%')",
    key == "linfocit." ~ "Lymphocytes~('%')",
    key == "plaqueta" ~ "Platelets~(10^{4}/mm^{3})",
    key == "monocit." ~ "Monocites~('%')",
    key == "basofil." ~ "Basophils~('%')", #
    TRUE ~ key
  )) %>% 
  rename(outcome=key_2) %>%
  mutate(group=case_when(
    group=="pfal"~"P. falciparum",
    group=="pviv"~"P. vivax",
    TRUE~group
  )) %>% 
  
  mutate(interval=fct_relevel(interval,
                              "diff_2_1",
                              "diff_3_1")) %>% 
  mutate(interval=fct_recode(interval,
                             "2 vs 1"="diff_2_1",
                             "3 vs 1"="diff_3_1",
                             "3 vs 2"="diff_3_2"
                             )) 

hem_diff_plot %>% 
  # filter(magrittr::is_in(x = key,table = c("basofil.",
  #                                          "monocit.",
  #                                          "abaston."))) %>%
  # ggplot(aes(x = value,fill=group)) + geom_histogram() + facet_grid(interval~outcome)
  ggplot(aes(x = interval,y = value,color=group)) +
  geom_violin(alpha=0,lwd=0.4#,draw_quantiles = c(0.5)
              ) +
  geom_point(position = position_jitterdodge(),alpha=0.2) +
  facet_wrap(~outcome,scales = "free_y",
             labeller = label_parsed) +
  # facet_wrap(~outcome,scales = "free",
  #            labeller = label_parsed) +
  geom_hline(aes(yintercept=0)) +
  # scale_y_log10() +
  labs(title = "Subject Differences in the Trend of hematological profiles",
       subtitle = "Between Visits at baseline, day 7 and 28",
       colour="Plasmodium\nspecie infection",
       y="Difference",
       x="Visit Intervals")+
  theme(legend.text = element_text(face = "italic"))
ggsave("figure/03-visit_violin-difference.png",height = 6,width = 8,dpi = "retina")

# ___ problematic distributions

hem_diff_plot %>% 
  filter(magrittr::is_in(x = key,table = c("basofil.",
                                           "monocit.",
                                           "abaston."))) %>%
  ggplot(aes(x = value,fill=group)) + 
  geom_histogram() + 
  facet_grid(interval~outcome)

# __ grupo-numero_visita ------------------------------------------------

hem_dist_plot <- hem %>% 
  filter(group!="control") %>% 
  select(-fecha,-edad, -sexo) %>% 
  gather(key,value,-new.code,-group, -diff_fecha, -num.visita) %>% 
  mutate(value=as.numeric(value),
         num.visita=as.factor(num.visita)) %>% 
  # mutate(key_2=case_when(
  #   key == "hto." ~ "Hematocrit~('%')",
  #   key == "leuco." ~ "White~blood~cells~(10^{3}/mm^{3})",
  #   # key == "abaston." ~ "Neutrophils~(band~cells)~('%')",
  #   key == "abaston." ~ "Band~cells~('%')",
  #   key == "segment." ~ "Neutrophils~('%')",
  #   key == "eosinof." ~ "Eosinophils~('%')",
  #   key == "linfocit." ~ "Lymphocytes~('%')",
  #   key == "plaqueta" ~ "Platelets~(10^{4}/mm^{3})",
  #   key == "monocit." ~ "Monocites~('%')",
  #   key == "basofil." ~ "Basophils~('%')",
  #   TRUE ~ key
  # )) %>% 
  mutate(key_2=case_when(
    key == "hto." ~ "Hematocrito~('%')",
    key == "leuco." ~ "RGB~(10^{3}/mm^{3})",
    # key == "abaston." ~ "Neutrophils~(band~cells)~('%')",
    key == "abaston." ~ "Abastonados~('%')",
    key == "segment." ~ "Segmentados~('%')",
    key == "eosinof." ~ "Eosinófilos~('%')",
    key == "linfocit." ~ "Linfocitos~('%')",
    key == "plaqueta" ~ "Plaquetas~(10^{4}/mm^{3})",
    key == "monocit." ~ "Monocitos~('%')",
    key == "basofil." ~ "Basófilos~('%')", #
    TRUE ~ key
  )) %>% 
  mutate(group=case_when(
    group=="pfal"~"P. falciparum",
    group=="pviv"~"P. vivax",
    TRUE~group
  )) 

hem_dist_plot %>% 
  #mutate(value=if_else(value==0,0.1,value)) %>% 
  ggplot(aes(num.visita,value,colour=group)) +
  #geom_line() +
  geom_point(position = position_jitterdodge(),alpha=0.2) +
  geom_violin(alpha=0,lwd=0.4#,draw_quantiles = c(0.5)
              ) +
  facet_wrap(~key_2,scales = "free_y",
             labeller = label_parsed) +
  # labs(title = "Trend of hematological profiles",
  #      subtitle = "Visits at baseline, day 7 and 28",
  #      colour="Plasmodium\nspecie infection") +
  # xlab("Visit number") + ylab("Value") +
  labs(
    x = "Número de visita*",
    y = "Valor",
    caption = "* (1) día 0, (2) día 7, (3) día 28 \n ** RGB = Recuento de Glóbulos Blancos",
    colour="Infección por\nespecie de\nPlasmodium") +
  #scale_y_log10() +
  theme(legend.text = element_text(face = "italic"))
ggsave("figure/02-visit_violin.png",height = 6,width = 8,dpi = "retina")
ggsave("figure/02-visit_violin.tiff",height = 6,width = 8,dpi = "retina")

# ___ problematic distributions

hem_dist_plot %>% 
  filter(magrittr::is_in(x = key,table = c("basofil.",
                                           "monocit.",
                                           "abaston."))) %>%
  ggplot(aes(x = value,fill=group)) + 
  geom_histogram() + 
  facet_grid(num.visita~key_2)

# __ trend persona-fecha_visita ------------------------------------------------
hem %>% 
  #mutate(group=fct_relevel(group,"pviv")) %>% 
  filter(group!="control") %>% 
  select(-fecha,-num.visita, -edad, -sexo) %>% 
  #select(-monocit.,-basofil.) %>% 
  gather(key,value,-new.code,-group, -diff_fecha) %>% 
  #mutate(key=forcats::fct_relevel("abaston.",))
  mutate(value=as.numeric(value)) %>% 
  ggplot(aes(diff_fecha,value,colour=group,group=new.code)) +
  geom_line(alpha=0.4,lwd=1) +
  facet_wrap(~key#+group
             ,scales = "free_y"
             #,ncol = 6
  ) +
  labs(title = "Trend of hematological profiles") +
  xlab("Time of follow-up") + ylab("Value")
ggsave("figure/01-trend_plot.png",height = 6,width = 8)

# __ trend persona-grupo-fecha_visita ------------------------------------------------
hem %>% 
  #mutate(group=fct_relevel(group,"pviv")) %>% 
  filter(group!="control") %>% 
  select(-fecha,-num.visita, -edad, -sexo) %>% 
  #select(-monocit.,-basofil.) %>% 
  gather(key,value,-new.code,-group, -diff_fecha) %>% 
  #mutate(key=forcats::fct_relevel("abaston.",))
  mutate(value=as.numeric(value)) %>% 
  ggplot(aes(diff_fecha,value,colour=group,group=new.code)) +
  geom_line(alpha=0.4,lwd=1) +
  facet_wrap(~key+group
             ,scales = "free_y"
             ,ncol = 6
  ) +
  labs(title = "Trend of hematological profiles") +
  xlab("Time of follow-up") + ylab("Value")
ggsave("figure/01-trend_plot-by_group.png",height = 6,width = 12,dpi = "retina")

# tabla 2: por visita Pv+Pf -----------------------------------------------------

compareGroups(num.visita ~ #sexo + edad +
                #num.visita + 
                diff_fecha + #group +
                hto. + leuco. + 
                abaston. + segment. + #neutrofilos
                eosinof. + basofil. +
                monocit. + linfocit. + plaqueta,
              data = hem %>% filter(group!="control") #, byrow=T 
              ,method = c(hto. = 2,
                          leuco. = 2,
                          abaston. = 2,
                          segment. = 2,
                          eosinof. = 2,
                          monocit. = 2,
                          linfocit. = 2, #cercana a normal
                          plaqueta = 2,
                          basofil.= 2, edad = 2
              )
) %>% 
  createTable(show.all = F, show.n = F,show.p.trend = T,show.p.mul = T, digits = 1) -> tab2_time

tab2_time
tab2_time %>% export2xls("table/h0-tab2_visitas.xls")


# tabla 2: por visita Pv -----------------------------------------------------

compareGroups(num.visita ~ #sexo + edad +
                #num.visita + 
                diff_fecha + 
                hto. + leuco. + 
                abaston. + segment. + #neutrofilos
                eosinof. + basofil. +
                monocit. + linfocit. + plaqueta,
              data = hem %>% filter(group=="pviv") #, byrow=T 
              ,method = c(hto. = 2,
                          leuco. = 2,
                          abaston. = 2,
                          segment. = 2,
                          eosinof. = 2,
                          monocit. = 2,
                          linfocit. = 2, #cercana a normal
                          plaqueta = 2,
                          basofil.= 2, edad = 2
                          )
              ) %>% 
  createTable(show.all = F, show.n = F,show.p.trend = T, digits = 1) -> tab2_time_pv

tab2_time_pv
tab2_time_pv %>% export2xls("table/h0-tab2_pv_visitas.xls")

# tabla 2: por visita Pf -----------------------------------------------------

compareGroups(num.visita ~ #sexo + edad +
                #num.visita + 
                diff_fecha + 
                hto. + leuco. + 
                abaston. + segment. + #neutrofilos
                eosinof. + basofil. +
                monocit. + linfocit. + plaqueta,
              data = hem %>% filter(group=="pfal") #, byrow=T 
              ,method = c(hto. = 2,
                          leuco. = 2,
                          abaston. = 2,
                          segment. = 2,
                          eosinof. = 2,
                          monocit. = 2,
                          linfocit. = 2, #cercana a normal
                          plaqueta = 2,
                          basofil.= 2, edad = 2
                          )
              ) %>% 
  createTable(show.all = F, show.n = F, show.p.trend = T, digits = 1) -> tab2_time_pf

tab2_time_pf
tab2_time_pf %>% export2xls("table/h0-tab2_pf_visitas.xls")

# trend regression --------------------------------------------------------

hem %>% dim()
hem_cc <- hem %>% filter(complete.cases(.))
hem_cc %>% dim() #perdida de 4 observaciones

hem_cc_trd <- hem_cc %>% 
  #this makes xtgee work!
  mutate(new.code=as.factor(new.code),
         #num.visita=as.integer(num.visita),
         num.visita=as.factor(num.visita)) %>% 
  #to use negative binomial distribution due to overdisperssion
  #mutate(abaston.=as.integer(abaston.)) %>% 
  filter(group!="control") #%>% filter(group!="pfal")
#hem_cc_fal <- hem_cc %>% 
#  filter(num.visita=="1") #%>% filter(group!="pviv")
hem_cc_trd %>% glimpse()
hem_cc_trd %>% count(group)
#hem_cc_fal %>% count(group)

hem_cc_trd %>% 
  #transformar vectores chr a fct
  mutate_if(is.character,as.factor) %>% 
  rename_all(funs(str_replace_all(.,"\\.","_"))) %>% 
  haven::write_dta("data/hem_cc_trd.dta")

# _evaluate correlation structure ------------------------------------------

custom_correlation_structure <- function(data) {
  data %>% 
    pivot_wider(names_from = num.visita,
                values_from = val) %>% 
    janitor::clean_names() %>% 
    select(-new_code) %>% 
    corrr::correlate() %>%
    corrr::shave()
}

hem_cc_trd %>% 
  pivot_longer(cols = hto.:plaqueta,names_to = "var",values_to = "val") %>% 
  arrange(var,new.code,num.visita) %>% 
  select(new.code,num.visita,var,val) %>% 
  group_by(var) %>% 
  nest() %>% 
  mutate(corr=map(.x = data,.f = custom_correlation_structure)) %>% 
  unnest(cols = c(corr)) %>% 
  print_inf()

#' hto and leuco share an almost equivalent correlation
#' but the others are not similar
#' however
#' since repeated measurements were taken through time
#' we can not use an exchangeble correlation matrix
#' measurements were taken on different time intervals
#' ar1 was used
#' however, since times were irregular
#' unstructured could also be applied


# execute -----------------------------------------------------------------

#ojo con supuestos
#- muestreo aleatorio: no
#- independencia de observaciones: no
#-- visitas consecutivas por sujeto
#-- objetivo: calcular si medidas (dependientes) varian con el tiempo (indep),
#--           o si el tiempo varía por la especie
#-- diferente de: modelar estatus por mediciones de var. independi en tiempo

# epi_tidymodel_coef <- function(wm1) {
#   m1 <- wm1 %>% tidy() %>% #mutate(coef=estimate) %>% 
#     rownames_to_column()
#   m2 <- wm1 %>% confint_tidy() %>% #mutate_all(list(exp)) %>% 
#     rownames_to_column()
#   
#   left_join(m1,m2) %>% 
#     dplyr::select(term,#log.coef=estimate,
#                   estimate ,#coef,
#                   se=std.error,
#                   conf.low,conf.high,
#                   p.value) %>% 
#     mutate_at(.vars = vars(-term,-p.value),round, digits = 5) %>% 
#     mutate_at(.vars = vars(p.value),round, digits = 5) %>% 
#     #print() %>% 
#     return()
# }

#plan: reportar modelos sin y con interacción (falta tabla sin!)

# _all gee regressions at once --------------------------------------------

#' plan
#' - create dataset with covariate names
#' - create the different equations
#' - nest
#' - apply the regression
#' - unnest

## _hematocrito ----

# #glm(hto. ~ edad + sexo, data = hem_cc_trd, family = gaussian(link = "identity")) %>% tidy()
# 
# glm.full <-geeglm(hto. ~ diff_fecha + edad + sexo + group
#                 , data = hem_cc_trd, family = gaussian(link = "identity"),
#                 id = new.code, waves = num.visita, corstr = "ar1")
## glm.full %>% tidy()
## glm.full %>% confint_tidy()
# 
# hto_01 <- epi_tidymodel_coef(glm.full) %>% 
#   filter(str_detect(term,"diff")|str_detect(term,"group")) %>% 
#   select(term,estimate,starts_with("conf."),p.value) %>% 
#   mutate(outcome="hto") %>% 
#   print()
# 
# glm.full <-geeglm(hto. ~ num.visita + edad + sexo + group
#                 , data = hem_cc_trd, family = gaussian(link = "identity"),
#                 id = new.code, waves = num.visita, corstr = "ar1")
## glm.full %>% tidy()
## glm.full %>% confint_tidy()
# 
# hto_02 <- epi_tidymodel_coef(glm.full) %>% 
#   filter(str_detect(term,"num.v")|str_detect(term,"group")) %>% 
#   select(term,estimate,starts_with("conf."),p.value) %>% 
#   mutate(outcome="hto") %>% 
#   print()

glm.full <-geeglm(hto. ~ diff_fecha*group + edad + sexo
                  , data = hem_cc_trd, family = gaussian(link = "identity"),
                  id = new.code, waves = num.visita, corstr = "ar1")
#glm.full %>% tidy()
#glm.full %>% confint_tidy()

hto_1 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"diff")|str_detect(term,"group")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="hto") %>% 
  print()

glm.full <-geeglm(hto. ~ num.visita*group + edad + sexo
                  , data = hem_cc_trd, family = gaussian(link = "identity"),
                  id = new.code, waves = num.visita, corstr = "ar1")
#glm.full %>% tidy()
#glm.full %>% confint_tidy()

hto_2 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"num.v")|str_detect(term,"group")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="hto") %>% 
  print()

## _leucocitos ----
# glm.full <-geeglm(leuco. ~ diff_fecha + edad + sexo
#                 , data = hem_cc_trd, family = gaussian(link = "identity"),
#                 id = new.code, waves = num.visita, corstr = "ar1")
## glm.full %>% tidy()
## glm.full %>% confint_tidy()
# 
# glm.full <-geeglm(leuco. ~ diff_fecha + group + edad + sexo
#                 , data = hem_cc_trd, family = gaussian(link = "identity"),
#                 id = new.code, waves = num.visita, corstr = "ar1")
## glm.full %>% tidy()
## glm.full %>% confint_tidy()


glm.full <-geeglm(leuco. ~ diff_fecha*group + edad + sexo
                  , data = hem_cc_trd, family = gaussian(link = "identity"),
                  id = new.code, waves = num.visita, corstr = "ar1")
#glm.full %>% tidy()
#glm.full %>% confint_tidy()

leuco_1 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"diff")|str_detect(term,"group")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="leuco") %>% 
  print()


glm.full <-geeglm(leuco. ~ num.visita*group + edad + sexo
                  , data = hem_cc_trd, family = gaussian(link = "identity"),
                  id = new.code, waves = num.visita, corstr = "ar1")
#glm.full %>% tidy()
#glm.full %>% confint_tidy()

leuco_2 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"num.v")|str_detect(term,"group")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="leuco") %>% 
  print()

## _abastonados ----
# glm.full <-geeglm(abaston. ~ diff_fecha + edad + sexo
#                 , data = hem_cc_trd, family = gaussian(link = "identity"),
#                 id = new.code, waves = num.visita, corstr = "ar1")
## glm.full %>% tidy()
## glm.full %>% confint_tidy()
# 
# glm.full <-geeglm(abaston. ~ diff_fecha + group + edad + sexo
#                 , data = hem_cc_trd, family = gaussian(link = "identity"),
#                 id = new.code, waves = num.visita, corstr = "ar1")
## glm.full %>% tidy()
## glm.full %>% confint_tidy()

# glm.full <-geeglm(abaston. ~ diff_fecha*group + edad + sexo
#                   , data = hem_cc_trd, family = gaussian(link = "identity"),
#                   id = new.code, waves = num.visita, corstr = "ar1")
# #glm.full %>% tidy()
# #glm.full %>% confint_tidy()
# 
# abaston_1 <- epi_tidymodel_coef(glm.full) %>% 
#   filter(str_detect(term,"diff")|str_detect(term,"group")) %>% 
#   select(term,estimate,starts_with("conf."),p.value) %>% 
#   mutate(outcome="abaston") %>% 
#   print()

glm.full <- geem(abaston. ~ diff_fecha*group + edad + sexo,
                 data = hem_cc_trd %>% as.data.frame(), 
                 family = MASS::negative.binomial(2),
                       id = new.code, 
                       #waves = num.visita, 
                       corstr = "ar1")
glm.full %>% summary()
glm.full %>% coef()
#glm.full %>% confint()
#glm.full %>% vcov() %>% diag() #issue

glm.full <-geeglm(abaston. ~ num.visita*group + edad + sexo
                  , data = hem_cc_trd, family = gaussian(link = "identity"),
                  id = new.code, waves = num.visita, corstr = "ar1")
#glm.full %>% tidy()
#glm.full %>% confint_tidy()

# abaston_2 <- epi_tidymodel_coef(glm.full) %>% 
#   filter(str_detect(term,"num.v")|str_detect(term,"group")) %>% 
#   select(term,estimate,starts_with("conf."),p.value) %>% 
#   mutate(outcome="abaston") %>% 
#   print()

## _segmentados ----
# glm.full <-geeglm(segment. ~ diff_fecha + edad + sexo
#                 , data = hem_cc_trd, family = gaussian(link = "identity"),
#                 id = new.code, waves = num.visita, corstr = "ar1")
## glm.full %>% tidy()
## glm.full %>% confint_tidy()
# 
# glm.full <-geeglm(segment. ~ diff_fecha + group + edad + sexo
#                 , data = hem_cc_trd, family = gaussian(link = "identity"),
#                 id = new.code, waves = num.visita, corstr = "ar1")
## glm.full %>% tidy()
## glm.full %>% confint_tidy()

glm.full <-geeglm(segment. ~ diff_fecha*group + edad + sexo
                  , data = hem_cc_trd, family = gaussian(link = "identity"),
                  id = new.code, waves = num.visita, corstr = "ar1")
#glm.full %>% tidy()
#glm.full %>% confint_tidy()

segment_1 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"diff")|str_detect(term,"group")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="segment") %>% 
  print()

glm.full <-geeglm(segment. ~ num.visita*group + edad + sexo
                  , data = hem_cc_trd, family = gaussian(link = "identity"),
                  id = new.code, waves = num.visita, corstr = "ar1")
#glm.full %>% tidy()
#glm.full %>% confint_tidy()

segment_2 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"num.v")|str_detect(term,"group")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="segment") %>% 
  print()

## _eosinofilos ----
# glm.full <-geeglm(eosinof. ~ diff_fecha + edad + sexo
#                 , data = hem_cc_trd, family = gaussian(link = "identity"),
#                 id = new.code, waves = num.visita, corstr = "ar1")
## glm.full %>% tidy()
## glm.full %>% confint_tidy()
# 
# glm.full <-geeglm(eosinof. ~ diff_fecha + group + edad + sexo
#                 , data = hem_cc_trd, family = gaussian(link = "identity"),
#                 id = new.code, waves = num.visita, corstr = "ar1")
## glm.full %>% tidy()
## glm.full %>% confint_tidy()

glm.full <-geeglm(eosinof. ~ diff_fecha*group + edad + sexo
                  , data = hem_cc_trd, family = gaussian(link = "identity"),
                  id = new.code, waves = num.visita, corstr = "ar1")
#glm.full %>% tidy()
#glm.full %>% confint_tidy()

eosinof_1 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"diff")|str_detect(term,"group")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="eosinof") %>% 
  print()

glm.full <-geeglm(eosinof. ~ num.visita*group + edad + sexo
                  , data = hem_cc_trd, family = gaussian(link = "identity"),
                  id = new.code, waves = num.visita, corstr = "ar1")
#glm.full %>% tidy()
#glm.full %>% confint_tidy()

eosinof_2 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"num.v")|str_detect(term,"group")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="eosinof") %>% 
  print()

## _linfocitos ----
# glm.full <-geeglm(linfocit. ~ diff_fecha + edad + sexo
#                 , data = hem_cc_trd, family = gaussian(link = "identity"),
#                 id = new.code, waves = num.visita, corstr = "ar1")
## glm.full %>% tidy()
## glm.full %>% confint_tidy()
# 
# glm.full <-geeglm(linfocit. ~ diff_fecha + group + edad + sexo
#                 , data = hem_cc_trd, family = gaussian(link = "identity"),
#                 id = new.code, waves = num.visita, corstr = "ar1")
## glm.full %>% tidy()
## glm.full %>% confint_tidy()

glm.full <-geeglm(linfocit. ~ diff_fecha*group + edad + sexo
                  , data = hem_cc_trd, family = gaussian(link = "identity"),
                  id = new.code, waves = num.visita, corstr = "ar1")
#glm.full %>% tidy()
#glm.full %>% confint_tidy()

linfocit_1 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"diff")|str_detect(term,"group")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="linfocit") %>% 
  print()

glm.full <-geeglm(linfocit. ~ num.visita*group + edad + sexo
                  , data = hem_cc_trd, family = gaussian(link = "identity"),
                  id = new.code, waves = num.visita, corstr = "ar1")
#glm.full %>% tidy()
#glm.full %>% confint_tidy()

linfocit_2 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"num.v")|str_detect(term,"group")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="linfocit") %>% 
  print()

## _plaqueta ----
# glm.full <-geeglm(plaqueta ~ diff_fecha + edad + sexo
#                 , data = hem_cc_trd, family = gaussian(link = "identity"),
#                 id = new.code, waves = num.visita, corstr = "ar1")
## glm.full %>% tidy()
## glm.full %>% confint_tidy()
# 
# glm.full <-geeglm(plaqueta ~ diff_fecha + group + edad + sexo
#                 , data = hem_cc_trd, family = gaussian(link = "identity"),
#                 id = new.code, waves = num.visita, corstr = "ar1")
## glm.full %>% tidy()
## glm.full %>% confint_tidy()

glm.full <-geeglm(plaqueta ~ diff_fecha*group + edad + sexo
                  , data = hem_cc_trd, family = gaussian(link = "identity"),
                  id = new.code, waves = num.visita, corstr = "ar1")
#glm.full %>% tidy()
#glm.full %>% confint_tidy()

plaqueta_1 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"diff")|str_detect(term,"group")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="plaqueta") %>% 
  print()

glm.full <-geeglm(plaqueta ~ num.visita*group + edad + sexo
                  , data = hem_cc_trd, family = gaussian(link = "identity"),
                  id = new.code, waves = num.visita, corstr = "ar1")
#glm.full %>% tidy()
#glm.full %>% confint_tidy()

plaqueta_2 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"num.v")|str_detect(term,"group")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="plaqueta") %>% 
  print()

# 00_ EXPORTAR ------------------------------------------------------------

# _mix table 3 -------------------------------------------------------------

full_t3 <- hto_1 %>% 
  union_all(hto_2) %>% 
  union_all(leuco_1) %>% 
  union_all(leuco_2) %>% 
  # union_all(abaston_1) %>% #obtained in stata / geeM error with confint
  # union_all(abaston_2) %>% 
  union_all(segment_1) %>% 
  union_all(segment_2) %>% 
  union_all(eosinof_1) %>% 
  union_all(eosinof_2) %>% 
  union_all(linfocit_1) %>% 
  union_all(linfocit_2) %>% 
  union_all(plaqueta_1) %>% 
  union_all(plaqueta_2) %>% 
  #diferenciar grouppviv de modelo 1 y 2
  group_by(outcome) %>% 
  mutate(id = row_number()) %>% 
  ungroup() %>% #count(outcome,term,sort = T)
  mutate(term=if_else(term=="grouppviv",str_c(term,"_",id),term)) %>% 
  select(-id)

full_t3 %>% print_inf()

full_t3 %>% 
  filter(p.value<=0.05) %>% 
  arrange(
    term,
    p.value
    )

full_t3 %>% 
  filter(p.value>0.05 & p.value <=0.1) %>% 
  arrange(
    term,
    p.value
  )

#fix here due to grouppviv addition
full_t3 %>% 
  mutate(estimate=round(estimate,2),
         conf.low=round(conf.low,2),
         conf.high=round(conf.high,2)) %>% 
  #count(term)
  unite(ic, c(conf.low,conf.high), sep = " - ") %>% 
  select(-p.value) %>% 
  gather(key,value,-term,-outcome) %>% 
  unite(new_col,c(term,key),sep = ".") %>% 
  mutate(outcome=fct_relevel(outcome,
                             "hto","leuco",
                             "abaston","segment",
                             "eosinof","linfocit")) %>% 
  pivot_wider(names_from = new_col,values_from = value) %>% 
  #spread(new_col,value) %>% 
  xlsx::write.xlsx("table/h0-tab3_visitas.xls")

# # correlation term --------------------------------------------------------
# 
# library(nlme)
# 
# cs1 <- corARMA(c(0.2, 0.3, -0.1), form = ~ 1 | Mare, p = 1, q = 2)
# 
# cs1ARMA <- corARMA(0.4, form = ~ 1 | Subject, q = 1)
# cs1ARMA <- Initialize(cs1ARMA, data = Orthodont)
# corMatrix(cs1ARMA)
# 
# cs2ARMA <- corARMA(c(0.8, 0.4), form = ~ 1 | Subject, p=1, q=1)
# cs2ARMA <- Initialize(cs2ARMA, data = Orthodont)
# corMatrix(cs2ARMA)
# 
# fm1Ovar.lme <- lme(follicles ~ sin(2*pi*Time) + cos(2*pi*Time),
#                    data = Ovary, random = pdDiag(~sin(2*pi*Time)))
# fm5Ovar.lme <- update(fm1Ovar.lme,
#                       corr = corARMA(p = 1, q = 1))
# 
# Seatbelts.df <- data.frame(Seatbelts)
# Seatbelts.df %>% as_tibble()
# Seatbelts.df$t <- 1:(dim(Seatbelts.df)[1])
# Seatbelts.df %>% as_tibble()
# m <- gls(drivers ~ kms + PetrolPrice + law,
#          data=Seatbelts.df,
#          correlation=corARMA(p=1, q=0, form=~t))
# summary(m)
# 
# 
# 
# # gee ---------------------------------------------------------------------
# 
# library(geepack)
# # robust standard error are default
# 
# hem_cc_trd %>% glimpse()
# hem_cc_trd %>% count(new.code)
# my <- plaqueta ~ num.visita*group + edad + sexo
# 
# glm(formula = my, data = hem_cc_trd, family = gaussian(link = "identity")) %>%
#   epi_tidymodel_coef()
# 
# geeglm(formula = my,
#        data = hem_cc_trd,
#        family = gaussian(link = "identity"),
#        id = new.code,
#        waves = num.visita,
#        corstr = "ar1" #corstr = "independence"
#        ) %>%
#   epi_tidymodel_coef()
# 
# #?geeglm
# 
# data(dietox)
# dietox$Cu     <- as.factor(dietox$Cu)
# mf <- formula(Weight~Cu*(Time+I(Time^2)+I(Time^3)))
# gee1 <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"),corstr="ar1")
# gee1 %>% broom::tidy()
# vcov(gee1)
# summary(gee1)
# str(gee1)
# 
# mf2 <- formula(Weight~Cu*Time+I(Time^2)+I(Time^3))
# gee2 <- geeglm(mf2, data=dietox, id=Pig, family=poisson("identity"),corstr="unstructured")
# anova(gee2)
# class(gee2)
