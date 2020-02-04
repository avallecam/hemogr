
# objetivo ----------------------------------------------------------------

#' importar bases
#' unir bases
#' explorar distribucion

# packages ------------------------------------------------------------------

library(readxl)
library(tidyverse)
library(zoo)
library(lubridate)
library(compareGroups)
library(broom)
library(rlang)
library(avallecam)

rm(list = ls())
theme_set(theme_bw())

# 00_ IMPORTAR ------------------------------------------------------------

# import + clean data -------------------------------------------------------------

viv <- read_xlsx("data-raw/BasesHemograma_Pv.xlsx",sheet = 1) %>%
  rename(x__1=1,x__14=14) %>% #any reason for bugs?
  rename_all(funs(make.names(.))) %>% 
  rename_all(funs(str_to_lower(.))) %>% 
  rename_all(funs(iconv(.,to = "ASCII//TRANSLIT"))) %>% 
  rename(codigo=cA3digo) %>% 
  do(na.locf(.)) %>% 
  select(#-x__1,
         #-x__2,
         -edad
         ) %>% distinct() %>% 
  left_join(
    read_xlsx("data-raw/BasesHemograma_Pv.xlsx",sheet = 2) %>% 
      rename(x__1=1) %>% #any reason for bugs?
      rename_all(funs(str_to_lower(.))) %>% 
      rename_all(funs(make.names(.))) %>% 
      rename_all(funs(iconv(.,to = "ASCII//TRANSLIT"))) %>% 
      #do(na.locf(.)) %>% 
      #select(-x__1) %>% #count(codigo) %>% arrange(desc(n)) %>% 
      distinct()
  ) %>%  #279 = 93*3
  mutate(group="pviv",
         new.code=str_c(x__1,"_",codigo)) %>% 
  group_by(new.code) %>% 
  arrange(new.code,fecha) %>% 
  mutate(num.visita=seq(1,3,1)) %>% 
  ungroup() %>% 
  select(new.code,group,fecha,num.visita,hto.:edad,-x__14)

fal <- read_xlsx("data-raw/BasesHemograma_Pf.xlsx",sheet = 1) %>% 
  rename(x__1=1,x__13=13) %>% #any reason for bugs?
  rename_all(funs(str_to_lower(.))) %>% 
  rename_all(funs(make.names(.))) %>% 
  rename_all(funs(iconv(.,to = "ASCII//TRANSLIT"))) %>% 
  do(na.locf(.)) %>% 
  select(-x__1) %>% 
  #select(#-x__1,
   #      -x__2) %>% 
  distinct() %>% 
  left_join(
    read_xlsx("data-raw/BasesHemograma_Pf.xlsx",sheet = 2) %>% 
      rename(x__1=1) %>% #any reason for bugs?
      rename_all(funs(str_to_lower(.))) %>% 
      rename_all(funs(make.names(.))) %>% 
      rename_all(funs(iconv(.,to = "ASCII//TRANSLIT"))) %>% 
      #select(-x__1)
      distinct() #,by="codigo"
  ) %>% 
  mutate(group="pfal",
         new.code=str_c(x__1,"_",codigo)) %>% 
  rename(fecha=fecha.de.visita) %>% 
  group_by(new.code) %>% 
  arrange(new.code,fecha) %>% 
  mutate(num.visita=seq(1,3,1)) %>% 
  ungroup() %>% 
  rename(abaston.=abas.,
         segment.=segm.,
         eosinof.=eosin.,
         monocit.=mon.,
         basofil.=bas.,
         linfocit.=lin.) %>% 
  select(new.code,group,fecha,num.visita,hto.:edad,-x__13,-x__1)

ctr <- read_xlsx("data-raw/BasesHemograma_Controles.xlsx",sheet = 1) %>% 
  rename_all(funs(make.names(.))) %>% 
  rename_all(funs(str_to_lower(.))) %>% 
  rename_all(funs(iconv(.,to = "ASCII//TRANSLIT"))) %>% 
  rename(codigo=cA3digo) %>% 
  do(na.locf(.)) %>% 
  left_join(
    read_xlsx("data-raw/BasesHemograma_Controles.xlsx",sheet = 2) %>% 
      rename(x__1=1) %>% #any reason for bugs?
      rename_all(funs(str_to_lower(.))) %>% 
      rename_all(funs(make.names(.))) %>% 
      rename_all(funs(iconv(.,to = "ASCII//TRANSLIT"))) %>% 
      select(-x__1) #%>% count(codigo) %>% arrange(desc(n))
            ) %>%  #%>% mutate(codigo_=as.numeric(codigo))
  mutate(group="control",
         new.code=codigo) %>% 
  #group_by(new.code) %>% 
  #arrange(new.code,fecha) %>% 
  mutate(num.visita=1) %>%  #%>% ungroup()
  select(new.code,group,fecha,num.visita,hto.:edad)

# glimpse data ------------------------------------------------------------

viv %>% glimpse()
fal %>% glimpse()
ctr %>% glimpse()

# recurrences -----------------------------------------------------------------

#ctr %>% 
#  group_by(group) %>% 
#  count(codigo) %>% 
#  ungroup() %>% 
#  arrange(desc(n))
#
#viv %>% 
#  group_by(group) %>% 
#  count(codigo) %>% 
#  ungroup() %>% 
#  arrange(desc(n))
#
#viv %>% filter(codigo==1862)
#
#viv %>% 
#  group_by(group) %>% 
#  count(new.code) %>% 
#  ungroup() %>% 
#  arrange(desc(n))
#
#fal %>% 
#  group_by(group) %>% 
#  count(codigo) %>% 
#  ungroup() %>% 
#  arrange(desc(n))


# number of visits ------------------------------------------------------------

viv %>% 
  group_by(new.code) %>% 
  arrange(new.code,fecha) %>% 
  mutate(num.visita=seq(1,3,1)) %>% 
  ungroup() %>% glimpse()


# ONE data set ------------------------------------------------------------

hem <- ctr %>% 
  full_join(viv) %>% 
  full_join(fal) %>% 
  mutate(num.visita=as.character(num.visita),
         plaqueta=plaqueta/10000,
         plaqueta=if_else(plaqueta>180,NA_real_,plaqueta),
         leuco.=leuco./1000,
         leuco.=if_else(leuco.>60,NA_real_,leuco.),
         abaston.=if_else(abaston.>50,NA_real_,abaston.),
         #basofil.=as.factor(basofil.),
         #monocit.=as.factor(monocit.),
         #abaston.=if_else(abaston.==0,0,1) %>% as.factor(.),
         #basofil.=if_else(basofil.==0,0,1) %>% as.factor(.),
         #monocit.=if_else(monocit.==0,0,1) %>% as.factor(.)
         ) %>% 
  group_by(new.code) %>% 
  #mutate(diff_fecha=fecha - lag(fecha)) %>% 
  mutate(diff_fecha= interval(min(fecha), fecha)/days(1) ,
         #diff_fecha=if_else(is.na(diff_fecha),0,diff_fecha)
         ) %>% 
  ungroup()


#hem %>% visdat::vis_dat()
hem %>% glimpse()
hem %>% count(group)

# identify missings -------------------------------------------------------
#http://juliejosse.com/wp-content/uploads/2018/06/DataAnalysisMissingR.html

#naniar::gg_miss_var(hem)
naniar::miss_var_summary(hem)

# distribuciones ----------------------------------------------------------

skimr::skim(hem) # visual
psych::describe(hem) # skewness + kurtosis
#Hmisc::describe(hem) # min + max list

hem %>% 
  ggplot(aes(sample=linfocit.)) + 
  stat_qq() +
  stat_qq_line()

hem %>% count(sexo)
hem %>% count(num.visita)
hem %>% count(group)
hem %>% count(new.code,group,sort = T) %>% count(group,n) #visitas para casos

# 00_ EXPORTAR ------------------------------------------------------------

write_rds(hem,"data/hemdb.rds")

