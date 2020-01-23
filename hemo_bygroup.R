
# objetivo ----------------------------------------------------------------

#' explorar distribucion
#' generar tablas epi por grupo (ctr,viv,fal)
#' estimar diferencia promedio entre controles y casos en la primera visita

# packages ------------------------------------------------------------------

library(readxl)
library(tidyverse)
library(zoo)
library(lubridate)
library(compareGroups)
library(broom)
library(rlang)

rm(list = ls())
theme_set(theme_bw())


# 00_ IMPORTAR ------------------------------------------------------------

hem <- read_rds("data/hemdb.rds")

#hem %>% visdat::vis_dat()
hem %>% glimpse()
hem %>% count(group)
hem %>% count(new.code,group,sort = T) %>% count(group,n) #visitas para casos

# identify missings -------------------------------------------------------
#http://juliejosse.com/wp-content/uploads/2018/06/DataAnalysisMissingR.html

#naniar::gg_miss_var(hem)
naniar::miss_var_summary(hem)

# distribuciones ----------------------------------------------------------

skimr::skim(hem) # visual
psych::describe(hem) # skewness + kurtosis
Hmisc::describe(hem) # min + max list

hem %>% 
  ggplot(aes(sample=linfocit.)) + 
  stat_qq() +
  stat_qq_line()

# data dictionary ---------------------------------------------------------

#reference ranges
#hematocrito | 39-52 % | 
#plaquetas | 150-450 10^3/mm3 | 
#leucocitos (wbc) | 04-11 10^3/mm3 | 
#neutrofilos abastonados (band cells) | 0-2 %  | 0-0.4 10^3/mm3
#neutrofilos segmentados (segmented cells) | 50-70 % | 2-7 10^3/mm3
#linfocitos | 25-40 % | 1.5-4 10^3/mm3
#monocitos | 2-10 % | 0-1.1 10^3/mm3
#eosinofilos | 0-5 % | 0-0.4 10^3/mm3
#basofilos | 0-1 % | 0-0.2 10^3/mm3

# tabla 1 -----------------------------------------------------------------

#library(compareGroups)

compareGroups(~ num.visita + 
                group + sexo + edad +
                hto. + leuco. + 
                abaston. + segment. + #neutrofilos
                eosinof. + basofil. +
                monocit. + linfocit. + plaqueta,
              data = hem %>% filter(num.visita=="1") #,byrow=T 
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
  createTable(show.all = T, show.n = T) #%>% 
#export2xls("table/z0-tab1_ind_r05.xls")

#hem %>% count(abaston.,group) %>% arrange(abaston.) %>% spread(group,n) %>% print(n=Inf)

# tabla 2: por grupos -----------------------------------------------------------------

compareGroups(group ~ sexo + edad +
                #num.visita + 
                hto. + leuco. + 
                abaston. + segment. + #neutrofilos
                eosinof. + basofil. +
                monocit. + linfocit. + plaqueta,
              data = hem %>% filter(num.visita=="1") #, byrow=T 
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
  createTable(show.all = F, show.n = F, show.p.mul = T) #%>% 
#export2xls("table/z0-tab1_ind_r05.xls")

# complete case analysis --------------------------------------------------

#todos los missing están en falciparum!
#hem %>% 
# select(group,x__1) %>% 
#filter(is.na(x__1)) %>% count(group)

hem_cc <- hem %>% filter(complete.cases(.))
hem_cc %>% dim()

hem_cc %>% glimpse()
hem_cc %>% count(group)
hem_cc_viv <- hem_cc %>% 
  filter(num.visita=="1") %>% 
  filter(group!="pfal")
hem_cc_fal <- hem_cc %>% 
  filter(num.visita=="1") %>% 
  filter(group!="pviv")
hem_cc_viv %>% count(group)
hem_cc_fal %>% count(group)

# exectute model --------------------------------------------------------

#library(broom)

#hem_cc_viv %>% count(hto.)
glm.full <- glm(hto. ~ group + edad + sexo
                , data = hem_cc_viv, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

#hem_cc_viv %>% count(leuco.)
glm.full <- glm(leuco. ~ group + edad + sexo
                , data = hem_cc_viv, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

#hem_cc_viv %>% count(abaston.)
glm.full <- glm(abaston. ~ group + edad + sexo
                , data = hem_cc_viv, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

#hem_cc_viv %>% count(segment.)
glm.full <- glm(segment. ~ group + edad + sexo
                , data = hem_cc_viv, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

#hem_cc_viv %>% count(eosinof.)
glm.full <- glm(eosinof. ~ group + edad + sexo
                , data = hem_cc_viv, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

hem_cc_viv %>% count(plaqueta)
glm.full <- glm(plaqueta ~ group + edad + sexo
                , data = hem_cc_viv, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()


# pendientes -no urgentes- ------------------------------------------------

#' ( ) modelos para pfal
#' ( ) actualizar modelamiento con funciones de avallecam: epi_tidymodel or epi_tidymodel_up
#' ( ) exportar tablas

