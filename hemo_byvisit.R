
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

# CORE: tiempo entre visitas -----------------------------------------
#fuente: https://stackoverflow.com/questions/32312925/time-difference-in-years-with-lubridate
#library(lubridate)

hem %>% 
  group_by(new.code) %>% 
  #mutate(diff_fecha=fecha - lag(fecha)) %>% 
  mutate(diff_fecha= new_interval(min(fecha), fecha)/days(1) ) %>% 
  ungroup() %>% 
  filter(group=="pviv") %>% 
  select(new.code,num.visita,fecha,diff_fecha) %>% 
  group_by(num.visita) %>% 
  skimr::skim(diff_fecha)

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

# __ grupo-numero_visita ------------------------------------------------
hem %>% 
  filter(group!="control") %>% 
  select(-fecha,-edad, -sexo) %>% 
  gather(key,value,-new.code,-group, -diff_fecha, -num.visita) %>% 
  mutate(value=as.numeric(value),
         num.visita=as.factor(num.visita)) %>% 
  ggplot(aes(num.visita,value,colour=group)) +
  #geom_line() +
  geom_point(position = position_jitterdodge(),alpha=0.2) +
  geom_boxplot(alpha=0,lwd=0.4) +
  facet_wrap(~key,scales = "free_y") +
  labs(title = "Trend of hematological profiles",
       subtitle = "Visits at the day 7 and 28") +
  xlab("Follow-up day") + ylab("Value")
ggsave("figure/02-visit_boxplot.png",height = 6,width = 8)


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
  createTable(show.all = F, show.n = F,show.p.trend = T, digits = 1) #%>% 
#export2xls("table/h0-tab2_pv_visitas.xls")

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
  createTable(show.all = F, show.n = F, show.p.trend = T, digits = 1) #%>% 
#export2xls("table/h0-tab2_pf_visitas.xls")



# trend regression --------------------------------------------------------

hem_cc <- hem %>% filter(complete.cases(.))
hem_cc %>% dim()

hem_cc_trd <- hem_cc %>% 
  filter(group!="control") #%>% filter(group!="pfal")
#hem_cc_fal <- hem_cc %>% 
#  filter(num.visita=="1") #%>% filter(group!="pviv")
hem_cc_trd %>% count(group)
#hem_cc_fal %>% count(group)


# execute -----------------------------------------------------------------

#ojo con supuestos
#- muestreo aleatorio: no
#- independencia de observaciones: no
#-- visitas consecutivas por sujeto
#-- objetivo: calcular si medidas (dependientes) varian con el tiempo (indep),
#--           o si el tiempo varía por la especie
#-- diferente de: modelar estatus por mediciones de var. independi en tiempo

epi_tidymodel_coef <- function(wm1) {
  m1 <- wm1 %>% tidy() %>% #mutate(coef=estimate) %>% 
    rownames_to_column()
  m2 <- wm1 %>% confint_tidy() %>% #mutate_all(list(exp)) %>% 
    rownames_to_column()
  
  left_join(m1,m2) %>% 
    dplyr::select(term,#log.coef=estimate,
                  estimate ,#coef,
                  se=std.error,
                  conf.low,conf.high,
                  p.value) %>% 
    mutate_at(.vars = vars(-term,-p.value),round, digits = 5) %>% 
    mutate_at(.vars = vars(p.value),round, digits = 5) %>% 
    #print() %>% 
    return()
}

#plan: reportar modelos sin y con interacción (falta tabla sin!)

## _hto visita*sp ----

#glm(hto. ~ edad + sexo, data = hem_cc_trd, family = gaussian(link = "identity")) %>% tidy()

glm.full <- glm(hto. ~ diff_fecha + edad + sexo + group
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

hto_01 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"diff")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="hto") %>% 
  print()

glm.full <- glm(hto. ~ num.visita + edad + sexo + group
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

hto_02 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"num.v")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="hto") %>% 
  print()

glm.full <- glm(hto. ~ diff_fecha*group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

hto_1 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"diff")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="hto") %>% 
  print()

glm.full <- glm(hto. ~ num.visita*group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

hto_2 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"num.v")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="hto") %>% 
  print()

## _leuco tiempo+sp ----
glm.full <- glm(leuco. ~ diff_fecha + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

glm.full <- glm(leuco. ~ diff_fecha + group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()


glm.full <- glm(leuco. ~ diff_fecha*group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

leuco_1 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"diff")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="leuco") %>% 
  print()


glm.full <- glm(leuco. ~ num.visita*group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

leuco_2 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"num.v")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="leuco") %>% 
  print()

## _abaston tiempo*sp ----
glm.full <- glm(abaston. ~ diff_fecha + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

glm.full <- glm(abaston. ~ diff_fecha + group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

glm.full <- glm(abaston. ~ diff_fecha*group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

abaston_1 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"diff")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="abaston") %>% 
  print()

glm.full <- glm(abaston. ~ num.visita*group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

abaston_2 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"num.v")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="abaston") %>% 
  print()

## _segment - no ----
glm.full <- glm(segment. ~ diff_fecha + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

glm.full <- glm(segment. ~ diff_fecha + group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

glm.full <- glm(segment. ~ diff_fecha*group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

segment_1 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"diff")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="segment") %>% 
  print()

glm.full <- glm(segment. ~ num.visita*group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

segment_2 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"num.v")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="segment") %>% 
  print()

## _eosinof ----
glm.full <- glm(eosinof. ~ diff_fecha + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

glm.full <- glm(eosinof. ~ diff_fecha + group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

glm.full <- glm(eosinof. ~ diff_fecha*group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

eosinof_1 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"diff")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="eosinof") %>% 
  print()

glm.full <- glm(eosinof. ~ num.visita*group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

eosinof_2 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"num.v")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="eosinof") %>% 
  print()

## _linfocit ----
glm.full <- glm(linfocit. ~ diff_fecha + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

glm.full <- glm(linfocit. ~ diff_fecha + group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

glm.full <- glm(linfocit. ~ diff_fecha*group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

linfocit_1 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"diff")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="linfocit") %>% 
  print()

glm.full <- glm(linfocit. ~ num.visita*group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

linfocit_2 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"num.v")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="linfocit") %>% 
  print()

## _plaqueta ----
glm.full <- glm(plaqueta ~ diff_fecha + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

glm.full <- glm(plaqueta ~ diff_fecha + group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

glm.full <- glm(plaqueta ~ diff_fecha*group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

plaqueta_1 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"diff")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="plaqueta") %>% 
  print()

glm.full <- glm(plaqueta ~ num.visita*group + edad + sexo
                , data = hem_cc_trd, family = gaussian(link = "identity"))
glm.full %>% tidy()
glm.full %>% confint_tidy()

plaqueta_2 <- epi_tidymodel_coef(glm.full) %>% 
  filter(str_detect(term,"num.v")) %>% 
  select(term,estimate,starts_with("conf."),p.value) %>% 
  mutate(outcome="plaqueta") %>% 
  print()


# 00_ EXPORTAR ------------------------------------------------------------

# _mix table 3 -------------------------------------------------------------

full_t3 <- hto_1 %>% 
  union_all(hto_2) %>% 
  union_all(leuco_1) %>% 
  union_all(leuco_2) %>% 
  union_all(abaston_1) %>% 
  union_all(abaston_2) %>% 
  union_all(segment_1) %>% 
  union_all(segment_2) %>% 
  union_all(eosinof_1) %>% 
  union_all(eosinof_2) %>% 
  union_all(linfocit_1) %>% 
  union_all(linfocit_2) %>% 
  union_all(plaqueta_1) %>% 
  union_all(plaqueta_2)

full_t3 %>% 
  filter(p.value<=0.1)

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
  spread(new_col,value) %>% 
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
# 
# #?geeglm
# 
# data(dietox)
# dietox$Cu     <- as.factor(dietox$Cu)
# mf <- formula(Weight~Cu*(Time+I(Time^2)+I(Time^3)))
# gee1 <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"),corstr="ar1")
# gee1
# summary(gee1)
# str(gee1)
# 
# mf2 <- formula(Weight~Cu*Time+I(Time^2)+I(Time^3))
# gee2 <- geeglm(mf2, data=dietox, id=Pig, family=poisson("identity"),corstr="unstructured")
# anova(gee2)
# class(gee2)
