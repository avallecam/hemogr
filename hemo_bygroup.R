
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
  createTable(show.all = T, show.n = T, digits = 1) %>% 
  export2xls("table/h0-tab1_null.xls")

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
  createTable(show.all = F, show.n = F, show.p.mul = T, digits = 1) %>% 
  export2xls("table/h0-tab1_groups.xls")

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
glm.full %>% tidy(conf.int=TRUE)
epitidy::epi_tidymodel_coef(model_output = glm.full,digits = 2)

#hem_cc_viv %>% count(leuco.)
glm.full <- glm(leuco. ~ group + edad + sexo
                , data = hem_cc_viv, family = gaussian(link = "identity"))
glm.full %>% tidy(conf.int=TRUE)
epitidy::epi_tidymodel_coef(model_output = glm.full,digits = 2)

#hem_cc_viv %>% count(abaston.)
glm.full <- glm(abaston. ~ group + edad + sexo
                , data = hem_cc_viv, family = gaussian(link = "identity"))
glm.full %>% tidy(conf.int=TRUE)
epitidy::epi_tidymodel_coef(model_output = glm.full,digits = 2)

#hem_cc_viv %>% count(segment.)
glm.full <- glm(segment. ~ group + edad + sexo
                , data = hem_cc_viv, family = gaussian(link = "identity"))
glm.full %>% tidy(conf.int=TRUE)
epitidy::epi_tidymodel_coef(model_output = glm.full,digits = 2)

#hem_cc_viv %>% count(eosinof.)
glm.full <- glm(eosinof. ~ group + edad + sexo
                , data = hem_cc_viv, family = gaussian(link = "identity"))
glm.full %>% tidy(conf.int=TRUE)
epitidy::epi_tidymodel_coef(model_output = glm.full,digits = 2)

# hem_cc_viv %>% count(linfocit.)
glm.full <- glm(linfocit. ~ group + edad + sexo
                , data = hem_cc_viv, family = gaussian(link = "identity"))
glm.full %>% tidy(conf.int=TRUE)
epitidy::epi_tidymodel_coef(model_output = glm.full,digits = 2)

# hem_cc_viv %>% count(plaqueta)
glm.full <- glm(plaqueta ~ group + edad + sexo
                , data = hem_cc_viv, family = gaussian(link = "identity"))
glm.full %>% tidy(conf.int=TRUE)
epitidy::epi_tidymodel_coef(model_output = glm.full,digits = 2)

# new ---

glm.full <- glm(monocit. ~ group + edad + sexo
                , data = hem_cc_fal, family = gaussian(link = "identity"))
glm.full %>% tidy(conf.int=TRUE)
epitidy::epi_tidymodel_coef(model_output = glm.full,digits = 2)

glm.full <- glm(basofil. ~ group + edad + sexo
                , data = hem_cc_fal, family = gaussian(link = "identity"))
glm.full %>% tidy(conf.int=TRUE)
epitidy::epi_tidymodel_coef(model_output = glm.full,digits = 2)

# pendientes -no urgentes- ------------------------------------------------

#' (x) actualizar modelamiento con funciones de avallecam: epi_tidymodel or epi_tidymodel_up
#' ( ) dar formato de salida en R!
#' ( ) modelos para pfal
#' ( ) exportar tablas


# plot only baseline ----------------------------------------------------------------

hem %>% 
  # filter(group!="control") %>% 
  select(-fecha,-num.visita, -edad, -sexo,-diff_fecha) %>% 
  gather(key,value,-new.code,-group) %>% 
  mutate(value=as.numeric(value)) %>% 
  
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
    group=="control"~"Negative",
    TRUE~group
  )) %>% 
  
  ggplot(aes(x = group,y = value, colour=group)) +
  geom_point(position = position_jitterdodge(),alpha=0.2)+
  geom_violin(alpha=0,lwd=0.4#,draw_quantiles = c(0.25,0.5,0.75)
              ) +
  scale_color_manual(values = c("#00BA38", "#F8766D", "#619CFF")) +
  facet_wrap(~outcome,scales = "free_y",
             labeller = label_parsed) +
  labs(title = "Baseline values of hematological profiles",
       subtitle = "Between negatives, Plasmodium falciparum and P. vivax infected subjects",
       colour="Plasmodium\nspecie infection",
       y="Value",
       x="Group")+
  theme(legend.text = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
ggsave("figure/04-group_violin-values.png",height = 6,width = 8,dpi = "retina")


# mutiples modelos multiples ----------------------------------------------

# vivax -------------------------------------------------------------------

var_dependent <- hem_cc_viv %>%
  select(group, edad, sexo) %>% 
  #transform columnames to tibble
  colnames() %>% 
  paste(., collapse=" + ")

hem_cc_viv %>%
  select(hto.:plaqueta) %>% 
  #transform columnames to tibble
  colnames() %>%
  enframe(name = NULL) %>%
  mutate(all_outcomes = map(.x = value,
                            .f = ~paste(.x,var_dependent,sep=" ~ ")),
         all_formulas = map(.x = all_outcomes,
                            .f = as.formula)) %>%
  # identity()
  # pull(all_formulas)
  mutate(all_multiple = map(.x = all_formulas,
                        .f = ~glm(formula = .x,
                                  data = hem_cc_viv, 
                                  family = gaussian(link = "identity")))) %>% 
  # pull(all_multiple)
  #tidy up the results
  mutate(simple_tidy=map(.x = all_multiple, .f = epi_tidymodel_coef)
  ) %>%
  #unnest coefficients
  unnest(cols = c(simple_tidy)) %>%
  #filter out intercepts
  filter(term!="(Intercept)") %>% 
  filter(term!="edad") %>% 
  filter(term!="sexoM") %>% 
  select(-c(all_outcomes:term,se))
