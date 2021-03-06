
# haematological profile of malaria patients



[![DOI](https://zenodo.org/badge/238349977.svg)](https://zenodo.org/badge/latestdoi/238349977)



## project structure

- `hemo_db.R` -> import and cleaning
- `hemo_bygroup.R` -> cross-sectional analysis using baseline data
- `hemo_byvisit.R` -> longitudinal analysis using folow-ups!


## método: analisis longitudinal

1. definir variable dependiente

  * Var. dependiente = cada una de las mediciones hematológicas
  * Y: gaussian distribution identity link function

2. definir variables independientes
  
  * Var. independiente [time-invariant] = infeccion (especie de plasmodium, dicotomica)
  * Var. independiente [confusores] = edad (continua), sexo (dicotómica)

3. objetivo:
  
  * test if, in average, the change of Y in time is related with a time-invariant exposure
  * this translates as including an interaction term between Y and the time-invariant exposure (Vittinghoff, 2nd edition, p271)

4. correlation structure
  * the repeated measurements were taken through time
  * only three time measurements per pacient were included
  * the measurement interval times are not equally spaced
  * autoregressive process 1 (ar1) because the correlation between times differ

5. clustered data
  * each pacient had 3 visits each with one measurement for all the haematological variables
  * dataset is in a long (tidy) format

--------

## historial

__20180103__

- recepción de las bases

- estructura:

  - Controles (n=308) del año 2010
  - Pf positivos (n=34) años 2011 y 2012
  - Pv positivos (n=93) año 2011

__20181201__

- importar y limpiar base de datos

__20190506__

- union de base de datos y ejecución de análisis descriptivos

__20200204__

- corrección por autocorrelación debido a mediciones repetidas


----------

## citar

```
@software{andree_valle_campos_2020_4014205,
  author       = {Andree Valle Campos},
  title        = {avallecam/hemogr: First release},
  month        = sep,
  year         = 2020,
  publisher    = {Zenodo},
  version      = {v0.1},
  doi          = {10.5281/zenodo.4014205},
  url          = {https://doi.org/10.5281/zenodo.4014205}
}
```
