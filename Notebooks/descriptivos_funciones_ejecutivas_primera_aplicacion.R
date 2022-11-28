## Obtención de descriptivos de la primera aplicación

################################################################################
## Librerías

# Datos

library(readxl)
library(xlsx)
library(tidyr)
library(dplyr)
library(googlesheets4)
library(stringr)

# Análsis de datos
library(likert)
library(nortest)
library(effsize)
library(psychometric)
library(ShinyItemAnalysis)

#Graficas
library(ggplot2)
library(gt)
library(DT)
library(ggtech)
library(ggthemr)
library(hrbrthemes)
library(ggthemes)

colores = c("#D94389", "#36BFB1", "#ADD96C", "#F2C230", "#F2F2F2")

#Funciones propias

source("./Functions/min_max_scaler.R")
source("./Functions/calificacion.R")
source("./Functions/change_to_zero.R")


# Otros
options(digits=5, scipen = 50)
set.seed(321)

###############################################################################
# Obtención de datos

url_pre = paste("https://docs.google.com/spreadsheets/d",
                "/1Ry73ckzruTQxtDnkCSscs16M3cv3u9XgJlcg4sN94BE/edit?usp=sharing",
                sep = "")

url_post = paste("https://docs.google.com/spreadsheets/d/",
                 "1tFkxbH5N9HMr5qRA-VF4JXh-MAJqW38-iidB4QBuKU4/edit#gid=1877736074",
                 sep = "")

################################################################################
# Funciones ejecutivas

## Memoria auditiva

### Pre

mem_audi_pre = read_sheet(url_pre, sheet = "Memoria Audi")
vector = c(colnames(mem_audi_pre)[1:6], as.vector(unlist(mem_audi_pre[1,][7:30])))
colnames(mem_audi_pre) = vector
mem_audi_pre = mem_audi_pre[-1,1:length(vector)]
mem_audi_pre = filter(mem_audi_pre, 
                      !is.na(`Código`), 
                      !is.na(Vaca),
                      Vaca != 'NULL')
item_1_pre = c("Caballo","Perro","Aguila","Pollito",
               "Foca","Vaca","Cocodrilo","Sapo")

item_2_pre = c("Oveja","Elefante","Tiburon" ,"Caracol",
               "Ratón" ,"Gato","Tortuga","Pez")

incorrectos_1_pre = c("León",	"Oso", "Ballena",	"Lagarto")
incorrectos_2_pre = c("Mariposa", "Mono",	"Jirafa",	"Pantera")

mem_audi_pre[,7:30] = apply(mem_audi_pre[,7:30], 2,
                            function(x) str_replace_all(x,'NULL', '0'))

mem_audi_pre$`Aciertos item 1_pre` = apply(apply(mem_audi_pre[,item_1_pre], 2,
                                                 function(x) as.numeric(x)),
                                           1, function(x) sum(x, na.rm= FALSE))

mem_audi_pre$`Errores item 1_pre` = apply(apply(mem_audi_pre[,incorrectos_1_pre],
                                                2, function(x) as.numeric(x)),
                                          1, function(x) sum(x, na.rm= FALSE))



mem_audi_pre$`Aciertos item 2_pre` = apply(apply(mem_audi_pre[,item_2_pre], 2,
                                                 function(x) as.numeric(x)),
                                           1, function(x) sum(x, na.rm= FALSE))

mem_audi_pre$`Errores item 2_pre` = apply(apply(mem_audi_pre[,incorrectos_2_pre],
                                                2, function(x) as.numeric(x)),
                                          1, function(x) sum(x, na.rm= FALSE))

mem_audi_pre$`Puntaje item 1_pre` = 
  mem_audi_pre$`Aciertos item 1_pre` - mem_audi_pre$`Errores item 1_pre`

mem_audi_pre$`Puntaje item 2_pre` = 
  mem_audi_pre$`Aciertos item 2_pre` - mem_audi_pre$`Errores item 2_pre`


mem_audi_pre$`Puntaje item 1_pre` = apply(mem_audi_pre[,"Puntaje item 1_pre"], 1,
                                          function(x) change_to_zero(x))

mem_audi_pre$`Puntaje item 2_pre` = apply(mem_audi_pre[,"Puntaje item 2_pre"], 1,
                                          function(x) change_to_zero(x))

### Post

mem_audi_post = read_sheet(url_post, sheet = "Memoria Audi")
vector = c(colnames(mem_audi_post)[1:6], as.vector(unlist(mem_audi_post[1,][7:30])))
colnames(mem_audi_post) = vector
mem_audi_post = mem_audi_post[-1,1:length(vector)]
mem_audi_post = filter(mem_audi_post, 
                       !is.na(`Código`), 
                       !is.na(Burro),
                       Burro != 'NULL')

item_1_post = c("Caballo","Gallina", "Conejo", 
                "Leopardo", "Ardilla", "Cerdo", "Cangrejo", "Burro")

item_2_post = c("Estrella", "Gato", "Abeja", 
                "Cebra", "Rana", "Armadillo", "Tortuga", "Iguana")

incorrectos_1_post = c("Perro",	"Cabra", "Avestruz",	"Hormiga")
incorrectos_2_post = c("Toro", "Delfín",	"Loro",	"Camello")

mem_audi_post[,7:30] = apply(mem_audi_post[,7:30], 2,
                             function(x) str_replace_all(x,'NULL', '0'))

mem_audi_post$`Aciertos item 1_post` = apply(apply(mem_audi_post[,item_1_post], 2,
                                                   function(x) as.numeric(x)),
                                             1, function(x) sum(x, na.rm= FALSE))

mem_audi_post$`Errores item 1_post` = apply(apply(mem_audi_post[,incorrectos_1_post], 
                                                  2, function(x) as.numeric(x)),
                                            1, function(x) sum(x, na.rm= FALSE))


mem_audi_post$`Aciertos item 2_post` = apply(apply(mem_audi_post[,item_2_post], 2,
                                                   function(x) as.numeric(x)),
                                             1, function(x) sum(x, na.rm= FALSE))

mem_audi_post$`Errores item 2_post` = apply(apply(mem_audi_post[,incorrectos_2_post], 
                                                  2, function(x) as.numeric(x)),
                                            1, function(x) sum(x, na.rm= FALSE))

mem_audi_post$`Puntaje item 1_post` = 
  mem_audi_post$`Aciertos item 1_post` - mem_audi_post$`Errores item 1_post`

mem_audi_post$`Puntaje item 2_post` = 
  mem_audi_post$`Aciertos item 2_post` - mem_audi_post$`Errores item 2_post`


mem_audi_post$`Puntaje item 1_post` = apply(mem_audi_post[,"Puntaje item 1_post"],
                                            1, function(x) change_to_zero(x))

mem_audi_post$`Puntaje item 2_post` = apply(mem_audi_post[,"Puntaje item 2_post"],
                                            1, function(x) change_to_zero(x))

### Totales

mem_audi_pre$Total_pre = mem_audi_pre$`Puntaje item 2_pre` +
                          mem_audi_pre$`Puntaje item 1_pre`

mem_audi_post$Total_post = mem_audi_post$`Puntaje item 2_post` +
                            mem_audi_post$`Puntaje item 1_post`

pre_post = inner_join(mem_audi_post, 
                      dplyr::select(mem_audi_pre, c("Código", "Aciertos item 1_pre", 
                                                    "Errores item 1_pre",  "Aciertos item 2_pre",
                                                    "Errores item 2_pre","Puntaje item 1_pre",
                                                    "Puntaje item 2_pre", "Total_pre")),
                      by = "Código")

summ = data.frame(unclass(psych::describe(pre_post[,c("Aciertos item 1_pre", 
                                                      "Errores item 1_pre",  "Aciertos item 2_pre",
                                                      "Errores item 2_pre","Puntaje item 1_pre",
                                                      "Puntaje item 2_pre", "Total_pre",
                                                      "Aciertos item 1_post", 
                                                      "Errores item 1_post",  "Aciertos item 2_post",
                                                      "Errores item 2_post","Puntaje item 1_post",
                                                      "Puntaje item 2_post", "Total_post")])),
                  check.names = FALSE, stringsAsFactors = FALSE) 

summ$vars = c("Aciertos item 1_pre", 
              "Errores item 1_pre",  "Aciertos item 2_pre",
              "Errores item 2_pre","Puntaje item 1_pre",
              "Puntaje item 2_pre", "Total_pre",
              "Aciertos item 1_post", 
              "Errores item 1_post",  "Aciertos item 2_post",
              "Errores item 2_post","Puntaje item 1_post",
              "Puntaje item 2_post", "Total_post")

summ$Prueba = "Memoria auditiva"

descriptivos_primera_aplicacion = summ

## Memoria visual

### Pre

mem_vis_pre = read_sheet(url_pre, sheet = "Memoria Vis")
vector = c(colnames(mem_vis_pre)[1:6], as.vector(unlist(mem_vis_pre[1,][7:22])))
colnames(mem_vis_pre) = vector
mem_vis_pre = mem_vis_pre[-1,1:length(vector)]
mem_vis_pre = filter(mem_vis_pre,
                     !is.na(`Código`),
                     !is.na(Gym),
                     Gym != 'NULL')

mem_vis_pre[,7:22] = apply(mem_vis_pre[,7:22], 2,
                           function(x) str_replace_all(x,'NULL', '0'))

item_1_pre = c("Gym","Museo","Cajas")
item_2_pre = c("Salón","Aeropuerto","Iglesia")

incorrectos_1_pre = c("Cine",	"Lavadoras",	"Oficina")
incorrectos_2_pre = c("Sala",		"Paradero",	"Café")

mem_vis_pre$`Aciertos item 1_pre` = apply(apply(mem_vis_pre[,item_1_pre], 2,
                                                function(x) as.numeric(x)),
                                          1, function(x) sum(x, na.rm= FALSE))

mem_vis_pre$`Errores item 1_pre` = apply(apply(mem_vis_pre[,incorrectos_1_pre], 2,
                                               function(x) as.numeric(x)),
                                         1, function(x) sum(x, na.rm= FALSE))

mem_vis_pre$`Aciertos item 2_pre` = apply(apply(mem_vis_pre[,item_2_pre], 2,
                                                function(x) as.numeric(x)),
                                          1, function(x) sum(x, na.rm= FALSE))

mem_vis_pre$`Errores item 2_pre` = apply(apply(mem_vis_pre[,incorrectos_2_pre], 2,
                                               function(x) as.numeric(x)),
                                         1, function(x) sum(x, na.rm= FALSE))

mem_vis_pre$`Puntaje item 1_pre` = 
  mem_vis_pre$`Aciertos item 1_pre` - mem_vis_pre$`Errores item 1_pre`

mem_vis_pre$`Puntaje item 2_pre` = 
  mem_vis_pre$`Aciertos item 2_pre` - mem_vis_pre$`Errores item 2_pre`

mem_vis_pre$`Puntaje item 1_pre` = apply(mem_vis_pre[,"Puntaje item 1_pre"], 1,
                                         function(x) change_to_zero(x))

mem_vis_pre$`Puntaje item 2_pre` = apply(mem_vis_pre[,"Puntaje item 2_pre"], 1,
                                         function(x) change_to_zero(x))

### Post

mem_vis_post = read_sheet(url_post, sheet = "Memoria Vis")
vector = c(colnames(mem_vis_post)[1:6], as.vector(unlist(mem_vis_post[1,][7:22])))
colnames(mem_vis_post) = vector

mem_vis_post = mem_vis_post[-1,1:length(vector)]
mem_vis_post = filter(mem_vis_post,
                      !is.na(`Código`),
                      # !is.null(Cancha),
                      Iglesia != 'NULL')

mem_vis_post[,7:22] = apply(mem_vis_post[,7:22], 2,
                            function(x) str_replace_all(x,'NULL', '0'))

item_1_post = c("Iglesia",	"GYM",	"Zoo")
item_2_post = c("Vet",		"Playa",	"Fruteria")

incorrectos_1_post = c("Cancha","Colegio_","Par. Diversiones")
incorrectos_2_post = c("Aeropuerto","Helado","Parque")

mem_vis_post$`Aciertos item 1_post` = apply(apply(mem_vis_post[,item_1_post], 2,
                                                  function(x) as.numeric(x)),
                                            1, function(x) sum(x, na.rm= FALSE))

mem_vis_post$`Errores item 1_post` = apply(apply(mem_vis_post[,incorrectos_1_post],
                                                 2, function(x) as.numeric(x)),
                                           1, function(x) sum(x, na.rm= FALSE))

mem_vis_post$`Aciertos item 2_post` = apply(apply(mem_vis_post[,item_2_post], 2,
                                                  function(x) as.numeric(x)),
                                            1, function(x) sum(x, na.rm= FALSE))

mem_vis_post$`Errores item 2_post` = apply(apply(mem_vis_post[,incorrectos_2_post],
                                                 2, function(x) as.numeric(x)),
                                           1, function(x) sum(x, na.rm= FALSE))

mem_vis_post$`Puntaje item 1_post` = 
  mem_vis_post$`Aciertos item 1_post` - mem_vis_post$`Errores item 1_post`

mem_vis_post$`Puntaje item 2_post` = 
  mem_vis_post$`Aciertos item 2_post` - mem_vis_post$`Errores item 2_post`

mem_vis_post$`Puntaje item 1_post` = apply(mem_vis_post[,"Puntaje item 1_post"], 1,
                                           function(x) change_to_zero(x))

mem_vis_post$`Puntaje item 2_post` = apply(mem_vis_post[,"Puntaje item 2_post"], 1,
                                           function(x) change_to_zero(x))

### Totales

mem_vis_pre$Total_pre = mem_vis_pre$`Puntaje item 2_pre` +
                         mem_vis_pre$`Puntaje item 1_pre`

mem_vis_post$Total_post = mem_vis_post$`Puntaje item 2_post` +
                          mem_vis_post$`Puntaje item 1_post`

pre_post = inner_join(mem_vis_post, 
                      dplyr::select(mem_vis_pre, c("Código", "Aciertos item 1_pre", 
                                                    "Errores item 1_pre",  "Aciertos item 2_pre",
                                                    "Errores item 2_pre","Puntaje item 1_pre",
                                                    "Puntaje item 2_pre", "Total_pre")),
                      by = "Código")

summ = data.frame(unclass(psych::describe(pre_post[,c("Aciertos item 1_pre", 
                                                      "Errores item 1_pre",  "Aciertos item 2_pre",
                                                      "Errores item 2_pre","Puntaje item 1_pre",
                                                      "Puntaje item 2_pre", "Total_pre",
                                                      "Aciertos item 1_post", 
                                                      "Errores item 1_post",  "Aciertos item 2_post",
                                                      "Errores item 2_post","Puntaje item 1_post",
                                                      "Puntaje item 2_post", "Total_post")])),
                  check.names = FALSE, stringsAsFactors = FALSE)

summ$vars = c("Aciertos item 1_pre", 
              "Errores item 1_pre",  "Aciertos item 2_pre",
              "Errores item 2_pre","Puntaje item 1_pre",
              "Puntaje item 2_pre", "Total_pre",
              "Aciertos item 1_post", 
              "Errores item 1_post",  "Aciertos item 2_post",
              "Errores item 2_post","Puntaje item 1_post",
              "Puntaje item 2_post", "Total_post")

summ$Prueba = "Memoria visual"

descriptivos_primera_aplicacion = rbind(descriptivos_primera_aplicacion, summ)

## Inhibición

### Pre

inhibicion_pre = read_sheet(url_pre, sheet = "Inhibiciòn")

col_items = paste(rep("Item", 26), rep(1:2, each = 13), 
                  rep(paste("_", rep(1:13, 2))))

vector = c(colnames(inhibicion_pre)[1:6], col_items)
colnames(inhibicion_pre) = vector

inhibicion_pre = inhibicion_pre[,1:32]

inhibicion_pre = filter(inhibicion_pre,
                        !is.na(`Código`),
                        !is.na(`Item 1 _ 1`))

inhibicion_pre[,col_items] = apply(inhibicion_pre[,col_items], 2, 
                                   function(x) str_to_upper(x))

claves_inhibicion_pre = c('N', 'M',	'M',	'N',	'N',	'M',	'N',	
                          'M',	'N',	'M',	'M',	'N',	'M',	'N',	
                          'M',	'N',	'M',	'N',	'M',	'M',	'M',	'N',	
                          'M',	'N',	'N',	'M')


inhibicion_pre[,7:32] = calificacion(inhibicion_pre[,7:32], claves_inhibicion_pre)

inhibicion_pre$`Aciertos item 1_pre` =  apply(inhibicion_pre[,7:19], 1, 
                                              function(x) sum(x, na.rm= FALSE))

inhibicion_pre$`Aciertos item 2_pre` =  apply(inhibicion_pre[,20:32], 1, 
                                              function(x) sum(x, na.rm= FALSE))


### Post

inhibicion_post = read_sheet(url_post, sheet = "Inhibición")

col_items = paste(rep("Item", 20), rep(1:2, each = 10), 
                  rep(paste("_", rep(1:10, 2))))

vector = c(colnames(inhibicion_post)[1:6], col_items)
colnames(inhibicion_post) = vector

inhibicion_post = inhibicion_post[,1:26]

inhibicion_post = filter(inhibicion_post,
                         !is.na(`Código`),
                         !is.na(`Item 1 _ 1`))

inhibicion_post[,col_items] = apply(inhibicion_post[,col_items], 2, 
                                    function(x) str_to_upper(x))

claves_inhibicion_post = c('E',	'S', 'S',	'E', 'S', 'E', 'E',	'S', 'E',	'S', 
                           'S',	'S', 'E',	'S', 'E',	'S', 'E',	'S',	'E', 'E')


inhibicion_post[,7:26] = calificacion(inhibicion_post[,7:26], claves_inhibicion_post)

inhibicion_post$`Aciertos item 1_post` =  apply(inhibicion_post[,7:16], 1,
                                                function(x) sum(x, na.rm= FALSE))

inhibicion_post$`Aciertos item 2_post` =  apply(inhibicion_post[,17:26], 1,
                                                function(x) sum(x, na.rm= FALSE))

### Totales

inhibicion_pre$Total_pre = inhibicion_pre$`Aciertos item 2_pre`
inhibicion_post$Total_post = inhibicion_post$`Aciertos item 2_post`

pre_post = inner_join(inhibicion_post, 
                      dplyr::select(inhibicion_pre, c("Código", "Total_pre")),
                      by = "Código")

summ = data.frame(unclass(psych::describe(pre_post[,c("Total_pre", "Total_post")])),
                  check.names = FALSE, stringsAsFactors = FALSE) 

summ$vars = c("Total_pre", "Total_post")

summ$Prueba = "Inhibición"

descriptivos_primera_aplicacion = rbind(descriptivos_primera_aplicacion, summ)

## Flexibilidad

### Pre

flexibilidad_pre = read_sheet(url_pre, sheet = "Flexibilidad")

col_items = paste(rep("Item", 26), rep(1:2, each = 13), 
                  rep(paste("_", rep(1:13, 2))))

vector = c(colnames(flexibilidad_pre)[1:6], col_items)

colnames(flexibilidad_pre) = vector

flexibilidad_pre = flexibilidad_pre[,1:32]

flexibilidad_pre = filter(flexibilidad_pre,
                          !is.na(`Código`),
                          !is.na(`Item 1 _ 1`))

flexibilidad_pre[,col_items] = apply(flexibilidad_pre[,col_items], 2, 
                                     function(x) str_to_upper(x))

claves_flexibilidad_pre = c('A', 'M', 'N', 'A', 'N', 'M', 'M', 'N', 'A',
                            'A', 'N', 'M', 'N', 'N', '3', 'M', 'M',	'3',
                            'N', 'N', 'M', '3', '3', 'M',	'N', 'M')


flexibilidad_pre[,7:32] = calificacion(flexibilidad_pre[,7:32],
                                       claves_flexibilidad_pre)

flexibilidad_pre$`Aciertos item 1_pre` =  apply(flexibilidad_pre[,7:19], 1,
                                                function(x) sum(x, na.rm= FALSE))

flexibilidad_pre$`Aciertos item 2_pre` =  apply(flexibilidad_pre[,20:32], 1,
                                                function(x) sum(x, na.rm= FALSE))

### Post

flexibilidad_post = read_sheet(url_post, sheet = "Flexibilidad")

col_items = paste(rep("Item", 20), rep(1:2, each = 10), 
                  rep(paste("_", rep(1:10, 2))))

vector = c(colnames(flexibilidad_post)[1:6], col_items)

colnames(flexibilidad_post) = vector

flexibilidad_post = flexibilidad_post[,1:26]

flexibilidad_post = filter(flexibilidad_post,
                           !is.na(`Código`),
                           !is.na(`Item 1 _ 1`))

flexibilidad_post[,col_items] = apply(flexibilidad_post[,col_items], 2, 
                                      function(x) str_to_upper(x))

claves_flexibilidad_post = c('F', 'S', 'E', 'E', 'F', 'S', 'F', 'E', 'F', 'S', 
                             'S', '3', 'E', 'E', '3', 'S', 'S', '3', 'E', 'S')

flexibilidad_post[,7:26] = calificacion(flexibilidad_post[,7:26],
                                        claves_flexibilidad_post)

flexibilidad_post$`Aciertos item 1_post` =  apply(flexibilidad_post[,7:16], 1,
                                                  function(x) sum(x, na.rm= FALSE))

flexibilidad_post$`Aciertos item 2_post` =  apply(flexibilidad_post[,17:26], 1,
                                                  function(x) sum(x, na.rm= FALSE))

### Totales

flexibilidad_pre$Total_pre = flexibilidad_pre$`Aciertos item 2_pre`
flexibilidad_post$Total_post = flexibilidad_post$`Aciertos item 2_post`

pre_post = inner_join(flexibilidad_post, 
                      dplyr::select(flexibilidad_pre, c("Código", "Total_pre")),
                      by = "Código")

summ = data.frame(unclass(psych::describe(pre_post[,c("Total_pre", "Total_post")])),
                  check.names = FALSE, stringsAsFactors = FALSE) 

summ$vars = c("Total_pre", "Total_post")

summ$Prueba = "Flexibilidad"

descriptivos_primera_aplicacion = rbind(descriptivos_primera_aplicacion, summ)

################################################################################
#Exporte final

write.xlsx(descriptivos_primera_aplicacion, 
           "../Data/processed/primera_aplicacion/Descriptivos Primera Aplicación - Funciones ejecutivas.xlsx", 
           row.names = FALSE)
