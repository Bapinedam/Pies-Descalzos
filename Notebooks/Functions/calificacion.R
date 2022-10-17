## Calificación

calificacion = function(data, claves){
  
  # Esta función tiene como propósito calificar una malla de respuestas con base
  # en un string que representa las claves.
  
  # data: Se refiere a un set de datos que contenga únicamente las malla de respuestas
  # claves: Es un vector que contiene las claves para cada columna en el mismo orden
  
  for(i in 1:dim(data)[2]){
    data[,i] = apply(data[,i], 1, function(x) {ifelse(x == claves[i], 1, 0)})
  }
  
  data

}