# Dar click en run o seleccionar

rmarkdown::render(input="1.0-bapinedam-Primera_aplicacion.Rmd",
                  output_file = "../Pdf/Resultados primera aplicacion.pdf")

# Reiniciar sesión en R para que no haya cruce de librerías

rmarkdown::render(input="2.0-bapinedam-Segunda_aplicacion.Rmd",
                  output_file = "../Pdf/Resultados segunda aplicacion.pdf")


