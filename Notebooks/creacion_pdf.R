# Dar click en run o seleccionar

rmarkdown::render(input="1.0-bapinedam-Primera_aplicacion.Rmd",
                  output_file = "../Pdf/Resultados primera aplicacion.pdf")

# Reiniciar sesi?n en R para que no haya cruce de librer?as

rmarkdown::render(input="2.0-bapinedam-Segunda_aplicacion.Rmd",
                  output_file = "../Pdf/Resultados segunda aplicacion.pdf")


