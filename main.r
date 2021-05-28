## ---- eval=FALSE, include=TRUE-----------------------------------------------------
## "Protocolo:
## 
##  1. Daniel Felipe Villa Rengifo
## 
##  2. Lenguaje: R
## 
##  3. Tema: Un isograma es una palabra, o frase, que no contiene letras repetidas. Utilice el archivo ListaPalabras_v20210520.txt como archivo de entrada (en la columna de referencias se indica la ruta para acceder al archivo) y genere un archivo de isogramas en orden alfabetico. Además imprima por pantalla un reporte de la cantidad de isogramas que tienen cuatro, cinco, seis y siete letras.
## 
##  4. Fuentes:
##     https://drive.google.com/drive/u/1/folders/1QfvE0DLXE5NQ3kJGEf0F7NZwUx8odv8N"


## ----------------------------------------------------------------------------------
#install.packages("tidyverse")
#library(tidyverse)


## ----------------------------------------------------------------------------------
palabras <- read.table(file = "ListaPalabras_v20200520.txt",
                       header = F, stringsAsFactors = F)


isograma1.0 <- function(x){
  r <- 0
  iso <- c()
  #ct <- 0
  t <- 0
  while (t < 23) {
    for(i in x){
      t <- t + 1
      sepa <- as.vector(strsplit(i, "")[[t]])
      q <- data.frame(table(sepa))$Freq
      ct <- 0
      for (n in q){
        if(n == 1){
          ct <- ct + 1
        }
      }
      if(ct == length(sepa)){
        r <- r + 1
        iso <- c(iso, r)
      }
    }
  }
  return(iso)
}


## ----------------------------------------------------------------------------------
isograma1.0(palabras)

## ---- eval=FALSE, include=FALSE----------------------------------------------------
## library(knitr)
## purl("Dia_70_25mayo.Rmd", "main.r")

