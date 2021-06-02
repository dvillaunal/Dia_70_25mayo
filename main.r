## ---- eval=FALSE, include=TRUE--------------------------------------------------
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


## -------------------------------------------------------------------------------
#Para este ejercicio no se utlizan librerias por ende puede correr tranquilamente en el replit.


## -------------------------------------------------------------------------------
# Leemos la base de datos:
palabras <- read.table(file = "ListaPalabras_v20200520.txt",
                       header = F, stringsAsFactors = F)

# Creamos la función para ver si es isograma según un numero dado de palabras:
isograma1.0 <- function(x, num){
  "Identifica isogramas, dando un dataframe (de una columna) de palabras y un [num] = que el el numero de palabras de la lista a identificar, (el numero no debe ser mayor a 267751)"
  "No tomar un numero muy grande para que el sistema no haga bug"
  r <- 0
  t <- 0
  norep <- c()
  while (t <= num) {
    t <- t + 1
    for(i in x){
      sepa <- as.vector(strsplit(i, "")[[t]])
      q <- data.frame(table(sepa))$Freq
      ct <- 0
      for (n in q){
        if(n == 1){
          ct <- ct + 1
        }
      }
      r <- r + 1
      if(ct == length(sepa)){
        norep <- c(norep, i[r])
      }
    }
  }
  norep <- data.frame("Isogramas" = norep)
  return(norep)
}


## -------------------------------------------------------------------------------
# Ahora por cuestiones de la memoria del sistema analizaremos los primeros 200 palabras según el orden del data.frame a ver si hay isogramas:

isograma <- isograma1.0(palabras, length(palabras[[1]]))
isograma$Isogramas <- as.character(isograma$Isogramas)


## -------------------------------------------------------------------------------
# Ahora exportamos el resultado por orden alfabletico:
isograma[order(isograma$Isogramas),]

#y exportamos:
write.table(isograma, file = "IsogramaAlfa.txt", row.names = F)
 
# Ahora a mirar el conteo de palabras:

conteo <- function(x){
  "Realza un conteo palabra por palabra del vector de isogramas, para sacar cuantas letras tiene cada palabra"
  t <- 0
  l <- as.integer(length(x[[1]]))
  numlet <- c()
  while (t < l) {
    t <- t + 1
   for (i in x){
    sp <- as.vector(strsplit(i, "")[[t]])
    let <- paste(length(sp)," letras")
    numlet <- c(numlet, let)
   } 
  }
  return(numlet)
}

# Anadimos el vector que genera la funcion al data frame:
isograma$NumLetras <- conteo(isograma)

# lo volvemos factor y sacamos el conteo:
isograma$NumLetras <- as.factor(isograma$NumLetras)

# Creamos el data.frame de las frecuencias por numero de letras:
NumeroDeLetras <- data.frame(table(isograma$NumLetras))

# Renombramos las columnas:
names(NumeroDeLetras) <- c("# de Letras", "Freq")


print(NumeroDeLetras)