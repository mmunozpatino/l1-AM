# dependency; if is passed sparse = TRUE then is necessary install package
# Matrix

###
## read_matrix
# La función read_matrix recibe los argumentos:
#
# {filename}: nombre del archivo que se quiere leer
#
# {ocurrence}: TRUE mantine el número de ocurrecias del token para cada mensaje, con FALSE 
#  cada elemento de la matriz indica la presencia/ausencia, i.e. {1/0}, 
#  del token en el mensaje
#
# {sparse}: TRUE si es necesario utilizar {sparse matrix}.
#
# Por defecto {ocurrence} y {sparse} son FALSE. 
# 
# Para utilizar {sparse matrix} se debe tener instalado el paquete {Matrix}.
#
# La función {read_matrix} retorna un objeto tipo {list} con 2 componentes: 
# {tokens}: un vector con los tokens que aparecen en el dataset;
# {matrix}: un objeto de tipo {dataset} que contiene la matriz documento-palabra 
# junto con la clasificación (SPAM/NO-SPAM) de cada mensaje.
#
read_matrix <- function(filename, ocurrence=FALSE, sparse=FALSE) {
  filename.expand <- path.expand(filename)

  if (!file.exists(filename.expand))
    stop(paste("The file", filename.expand, "does not exist."))

  # get file descriptor
  fd <- file(filename.expand, 'r')

  # name of dataset; drop
  readLines(fd, 1)

  # second line is the dimension of matrix
  ## dims <- as.numeric(unlist(strsplit(readLines(fd)[2]," ")))
  dims <- as.numeric(unlist(strsplit(readLines(fd,1)," ")))
  # third lines are the tokens
  ## tokens <- unlist(strsplit(readLines(fd)[3]," "))
  tokens <- unlist(strsplit(readLines(fd,1)," "))

  # vector for label, i.e., 0 is no-span, 1 is spam
  category <- array(0, dim=c(dims[1]))

  if (sparse) {
    library(Matrix)
    matrix <- Matrix(0, dims[1], dims[2], sparse=TRUE)
  } else {
    matrix <- array(0, dim=dims)
  }

  # From line number 4 start tokens of documents
 for (m in 1:dims[1]) {
   # read a document by lines from file. Each line begin with label (0/1)
   # and end with -1
   document <- as.numeric(unlist(strsplit(readLines(fd, 1), " ")))
   document.length <- length(document) - 1

   # first value of line is label, i.e. spam/no-spam
   category[m] <- document[1]

   #  the (cumulated) even values are tokens id
   indices <- 1 + cumsum(document[seq(2, document.length, by=2)])

   # the odd values are frecuencies of tokens
   values <- document[seq(3, document.length, by=2)]

   if (ocurrence)
     matrix[m, indices]  <- values
   else
     matrix[m, indices]  <- 1
 }

  # close file
  close(fd)

  if (sparse) {
    matrix <- cbind(category, matrix)

    return(list(tokens=tokens, matrix=matrix))
  }

  # transform vector to factor
  category <- factor(category, levels=c(0,1), labels=c('SPAM', 'NO-SPAM'))

  # transform matrix to data frame and bind it factor with categories
  emails <- as.data.frame(matrix)
  emails$category <- category

  list(tokens=tokens, matrix=emails)
}
