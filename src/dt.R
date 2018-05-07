source("utils.R")

deps <- c("read-matrix.R", "tree.R")

dependencies.loader(deps)

VALUES <- list() # variable de acceso global

# Funcion auxiliar para el calculo entropia
# Parametros: 
#   values: es un vector de valores de la "variable" 

entropia <- function(p){
  
  ent <- 0
  
  total <- sum(p)


  for(i in 1:length(p)){
    if(p[i] != 0){
      ent <- ent + ((-p[i]/total) * (log2(p[i]/total)))
    }
  }
  
  return(ent)
}


# Elige el mejor atributo para clasificar los ejemplos en base a 'Information Gain'.
#
# Los parÃ¡metros son:
#
#     "examples" es el conjunto de ejemplos
#
#     "target" es un string con el nombre del atributo cuyo valor debe ser predecido por el Ã¡rbol
# Return a string with the name of the best attribute to classify the examples.
# Use la funcion "entropia" para el cÃ¡lculo de la misma

best.attribute <- function(examples, attributes, target, labels, splitInf=FALSE) {
  
  best.att <- "" #guardarÃ¡ nombre del mejor atributo
  
  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  
  dataTarget <- examples[,target]
  labelsTarget <- unique(examples[,target])
  cantsTarget <- matrix(data = 0, nrow = length(labelsTarget), ncol=2)
  
  
    #ver para muchos target!
  # print("labelstargets!")
  # print(labelsTarget)
  cants.target.vector <- c()
  
  for(i in 1:length(labelsTarget)){
    cantsTarget[i,1] <- labelsTarget[i]
    cantsTarget[i,2] <- length(which(examples[,target] == labelsTarget[i]))
    cants.target.vector <- c(cants.target.vector, length(which(examples[,target] == labelsTarget[i])))
  }
  # print("vector:")
  # print(cants.target.vector)
  entropia.target <- entropia(cants.target.vector)
  
  # print("entropia:")
  # print(entropia.target)
  
  gain.matrix <- matrix(data=0,ncol=2,nrow = length(attributes))
  

  for(i in 1:length(attributes)) {
    ## FOR POR CADA COLUMNA
    # print(paste("ATRIBUTO",attributes[i]))
    data <- matrix(data = 0, nrow = length(dataTarget), ncol=2)
    data[,1] <- examples[,attributes[i]]
    data[,2] <- dataTarget
    
    etiquetas <- unique(data[,1])
    
    entropias <- c()
    
    gain.attribute<-0
    
    gain.matrix[i,1]<- attributes[i]
    
    # print("DATA")
    # print(data)
    
    for(j in 1:length(etiquetas)) {
      ## FOR POR CADA VALOR DE LA COLUMNA
      # print(paste("ETIQUETA", etiquetas[j]))
      # print(etiquetas[j])
      dataEtiqueta <- data[which(data[,1] == etiquetas[j]),]
      cant.etiqueta <- length(which(data[,1] == etiquetas[j]))
      p <- c()
      for(t in 1:length(labelsTarget)){
        pn <- length(dataEtiqueta[which(dataEtiqueta[,2] == labelsTarget[t])])
        p <- c(p, pn)
        # print(paste("cantidad de",etiquetas[j],"con",labelsTarget[t], pn))
      }
      entropia.attribute <- entropia(p)
      gain.etiquetas <- (cant.etiqueta / length(examples[,target])) * entropia.attribute
      
      gain.attribute <- gain.attribute - gain.etiquetas
      
      # print(paste("valos para",etiquetas[j],"=",gain.attribute))
      # print(paste("gain.etiqueta=",gain.etiquetas))
    }
    
    gain.attribute <- gain.attribute + entropia.target
    
    gain.matrix[i,2] <- gain.attribute
    
    # print(paste("Ganancia de",attributes[i],"=",gain.attribute))
    # print(gain.matrix)
    
  }
  
  best.att <- gain.matrix[which(gain.matrix[,2] == max(gain.matrix[,2]))]
  # Mostrar nombre del mejor atributo best.att y su valor de ganancia (gan)
  # print(c('mejor attributo:',best.att,gan))
  
  return(best.att)
}

# Choose the most common value from a set of examples
#
# The parameters of this function are:
#     "examples" are a set of example
#
#     "target" is a string with the name of attribute whose value is to be
#     predicted by the tree.
#
# Return a string with the name of the best attribute to classify the examples.
most.common.value <- function(examples, target) {
  
  value <- NULL
  labels <- unique(examples[,target])
  cants <- matrix(data = 0, nrow = length(labels), ncol=2)
  
  for(i in 1:length(labels)){
    cants[i,1] <- labels[i]
    cants[i,2] <- length(which(examples[,target] == labels[i]))
  }
  
  value <- cants[order(cants[,2], decreasing = TRUE)[1],1]
  
  return(value)
}

# Get the decision tree from a set of examples.
#
# The parameters of this function are:
#
# 	"examples" are the set of examples
#
# 	"target" is the attribute whose value is to be predicted by the tree
#
# 	"attributes" is a vector of other attributes that may be tested by the
# 	learned decision tree.
#
#   "labels" is a vector with the labels to classify the "examples"
#
#   "tree" is data structure to save the decision tree
#
# Esta funciÃ³n regresa una lista con dos componentes: 'root', que es la variable nodo
# obtenido de la iteraciÃ³n con id3, y 'tree' que es el Ã¡rbol construido.

id3 <- function(examples, target, attributes, labels, tree) {
  
  examples <- matrix(examples,ncol=(length(attributes)+1),
                     dimnames=list(rownames=NULL,colnames=c(attributes,target)))

  #se crea una estructura vacia de nodo
  root <- NULL

  # Â¿Tienen todos los ejemplos la misma etiqueta? 
  for (i in 1:length(labels))
    if (all(examples[,target] == labels[i])) {

      class <- labels[i]
      root <- new.leaf(class)

      print(paste("leaf ", labels[i]))
      

      return(new.tree(root))
    }

  # Â¿Se encuentra vacÃ???o el conjunto de los atributos?
  if (length(attributes)==0) {

    class <- most.common.value(examples, target)
    root <- new.leaf(class)

    print(paste("leaf ", root$label))

    return(new.tree(root))
  }

  attribute <- best.attribute(examples, attributes, target, labels)
  
  # print(paste("mejor atributo:",attribute))

  root <- new.node(attribute, VALUES[[attribute]])
  print(root)
  if (is.null(tree)) tree <- new.tree(root)
  cat("attribute selected: ", attribute, "\n")

  # print(paste("values:",VALUES[[attribute]]))
  for (i in 1:length(VALUES[[attribute]])){
    ## FOR POR CADA VALOR "atributo" DEL SELECCIONADO (ejemplo sunny,rain,overcast) 
    #Add a new tree branch below Root, corresponding to the test A = vi
    branchId <- root$branches[[i]]
    # print(paste("branchId:",branchId))
    value <- as.character(VALUES[[attribute]][i])
    print(paste("root is ", root$name, "value selected: ",value))

    #Subset of examples that have value vi for A
    Anumber <- which(attribute == colnames(examples)) #column number of attribute RECORDAR atribute es el elegido
    # print(paste("Anumber:",Anumber))
    fila <- which(examples[,Anumber]==value)
    # print(paste("fila:",fila))
    examplesi <- examples[fila,] #tomo las filas que tiene sunny
    # print(paste("examplesi:",examples[fila,])) 
    # print(length(examplesi))
    #examplesi as 1 row?
    if(length(examplesi)==(length(attributes)+1)){
      examplesi <- matrix(examplesi,ncol=(length(attributes)+1),dimnames=list(rownames=NULL,colnames=c(attributes,target)))
    }

    if (is.null(examplesi) | nrow(examplesi)==0){
      # Add a leaf node with label = most common value of target in examples
  
      #######
      #
      # ADD YOUR CODE HERE
      #
      ########
      
      common.value <- most.common.value(examplesi,examplesi$target)
      #NOSE COMO PROBAR!

    } else {

      # <-- ES UNA MATRIX. Es un subconjunto de Examplesi (ver seudocÃ³digo en Mitchel.)
      exam <- NULL
      #mandar el resto de la matriz examples?
      print(paste("values actual",VALUES[[attribute]][i]))
      
      
      #######
      #
      # ADD YOUR CODE HERE
      #
      ########
      tree <- new.tree(root)
      subtree <- id3(exam, target, attributes[-Anumber], labels, NULL)
      plot.tree(tree)

      tree <- add.subtree(tree,subtree,branchId)
    }

  }

  return(tree)
  # return(NULL)
}

# Classify an example from the tree model
#
# The parameters are
#
#	"tree" the tree model obtained with learn.tree()
#	"example" one example to be classify
#
# Return the label for "example"
classify.example <- function(tree=NULL, example=NULL) 
{
  label <- NULL
  
  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  
  return(label)
}

# Realice todo el pre-procesamiento necesario de los datos en esta funciÃ³n.
# En la funciÃ³n se encuentra el pre-procesamiento del dataset PlayTennis para que 
# se obtengan estructuras necesarias para trabajar con ID3.
# Modifique esta funciÃ³n para poder manipular distintos tipos de dataset 
# (spam, restaurant,tennis)
# De esta manera se le facilitarÃ¡ la carga de datos
#
# REGRESA: 
# target.attribute: la etiqueta del atributo objetivo target 
# labels: los valores posibles de clasificaciÃ³n
# examples: matriz conjunto de ejemplos que serÃ¡n utilizados para clasificar el Ã¡rbol
# attributes: vector listado de nombres de atributos

load.data <- function(path.data="../data/",name="tennis.csv")
{
  #variables que debe completar, luego de cargar cada dataset
  target <- NULL
  labels <- NULL 
  examples<- NULL
  attributes <- NULL 
  
  if(startsWith(name,"tennis"))
  {
    path <- paste(path.data,name,sep="")
    examples <- read.csv(path,header=TRUE, stringsAsFactors=FALSE)
    examples <- as.matrix(examples)
    attributes <- as.vector(dimnames(examples)[[2]])
    attributes <- attributes[-ncol(examples)] #quita la última columna, porque tiene los true o false
    etiquetas <- unique(examples[,ncol(examples)]) #obtenemos "no" y "yes", parametros para las etiquetas
    target <- (colnames(examples))[length(colnames(examples))] #devuleve playtennis
    ## las siguientes lÃ???neas guardan por c/atributo sus correspondientes valores
    for (i in 1:length(attributes))
     VALUES[[attributes[i]]] <<- unique(examples[,attributes[i]]) # vector de vectores con los valores posibles por orden
  }
  else if(startsWith(name,"restaurant"))
  {
    path <- paste(path.data,name,sep="")
    examples <- read.csv(path,header=TRUE, stringsAsFactors=FALSE)
    examples <- as.matrix(examples)
    attributes <- as.vector(dimnames(examples)[[2]])
    attributes <- attributes[-ncol(examples)] #quita la última columna, porque tiene los true o false
    etiquetas <- unique(examples[,ncol(examples)]) #obtenemos "no" y "yes", parametros para las etiquetas
    target <- (colnames(examples))[length(colnames(examples))] #devuleve playtennis
    ## las siguientes lineas guardan por c/atributo sus correspondientes valores
    for (i in 1:length(attributes))
      VALUES[[attributes[i]]] <<- unique(examples[,attributes[i]])
    

  }else if(startsWith(name,"web_sample"))
  {
    path <- paste(path.data,name,sep="")
    examples <- read.csv(path,header=TRUE, stringsAsFactors=FALSE)
    examples <- as.matrix(examples)
    attributes <- as.vector(dimnames(examples)[[2]])
    attributes <- attributes[-ncol(examples)] #quita la última columna, porque tiene los true o false
    etiquetas <- unique(examples[,ncol(examples)]) #obtenemos "no" y "yes", parametros para las etiquetas
    target <- (colnames(examples))[length(colnames(examples))] #devuleve playtennis
    ## las siguientes lineas guardan por c/atributo sus correspondientes valores
    for (i in 1:length(attributes))
      VALUES[[attributes[i]]] <<- unique(examples[,attributes[i]])

  }else stop("ERROR Debe brindar un dataset. Verifique argumentos path.data y name")
  return (list(target.attribute=target, labels = etiquetas, examples=examples,attributes=attributes))
}

run.tree.experiment <- function(name)
{
  
  # 1- CARGA DE DATOS
  # path.data : donde se encuentra el directorio data de este laboratorio 1
  # name: nombre del dataset incluida su extensiÃ³n de archivo, ej: restaurant.csv
  # COMPLETO
  dataset <- load.data(path.data="../data/",name) 
  ## Para ver los elementos de dataset, 
  ## descomente las siguientes lÃ???neas antes de ejecutar
  print("target: ")
  print(dataset$target.attribute)
  print("labels: ")
  print(dataset$labels)
  print("attributes: ")
  print(dataset$attributes)

  
  # 2- CONSTRUCCIÃN DEL ÃRBOL USANDO ID3
  result <- id3(dataset$examples,dataset$target.attribute,dataset$attributes,dataset$labels,NULL)
  
  # La funciÃ³n plot.tree permite ver el Ã¡rbol graficado
  plot.tree(result)
  
  # 3- CLASIFICAR un nuevo ejemplo
  # Complete el argumento example con el nuevo ejemplo a clasificar 
  # (e.g example=c("v1","v2","valor3"))
  # El ejemplo dependerÃ¡ del dataset con el que estÃ© trabajando
  # Muestre en consola el ejemplo a clasificar y el resultado.
  classify.example(tree=result, example=NULL) 

}