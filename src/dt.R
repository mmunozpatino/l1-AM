source("utils.R")

deps <- c("read-matrix.R", "tree.R")

dependencies.loader(deps)

VALUES <- list() # variable de acceso global

# Funcion auxiliar para el calculo entropia
# Parametros: 
#   values: es un vector de valores de la "variable" 

entropia <- function(values){
  
  ent <- 0
  total <- sum(values)
  
  #######
  total <- sum(values)
  
  for(i in 1:length(values)) {
    if(values[i]!=0){
      ent <- ent + ((- values[i]/total) * log2(values[i]/total))
    }
  }
    
    return(ent)
  
  
  ########
  
  #descomente si desea que el valor sea mostrado en pantalla
  #print(c("calculo entropia",ent))
  
  return(ent)
  
  
  ########
  
  #descomente si desea que el valor sea mostrado en pantalla
  #print(c("calculo entropia",ent))
  
}

# Elige el mejor atributo para clasificar los ejemplos en base a 'Information Gain'.
#
# Los parámetros son:
#
#     "examples" es el conjunto de ejemplos
#
#     "target" es un string con el nombre del atributo cuyo valor debe ser predecido por el árbol
# Return a string with the name of the best attribute to classify the examples.
# Use la funcion "entropia" para el cálculo de la misma

best.attribute <- function(examples, attributes, target, labels, splitInf=FALSE) {
  
  best.att <- "" #guardará nombre del mejor atributo
  
  #######
  
  best.att <- "" #guardará nombre del mejor atributo

  dataTarget <- examples[,target]
  labelsTarget <- unique(examples[,target])
  canTotal <- length(dataTarget)
  cants <- matrix(data= 0, nrow = length(labelsTarget),ncol=2)
  
  for(i in 1:length(labelsTarget)){
    cants[i,1] <- labelsTarget[i]
    cants[i,2] <- length(which(examples[,target] == labelsTarget[i]))
  }
  entropia.target <- entropia(as.numeric(cants[,2]))
  # print(entropia.target)
  
  gain.matrix <- matrix(data=0, nrow=length(attributes),ncol =2)
  
  #atributos
 
  
  for(i in 1:length(attributes)){
    
    data <- matrix(data=0,nrow = length(dataTarget), ncol= 2)
    data[,1] <- examples[,attributes[i]]
    data[,2] <- dataTarget
    
    etiqueta <- unique(data[,1])
    entropias.vector <- c()
    splitvalue <- 0
    
    for(j in 1:length(etiqueta)){
      dataEtiqueta <- matrix(data=0, nrow= length(data[which(data[,1] == etiqueta[j])]), ncol=2)
      dataEtiqueta[,1] <- data[which(data[,1] == etiqueta[j]),1]
      dataEtiqueta[,2] <- data[which(data[,1] == etiqueta[j]),2] #cortamos la tabla por etiqueta. Por ej: Sunny
      # print("DataEtiqueta")
      # print(dataEtiqueta)
      cantDataEtiqueta <- nrow(dataEtiqueta) # cantidad de valores con la etiqueta 
      p <- c()
      
      for(k in 1:length(labelsTarget)){
        if(nrow(dataEtiqueta)>1){
          pm <- length(dataEtiqueta[which(dataEtiqueta[,2] == labelsTarget[k])]) #cuenta cuantos si/no hay
        }else{
          pm <- 1
        }
        
        p <- c(p,pm) #los guarda en un arreglo que es el que va a la entropia
        #print(paste("P",p))
      }
      splitvalue <- splitvalue - (cantDataEtiqueta / canTotal)* log2(cantDataEtiqueta / canTotal)
      
      entropia.attribute <- entropia(p)
      entropias.vector <- c(entropias.vector,(cantDataEtiqueta/canTotal) * entropia.attribute)
      
      #print(dataEtiqueta[j,1])
      #print(entropia.attribute)
      #print(entropias.vector)
    }
    gain.matrix[i,1] <- attributes[i]
    
    gain.matrix[i,1] <- attributes[i]
    gain.matrix[i,2] <- entropia.target - sum(entropias.vector)
    
    if(splitInf){
      gain.matrix[i,2] <- (entropia.target - sum(entropias.vector))/splitvalue
    }else{
      gain.matrix[i,2] <- entropia.target - sum(entropias.vector)
    }
  }
  
  #print(gain.matrix)
  best.att <- gain.matrix[order(gain.matrix[,2],decreasing = TRUE)[1],1]
  gan <- gain.matrix[order(gain.matrix[,2], decreasing = TRUE)[1],2]
  
  
  ########
  # Mostrar nombre del mejor atributo best.att y su valor de ganancia (gan)
  
  print("------------------------------------------")  
  print(c('mejor attributo:',best.att,gan))
  print("------------------------------------------")
  
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
  cants <- matrix(data= 0, nrow = length(labels),ncol=2)
  
  for(i in 1:length(labels)){
    cants[i,1] <- labels[i]
    cants[i,2] <- length(which(examples[,target] == labels[i]))
  }
  
  #print(cants)
  value <- cants[order(cants[,2],decreasing=TRUE)[1],1] #Devuelve el [1] porque quiero el primer valor que es el mas comun
  
  
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
# Esta función regresa una lista con dos componentes: 'root', que es la variable nodo
# obtenido de la iteración con id3, y 'tree' que es el árbol construido.

id3 <- function(examples, target, attributes, labels, tree) {
  
  examples <- matrix(examples,ncol=(length(attributes)+1),
                     dimnames=list(rownames=NULL,colnames=c(attributes,target)))
  
  #se crea una estructura vacia de nodo
  root <- NULL
  
  # ¿Tienen todos los ejemplos la misma etiqueta? 
  for (i in 1:length(labels))
    if (all(examples[,target] == labels[i])) {
      
      class <- labels[i]
      root <- new.leaf(class)
      
      # print(paste("leaf ", labels[i]))
      
      
      return(new.tree(root))
    }
  
  # ¿Se encuentra vac�?o el conjunto de los atributos?
  if (length(attributes)==0) {
    
    class <- most.common.value(examples, target)
    root <- new.leaf(class)
    
    # print(paste("leaf ", root$label))
    
    return(new.tree(root))
  }
  
  attribute <- best.attribute(examples, attributes, target, labels)
  
  
  root <- new.node(attribute, VALUES[[attribute]])
  
  if (is.null(tree)) tree <- new.tree(root) #Si el arbol esta vacio agrego la raiz
  # cat("attribute selected: ", attribute, "\n")
  #print(root)
  
  #Va a recorrer cada valor de cada atributo -> por ejemplo para Outlook "Sunny", Overcast y Rain
  for (i in 1:length(VALUES[[attribute]])){  
    
    #Add a new tree branch below Root, corresponding to the test A = vi
    branchId <- root$branches[[i]] 
    
    #Selecciona el primer valor que puede tomar la variable
    value <- as.character(VALUES[[attribute]][i])
    # print(paste("root is ", root$name, "value selected: ",value))
    
    
    #Subset of examples that have value vi for A
    Anumber <- which(attribute == colnames(examples)) #column number of attribute
    #print(Anumber)
    
    fila <- which(examples[,Anumber]==value) #indices de las filas que son el valor del atributo analizado
    #print(fila)
    
    examplesi <- examples[fila,] #deja todas las filas del valor del atributo completas
    # print(examplesi)
    
    #examplesi as 1 row?
    if(length(examplesi)==(length(attributes)+1)){
      examplesi <- matrix(examplesi,ncol=(length(attributes)+1),dimnames=list(rownames=NULL,colnames=c(attributes,target)))
    }
    
    if (is.null(examplesi) | nrow(examplesi)==0){
      # Add a leaf node with label = most common value of target in examples
      
      #######
      
      class <- most.common.value(examples, target)
      leaf <- new.leaf(class)
      
      tree <- add.subtree(tree, leaf, branchId)
      
      ########
      
    } else {
      
      # <-- ES UNA MATRIX. Es un subconjunto de Examplesi (ver seudocódigo en Mitchel.)
      exam <- NULL
      
      #######
      
      exam <- examplesi[,-Anumber]
      
      ########
      
      subtree <- id3(exam, target, attributes[-Anumber], labels, NULL)
      
      tree <- add.subtree(tree,subtree,branchId)
    }    
    
  }  
  
  return(tree)
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
  parent <- NULL
  
  if(is.null(parent)){ #Es la ra�z?
    
    varExam <- NULL
    
    # Itero en example hasta que coincida con la raiz
    for(i in 1:length(example)){
      #print(names(tree$nodes[[1]]$branches))
      
      #any -> verifica si en un arreglo al menos alguno de los valores es verdadero. 
      
      #Si alguno de los valores del ejemplo est� presente en alguna de las ramas del arbol
      if(any(example[i] == names(tree$nodes[[1]]$branches))){
        
        #Guardo la variable del ejemplo que coincide con la raiz
        varExam <- example[which(any(example[i] == names(tree$nodes[[1]]$branches)))]
        break
      }
    }
    
    #Guardo el parentId de la raiz
    parent <- tree$nodes[[1]]$branches[which(names(tree$nodes[[1]]$branches) == varExam)]
    
  }
  
  # Si no es la raiz tengo que recorrer el resto del arbol
  # Itero sobre los nodos del arbol
  for(f in 2:tree$nodesCount){
    
    # Es la ra�z del subasrbol?
    if(tree$nodes[[f]]$parentId == parent){
      
      # Es una hoja?
      if(!is.null(tree$nodes[[f]]$label)){
        # print(paste("Resultado: ", tree$nodes[[f]]$label))
        label <- tree$nodes[[f]]$label
        break
      }
      
      # No es una hoja, es un branch
      # Determino por cual nodo debo seguir recorriendo
      
      for(i in 1:length(example)){
        # Verifico si algun valor del ejemplo se corresponde con algun branch para seguir recorriendo
        if(any(names(tree$nodes[[f]]$branches) == example[i])){
          
          #Guardo el parentId
          parent <- tree$nodes[[f]]$branches[which(example[i] == names(tree$nodes[[f]]$branches))] 
          break
        }
      }
      # print(label)
      
    }
  }
  ########
  
  return(label)
}

# Realice todo el pre-procesamiento necesario de los datos en esta función.
# En la función se encuentra el pre-procesamiento del dataset PlayTennis para que 
# se obtengan estructuras necesarias para trabajar con ID3.
# Modifique esta función para poder manipular distintos tipos de dataset 
# (spam, restaurant,tennis)
# De esta manera se le facilitará la carga de datos
#
# REGRESA: 
# target.attribute: la etiqueta del atributo objetivo target 
# labels: los valores posibles de clasificación
# examples: matriz conjunto de ejemplos que serán utilizados para clasificar el árbol
# attributes: vector listado de nombres de atributos

load.data <- function(path.data="../data/",name="tennis.csv")
{
  #variables que debe completar, luego de cargar cada dataset
  target <- NULL
  labels <- NULL 
  examples<- NULL
  attributes <- NULL 
  
  if(startsWith(name,"tennis")){
    path <- paste(path.data,name,sep="")
    examples <- read.csv(path,header=TRUE, stringsAsFactors=FALSE)
    examples <- as.matrix(examples)
    View(examples)
    attributes <- as.vector(dimnames(examples)[[2]])
    attributes <- attributes[-ncol(examples)] #quita la �ltima columna, porque tiene los true o false
    etiquetas <- unique(examples[,ncol(examples)]) #obtenemos "no" y "yes", parametros para las etiquetas
    target <- (colnames(examples))[length(colnames(examples))] #devuleve playtennis
    ## las siguientes l�?neas guardan por c/atributo sus correspondientes valores
    for (i in 1:length(attributes))
      VALUES[[attributes[i]]] <<- unique(examples[,attributes[i]]) # vector de vectores con los valores posibles por orden
  }
  else if(startsWith(name,"restaurant"))
  {
    path <- paste(path.data,name,sep="")
    examples <- read.csv(path,header=TRUE, stringsAsFactors=FALSE)
    examples <- as.matrix(examples)
    attributes <- as.vector(dimnames(examples)[[2]])
    attributes <- attributes[-ncol(examples)] #quita la �ltima columna, porque tiene los true o false
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
    attributes <- attributes[-ncol(examples)] #quita la �ltima columna, porque tiene los true o false
    etiquetas <- unique(examples[,ncol(examples)]) #obtenemos "no" y "yes", parametros para las etiquetas
    target <- (colnames(examples))[length(colnames(examples))] #devuleve playtennis
    ## las siguientes lineas guardan por c/atributo sus correspondientes valores
    for (i in 1:length(attributes))
      VALUES[[attributes[i]]] <<- unique(examples[,attributes[i]])
    
  }else if(startsWith(name,"SPAM")){
    source("read-matrix.R") 
    m.train <- read_matrix(filename="../data/MATRIX.TRAIN.50",ocurrence=FALSE,sparse=FALSE)
    examples <- as.matrix(m.train$matrix)
    for(j in 1:(ncol(examples)-1)){
      dimnames(examples)[[2]][j] <- m.train$tokens[j]
    }
    attributes <- as.vector(dimnames(examples)[[2]])
    attributes <- attributes[-ncol(examples)]
    etiquetas <- unique(examples[,ncol(examples)])
    target <- (colnames(examples))[length(colnames(examples))]
    for(i in 1:length(attributes))
      VALUES[[attributes[i]]] <<- unique(examples[,attributes[i]])
    
  }else stop("ERROR Debe brindar un dataset. Verifique argumentos path.data y name")
  return (list(target.attribute=target, labels = etiquetas, examples=examples,attributes=attributes))
}

run.tree.experiment <- function(name){
  
  # 1- CARGA DE DATOS
  # path.data : donde se encuentra el directorio data de este laboratorio 1
  # name: nombre del dataset incluida su extensión de archivo, ej: restaurant.csv
  # COMPLETO
  dataset <- load.data(path.data="../data/",name) 
  ## Para ver los elementos de dataset, 
  ## descomente las siguientes l�?neas antes de ejecutar
  print("target: ")
  print(dataset$target.attribute)
  print("---labels:---------------------------------------------------------------")
  print(dataset$labels)
  print("---attributes:-----------------------------------------------------------")
  print(dataset$attributes)
  
  # 2- CONSTRUCCIÓN DEL ÁRBOL USANDO ID3
  result <- id3(dataset$examples,dataset$target.attribute,dataset$attributes,dataset$labels,NULL)
  
  # La función plot.tree permite ver el árbol graficado
  plot.tree(result)
  
  # 3- CLASIFICAR un nuevo ejemplo
  # Complete el argumento example con el nuevo ejemplo a clasificar 
  # (e.g example=c("v1","v2","valor3"))
  # El ejemplo dependerá del dataset con el que esté trabajando
  # Muestre en consola el ejemplo a clasificar y el resultado.
  
  # example <- c("Rain","Mild","Normal","Strong")
  # example <- c("Rain","Mild","High","Strong")
  # example <- c("Sunny","Mild","Normal","Strong")
  # example <- c("Sunny","Mild","High","Strong")
  example <- c("Overcast","Mild","Normal","Strong")
  
  classify.example(tree=result, example=example) 
  
}