# load some staff functions
source("utils.R")

deps <- c("read-matrix.R")

dependencies.loader(deps)


# En este template están las funciones que debes codificar.
# Debe escribir tan sólo lo que se le pide.
# No debe alterar las cabeceras de las funciones (tipo y parámetros que pasan, nombres de la función)


##################-----------------##################
# Esta función calcula los parámetros aprendidos para un modelo clasificador Naïve Bayes.
#
# Los parámetros son:
#
#   examples: conjunto de ejemplos de entrenamiento
#
#   target: atributo cuyo valor debe ser predecido por el modelo. También llamado variable de clase
#
# 	attributes: un vector con el nombre de los atributos usados para clasificar
#
#   labels: un vector con las etiquetas usadas para clasificar los "examples"
#   
#
# Debe regresar una lista con los parámetros aprendidos

naiveBayesModel <- function(examples,                            
                            attributes,
                            target.attribute,
                            labels,
                            laplace=1) {
  
  #Verifica que todos los valores esten completados
  if(is.null(examples)) stop("examples no puede ser nulo")
  if(is.null(attributes)) stop("attributes no puede ser nulo")
  if(is.null(target.attribute)) stop("target.attribute no puede ser nulo")
  if(is.null(labels)) stop("labels no puede ser nulo")
  
  #creo la estructura model, donde se guardará por cada atributo sus tablas de probabilidad condicional
  model <- list()
  
  #hay un elemento en la lista cuyo nombre es "apriori" que guarda las probabilidades "a priori" de cada una de las clases
  #Por ej. para el dataset de spam, sería la probabilidad de SPAM y de NO-SPAM dado el dataset de entrenamiento
  #para el dataset de tennis, probabilidad de YES y de NO dado el dataset de entrenamiento.
    
  col_names <- labels
  row_names <- target.attribute  #class
  model[["apriori"]] <- matrix(NA,
                               nrow=length(row_names),
                               ncol=length(col_names),
                               dimnames=list(row_names,col_names))
  
  #se crea un elemento de la lista por cada atributo
  #de cada atributo se guardará su tabla con probabilidades condicionales
  for (att in attributes){
    col_names <- sort(unique(examples[,att]))
    row_names <- labels
    model[[att]] <- matrix(NA,
                          nrow=length(row_names),
                          ncol=length(col_names),
                          dimnames=list(row_names,col_names))
  }
  
  # Calcular las probabilidades a priori de cada clase.
  # Hay varias formas de establecer la probabilidad a priori a cada clase.
  # La más intuitiva es que todas ellas tengan la misma probabilidad,
  # es decir, 1 divido entre el número de clases. Si contamos con la opinión de un experto,
  # puede que éste nos proporciones dichas probabilidades.
  # Nosotros utilizaremos la probabilidad del conjunto de entrenamiento.
  for (class in labels){
    #guarda en la matriz apriori la probabilidad de que el resultado sea un "si" o un "no" / "spam" o "No-spam"
    model[["apriori"]][1,class] <- sum(examples[,target.attribute]==class)/nrow(examples) 
  }
  
  # Calculo probabilidades condicionales para cada atributo
  for (att in attributes){ # <--- por cada atributo
    for (class in labels){ # <--- por cada etiqueta de clasificación
      for(value in colnames(model[[att]])){ # <--- por cada valor que puede tomar ese atributo
        # print(att)
        # print(class)
        # print(value)
        
        
        f_cond <- 0
        
        # Para cada clase, realizar un recuento de los valores de atributos que toma cada ejemplo.
        # setee el valor en la variable f_cond  
        #######
        
        # cant.value <- length(which(examples[,att]==value))
        f_cond<- sum(examples[which(examples[,att]==value),target.attribute]==class)
        # print(f_cond)
        
        ########
        # Aplicar la Corrección de Laplace, para que los valores "cero" no den problemas.
        #Si queda 0 al multiplicarlo la probabilidad va a dar 0. 
        f_cond <- f_cond + laplace
        
        # Guardo el valor en mi tabla
        model[[att]][class,value] <- f_cond
        
      }
    }
    # Normalizar para obtener un rango de valores [0,1]
    # Normalizar significa dividir cada uno de los valores de una clase por su total
    # Recuerde que las probabilidades deben ir entre 0 y 1. 
    # Diríjase al libro, para entender este concepto o a los apuntes.
    
    for(class in labels){
   
      #######
      total.class <- sum(model[[att]][class,])
      # print("ACA")
      # print(total.class)
      
      for(value in colnames(model[[att]])){
        model[[att]][class,value] <- model[[att]][class,value]/total.class
      }
      ########
    }
  }
  

  return(model)
}

#########################################################-----------------##################
  # Esta función debe regresar por cada ejemplo recibido la etiqueta correspondiente de clasificación
  #
  # Los parámetros son:

  #    model: es una lista con los parámetros del modelo Naive Bayes
  #
  #    test_set: una matriz con ejemplos a clasificar
  #
  # Debe regresar una matrix con la probabilidad condicional posterior de cada clase,
  # y la clase (elemento "class" ) cuyo valor de probabilidad resultó máximo
  #
classify.example <- function(model, test_set) {
 
  # Si no hay test_set paro
  if(is.null(test_set)) stop("test_set no puede ser nulo")
  if(is.null(model)) stop("model no puede ser nulo")
  
  # Si es solo 1 ejemplo
  if(is.null(nrow(test_set))){
    filas <- 1
    test_set <- matrix(test_set,nrow=1,ncol=length(test_set))
  }  else{
    filas <- nrow(test_set)
  }
  
  classes <- colnames(model[["apriori"]]) #nombres de las clases
  
  # Si no tienen nombre
  if(is.null(colnames(test_set))) colnames(test_set) <- names(model)[-1]
  
  result <- matrix(0,nrow=filas,ncol=length(classes)+1,dimnames=list(NULL,c(classes,"class")))  
    
  for(class in classes){
    for(i in 1:filas){      
      for(p_cond in names(model)){
        if (p_cond == "apriori"){
          result[i,class] <- model[[p_cond]][1,class]
        }else{
          result[i,class] <- result[i,class] * model[[p_cond]][class,test_set[i,p_cond]]
        }          
      }      
    }
  }
  # Debe realizar los siguientes dos pasos:
  # 1) Normalizar los resultados para poder obtener la probabilidad condicional a-posteriori
  # 2) Calcular el ArgMax y guardarlo en result[i,"class"]
  
  #######
  #Normalizar
  for(i in 1:nrow(result)){
    total.sumfila <- 0
    for(j in 1:(ncol(result)-1)){
      total.sumfila <- total.sumfila + as.numeric(result[i,j])
    }
    # print(total.sumfila)
    result[i,] <- result[i,]/total.sumfila
    
    print(colnames(result)[which(result[i,]==max(result[i,]))])
    result[i,ncol(result)] <- which(result[i,]==max(result[i,]))
  }
  
  #Elegir m�ximo
  
  
  ########
  
  return(result)
}

##################-----------------##################
  # Realice todo el pre-procesamiento necesario de los datos en esta función
  # En la función se encuentra el pre-procesamiento del dataset PlayTennis para que 
  # se obtengan estructuras necesarias para trabajar con Naïve Bayes.
  # Modifique esta función para poder manipular distintos tipos de dataset (spam, restaurant,tennis)
  # De esta manera se le facilitará la carga de datos
  #
  # REGRESA: 
  # target.attribute: la etiqueta del atributo objetivo target 
  # labels: los valores posibles de clasificación
  # examples: matriz conjunto de ejemplos que serán utilizados para clasificar el árbol
  # attributes: vector listado de nombres de atributos

load.data <- function(path='../data/tennis.csv')
{
  if(endsWith(path,"csv")){
    examples <- read.csv(path,header=TRUE, stringsAsFactors=FALSE)
    examples <- as.matrix(examples)  
    attributes <- as.vector(dimnames(examples)[[2]])
    attributes <- attributes[-(ncol(examples))]
    etiquetas <- unique(examples[,ncol(examples)])
    target <- (colnames(examples))[length(colnames(examples))]
  }else{
    source("read-matrix.R") 
    m.train <- read_matrix(filename="../data/MATRIX.TRAIN.50",ocurrence=FALSE,sparse=FALSE)
    # View(m.train)
    examples <- as.matrix(m.train$matrix)
    for(j in 1:(ncol(examples)-1)){
      dimnames(examples)[[2]][j] <- m.train$tokens[j]
    }
    # View(examples)
    attributes <- as.vector(dimnames(examples)[[2]])
    attributes <- attributes[-ncol(examples)]
    # View(attributes)
    etiquetas <- unique(examples[,ncol(examples)])
    target <- (colnames(examples))[length(colnames(examples))]
    # for(i in 1:length(attributes))
    #   VALUES[[attributes[i]]] <<- unique(examples[,attributes[i]])
  }

  
  return (list(target.attribute=target,
		        labels = etiquetas,
		        examples = examples,
		        attributes = attributes))
}




run.nb.experiment <- function(name)
{
  
  # 1- CARGA DE DATOS
  # pasar el argumento path='' con el path del dataset que desee cargar
  path.data <- paste("../data/",name,sep="")
  # print(path.data)
  dataset <- load.data(path.data) 
  
  ##Para ver los elementos de dataset, descomente las siguientes líneas antes de ejecutar
  # print(dataset$examples)
  # print(dataset$target)
  # print(dataset$labels)
  # print(dataset$attributes)
  
  # 2- CONSTRUCCIÓN DEL MODELO NAÏVE BAYES
  nb.model <- naiveBayesModel(examples = dataset$examples,
			      target.attribute = dataset$target.attribute,
			      attributes = dataset$attributes,
			      labels = dataset$labels,
            laplace=1)
  
  print(nb.model)
  
  # 3- CLASIFICAR un nuevo ejemplo
  # Complete el argumento example con el nuevo ejemplo a clasificar (e.g: my_example <- c("Overcast","Cold","Normal","Weak"))
  # El ejemplo dependerá del dataset con el que esté trabajando
  # Muestre en consola el ejemplo a clasificar y el resultado.
  # Por defecto clasifica con los mismos ejemplos de entrenamiento.
  my_example <- dataset$examples
  
  classify.example(model=nb.model, test_set=my_example) 
  
  
}
