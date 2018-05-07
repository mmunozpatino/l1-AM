# Documentation for these function can be found it in file dt.tutorial.R
#
library("uuid")

new.node <- function(name=NULL, branches=NULL) {  
  if (is.null(name)) stop ("name is NULL")
  if (is.null(branches)) stop ("branches is NULL")
  branchesId <- NULL
  #generate a unique id for each branch
  for (branch in branches){
    branchesId[[branch]] <- UUIDgenerate(TRUE)
  }
  list(parentId=NULL,
       name = name,
       branches = branchesId,
       label=NULL)
}

new.leaf <- function(label=NULL){
  if (is.null(label)) stop("label is null")
  list(parentId=NULL,
       name=NULL,
       branches=NULL,
       label = label)
}

new.tree <- function(root) {
  if (!is.list(root))
    stop("Root should be a object of type 'node'.")
  
  if (!is.null(root$parentId)) stop ("The parentId of the root node should be NULL")
  
  nodes <- list(root)
  tree <- list(nodesCount=1,
               nodes = nodes)
  tree
}

add.subtree <- function(tree, subtree, parentId=NULL) { 
  
  if (is.null(parentId)) stop ("parentId is null")
  salida <- TRUE
  for (branchIndex in 1:tree$nodesCount){
    if (any(tree$nodes[[branchIndex]]$branches == parentId)){
      salida <- FALSE
      break
    } 
  }
  if (salida) stop("parentId should be exists on tree")
  
  #subtree is a tree?
  if ( length(subtree) == 2 ){
    subtree$nodes[[1]]$parentId <- parentId
    tree$nodesCount <- tree$nodesCount + subtree$nodesCount
    tree$nodes <- append(tree$nodes,subtree$nodes)
  }else{
    #subtree is a node or a leaf
	  subtree$parentId <- parentId
    tree$nodesCount <- tree$nodesCount + 1
    tree$nodes <- append(tree$nodes,list(subtree))
  }
  tree
}

plot.tree <- function(tree, nodeIndex=1, ctab="  ", level=0, value=NULL) {

	if (is.null(tree)) stop ("tree is NULL")
	if (length(tree)!= 2) stop ("tree should be a object of type 'tree'.")
    
    node <- tree$nodes[[nodeIndex]]
    if (nodeIndex == 1) {            
      value <- "[ROOT]"      
    }else{
      value <- paste("[Value: ",value,"]",sep="")
    }
    # is a leaf?
    if (is.null(node$name)){
      values <- c()
    }else{
      values <- names(node$branches)
    }
    
    if (length(values)) {
      cat(paste(ctab, level, ") ", value, " - Test: ", node$name, "\n",sep=""))
      
      for (i in 1:length(values)) {
        
        next.node.parentId <- node$branches[[i]]
        next.node.Index <- NULL
        for (j in 2:tree$nodesCount){
          if (tree$nodes[[j]]$parentId == next.node.parentId){
            next.node.Index <- j
            break
          }
        }
        if (is.null(next.node.Index)){
          fail.leaf <- new.leaf("INCOMPLETE ???????")
          tree <- add.subtree(tree,fail.leaf,next.node.parentId)
          plot.tree(tree, tree$nodesCount, paste(ctab,"  "), level + 1, values[i])
        }else{          
          plot.tree(tree, next.node.Index, paste(ctab,"  "), level + 1, values[i])
        }
        
      }
    }
    else{
      cat(paste(ctab, level, ") ", value, " - Label: ", node$label, "\n", sep=""))
    }
}

