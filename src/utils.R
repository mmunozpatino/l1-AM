
dependencies.loader <- function(deps) {
  for (i in 1:length(deps)) {
    file.i <- path.expand(deps[i])

    if (!file.exists(file.i))
      stop(paste("The file",
                 file.i,
                 "should be into the directory",
                 getwd(),
                 "to run this script."))
    else source(file.i)
  }
}

read.dataset <- function(filename, type="CSV") {
  file <- path.expand(filename)

  if (!file.exists(file))
    stop(paste("The file", filename, "does not exist."))

  if (type == "CSV") {
    data <- read.csv(file)
  }
  else {
    stop(paste("The type", type, "is unknown"))
  }

  return(data)
}
