myCacheMatrix <- function(x = matrix()){
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() {x}
  setMatrix <- function(inverse) {inver <<- inverse}
  getMatrix <- function() {inver}
  list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}

sol <- function(x, ...){
  inver <- x$getMatrix()
  if(!is.null(inver)){
    message(" retrieving cached data ")
    return(inver)
  }
  k <- x$get()
  inver <- solve(k, ...)
  x$setMatrix(inver)
  inver
}
