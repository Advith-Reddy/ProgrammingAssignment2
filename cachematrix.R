myCacheMatrix <- function(x = matrix()){
  #function takes matrix and process for the inverse
  inver <- NULL
  set <- function(y){ #sets the matrix 
    x <<- y
    inver <<- NULL
  }
  get <- function() {x} #calling get will show the matrix
  setMatrix <- function(inverse) {inver <<- inverse} #helps to set the matrix for inverse function as in line 22
  getMatrix <- function() {inver} #shows the inverse as in line 15
  list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}

sol <- function(x, ...){ #gives the solution that is inverse of matrix
  inver <- x$getMatrix()
  # if the inverse has already been calculated and returns null, 
  # return this instance
  if(!is.null(inver)){
    message(" retrieving cached data ")
    return(inver)
  }
  
  # if it is not null, calculate it using solve()
  # then return it
  k <- x$get()
  inver <- solve(k, ...)
  x$setMatrix(inver)
  inver
}
