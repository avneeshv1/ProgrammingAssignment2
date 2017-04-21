## The first function makeCacheMatrix to make an object that is caching its inverse
## Second function calculates the inverse of a matrix. It receives the object created by makeCacheMatrix to acess data and check if 
## inverse is already calculated or if the object is recently initiated it calculates the inverse and set atrributes in the object

## makeCacheMatrix returns a list of functions to get and set data : the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
          m <-NULL
  set <-function(y){
    x <<-y
    m <<-NULL
  }
  get <-function() x
  setmatrix <-function(solve) m <<- solve
  getmatrix <-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## cacheSolve receives an object that contains a list of functions and its environment created when it is instatntiated by makeCacheMatrix
## It checks if the inverse is already calculated in which case the function returns it directly otherwise it calculates and set the value
## to the variable of the object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <-x$get()
  m <-solve(matrix, ...)
  x$setmatrix(m)
  m
}
