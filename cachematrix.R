## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## makeCacheMatrix function create a new enviroment to store the
## makerCheMatrix list objects, each object contains 4 attributes(functions)
## 1. set: set the value of matrix
## 2. get: get the value of the matrix
## 3. setInv: set the value of the matrix's inverse
## 4. getInv: get the value of the matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
        ## INPUT:  matrix
        ## OUTPUT: list with 4 attributes(functions)
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

## The function first check if the inverse matrix has already been stored in
## the special enviroment
## If yes, the function print out the message and return the inverse matrix
## which get from the "special" enviroment
## if no, the function store the new matrix into the "special" enviroment
## and set the inverse of the new matrix and store into the "special" env
## and return the inverse
cacheSolve <- function(x, ...) {
        ## INPUT:  makeCacheMatrix list
        ## OUTPUT: inverse of a matrix
  
  
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
