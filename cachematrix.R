## I wrote two function that will operate together. First one, makeCacheMatrix
## will create a matrix wich will be able to cache its own inverse. The second
## function, cacheSolve is a function to calculate the inverse of the matrix
## given by the first function. The interesting point in doing it is that if
## the inverse had been calculated once before, this function will then take 
## result (the inverse of the matrix) directly from the cache.

## In this function we will basically follow the instructions of the assignment,
## and create a function that will set the value of a matrix, get its value, 
## set the inverse of the matrix and finally, get its inverse. It will return a
## list with these elements in order.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL              ## notice it will initialize inverse as NULL
        set <- function(z) {
                x <<- z
                inv <<- NULL
        }
        get <- function() {
                x                ## this function gets the matrix x
        }
        setInverseMatrix <- function(inverse) {
                inv <<- inverse
        }
        getInverseMatrix <- function() {
                inv
        }
        list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
        ## list that was mentioned earlier
}


## This function will compute the inverse of the matrix returned by the function
## above. The most important feature of this function is the use of the control
## structure if, to check if the inverse of the matrix were already calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverseMatrix()
        if(!is.null(inv))      {
                return(inv)
        }
        matriz <- x$get()
        inv <- solve(matriz, ...)
        x$setInverseMatrix(inv)
        inv
}