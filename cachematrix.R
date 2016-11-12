## Overall description: 
## The pair of functions bellow cache the inverse of a matrix.
## Rather than compute it repeatedly, the inverse of the matrix
## is cached to avoid time-consuming computations
## Hypothesis: x is an inverted squared matrix 

## Function makeCacheMatrix:
## This function creates a special square "matrix" object
## that can cache its inverse inside the "i" variable.
## The set function pushes its argument y to the local scope x
## and sets the inverse matrix i to NULL
## The get function returns the matrix x.
## The setinverse function stores its arg into the local scope i variable
## The getinverse function returns the local scope i variable
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## Function cacheSolve:
## This function computes the inverse of the special square
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
##  already been calculated (and the matrix has not changed), then the
## `cachesolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'`
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

