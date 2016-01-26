## makeCacheMatrix is a data object which caches the inverse of a matrix, if it has already been computed

## makeCacheMatrix consists of four function - getter and setter for the matrix itself and getter and setter for the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(newmatrix) {
                x <<- newmatrix
                inverse <<- NULL
        }
        get <- function() {
           x
        }
        setinverse <- function(newinverse) {
           inverse <<- newinverse
        }
        getinverse <- function() {
           inverse
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve return the inverse of a matrix, if it has already been computed. If not, the inverse is computed and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        ## the inverse has to be computed
        data <- x$get()
        computedinverse <- solve(data, ...)
        x$setinverse(computedinverse)
        computedinverse
}
