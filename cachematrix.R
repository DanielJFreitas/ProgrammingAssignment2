## The functions calculate the inverse of matrices efficiently. If the inverse has not already been calculated, then it is generated and saved in the cache for later use

## The "makeCacheMatrix" function creates an object, containing the functions "set", to set the matrix; "get", to get the matrix; "setInverse", to set the inverse of the matrix; and "getInverse", to get the inverse of the matrix. The argument must be a matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The "cacheSolve" function calculates the inverse of a matrix. The argument is the object that results from the "makeCacheMatrix" function. If the inverse has not already been calculated, then it is generated and saved in the cache

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}

## Demo:
## originalMatrix <- matrix(rnorm(1000000), 1000, 1000)
## originalMatrix_cache <- makeCacheMatrix(originalMatrix)
## cacheSolve(originalMatrix_cache)