## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(cached_matrix = matrix()) {
    cached_solve <- NULL
    set <- function(matrix) {
        cached_matrix <<- matrix
        cached_solve <<- NULL
    }
    get <- function() cached_matrix
    setsolve <- function(solve_value) cached_solve <<- solve_value
    getsolve <- function() cached_solve
    list(
        set      = set, 
        get      = get,
        setsolve = setsolve,
        getsolve = getsolve
    )
}


## cacheSolve  computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cached_solve <- x$getsolve()
    if(!is.null(cached_solve)) {
      message("getting cached data")
      return(cached_solve)
    }
    cached_matrix <- x$get()
    value <- solve(cached_matrix, ...)
    x$setsolve(value)
    value
}


## Uncomment next lines to see the functions in action 
##
## m <- matrix(c(-1, -2, 1, 1), 2,2)
## x <- makeCacheMatrix(m)
## print(x$get())
## inv <- cacheSolve(x)
## print(inv)
## inv <- cacheSolve(x)
## print(inv)
