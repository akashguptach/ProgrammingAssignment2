## makeCacheMatrix creates a special "matrix" that can cache its inverse
## cacheSolve returns the inverse of the special "matrix" described in the function above

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y){
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinv <- function(inv) s <<- inv
    getinv <- function() s
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    s <- x$getinv()
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinv(s)
    s
}
