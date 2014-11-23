

## The function makeCacheMatrix will create a special matrix that will be able to store it's 
## inverse for future use
##
## The function cacheSolve checks if the inverse of the matrix has been calculated, if so, 
##returns it, otherwise, calculates the inverse, stores it and returns the value.




## The function makeCacheMatrix will create a special matrix that will be able to store it's 
## inverse for future use


makeCacheMatrix <- function(x = numeric()) 
{
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function


## The function cacheSolve checks if the inverse of the matrix has been calculated, if so, 
##returns it, otherwise, calculates the inverse, stores it and returns the value.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


