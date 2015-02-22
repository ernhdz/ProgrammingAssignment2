

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

M <- matrix(c(1,2,3,4),2,2)
M
det(M)
solve(M)

N <- makeCacheMatrix(M)
N

cacheSolve(N)

cacheSolve(N) %*% M

M <- matrix(c(2,2,3,4,5,6,7,8,9),3,3)
det(M)
N <- makeCacheMatrix(M)
solve(M)
cacheSolve(N)

N$get()
N$getinverse()

N$get() %*% N$getinverse()


