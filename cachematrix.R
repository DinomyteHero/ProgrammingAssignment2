
## The makeCacheMatrix function is supposed to create a special "matrix", which
## is a list that sets the value of the matrix, gets the value of the matrix,
## sets the value of the inverse, and then gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function calculates the inverse of the special "matrix" which
## was returned by makeCacheMatrix. If the inverse has already been calculated in
## the cache then a message saying "getting cached data" will appear, which will 
## return the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
