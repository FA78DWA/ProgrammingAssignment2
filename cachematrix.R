## Put comments here that give an overall description of what your
## functions do

## Create a matrix that cashes its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        
        get<- function() x
        setInverse <- function(y){
                inverse <<- y
        }
        
        getInverse <- function() inverse
        
        list(set = set, get = get,
             setinverse = setInverse,
             getinverse = getInverse)
}


## Caculate the inverse for the new matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
               return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
