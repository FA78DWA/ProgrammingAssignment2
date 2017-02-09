## Put comments here that give an overall description of what your
## functions do

## Create a matrix that cashes its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
		#set the matrix value
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        #get the last stored matrix 
        get<- function() x
		#set the inverse of the matrix
        setInverse <- function(y){
                inverse <<- y
        }
        #get the stored inverse of the matrix
        getInverse <- function() inverse
        
        list(set = set, get = get,
             setinverse = setInverse,
             getinverse = getInverse)
}


## Caculate the inverse for the new matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
        if(!is.null(inv)){ #we already calculated it
                message("getting cached data")
               return(inv) #return the stored invers
        }
		#otherwise, get the matrix, calculate its inverse, store it, and return it
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
