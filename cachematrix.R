## makeCacheMatrix and cacheSolve are two functions that utilize lexical scoping
#to calculate, cache and retrieve inverses of input matrices.  In this way, R 
#does not need to re-calculate inverses as matrix inversion can be a "costly"
#computing process.

#makeCacheMatrix creates a "matrix," that is a list containing
#functions that set the value of the matrix, get the value of the matrix, 
#set the value of the inverse and get the value of the inverse.  

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
                get <- function() x
                setinverse <- function(solve) i <<- solve
                getinverse <- function() i
                list(set = set, get = get,
                      setinverse = setinverse,
                      getinverse = getinverse)
}        

#cacheSolve calculates the inverse of the "matrix" created from makeCacheMatrix.
#First, it checks to see if the inverse has already been calculated and cached.

#If so, cacheSolve retrieves and returns the inverse (i) from the cache, thereby
#skipping re-calculation.  

#If not, cacheSolve calculates the inverse of data provided and sets the value of
#the inverse in the cache with 'setinverse'.  In the future, using cacheSolve 
#will find this cached inverse and return it.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Retrieving cached data")
                return(i)
        }
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i)
                i
        
}

