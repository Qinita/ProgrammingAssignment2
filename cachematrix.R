## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        ## 0.give the value to the inverse matrix
        m <- NULL
        
        ## 1.set the value of the matrix
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## 2.get the value of the matrix
        get <- function() x
        
        ## 3.set the value of the inverse
        setinverse <- function(inverse_input) m <<- inverse_input
        
        ## 4.get the value of the inverse
        getinverse <- function() m
        
        ##reture the list of the functions above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The following function calculates the inverse of the special "matrix" created with the above function.
## 1. It first checks to see if the matrix has already been calculated
## 2. If so, it gets the inverse from the cache and skips the computation;
##    Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {

        ##  1. It first checks to see if the matrix has already been calculated
        m <- x$getinverse()
        
        ##2. If so, it gets the inverse from the cache and skips the computation;
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        
        ##   Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

        data <- x$get()
        
        m <- solve(data, ...)
        
        x$setinverse(m)
        
        ## and there is the final result
        m
}
