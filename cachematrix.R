## Peer-graded assignment by Wing Chum
## Nov 27, 2016

## The two functions included in this script are helper function to work with matrices 
## and caching their inverse

## Assumptions: matrix must be invertible to use these two functions

## makeCacheMatrix : creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve : 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        #if the inverse already exists in cached, return it without calculating it.
        if(!is.null(m)) {
                message("getting cached inverse of the matrix")
                return(m)
        }
        data <- x$get()
        m <- Solve(data, ...)
        x$setInverse(m)
        m
}

## sample Code for testing
testM = matrix( 
           c(10, 6, 3, 1, 5, 7, 9, 1,5), 
           nrow=3, 
           ncol=3) 

testM

invM = solve(testM)

invM

CachedMatrix <- makeCacheMatrix(testM)
cacheSolve(CachedMatrix)
