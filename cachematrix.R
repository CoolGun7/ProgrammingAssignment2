Matrix inversion is usually a costly computation and there may be some benefit to caching
the inverse of a matrix rather than compute it repeatedly (there are also alternatives to
matrix inversion that we will not discuss here). Your assignment is to write a pair of functions
that cache the inverse of a matrix.

makeCacheMatrix: This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve: This function calculates the inverse of the special matrix returned by above function makeInverse
The inverse if is already calculated then the cacheinverse fetches and returns the inverse from cache

cacheSolve <- function(x)
{
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting matrix inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
TEST:
klm<-matrix(c(2,-5,8,3,0,9,1,5,4),3,3)
klminv<-makeCacheMatrix(klm)
cacheSolve(klminv)
cacheSolve(klminv)
