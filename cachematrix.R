makeInverse <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
cacheinverse <- function(x)
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
