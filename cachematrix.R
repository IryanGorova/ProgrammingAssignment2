## Create special matrix

makeCacheMatrix <- function(x = as.matrix()) {
        inv <- NULL  
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        ##set Inverse after calculation in cacheSolve()
        setinv <- function(inv) inv <<- inv ##do not call diectly,will overwrite true Inverse
        ##retrieve inverse, will be NULL if cacheSolve is not called
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##calculate or cache the Inverse
cacheSolve <- function(x) {
        inv <- x$getinv()
        ##check if Inverse is already cache'd
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ##initial calculation of Inverse
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
