# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# The first function, makeCacheMatrix, creates a special "matrix" object that can 
# cache its inverse and which contains a function to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# The second function returns the inverse of the matrix. First, it checks if
# the inverse has already been calculated. If so, it retrieves the inverse 
# result from the cache. If not, it computes the inverse, sets the value in 
# the cache via setinv function.

cacheSolve <- function(x, ...) {
       inv <- x$getinv()
       if (!is.null(inv)){
		message("getting cached data")
     		return(inv)
       }
       matrix <- x$get()
       inv <- solve(matrix, ...)
       x$setinv(inv)
       inv
}
