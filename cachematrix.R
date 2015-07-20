###This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        ### Clear the transient variables within the current function      
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #Check to see if the matrix inverse information has been cached
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        
        #Create a list to store the matirx information
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        
}

###This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
###  If the inverse has already been calculated (and the matrix has not changed), 
###  then cacheSolve  retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        
        inv = x$getinv()
        
        # Check to see if the matrix has been solved and is cached
        #If it is cached get the cashed data and do not calculate
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        #If it is not cached - calculate the inverse
        mat.data = x$get()
        print(mat.data)
        inv = solve(mat.data, ...)
        
        # Cache the solved or retreived matrix.
        x$setinv(inv)
        
        return(inv)
}


###Quick Test to see that it works
#random = c(1,2,3,4)
#m = matrix(random,2,2)
#x = makeCacheMatrix(m)

#cacheSolve(x)
#cacheSolve(x)
