## Functions help cache the value of an inverse matrix, so it can be looked up again from memory instead of being recomputed. 

## creates a list of functions to call, along with the objects that hold the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y   #set value 
        m <<- NULL #set initial null value
        
    }
    get <- function() x #return matrix x
    setinverse <- function(inverse) m <<- inverse #set inverse
    getinverse <- function() m #return inverse
    
    list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## looks for cached inverse to return that value if it exists, otherwise calculate new value and return that

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse() #call get inverse to assign value to m
    if(!is.null(m)) {   #if m has been calculated, return its cached value
        message("getting cached data")
        return(m)
    }
    data <- x$get()  #get the initial matrix
    m <- solve(data, ...)   #calculate its inverse
    x$setinverse(m)   #call set inverse to assign calculated inverse matrix m
    m   #return m
}
