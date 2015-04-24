## Below are two functions that are used to create a special object 
## that stores a numeric matrix and caches its inverse


## running makeCacheMatrix(mat) and assigning its output to a variable ('cache_mat')
## results in a list of functions that respectively allow to: 
##
## a) set the value of the matrix x to y and default it's inverse to NULL
##    by calling cache_mat$set(y). 
## b) get (retrieve) the current value of x by calling cache_mat$get()
## c) 'manually' set the inverse of the matrix by attributing it a specified inverse_mat 
##    when calling cache_mat$setinv(inverse_mat)
##    Note that when this function is called on it's own on the command line you can arbitrarily 
##    attribute any inverse to a matrix that doesn't actually correspond to the real inverse.
## d) get the current value for the inverse of x by calling cache_mat$getinv()
##
## simply running makeCacheMatrix(mat) will result in chaching the value of mat, 
## while defaulting its inverse to NULL. Both can be retrieved using the respective get function

makeCacheMatrix <- function(x = matrix()) {
        
        ## defaults value of inv_mat to NULL
        ## i.e. when running makeCacheMatrix(mat) once, the inverse will default to NULL,
        ## which can be checked by calling cache_mat$getinv() after running makeCacheMatrix(mat)
        ## you can manually change it by a posteriori setting it with 
        ## cache_mat$setinv(inverse_mat) (see below) 
        inv_mat <- NULL
        
        ## 'set' attributes the specified value y to x and resets the inverse to NULL.
        ## this is last part is useful because every time you set a new matrix the 
        ## the value you have cached for the inverse won't be applicable anymore and has to be 
        ## recalculated.
        ## note that by using '<<-' one is not creating a variable in the environment of 'set',
        ## but instead modifying an existing variable in the parent environment, which in this 
        ## case is the environment of the function makeCacheMatrix
        ## that's why if you use $set to assign x with a specific value y and then call $get()
        ## you'll retrieve the new value of x
        ## see more in http://adv-r.had.co.nz/Functional-programming.html#closures
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        
        ## the get functions simply outputs the current value of x that is cached in memory
        get <- function() x
        
        ## same idea as the set function, but this time for setting the inverse
        setinv <- function(inverse_mat) inv_mat <<- inverse_mat
        
        ## defining the function that outputs the current value for the inverse of the matrix
        getinv <- function() inv_mat

        ## outputting a list with the above defined functions.
        ## individual functions are accessible by calling 
        ## cache_mat$'name_of_function'() 
        ## (the set functions need a specified argument)
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## in case it has not been previously calculated, cacheSolve calculates the inverse a matrix
## contained in the special object created with the above function. Otherwise it retrieves the
## inverse from the cache and skips any calculations

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of the matrix contained in the object 'x'
        inv_mat <- x$getinv()
        
        ## if the inverse has been previously determined
        ## it retrieves its value from cache and returns
        ## (i.e. skips any further calculations)
        if(!is.null(inv_mat)) {
                message("getting cached data")
                ## outputs cached inverse
                return(inv_mat)
        }
        
        ## if the inverse has not been previously determined
        ## it attributes the value of the matrix from x to the variable 'data' ...
        data <- x$get()
        
        ## ... calculates its inverse using solve ...
        inv_mat <- solve(data, ...)
        
        ## ... and attributes the obtained value to the inverse of the matrix in x by 
        ## using the $setinv() function
        x$setinv(inv_mat)
        
        ## outputs the inverse
        inv_mat
}


## USAGE EXAMPLE:
## 1) defining matrix to b cached
# n <- 5
# mat <- matrix(runif(n*n), nrow = n, ncol = n)
## 2) cache created matrix by running makeCacheMatrix
# cache_mat <- makeCacheMatrix(mat)
## 3) check what is cached for the matrix and its inverse 
# cache_mat$get()       ## should be mat 
# cache_mat$getinv()    ## should be NULL
## 4) calculate inverse for mat using cacheSolve 
##    (first time you run it inv_mat is set to NULL hence it calculates the inverse)
# inv_mat <- cacheSolve(cache_mat)
# cache_mat$getinv()    ## should be the inverse
# round(inv_mat%*%mat, digits = 0)      ## should give the identity matrix
## 5) retrieve calculated inverse from cache without calculating it again
# inv_mat_cached <- cacheSolve(cache_mat) 
## message 'getting cached data' should be printed out and function should run faster
## more noticible if matrices are big