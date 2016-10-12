## This pair of functions can be used to calculate the inverse of a Matrix
## 1- Create an object this way: TempObject = makeCacheMatrix ( YourData )
## 2- Calculate the inverse matrix : InverseM = cachesolve(TempObject)
## 3- For changing the data, execute: matx$set( NewData )
## 4- Calculate the inverse matrix : InverseM = cachesolve(TempObject)

## --------------------- makeCacheMatrix --------------------------------

# This first function creates a special "matrix" object that can cache 
# its inverse

# makeCacheMatrix function builds a list of functions 
# with two internal variables:
# matr : the data matrix
# minv : the inverse of the data matrix. Starts like NULL and becomes NULL 
#           when MATR changes, with the SET function 

makeCacheMatrix <- function( matr = matrix()  )  {
    minv <- NULL
    set <- function(y) {
        matr <<- y
        minv <<- NULL
    }
    get <- function() matr
    setminv <- function(minv_new) minv <<- minv_new
    getminv <- function() minv
    list(set = set, get = get,
         setminv = setminv,
         getminv = getminv)
}

## ------------------------ cachesolve ------------------------------

# This second function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

# cachesolve function calculates de inverse matrix, if it doesn't exist.
# If exists, returns the last value, without calculating it again 

cachesolve <- function(matrli , ... ) {
    minv <- matrli$getminv()
    if(!is.null(minv) ) {
        message("Inverse matrix hasn't changed. Getting cached data")
        return(minv)
    }
    data <- matrli$get() 
    minv <- solve(data , ... )
    matrli$setminv(minv)
    minv
}

## --------------------------   Tests  ------------------------------------
# Tests
# matx = makeCacheMatrix ( matrix(rnorm(4) ,2 , 2) )
# matx = makeCacheMatrix ( )
# matx$set(matrix(rnorm(4) ,2 , 2) )
# cachesolve(matx)
