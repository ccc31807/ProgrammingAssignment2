## Put comments here that give an overall description of what your
## functions do

# create a matrix named MAT
 mat <- matrix(c(1,1,1,3,4,3,3,3,4),nrow=3,ncol=3)
 mat1 <- matrix(c(1,0,5,2,1,6,3,4,0),nrow=3,ncol=3)
# create the inverse of MAT called TAM
 tam <- solve(mat)
 tam1 <- solve(mat1)
# check for the identy matrix IDENT my multyplying MAT and TAM
 ident <- mat %*% tam
 ident1 <- mat1 %*% tam1


## Write a short comment describing this function
# input parameters are an invertible matrix or nil
# return value is an object containing a list with four methods
# usage:
#   obj$set(matrix) -> sets an invertible matrix
#   obj$get() -> an invertible matrix
#   obj$set_inv_mat -> sets the inverse of an invertible matrix
#   obj$get_inv_mat -> returns the inverse of an invertible matrix

makeCacheMatrix <- function(x = matrix()) 
{
    inv_mat <- NULL
    set <- function(mat)
    {
        x <<- mat
        inv_mat <<- NULL
    }
    get <- function() x
    set_inv_mat <- function(inverted_matrix) inv_mat <<- inverted_matrix
    get_inv_mat <- function() inv_mat
    list(
        set = set,
        get = get,
        set_inv_mat = set_inv_mat,
        get_inv_mat = get_inv_mat
    )
}


## Write a short comment describing this function
# input parameters are a matrix object created by makeCacheMatrix
# return value is the inverse of the matrix stored by makeCacheMatrix
# usage: cacheSolve(obj)

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    inv_mat <- x$get_inv_mat()
    if (!is.null(inv_mat))
    {
        message("Getting cashed data ...")
        return(inv_mat)
    }
    mat <- x$get()
    inv_mat <- solve(mat, ...)
    x$set_inv_mat(inv_mat)
    inv_mat
}

##########################
#   testing routinesw
##########################
### inspect text matrix
# mat
### inspect text inverted matrix
# tam
### multiply MAT and TAM to check that the product is the identity matrix
# mat %*% tam
### create an object with four methods: set, get, set_inv_mat, and get_inv_mat
# mat_obj <- makeCacheMatrix()
### inspect the object
# mat_obj
### set the test matrix
# mat_obj$set(mat)
### inspect the set matrix
# mat_obj$get()
### get inverted matrix
# cacheSolve(mat_obj)
### get inverted matrix again, check for getting cached data
# cacheSolve(mat_obj)
