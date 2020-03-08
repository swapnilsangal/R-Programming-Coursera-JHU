## The first function returns the special matrix object that can store cached Inverse of the matrix.
## The second function checks the object for stored cache and returns the inverse accordingly.

## makeCacheMatrix Function
##This function takes a matrix as an input and return the list with Inverse of the matrix in it. This forms the special object returned.
## The function has 4 functions inside the environment to set and get the matix and it's inverse.

makeCacheMatrix <- function(mat = matrix()){
  inv <- NULL
  set <- function(mat){
    mat<<-mat
    inv<<-NULL
  }
  
  get <- function(){
    mat
  }
  
  setInv <- function(inv){
    inv<<-inv
  }
  
  getInv <- function(){
    inv
  }
  
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## cacheSolve
## This particular function checks if the matrix entered already has a computed inverse of the matrix in it's cache or not.
## If the inversed matrix is not found then the Inverse is calculated with the help of matrix multiplication and the inverse is set and returned.


cacheSolve <- function(mat,...){
  minv <- mat$getInv()
  
  if(!is.null(minv)){
    return(minv)
  }
  
  data<-mat$get()
  
  minv<-solve(data)
  
  mat$setInv(minv)
  
  minv
}
