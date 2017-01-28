## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##the below function makeVectorMat creates inverse of the matrix
## if it is not present in the cache
makeVectorMat <- function(x = matrix()) { 

  #assigns the initial value of inverseofMatrix to NULL
  inverseOfMatrix <- NULL 
  
  #set function assigns y to x in the exlcosing enviornment
  # assigns NULL to inverseOfMatrix in the exlcosing enviornment
  set <- function(y) {                      
    x <<- y
    inverseOfMatrix <<- NULL              
  }
  
  #get functions is used to get values
  get <- function() x                 
  
  #SetMatrixInverse is used to solve the inverse of matrix
  #and assign to the variable inverseOfMatrix
  SetMatrixInverse <- function(solve) inverseOfMatrix <<- solve 
  
  #GetMatrixInverse is used to create a list
  GetMatrixInverse <- function() inverseOfMatrix        
  list(set = set, get = get,                    
       SetMatrixInverse = SetMatrixInverse,
       GetMatrixInverse = GetMatrixInverse)
}

# Below function is used to get the cache of the matrix
CacheMatrixSolve<- function(x, ...) {      
  
  
  inverseOfMatrix <- x$GetMatrixInverse()
  
  #checks if the inverse of them matrix exists . If true then it gets it
  if(!is.null(inverseOfMatrix)) {                 
    message("Inverse of the matrix obtained from Cache")
    return(inverseOfMatrix)
  }
  
  #when inverse of matrix does not exist it is first calculated and returned
  data <- x$get()                               
  inverseOfMatrix <- solve(data, ...)
  x$SetMatrixInverse(inverseOfMatrix)
  inverseOfMatrix
}

