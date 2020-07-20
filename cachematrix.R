## Put comments here that give an overall description of what your
## functions do
##Week 3 assignment, Github:raguvila

## Write a short comment describing this function
#This function generates a matrix capable to cache its inverse in case needed an retain it.

#Initialize objects x and m, setting the expected default values for both.
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
#Reset the value of inv matrix in case the value of the inverse matrix in case of new X in parent environment 
    set<-function(y){
        x<<-y
        inv<-NULL
    }
    get<-function()x#Defining get function,X will be retrieved from global as it not defined inside ()
    setinvertion <- function(invertion) inv <<- invertion #setting the new invertion value to the parent environment
    getinvertion<-function() inv#Defining get function,inv will be retrieved from parent environment.
    list(set=set,get=get,setinvertion=setinvertion,getinvertion=getinvertion)#Define the results into a list, as will 
                                                                            #be needed to refer in the cachesolve funct.
  }
  

## Write a short comment describing this function
#This function solves for the inverse of the generated matrix  in function makeCacheMatrix above.
## If the inverse is already stored and matrix hasn't changed,cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinvertion()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    #Calculate the inverse if not already stored.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinvertion(inv)
    inv
  }
  