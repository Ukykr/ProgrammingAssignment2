## Put comments here that give an overall description of what your
## functions do

## stores a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        mi<-NULL #initialize cache variable
                set<-function(y){
                        if (!(is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y))){#new matrix       
                                x <<- y
                                mi <<- NULL #empty the cache variable
                        }
                }
                get<-function(){x}#return x from makeCacheMatrix env
                setinverse <-function(inverse){
                        mi<<-inverse
                }   
                getinverse <-function(){mi}
                list(set = set, get = get, 
                     setinverse = setinverse,
                     getinverse = getinverse) #return a named list
                }


## returns inverse or cached inverse of matrix 

cacheSolve <- function(x, ...) {        
        
        mi <- x$getinverse()
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi) ## Return a matrix that is the inverse of 'x'
        }
        amatrix <- x$get()
        mi <- solve(amatrix) #this calulates the inverse of a matrix
        x$setinverse(mi) #store cache in makeCacheMatrix env
        mi
}
