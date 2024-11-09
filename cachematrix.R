## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##library(MASS) is used to calculate the inverse for non squared as well as squared matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function()x
        setinv<-function(inverse)inv<<-inverse
        getinv<-function(){
                inver<-ginv(x)
                inver%*%x #function to obtain the inverse of the matrix
        }
        list(set=set,get=get,
        setinv=setinv,
        getinv=getinv)

}


## Write a short comment describing this function
##This is used to get the catche data
cacheSolve <- function(x, ...) {##gets catche data
        inv<-x$getinv()
        if(!is.null(inv)){#checking whether the inverse is null
                message("getting cached data!")
                return(inv) #returns inverse value
        }
        data<-x$get()
        inv<-solve(data,...) # calculate inverse
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
