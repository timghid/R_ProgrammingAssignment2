## Put comments here that give an overall description of what your
## functions do

## CacheMatrix creates and contains the list of functions that will be 
## available to an instance of CacheMatrix

makecachematrix <- function(inputMatrix = matrix()) {

## Pull in matrix and create global instance
        setGlobalInput<-function(tmpMatrix) {
                GlobalInputMatrix<<-tmpMatrix
                GlobalInverseMatrix<<-NULL
        }
## get global copy of input matrix
        getGlobalInput<-function() GlobalInputMatrix
## gets global copy of inverted matrix
        getGlobalInverse<-function() GlobalInverseMatrix
## sets global copy of inverted matrix
        setGlobalInverse<-function(tmpMatrix) GlobalInverseMatrix<<-tmpMatrix
## list the functions that are available
        list(setGlobalInput=setGlobalInput,getGlobalInput=getGlobalInput,
             getGlobalInverse=getGlobalInverse,
             setGlobalInverse=setGlobalInverse )      
}


## cacheSolve will take in a matrix and invert it if it is square
## if the matrix has been inverted before, then it will pull that cached
## copy of the matrix

cacheSolve <- function(inputMatrix, ...) {
        localMatrix<-inputMatrix$getGlobalInput()

## Check if input matrix matches input global and inverted matrix already exists
if((all.equal(localMatrix,GlobalInputMatrix)) & (!is.null(GlobalInverseMatrix))){
        message("You have already inverted this matrix, using cached value")
        return(GlobalInverseMatrix)
} 

## Check to verify the matrix can be inverted, is square
if (nrow(localMatrix) != ncol(localMatrix)) {
        CacheSolve<-paste(nrow(localMatrix)," by ",ncol(localMatrix),"not invertable")
        return(CacheSolve)
} else {
}
## Return a matrix that is the inverse of 'inputMatrix'
        localInvertedMatrix<-solve(localMatrix)
        inputMatrix$setGlobalInverse(localInvertedMatrix)
        localInvertedMatrix
}
