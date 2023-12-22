## makeCacheMatrix 函數創建一個特殊的 "matrix" 對象，這個對象可以緩存它的逆。
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL  # 初始化逆矩陣為 NULL
    
    # 設置矩陣
    set <- function(y) {
        x <<- y
        inverse <<- NULL  # 當設置新的矩陣時，重置逆矩陣
    }
    
    # 獲取矩陣
    get <- function() x
    
    # 設置逆矩陣
    setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
    
    # 獲取逆矩陣
    getInverse <- function() inverse
    
    # 返回一個列表，包含上述函式
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve 函數計算特殊 "matrix" 的逆。如果這個逆已經被計算過，則從緩存中提取。
cacheSolve <- function(x, ...) {
    ## 從 x 中獲取已緩存的逆矩陣
    inverse <- x$getInverse()
    
    ## 如果逆矩陣已經緩存，則直接返回
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## 計算逆矩陣
    data <- x$get()
    inverse <- solve(data, ...)
    
    ## 緩存逆矩陣
    x$setInverse(inverse)
    
    ## 返回逆矩陣
    inverse
}
