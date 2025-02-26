## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Esta función crea un objeto especial que puede almacenar una matriz y su inversa en caché. 
# Aquí está el desglose de las partes de la función:
# inv es una variable que se inicializa como NULL. Esta variable almacenará la inversa de la matriz.
# set(y) es una función que asigna la matriz y a la variable x y resetea la variable inv a NULL (esto es útil si se cambia la matriz original).
# get() es una función que devuelve la matriz almacenada en x.
# setInverse(inverse) es una función que asigna la inversa inverse a la variable inv.
# getInverse() es una función que devuelve la inversa almacenada en inv.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
# El proposito de esta funcion es calcular la inversa de la matriz y almacenar el resultado en caché 
# A partir de un objeto especal creado por la función makeCacheMatrix


cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
