rotate <- function(mat){
    mat <- transpose(mat)
    mat <- reflex(mat)
    mat
}

transpose <- function(mat){
    # mat is a n by m matrix
    n <- length(mat[,1])
    m <- length(mat[1,])
    for(i in 1:n){
        if((i + 1) <= m){
            for(j in (i + 1):m){
                temp <- mat[i, j]
                mat[i, j] <- mat[j, i]
                mat[j, i] <- temp
            }
        }
    }
    mat
}

reflex <- function(mat){
    # mat is a n by m matrix
    n <- length(mat[,1])
    m <- length(mat[1,])
    for(i in 1:n){
        for(j in 1:(m %/% 2)){
            temp <- mat[i, j]
            mat[i, j] <- mat[i, m - j + 1]
            mat[i, m - j + 1] <- temp
        }
    }
    mat
}

n <- 5
mat <- matrix(data = 1:n^2, nrow = n, ncol = n, byrow = TRUE)
print(mat)
print(rotate(mat))