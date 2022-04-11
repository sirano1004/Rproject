genParen <- function(n){
    addParen <- function(res, left, right, cand, count){
        if(left > 0 & left <= right){
        
            if(left == 0 & right == 0){
                res[[length(res) + 1]] <- cand
            }
            else{
                if(left > 0){
                    cand[count] <- "("
                    addParen(res, left - 1, right, cand, count + 1)
                }

                if(right > 0){
                    cand[count] <- ")"
                    addParen(res, left, right - 1, cand, count + 1)
                }
            }
        }
    }
    cand <- character(2 * n)
    res <- list()
    addParen(res, n, n, cand, 0)
    res
}


print(genParen(2))

