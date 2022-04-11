genParen <- function(n){
    addParen <- function(res, left, right, cand, count){
        print(left)
        print(right)
        print(cand)
        if(left >= 0 & left <= right){
            
            if(left == 0 & right == 0){
                res[[length(res) + 1]] <- cand
                res
            }
            else{
                if(left > 0){
                    cand[count] <- "("
                    res <- addParen(res, left - 1, right, cand, count + 1)
                }

                if(right > 0){
                    cand[count] <- ")"
                    res <- addParen(res, left, right - 1, cand, count + 1)
                }
            }
        }
        print(cand)
        res
    }
    cand <- character(2 * n)
    res <- list()
    res <- addParen(res, n, n, cand, 1)
    res
}

print(genParen(3))
