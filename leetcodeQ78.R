Subsets <- function(nums){
    res <- list(list())
    n <- length(nums)
    Append <- function(arr, nums){
        n <- length(nums)
        for(k in 1:n){
            arr <- append(arr, nums[k])
            res[[length(res) + 1]] <- arr
            if(k + 2 <= n){
                for(l in (k + 2):n){
                    res <- Append(arr, nums[l:n])
                }
            }
        }
        res
    }
    for(i in 1:n){
        temp <- list(nums[i])
        res[[length(res) + 1]] <- temp
        if(i + 1 <= n){
            for(j in (i + 1):n){
                res <- Append(temp, nums[j:n])
            }
        }
    }
    res
}


test <- list(c(1,2,3,4),
             c(1,5,6,7,0),
             c(0))

for(t in test){
    print(Subsets(t))
}