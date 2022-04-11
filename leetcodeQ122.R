maxProfit <- function(arr){
    maxprofit <- 0
    for(i in 2:length(arr)){
        if(arr[i] > arr[i - 1]){
            maxprofit <- arr[i] - arr[i - 1] + maxprofit
        }
    }
    maxprofit
}

test <- list(c(7,1,5,3,6,4), c(1,2,3,4,5), c(7,6,4,3,1))
for(t in test){
    print(maxProfit(t))
}