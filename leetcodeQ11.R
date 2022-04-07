#Q4
maxArea <- function(height){
    
    n <- length(height)
    left <- 1
    right <- n
    max_area <- min(height[left], height[right]) * (n - 1)
    
    while(right > left){
        max_area <- max(max_area, min(height[left], height[right]) * (right - left) )
        if(height[left] < height[right]){
            left <- left + 1
        }
        else{
            right <- right - 1
        }
    }
    max_area
}

test <-list(c(1,8,6,2,5,4,8,3,7), 
            c(16,1346,1,137,14,61,46,13,71,357,157,13,713,461,46,13,61,71,37,14,61,3461,7137),
            c(1346,146,134,61,461,7,17,13,5713,751,71,57,17,17,1,57,157,13,713,7,1357,157,1357))

for(i in test){
    print(maxArea(i))
}